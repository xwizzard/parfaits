(ns ogres.server.core
  (:gen-class)
  (:refer-clojure :exclude [send])
  (:import [clojure.lang IPersistentMap]
           [java.awt RenderingHints]
           [java.awt.image BufferedImage]
           [java.io ByteArrayOutputStream ByteArrayInputStream]
           [java.net InetAddress URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.nio ByteBuffer]
           [java.time Duration]
           [javax.imageio IIOImage ImageIO ImageWriteParam]
           [org.msgpack.core MessagePack])
  (:require [clojure.string :refer [starts-with? upper-case]]
            [cognitect.transit :as transit]
            [datascript.core]
            [datascript.transit :refer [read-handlers write-handlers]]
            [io.pedestal.connector :as conn]
            [io.pedestal.http.jetty :as jetty]
            [io.pedestal.log :as log]
            [io.pedestal.metrics :as metrics]
            [io.pedestal.websocket :as ws]))

(def state! (atom {}))
(def opts-reader {:handlers read-handlers})
(def opts-writer {:handlers write-handlers})

(metrics/gauge
 :ogres.server.conns/count
 {::metrics/description "The number of connections persisted in state."}
 (fn [] (count (:conns (deref state!)))))

(metrics/gauge
 :ogres.server.rooms/count
 {::metrics/description "The number of rooms persisted in state."}
 (fn [] (count (:rooms (deref state!)))))

(def stat-size-message!
  (metrics/histogram
   :ogres.server.message/size
   {::metrics/description "The size of an event forward to one or more connections."
    ::metrics/unit "By"}))

(def stat-size-image!
  (metrics/histogram
   :ogres.server.image/size
   {::metrics/description "The size of an image forwarded to another connection."
    ::metrics/unit "By"}))

(defn room-create-key []
  (let [keys (:rooms (deref state!))]
    (loop []
      (let [code (->> (datascript.core/squuid) (str) (take-last 4) (apply str) (upper-case))]
        (if (contains? keys code) (recur) code)))))

(defn room-create [data room uuid session]
  (-> data
      (update-in [:conns uuid] assoc :session session :room room)
      (update-in [:rooms room] assoc :conns #{uuid} :host uuid)))

(defn room-join [data room uuid session]
  (-> data
      (update-in [:conns uuid] assoc :session session :room room)
      (update-in [:rooms room :conns] conj uuid)))

(defn room-leave [data uuid]
  (let [room (get-in data [:conns uuid :room])]
    (if-let [host (get-in data [:rooms room :host])]
      (cond-> data
        true             (update :conns dissoc uuid)
        (= uuid host)    (update :rooms dissoc room)
        (not= uuid host) (update-in [:rooms room :conns] disj uuid))
      (update data :conns dissoc uuid))))

(defn uuid->room [data uuid]
  (let [room (get-in data [:conns uuid :room])]
    (get-in data [:rooms room])))

(defn uuid->conns [data uuid]
  (let [room  (get-in data [:conns uuid :room])
        uuids (get-in data [:rooms room :conns])]
    (into [] (comp (map (:conns data)) (map :session))
          (disj uuids uuid))))

(defn encode [value]
  (let [stream (ByteArrayOutputStream.)
        writer (transit/writer stream :json opts-writer)]
    (transit/write writer value)
    (.toString stream)))

(defn send [session message]
  (when (.isOpen session)
    (condp instance? message
      String
      (.sendText (.getAsyncRemote session) message)
      ByteBuffer
      (.sendBinary (.getAsyncRemote session) message)
      IPersistentMap
      (.sendText (.getAsyncRemote session) (encode message)))))

(defn send-many [sessions message]
  (condp instance? message
    String
    (doseq [session sessions :when (.isOpen session)]
      (.sendText (.getAsyncRemote session) message))
    ByteBuffer
    (doseq [session sessions :when (.isOpen session)]
      (.sendBinary (.getAsyncRemote session) message))
    IPersistentMap
    (let [serialized (encode message)]
      (doseq [session sessions :when (.isOpen session)]
        (.sendText (.getAsyncRemote session) serialized)))))

;; -- Thumbnails --
;; A small, self-hosted stand-in for a fraction of what weserv/images
;; (https://github.com/weserv/images) offers as a public service: given a
;; source image URL, fetch it server-side, center-crop it to a square, and
;; resize it -- just enough to generate a real cropped thumbnail for a
;; URL-added image (see provider/image.cljs's use-image-url-adder), without
;; taking on a third-party network dependency. Deliberately narrow: no
;; format negotiation, rotation, filters, or watermarking -- just
;; ?url=&w=&fit=cover, reimplemented with the JDK's own java.net.http and
;; javax.imageio (no new dependency).

(def ^:private thumbnail-default-size 256)
(def ^:private thumbnail-request-timeout (Duration/ofSeconds 5))

(def ^:private http-client
  (delay (-> (HttpClient/newBuilder) (.connectTimeout thumbnail-request-timeout) (.build))))

(defn ^:private safe-uri
  "Parses url-str as an http(s) URI whose host resolves to only public
   addresses, or nil if it's malformed, uses an unsupported scheme, or
   resolves to a loopback/link-local/site-local/multicast/wildcard address
   -- a basic guard against this server being made to fetch its own
   internal network on an attacker's behalf (SSRF). Known limitation: this
   checks the resolved address at validation time, not at the moment of
   the actual connection, so it doesn't close a DNS-rebinding race --
   adequate for a self-hosted, hobby-scale deployment, not a hardened
   multi-tenant one."
  [url-str]
  (try
    (let [uri (URI. url-str)
          scheme (some-> (.getScheme uri) (.toLowerCase))]
      (when (and (some? url-str) (contains? #{"http" "https"} scheme) (some? (.getHost uri)))
        (let [addresses (InetAddress/getAllByName (.getHost uri))]
          (when (and (seq addresses)
                     (not-any?
                      (fn [^InetAddress addr]
                        (or (.isLoopbackAddress addr)
                            (.isLinkLocalAddress addr)
                            (.isSiteLocalAddress addr)
                            (.isMulticastAddress addr)
                            (.isAnyLocalAddress addr)))
                      addresses))
            uri))))
    (catch Exception _ nil)))

(defn ^:private fetch-image-bytes
  "Fetches the bytes at the given URI with a short timeout, returning them
   only on a 200 response with an image/* content-type -- nil otherwise."
  [^URI uri]
  (try
    (let [request  (-> (HttpRequest/newBuilder uri) (.timeout thumbnail-request-timeout) (.GET) (.build))
          response (.send @http-client request (HttpResponse$BodyHandlers/ofByteArray))
          type     (.orElse (.firstValue (.headers response) "content-type") "")]
      (when (and (= (.statusCode response) 200) (starts-with? type "image/"))
        (.body response)))
    (catch Exception _ nil)))

(defn ^:private crop-square
  "Returns img cropped to a centered square using its larger dimension --
   the same 'cover' convention provider/image.cljs's client-side
   create-thumbnail already uses for local uploads, reimplemented here for
   server-side URL thumbnails."
  [^BufferedImage img]
  (let [w (.getWidth img) h (.getHeight img) len (min w h)]
    (.getSubimage img (quot (- w len) 2) (quot (- h len) 2) len len)))

(defn ^:private resize-square
  "Returns a new size x size RGB image (no alpha -- JPEG has none; this
   only affects the generated thumbnail, never the full image, which the
   browser renders directly from the original URL with alpha intact)
   containing img scaled with bilinear interpolation."
  [^BufferedImage img size]
  (let [out (BufferedImage. size size BufferedImage/TYPE_INT_RGB)
        g   (.createGraphics out)]
    (.setRenderingHint g RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_BILINEAR)
    (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)
    (.drawImage g img 0 0 size size nil)
    (.dispose g)
    out))

(defn ^:private encode-jpeg
  "Encodes img as a JPEG byte array at the given compression quality
   (0.0-1.0), matching the local-upload pipeline's own JPEG quality (0.80)."
  [^BufferedImage img quality]
  (let [writer (.next (ImageIO/getImageWritersByFormatName "jpeg"))
        param  (doto (.getDefaultWriteParam writer)
                 (.setCompressionMode ImageWriteParam/MODE_EXPLICIT)
                 (.setCompressionQuality quality))
        stream (ByteArrayOutputStream.)
        output (ImageIO/createImageOutputStream stream)]
    (.setOutput writer output)
    (.write writer nil (IIOImage. img nil nil) param)
    (.dispose writer)
    (.close output)
    (.toByteArray stream)))

(defn ^:private clamp-size [value]
  (let [n (try (Integer/parseInt value) (catch Exception _ thumbnail-default-size))]
    (max 16 (min thumbnail-default-size n))))

(defn handle-thumbnail
  "Serves a cropped, resized JPEG thumbnail of the image at ?url=, sized to
   ?w= (defaults to and capped at 256 -- this only needs to serve this
   app's own thumbnail size, not be a general-purpose resizer)."
  [{{:keys [url w]} :params}]
  (if-let [uri (and url (safe-uri url))]
    (if-let [bytes (fetch-image-bytes uri)]
      (if-let [decoded (try (ImageIO/read (ByteArrayInputStream. bytes)) (catch Exception _ nil))]
        (let [size (clamp-size (or w (str thumbnail-default-size)))
              jpeg (-> decoded crop-square (resize-square size) (encode-jpeg 0.80))]
          {:status  200
           :headers {"Content-Type" "image/jpeg"
                     "Access-Control-Allow-Origin" "*"
                     "Cache-Control" "public, max-age=86400"}
           :body    jpeg})
        ;; Every branch below carries the same CORS header as the success
        ;; case above -- otherwise a failure here (bad URL, dead link,
        ;; corrupt image) surfaces to the browser as an opaque "blocked by
        ;; CORS policy" console error instead of this response's own,
        ;; actually-useful status/body, since the browser can't read a
        ;; cross-origin error response missing that header at all.
        {:status 422 :headers {"Access-Control-Allow-Origin" "*"} :body "Unsupported or corrupt image."})
      {:status 502 :headers {"Access-Control-Allow-Origin" "*"} :body "Failed to fetch image from that URL."})
    {:status 400 :headers {"Access-Control-Allow-Origin" "*"} :body "Missing or disallowed url parameter."}))

(defn handle-root [_]
  {:status 405})

(defn handle-ws [{{host :host join :join} :params}]
  (let [data (deref state!)]
    (cond (and host join)
          {:status 400}
          (and host (get-in data [:rooms host]))
          {:status 403}
          (and join (nil? (get-in data [:rooms (upper-case join)])))
          {:status 404})))

(defn handle-ws-open [session _]
  (.setMaxTextMessageBufferSize   session 1e7)
  (.setMaxBinaryMessageBufferSize session 1e7)
  (let [params (.getRequestParameterMap session)
        host (some-> params (.get "host") (.get 0))
        join (some-> params (.get "join") (.get 0) (upper-case))
        uuid (.getId session)]
    (cond (some? host)
          (do (swap! state! room-create host uuid session)
              (send session {:type :event :src uuid :dst uuid :data {:name :session/created :room host :uuid uuid}}))
          (some? join)
          (let [data (swap! state! room-join join uuid session)]
            (send session {:type :event :src uuid :dst uuid :data {:name :session/joined :room join :uuid uuid}})
            (send-many (uuid->conns data uuid) {:type :event :src uuid :data {:name :session/join :room join :uuid uuid}}))
          :else
          (let [room (room-create-key)]
            (swap! state! room-create room uuid session)
            (send session {:type :event :src uuid :dst uuid :data {:name :session/created :room room :uuid uuid}})))
    session))

(defn handle-ws-close [session _ _]
  (let [data (deref state!)
        uuid (.getId session)
        room (get-in data [:conns uuid :room])
        room (get-in data [:rooms room])
        conns (uuid->conns data uuid)]

    ;; The connection has been closed; close the associated session.
    (when (.isOpen session)
      (.close session))

    (if (= (:host room) uuid)
      ;; The host has left, destroying the session entirely. Find and close
      ;; all remaining connections.
      (doseq [session conns :when (.isOpen session)]
        (.close session))

      ;; Notify all other connections in the same session that a connection
      ;; has been closed.
      (send-many conns {:type :event :data {:name :session/leave :uuid uuid}}))

    ;; Update the sessions to remove the closing connection, potentially
    ;; also removing the room and closing all related connections within.
    (swap! state! room-leave uuid)))

(defn handle-ws-error [_ _ error]
  (log/error :message (.getMessage error)))

(defn handle-ws-text [session message]
  (stat-size-message! (.length message))
  (let [data (deref state!)
        uuid (.getId session)]
    (if (uuid->room data uuid)
      (let [stream (ByteArrayInputStream. (.getBytes message))
            reader (transit/reader stream :json opts-reader)
            decode (transit/read reader)]
        (if-let [uuid (:dst decode)]
          (send      (get-in data [:conns uuid :session]) message)
          (send-many (uuid->conns data uuid) message))))))

(defn handle-ws-binary [session message]
  (stat-size-image! (.remaining message))
  (let [data (deref state!)
        uuid (.getId session)]
    (if (uuid->room data uuid)
      (let [unpacker (MessagePack/newDefaultUnpacker message)
            max-keys (.unpackMapHeader unpacker)]
        (loop [idx 0]
          (if (< idx max-keys)
            (if (= (.unpackString unpacker) "dst")
              (let [dest (.unpackString unpacker)]
                (when-let [session (get-in data [:conns dest :session])]
                  (send session message)))
              (do (.skipValue unpacker)
                  (recur (inc idx))))))
        (.close unpacker)))))

(def upgrade-ws
  (ws/websocket-upgrade
   {:on-open   handle-ws-open
    :on-close  handle-ws-close
    :on-error  handle-ws-error
    :on-text   handle-ws-text
    :on-binary handle-ws-binary
    :idle-timeout-ms (* 1000 60 3)}))

(defn create-connector
  ([] (create-connector {}))
  ([{:keys [port] :or {port 5000}}]
   (-> (conn/default-connector-map port)
       (conn/with-default-interceptors)
       (conn/with-routes
         #{["/"          :get [handle-root]]
           ["/ws"        :get [handle-ws upgrade-ws]]
           ["/thumbnail" :get [handle-thumbnail]]})
       (jetty/create-connector nil))))

(defn -main [port]
  (conn/start! (create-connector {:port (Integer/parseInt port)})))

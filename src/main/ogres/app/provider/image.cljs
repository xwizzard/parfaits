(ns ogres.app.provider.image
  (:require [clojure.string :as string]
            [datascript.core :as ds]
            [ogres.app.const :as const]
            [ogres.app.provider.dispatch :refer [use-dispatch]]
            [ogres.app.provider.events :as events]
            [ogres.app.provider.idb :as idb]
            [ogres.app.provider.state :as state]
            [uix.core :as uix :refer [defui $]]))

(def ^:private hash-fn "SHA-1")

(defn ^:private url?
  "True if the given :image/hash value is a live URL reference (added via
   use-image-url-adder, or a bundled data: URI seeded at boot -- see
   provider/state.cljs's seed-props-images) rather than a SHA-1 checksum
   key into IndexedDB -- SHA-1 hex digests never start with 'http' or
   'data:', so this is an unambiguous discriminator."
  [s]
  (or (string/starts-with? s "http://")
      (string/starts-with? s "https://")
      (string/starts-with? s "data:")))

(def ^:private context (uix/create-context))

(defn ^:private decode-digest
  "Returns a hash string of the given digest array."
  [^js/Uint8Array digest]
  (.. (js/Array.from digest)
      (map (fn [s] (.. s (toString 16) (padStart 2 "0"))))
      (join "")))

(defn ^:private create-hash
  "Returns a Promise which resolves with a hash string for the
   image data given as a Blob object."
  [^js/Blob image]
  (-> (.arrayBuffer image)
      (.then (fn [buf] (js/crypto.subtle.digest hash-fn buf)))
      (.then (fn [dig] (js/Uint8Array. dig)))
      (.then decode-digest)))

(defn ^:private create-thumbnail
  "Returns a <canvas> element with the contents of the src <canvas> cropped
   and resized to the given dimensions. Accepts two points given as {Ax Ay}
   and {Bx By} which define a square region of the source image to use as
   the crop. When no region is given, the crop is centered horizontally and
   to the top."
  ([^js/HTMLCanvasElement src len]
   (let [cx (.-width src) cy (.-height src)]
     (cond (or (= cx cy) (and (<= cx len) (<= cy len)))
           (create-thumbnail src len 0 0 cx cy)
           (> cx cy)
           (create-thumbnail src len (/ (- cx cy) 2) 0 (/ (+ cx cy) 2) cy)
           (> cy cx)
           (create-thumbnail src len 0 0 cx cx))))
  ([^js/HTMLCanvasElement src len ax ay bx by]
   (let [canvas (js/document.createElement "canvas")]
     (set! (.-width canvas) len)
     (set! (.-height canvas) len)
     (.drawImage (.getContext canvas "2d") src ax ay (- bx ax) (- by ay) 0 0 len len)
     canvas)))

(defn ^:private create-canvas
  "Returns a <canvas> with the contents of the given image drawn on it."
  [^js/ImageBitmap image]
  (let [canvas (js/document.createElement "canvas")]
    (set! (.-width canvas) (.-width image))
    (set! (.-height canvas) (.-height image))
    (.drawImage (.getContext canvas "2d") image 0 0 (.-width image) (.-height image))
    canvas))

(defn ^:private extract-image
  "Returns the image and metadata associated with the <canvas> element passed
   given an image type and quality ratio in the form of a map whose keys are
   'data', 'hash', 'width', and 'height'."
  ([^js/HTMLCanvasElement canvas]
   (extract-image canvas "image/jpeg" 0.80))
  ([^js/HTMLCanvasElement canvas type quality]
   (js/Promise.
    (fn [resolve]
      (.toBlob
       canvas
       (fn [blob]
         (.then
          (create-hash blob)
          (fn [hash]
            (resolve
             {:data   blob
              :hash   hash
              :width  (.-width canvas)
              :height (.-height canvas)})))) type quality)))))

(defn ^:private process-file
  "Returns a Promise.all which resolves with three elements:
     1. The filename
     2. The image extracted from the file
     3. The image, cropped and resized to be used as a thumbnail"
  ([^js/File file]
   (process-file file "image/jpeg" 0.80))
  ([^js/File file type quality]
   (.then (js/createImageBitmap file)
          (fn [image]
            (let [canvas (create-canvas image)]
              (js/Promise.all
               #js [(js/Promise.resolve (.-name file))
                    (extract-image canvas type quality)
                    (extract-image (create-thumbnail canvas 256) type quality)]))))))

(defn ^:private create-store-records
  "Returns a two-element vector containing the Javascript objects which
   will be persisted to IndexedDB. Each object contains the image data
   represented as a Blob and a digest of the image represented as a
   SHA-1 string."
  [[_ image thumb]]
  [#js {"checksum" (:hash image) "data" (:data image)}
   #js {"checksum" (:hash thumb) "data" (:data thumb)}])

(defn ^:private create-state-record
  "Returns a description of an image to be persisted to DataScript state,
   containing the digest, filename, size in kb, width, and height."
  [filename image]
  {:hash   (:hash image)
   :name   filename
   :size   (.-size (:data image))
   :width  (:width image)
   :height (:height image)})

(defn ^:private create-state-records
  "Returns a two-element vector containing descriptions of images to be
   persisted to DataScript state. The first is the original image and
   the second is the thumbnail."
  [[filename image thumb]]
  [(create-state-record filename image)
   (create-state-record filename thumb)])

(defui provider [props]
  (let [conn           (uix/use-context state/context)
        read           (idb/use-reader "images")
        write          (idb/use-writer "images")
        loading        (atom {})
        publish        (events/use-publish)
        dispatch       (use-dispatch)
        [urls set-url] (uix/use-state {})
        on-request
        (uix/use-callback
         (fn [hash]
           (let [url (get urls hash)]
             (if (not url)
               (when (not (get @loading hash))
                 (swap! loading assoc hash true)
                 (-> (read hash)
                     (.then (fn [rec] (js/URL.createObjectURL (.-data rec))))
                     (.then (fn [url] (set-url (fn [urls] (assoc urls hash url)))))
                     (.catch (fn [] (publish :image/request hash)))))
               url))) ^:lint/disable [])]
    (events/use-subscribe :image/create-token
      (uix/use-callback
       (fn [blob]
         (-> (js/createImageBitmap blob)
             (.then
              (fn [image]
                (let [canvas (create-canvas image)]
                  (js/Promise.all
                   #js [(js/Promise.resolve "")
                        (extract-image canvas)
                        (extract-image (create-thumbnail canvas 256))]))))
             (.then
              (fn [images]
                (.then
                 (write :put (create-store-records images))
                 (constantly images))))
             (.then
              (fn [[filename image thumbnail]]
                (dispatch
                 :token-images/create-many
                 [[(create-state-record filename image)
                   (create-state-record filename thumbnail)]] true))))) [write dispatch]))
    (events/use-subscribe :image/cache
      (uix/use-callback
       (fn [image]
         (-> (create-hash image)
             (.then (fn [hash] (write :put [#js {"checksum" hash "data" image}]) hash))
             (.then (fn [hash] (set-url (fn [urls] (assoc urls hash (js/URL.createObjectURL image)))))))) [write]))
    (events/use-subscribe :image/change-thumbnail
      (uix/use-callback
       (fn [hash [ax ay bx by :as rect]]
         (let [entity (ds/entity (ds/db conn) [:image/hash hash])]
           (-> (read hash)
               (.then (fn [rec] (js/createImageBitmap (.-data rec))))
               (.then
                (fn [src]
                  (-> (create-canvas src)
                      (create-thumbnail 256 ax ay bx by)
                      (extract-image))))
               (.then
                (fn [out]
                  (let [thumb (:image/hash (:image/thumbnail entity))]
                    (js/Promise.all
                     #js [(write :put [#js {"checksum" (:hash out) "data" (:data out)}])
                          (if (not= hash thumb)
                            (write :delete [thumb]))
                          out]))))
               (.then
                (fn [[_ _ data]]
                  (dispatch :token-images/change-thumbnail hash data rect)))))) [read dispatch conn write]))
    ($ context {:value [urls on-request]}
      (:children props))))

(defn use-image-uploader
  "React hook which provides a function that accepts one or more uploaded
   File objects, processes those files to be used as images, and persists
   them to storage and state."
  [{:keys [type]}]
  (let [dispatch (use-dispatch)
        publish (events/use-publish)
        entity (state/use-query [:user/host])
        write (idb/use-writer "images")
        xform (if (contains? #{:props :scene} type)
                ;; WebP (unlike JPEG, process-file's default) keeps an
                ;; alpha channel, which board/background pieces need --
                ;; Gloomhaven-style map tiles are irregular jigsaw shapes
                ;; with a transparent background around them. Matching
                ;; props' own format+quality here also means the same
                ;; source file uploaded to either gallery now hashes the
                ;; same way, so a saved :image/cell-px or :image/anchor
                ;; calibration carries over between board pieces and
                ;; props for that image, not just within one gallery.
                (map (fn [file] (process-file file "image/webp" 0.80)))
                (map process-file))]
    (if (:user/host entity)
      (fn [files]
        (-> (js/Promise.all (into-array (into [] xform files)))
            (.then
             (fn [files]
               (let [records (sequence (mapcat create-store-records) files)]
                 (.then (write :put records) (constantly files)))))
            (.then
             (fn [files]
               (let [records (into [] (map create-state-records) files)]
                 (case type
                   :token (dispatch :token-images/create-many records)
                   :scene (dispatch :scene-images/create-many records)
                   :props (dispatch :props-images/create-many records)))))))
      (fn [files]
        (.then (js/Promise.all (into-array (into [] (map process-file) files)))
               (fn [files]
                 (doseq [[_ image _] files]
                   (publish :image/create (:data image)))))))))

(defn use-image
  "React hook which accepts a string that uniquely identifies an image
   and returns a URL string once the image has been retrieved from
   local storage or from over the network. Calls to this hook are cached
   and simultaneous calls do not result in duplicate retrievals.
   ```
   (let [url (use-image 'ded8f539f84b9eb1bce78836aa017affae7661c3')]
     ($ :img {:src url}))
   ```"
  [hash]
  (let [[urls on-request] (uix/use-context context)]
    (uix/use-effect
     (fn [] (when (and (some? hash) (not (url? hash))) (on-request hash))) [on-request hash])
    (if (and (some? hash) (url? hash)) hash (get urls hash))))

(defn ^:private measure-image-url
  "Returns a Promise which resolves with {:width :height} once the image at
   url has loaded, or rejects if it fails to load (a bad URL, a 404, or a
   host that blocks hotlinking). No CORS is needed for this -- naturalWidth/
   naturalHeight are readable on cross-origin images even without it; only
   pixel data reads (e.g. via <canvas>) are blocked without CORS."
  [url]
  (js/Promise.
   (fn [resolve reject]
     (let [img (js/Image.)]
       (set! (.-onload img) (fn [] (resolve {:width (.-naturalWidth img) :height (.-naturalHeight img)})))
       (set! (.-onerror img) (fn [] (reject (js/Error. "Failed to load image from that URL."))))
       (set! (.-src img) url)))))

(defn ^:private url->name
  "Derives a display name for a URL-added image from its last path
   segment, falling back to the full URL if none is found."
  [url]
  (or (last (re-seq #"[^/]+" (first (string/split url #"[?#]"))))
      url))

(defn ^:private fetch-thumbnail
  "Returns a Promise which resolves with a cropped, resized thumbnail of
   the image at url, fetched from this app's own /thumbnail backend route
   (see ogres.server.core/handle-thumbnail) and hashed the same way a local
   upload's thumbnail is -- the resolved map has the same shape
   extract-image produces locally ({:data :hash :width :height}), so it
   flows through the existing create-state-record helper unchanged. Rejects
   on any backend/network failure so the caller can fall back to an
   uncropped self-referential thumbnail instead of failing the whole add."
  [url]
  (-> (js/fetch (str const/THUMBNAIL-URL "?url=" (js/encodeURIComponent url) "&w=256"))
      (.then (fn [res] (if (.-ok res) (.blob res) (throw (js/Error. "thumbnail request failed")))))
      (.then (fn [blob] (.then (create-hash blob) (fn [hash] {:data blob :hash hash :width 256 :height 256}))))))

(defn use-image-url-adder
  "React hook returning a function that accepts an image URL, measures its
   dimensions, generates and caches a cropped thumbnail via this app's own
   /thumbnail backend route (falling back to an uncropped self-reference if
   that route is unreachable), and adds it as an image of the given type
   (:token, :scene, or :props) using the URL itself as the full image's
   :image/hash -- see ogres.app.provider.image/url?, which makes that hash
   render directly from the URL instead of going through IndexedDB.

   Host-only, matching local upload's host-authoritative write path --
   unlike file upload there is no guest-forwarding branch for this, since
   scene/props galleries are already effectively host-only and a guest can
   still use local file upload for their own token."
  [{:keys [type]}]
  (let [dispatch (use-dispatch)
        entity (state/use-query [:user/host])
        write (idb/use-writer "images")]
    (fn [url]
      (if-not (:user/host entity)
        (js/Promise.reject (js/Error. "Only the host can add images by URL."))
        (.then (measure-image-url url)
               (fn [{:keys [width height]}]
                 (let [name (url->name url)
                       image {:hash url :name name :size 0 :width width :height height}]
                   (-> (fetch-thumbnail url)
                       (.then (fn [thumb]
                                (.then (write :put [#js {"checksum" (:hash thumb) "data" (:data thumb)}])
                                       (constantly (create-state-record name thumb)))))
                       (.catch (constantly image))
                       (.then (fn [thumb-record]
                                (case type
                                  :token (dispatch :token-images/create-many [[image thumb-record]])
                                  :scene (dispatch :scene-images/create-many [[image thumb-record]])
                                  :props (dispatch :props-images/create-many [[image thumb-record]]))))))))))))

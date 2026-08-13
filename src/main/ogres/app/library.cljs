(ns ogres.app.library
  "Per-gallery (Tokens/Props/Scene) image asset libraries -- a small,
   portable EDN file recording an image's identity (SHA-1 hash, see
   ogres.app.provider.image's hash-fn), its location (the URL it was
   added from, when it has one; absent for a local upload), and its
   already-existing 'fingerprint' calibration (:image/cell-px,
   :image/anchor, :image/rotation) -- NOT the image bytes themselves.
   Deliberately a reuse helper, not an archive, per direct correction:
   'The library feature should be isolated per gallery type... it's
   effectively an import/asset reuse helper, not an archive.'

   Mirrors panel_character.cljs's export-character!/import-character!
   and panel_game_type_builder.cljs's export-game-type!/import-game-type!
   exactly: plain EDN via pr-str/read-string, no binary bundling, no
   React/hooks -- pure data in, pure data out, easy to unit-test (see
   ogres.app.library-test).

   :image/anchor is a Vec2 (ogres.app.vec), which does NOT survive a
   raw pr-str/read-string round-trip -- its own printer emits a
   #vec2[x,y] tagged literal and no reader is registered for that tag
   anywhere in this codebase, so read-string would throw. Entries carry
   it as a plain [x y] instead (Vec2 is ISeqable), rebuilt with
   (Vec2. x y) by the caller on import."
  (:require [cljs.reader :refer [read-string]]
            [clojure.string :as string]
            [ogres.app.provider.image :refer [url?]]
            [ogres.app.util :as util]))

(def format-id "parfaits.image-library")
(def format-version 1)

(def galleries
  "The three galleries a library can belong to -- kept isolated from one
   another per direct request; a library exported from one is refused
   by another's import control."
  #{:token :props :scene})

;; ---- export: pulled image entity -> plain EDN-safe entry ----

(defn image->entry
  "A pulled image entity map (see each gallery panel's own query, which
   must include at least :image/hash and the calibration attributes
   below to get a useful entry) -> a plain library entry. Only :hash is
   guaranteed; every other key is omitted rather than written as nil or
   false, so a partially-calibrated image doesn't round-trip fake
   values, and so sanitize-entry's own absence checks on import stay
   meaningful."
  [image]
  (cond-> {:hash (:image/hash image)}
    (some? (:image/name image))
    (assoc :name (:image/name image))
    (some? (:image/width image))
    (assoc :width (:image/width image))
    (some? (:image/height image))
    (assoc :height (:image/height image))
    (some? (:image/size image))
    (assoc :size (:image/size image))
    (some? (:image/cell-px image))
    (assoc :cell-px (:image/cell-px image))
    (some? (:image/rotation image))
    (assoc :rotation (:image/rotation image))
    (some? (:image/anchor image))
    (assoc :anchor (vec (:image/anchor image)))
    ;; A URL-added image's :image/hash IS the URL it was fetched from
    ;; (see ogres.app.provider.image's own url? and use-image-url-adder)
    ;; -- there's no separate location attribute to read. A local
    ;; upload's hash is a bare SHA-1 digest, never fetchable, so it
    ;; carries no :location at all.
    (url? (:image/hash image))
    (assoc :location (:image/hash image))
    (true? (:image/public image))
    (assoc :public true)
    (some? (:token-image/default-label image))
    (assoc :default-label (:token-image/default-label image))
    (some? (:token-image/url image))
    (assoc :url (:token-image/url image))))

(defn manifest
  "entries (a seq of image->entry maps) + a gallery keyword + a display
   name -> the full file contents, ready for pr-str. `library-name`,
   not `name` -- clojure.core/name (keyword -> string) is used
   elsewhere in this file and a shadowing local here would be an easy
   silent bug to introduce later."
  [{:keys [gallery library-name entries]}]
  {:library/format format-id
   :library/format-version format-version
   :library/gallery gallery
   :library/name library-name
   :library/images (vec entries)})

;; ---- import: untrusted data -> sanitized entry/manifest, or nil ----
;; Modeled directly on events.cljs's own import-character-text/
;; import-character-keyword/import-character-card: reject-don't-coerce
;; the wrong type, drop only the one unusable value/entry rather than
;; fail the whole import. A bad library file must yield a smaller
;; import, never a failed transaction or a corrupted local entity.

(defn ^:private sane-text
  "Untrusted value -> trimmed string, or nil for anything blank or
   non-string. Not `str`, which happily renders nil as \"\" and a map
   as its literal source -- both of which would then show up verbatim
   in the UI or get written into the DB as if they were real."
  [x]
  (if (string? x)
    (let [t (string/trim x)]
      (if (seq t) t))))

(defn ^:private sane-hash
  [x]
  (sane-text x))

(defn ^:private sane-number
  [x]
  (if (number? x) x))

(defn ^:private sane-anchor
  "A 2-element sequential of two numbers -> [x y], else nil. Guards
   against a hand-edited or truncated file supplying the wrong shape
   entirely, not just the wrong value."
  [x]
  (if (and (sequential? x) (= (count x) 2) (every? number? x))
    (vec x)))

(defn ^:private sane-gallery
  [x]
  (if (contains? galleries x) x))

(defn sanitize-entry
  "One untrusted entry map -> a clean entry, or nil to drop it entirely
   -- dropped when it has no usable hash, since every other field is
   meaningless without one."
  [entry]
  (if (map? entry)
    (if-let [hash (sane-hash (:hash entry))]
      (cond-> {:hash hash}
        (some? (sane-text (:name entry)))
        (assoc :name (sane-text (:name entry)))
        (some? (sane-number (:width entry)))
        (assoc :width (sane-number (:width entry)))
        (some? (sane-number (:height entry)))
        (assoc :height (sane-number (:height entry)))
        (some? (sane-number (:size entry)))
        (assoc :size (sane-number (:size entry)))
        (some? (sane-number (:cell-px entry)))
        (assoc :cell-px (sane-number (:cell-px entry)))
        (some? (sane-number (:rotation entry)))
        (assoc :rotation (sane-number (:rotation entry)))
        (some? (sane-anchor (:anchor entry)))
        (assoc :anchor (sane-anchor (:anchor entry)))
        (some? (sane-text (:location entry)))
        (assoc :location (sane-text (:location entry)))
        (true? (:public entry))
        (assoc :public true)
        (some? (sane-text (:default-label entry)))
        (assoc :default-label (sane-text (:default-label entry)))
        (some? (sane-text (:url entry)))
        (assoc :url (sane-text (:url entry)))))))

(defn sanitize-manifest
  "An untrusted top-level map (already `read-string`'d, but otherwise
   fully hostile -- hand-edited, truncated, or from an older/newer
   release) -> {:gallery :name :entries [...]}, or nil to refuse the
   whole file outright. Refused (not degraded) when the format id is
   unrecognized or the gallery doesn't name one of the three known
   galleries -- this is the enforcement point for keeping libraries
   isolated per gallery type, e.g. a .token-library file pointed at the
   Props panel's import control must not partially succeed."
  [data expected-gallery]
  (if (and (map? data)
           (= (:library/format data) format-id)
           (= (sane-gallery (:library/gallery data)) expected-gallery))
    {:gallery expected-gallery
     :name (or (sane-text (:library/name data)) "Imported library")
     :entries (into [] (keep sanitize-entry) (if (sequential? (:library/images data))
                                                (:library/images data)
                                                []))}))

;; ---- file I/O: the only impure functions in this namespace ----
;; Mirror panel_character.cljs's export-character!/import-character!
;; and panel_game_type_builder.cljs's export-game-type!/import-game-type!
;; exactly -- plain functions, not hooks (there's no IndexedDB blob to
;; read/write for this feature, unlike use-image-uploader/use-image-
;; url-adder, which need hooks specifically to reach that store).

(defn export!
  "Downloads a `<library-name>.<gallery>-library` file. `entries` are
   already-built image->entry maps for every image currently in that
   gallery (the caller pulls them from its own existing query -- see
   each panel's actions component)."
  [gallery library-name entries]
  (let [data (manifest {:gallery gallery :library-name library-name :entries entries})
        blob (js/Blob. #js [(pr-str data)] #js {"type" "application/edn"})]
    (util/download blob (str library-name "." (name gallery) "-library"))))

(defn import!
  "Reads `file` as text and calls `on-result` with a sanitized
   {:gallery :name :entries [...]} map, or nil if the file couldn't be
   read/parsed/matched to `gallery` at all -- the caller (an event
   dispatch) decides what a nil result means for the UI. Kept free of
   any dispatch/event-name coupling, unlike import-character!/import-
   game-type!, which dispatch directly -- this is shared by three
   different panels dispatching three differently-scoped imports."
  [gallery file on-result]
  (-> (.text file)
      (.then
       (fn [text]
         (try
           (on-result (sanitize-manifest (read-string text) gallery))
           (catch :default e
             (js/console.error "Failed to import library:" e)
             (on-result nil)))))))

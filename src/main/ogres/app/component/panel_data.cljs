(ns ogres.app.component.panel-data
  (:require [ogres.app.const :refer [VERSION]]
            [ogres.app.hooks :as hooks]
            [ogres.app.provider.release :as release]
            [uix.core :as uix :refer [defui $]]))

(def ^:private confirm-upgrade
  "Upgrading will delete all your local data and restore this application to its original state.")

(def ^:private confirm-delete
  "Delete all your local data and restore this application to its original state?")

(def ^:private confirm-backup
  "Backup your local data and images?")

(def ^:private confirm-restore
  "Delete all your local data and restore this application using the provided backup?")

;; Every image's identity + calibration lives on the same shared entity
;; as its :root/library membership (see events.cljs), so it's already
;; included in full by the plain transit dump of the DataScript "app"
;; store below -- no separate query needed for that. This query exists
;; only to compute which image *blobs* (the IndexedDB "images" store)
;; are worth backing up: everything currently active in a gallery, not
;; every archived-only library entry, which keeps a backup file from
;; ballooning with bytes for images the user isn't currently using.
(def ^:private query-backup
  [{:root/token-images [:image/hash {:image/thumbnail [:image/hash]}]}
   {:root/props-images [:image/hash {:image/thumbnail [:image/hash]}]}
   {:root/scene-images [:image/hash {:image/thumbnail [:image/hash]}]}])

(defui ^:memo panel []
  (let [[file-name set-file-name] (uix/use-state nil)
        releases (uix/use-context release/context)
        dispatch (hooks/use-dispatch)
        input (uix/use-ref)
        backup-result (hooks/use-query query-backup [:db/ident :root])
        active-hashes
        (into #{}
              (mapcat (juxt :image/hash (comp :image/hash :image/thumbnail)))
              (concat (:root/token-images backup-result)
                      (:root/props-images backup-result)
                      (:root/scene-images backup-result)))]
    ($ :.form-help
      ($ :header ($ :h2 "Data"))
      ($ :fieldset.fieldset
        ($ :legend "Version" " [ " VERSION " ]")
        ($ :div.form-notice
          (if-let [latest (last releases)]
            (if (not= VERSION latest)
              ($ :<>
                ($ :p ($ :strong "There are updates available!"))
                ($ :p "Upgrading to the latest version will "
                  ($ :strong "delete all your local data") ". "
                  "Only upgrade if you are ready to start over from scratch.")
                ($ :br)
                ($ :button.button.button-primary
                  {:on-click
                   (fn []
                     (if-let [_ (js/confirm confirm-upgrade)]
                       (dispatch :store/reset)))} "Upgrade to latest version [ " latest " ]"))
              ($ :<>
                ($ :p ($ :strong "You're on the latest version."))
                ($ :p "Pressing this button will delete all your local data and
                       restore the application to its original state.")
                ($ :br)
                ($ :button.button.button-neutral
                  {:on-click
                   (fn []
                     (if-let [_ (js/confirm confirm-delete)]
                       (dispatch :store/reset)))} "Delete local data"))))))
      ($ :fieldset.fieldset
        ($ :legend "Backup and Restore")
        ($ :div.form-notice
          ($ :<>
            ($ :p {:style {:margin-bottom 4}}
              "Create a backup file that contains all your data and images. You
               can then use this file to restore your work on another computer
               or browser.")
            ($ :button.button.button-neutral
              {:on-click
               (fn []
                 (if-let [_ (js/confirm confirm-backup)]
                   (dispatch :store/create-backup active-hashes)))} "Create Backup")
            ($ :br)
            ($ :p {:style {:margin-bottom 4}}
              "Select a backup file to restore your data and images. Note that "
              ($ :strong "restoring from a file will delete all your current data") ".")
            ($ :form
              {:class "form-restore"
               :on-submit
               (fn [event]
                 (.preventDefault event)
                 (let [file (first (.. input -current -files))]
                   (if (and (some? file) (js/confirm confirm-restore))
                     (dispatch :store/restore-backup file))))}
              ($ :div
                {:style
                 {:display "flex"
                  :flex-flow "row rap"
                  :gap "4px"}}
                ($ :label
                  {:for "restore-upload"
                   :class "button button-neutral"
                   :style {:box-sizing "border-box"}}
                  (or file-name "Choose file"))
                ($ :input
                  {:type "file"
                   :name "restore-upload"
                   :id "restore-upload"
                   :style {:display "none"}
                   :accept ".backup"
                   :ref input
                   :on-change
                   (fn [event]
                     (let [files (.. event -target -files)
                           file (first files)]
                       (set-file-name (.-name file))))})
                ($ :button.button.button-primary
                  {:type "submit" :disabled (if (nil? file-name) true)}
                  "Restore")))))))))

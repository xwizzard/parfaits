(ns ogres.app.provider.state
  (:require [cognitect.transit :as t]
            [datascript.core :as ds]
            [goog.functions :refer [throttle]]
            [ogres.app.const :refer [VERSION]]
            [ogres.app.game-type :as game-type]
            [ogres.app.provider.events :as events]
            [ogres.app.serialize :refer [reader writer]]
            [ogres.app.provider.idb :as idb]
            [ogres.app.vec :as vec]
            [uix.core :as uix :refer [defui $]]))

(def schema
  {:board/image          {:db/valueType :db.type/ref}
   :camera/scene         {:db/valueType :db.type/ref}
   :camera/selected      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :card/holder          {:db/valueType :db.type/ref}
   :db/ident             {:db/unique :db.unique/identity}
   :deck/cards           {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :game-type/key        {:db/unique :db.unique/identity}
   :image/hash           {:db/unique :db.unique/identity}
   :image/thumbnail      {:db/valueType :db.type/ref :db/isComponent true}
   :initiative/played    {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :initiative/turn      {:db/valueType :db.type/ref}
   :object/owner         {:db/valueType :db.type/ref}
   :player/controller    {:db/valueType :db.type/ref}
   :prop/image           {:db/valueType :db.type/ref}
   :prop/image-alt       {:db/valueType :db.type/ref}
   :root/game-types      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/players         {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/scene-images    {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/scenes          {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/session         {:db/valueType :db.type/ref :db/isComponent true}
   :root/token-images    {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/props-images    {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/user            {:db/valueType :db.type/ref :db/isComponent true}
   :scene/board          {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/decks          {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/game-type      {:db/valueType :db.type/ref}
   :scene/initiative     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :scene/masks          {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/shapes         {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/tokens         {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/notes          {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/props          {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :session/conns        {:db/valueType :db.type/ref :db.cardinality :db.cardinality/many :db/isComponent true}
   :session/host         {:db/valueType :db.type/ref}
   :token/image          {:db/valueType :db.type/ref}
   :token/image-alt      {:db/valueType :db.type/ref}
   :user/camera          {:db/valueType :db.type/ref}
   :user/cameras         {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :user/dragging        {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :user/game-type-editing {:db/valueType :db.type/ref}
   :user/image           {:db/valueType :db.type/ref}
   :user/uuid            {:db/unique :db.unique/identity}})

(def ^:private seed-game-types
  "Idempotent tx-data (keyed by the unique :game-type/key) that seeds the
   bundled starter game-types -- 'Default' (the bare universal element set),
   plus 'D&D 5e'/'Gloomhaven' as curated templates demonstrating the
   pluggable game-module framework (see `ogres.app.game-type`). Re-
   transacting this on every boot is a safe upsert, not a duplicate, so a
   database saved before a given template existed still gets it added."
  [{:db/id [:db/ident :root]
    :root/game-types [{:game-type/key :default
                        :game-type/name "Default"
                        :game-type/enabled-elements game-type/default-enabled-elements}
                       {:game-type/key :dnd5e
                        :game-type/name "D&D 5e"
                        :game-type/enabled-elements game-type/dnd5e-enabled-elements}
                       {:game-type/key :gloomhaven
                        :game-type/name "Gloomhaven"
                        :game-type/enabled-elements game-type/gloomhaven-enabled-elements}]}])

(defn initial-data [host]
  (ds/db-with
   (ds/empty-db schema)
   [[:db/add -1 :db/ident :root]
    [:db/add -1 :root/release VERSION]
    [:db/add -1 :root/scenes -2]
    [:db/add -1 :root/user -3]
    [:db/add -1 :root/session -5]
    [:db/add -2 :db/empty true]
    [:db/add -3 :db/ident :user]
    [:db/add -3 :user/ready false]
    [:db/add -3 :user/color "red"]
    [:db/add -3 :user/camera -4]
    [:db/add -3 :user/cameras -4]
    [:db/add -3 :user/host host]
    [:db/add -3 :panel/selected :tokens]
    [:db/add -4 :camera/scene -2]
    [:db/add -4 :camera/point vec/zero]
    [:db/add -5 :db/ident :session]
    [:db/add -1 :root/game-types -6]
    [:db/add -6 :game-type/key :default]
    [:db/add -6 :game-type/name "Default"]
    [:db/add -6 :game-type/enabled-elements game-type/default-enabled-elements]
    [:db/add -2 :scene/game-type -6]
    [:db/add -1 :root/game-types -7]
    [:db/add -7 :game-type/key :dnd5e]
    [:db/add -7 :game-type/name "D&D 5e"]
    [:db/add -7 :game-type/enabled-elements game-type/dnd5e-enabled-elements]
    [:db/add -1 :root/game-types -8]
    [:db/add -8 :game-type/key :gloomhaven]
    [:db/add -8 :game-type/name "Gloomhaven"]
    [:db/add -8 :game-type/enabled-elements game-type/gloomhaven-enabled-elements]]))

(def context (uix/create-context))

(defui ^:private listeners []
  (let [write (idb/use-writer "images")]
    ;; Removes the given scene image and its thumbnail from the
    ;; IndexedDB images object store.
    (events/use-subscribe :scene-images/remove
      (uix/use-callback
       (fn [& hashes] (write :delete hashes)) [write]))

    ;; Removes the given token image and its thumbnail from the
    ;; IndexedDB images object store.
    (events/use-subscribe :token-images/remove
      (uix/use-callback
       (fn [& hashes] (write :delete hashes)) [write]))

    ;; Removes the given token images from the IndexedDB images
    ;; object store.
    (events/use-subscribe :token-images/remove-all
      (uix/use-callback
       (fn [hashes] (write :delete hashes)) [write]))))

(def ^:private ignored-attrs
  #{:user/host :user/ready :session/status})

(defui ^:private persistence [{:keys [host]}]
  (let [conn  (uix/use-context context)
        read  (idb/use-reader "app")
        write (idb/use-writer "app")]
    ;; Persists the DataScript state to IndexedDB whenever changes
    ;; are made to it.
    (uix/use-effect
     (fn []
       (ds/listen! conn :marshaller
         (throttle
          (fn [{:keys [db-after]}]
            (if (:user/ready (ds/entity db-after [:db/ident :user]))
              (-> db-after
                  (ds/db-with [[:db/retract [:db/ident :session] :session/host]
                               [:db/retract [:db/ident :session] :session/conns]])
                  (ds/filter (fn [_ [_ attr _ _]] (not (contains? ignored-attrs attr))))
                  (ds/datoms :eavt)
                  (as-> datoms (t/write writer datoms))
                  (as-> marshalled #js {:release VERSION :updated (* -1 (.now js/Date)) :data marshalled})
                  (as-> record (write :put [record])))))
          600))
       (fn [] (ds/unlisten! conn :marshaller))) [conn write])

    ;; Reads existing state from IndexedDB, if it exists, and replaces
    ;; the DataScript state with it.
    (uix/use-effect
     (fn []
       (let [tx-data
             (into [[:db/add [:db/ident :user] :user/ready true]
                    [:db/add [:db/ident :user] :user/host host]]
                   seed-game-types)]
         (.then (read VERSION)
                (fn [record]
                  (if (nil? record)
                    (ds/transact! conn tx-data)
                    (-> (t/read reader (.-data record))
                        (ds/conn-from-datoms schema)
                        (ds/db)
                        (ds/db-with tx-data)
                        (as-> data (ds/reset-conn! conn data)))))))) ^:lint/disable [])))

(defui provider
  "Provides a DataScript in-memory database to the application and causes
   re-renders when transactions are performed."
  [{:keys [children host] :or {host true}}]
  (let [[conn] (uix/use-state (ds/conn-from-db (initial-data host)))]
    ($ context {:value conn}
      (if host ($ persistence {:host host}))
      ($ listeners)
      children)))

(defn use-query
  ([pattern]
   (use-query pattern [:db/ident :user]))
  ([pattern entity-id]
   (let [conn                   (uix/use-context context)
         get-result             (uix/use-callback #(ds/pull @conn pattern entity-id) ^:lint/disable [])
         [listen-key]           (uix/use-state random-uuid)
         [prev-state set-state] (uix/use-state get-result)]
     (uix/use-effect
      (fn []
        (ds/listen! conn listen-key
          (fn []
            (let [next-state (get-result)]
              (if (not= prev-state next-state)
                (set-state next-state)))))
        (fn []
          (ds/unlisten! conn listen-key))) ^:lint/disable [prev-state])
     prev-state)))

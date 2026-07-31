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
   :draw/deck            {:db/valueType :db.type/ref}
   :game-type/key        {:db/unique :db.unique/identity}
   :image/hash           {:db/unique :db.unique/identity}
   :image/thumbnail      {:db/valueType :db.type/ref :db/isComponent true}
   :initiative/played    {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :initiative/turn      {:db/valueType :db.type/ref}
   :minigame/deck        {:db/valueType :db.type/ref :db/isComponent true}
   :minigame/props       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :minigame/seats       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :object/owner         {:db/valueType :db.type/ref}
   :player/attack-deck   {:db/valueType :db.type/ref :db/isComponent true}
   :player/controller    {:db/valueType :db.type/ref}
   :player/items         {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :prop/image           {:db/valueType :db.type/ref}
   :prop/image-alt       {:db/valueType :db.type/ref}
   :roll/owner           {:db/valueType :db.type/ref}
   :root/game-types      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/players         {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/scene-images    {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/scenes          {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/session         {:db/valueType :db.type/ref :db/isComponent true}
   :root/token-images    {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/props-images    {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/user            {:db/valueType :db.type/ref :db/isComponent true}
   :scene/attack-draws   {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/board          {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/crazy-eights-deck {:db/valueType :db.type/ref}
   :scene/decks          {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/dice-rolls     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/game-type      {:db/valueType :db.type/ref}
   :scene/go-fish-deck   {:db/valueType :db.type/ref}
   :scene/initiative     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :scene/minigames      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/monster-attack-deck {:db/valueType :db.type/ref :db/isComponent true}
   :scene/old-maid-deck  {:db/valueType :db.type/ref}
   :scene/masks          {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/shapes         {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/tokens         {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/notes          {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/props          {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/rummy-deck     {:db/valueType :db.type/ref}
   :scene/war-deck       {:db/valueType :db.type/ref}
   :seat/controller      {:db/valueType :db.type/ref}
   :seat/player          {:db/valueType :db.type/ref}
   :session/conns        {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :session/host         {:db/valueType :db.type/ref}
   :token/image          {:db/valueType :db.type/ref}
   :token/image-alt      {:db/valueType :db.type/ref}
   :user/camera          {:db/valueType :db.type/ref}
   :user/cameras         {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :user/dragging        {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :user/game-type-editing {:db/valueType :db.type/ref}
   :user/image           {:db/valueType :db.type/ref}
   :user/minigame-viewing {:db/valueType :db.type/ref}
   :user/uuid            {:db/unique :db.unique/identity}})

(def ^:private seed-game-types
  "Idempotent tx-data (keyed by the unique :game-type/key) that seeds the
   bundled starter game-types -- 'Default' (the bare universal element set),
   plus 'D&D 5e'/'Gloomhaven'/'Memory'/'Go Fish'/'Old Maid'/'Crazy 8s'/
   'Rummy'/'War' as curated templates demonstrating the pluggable
   game-module framework (see `ogres.app.game-type`). Re-transacting
   this on every boot is a safe upsert, not a duplicate, so a database
   saved before a given template existed still gets it added. 'Memory'/
   'Go Fish'/'Old Maid'/'Crazy 8s'/'Rummy'/'War' also carry a :game-
   type/category \"card\" -- a plain, unregistered string used purely
   to group the Game Builder's template picker (see panel_game_type_
   builder.cljs), not part of the element/enabled-elements toggle
   system at all; Default/D&D 5e/Gloomhaven are deliberately left
   uncategorized."
  [{:db/id [:db/ident :root]
    :root/game-types [{:game-type/key :default
                        :game-type/name "Default"
                        :game-type/enabled-elements game-type/default-enabled-elements}
                       {:game-type/key :dnd5e
                        :game-type/name "D&D 5e"
                        :game-type/enabled-elements game-type/dnd5e-enabled-elements}
                       {:game-type/key :gloomhaven
                        :game-type/name "Gloomhaven"
                        :game-type/enabled-elements game-type/gloomhaven-enabled-elements}
                       {:game-type/key :memory
                        :game-type/name "Memory"
                        :game-type/category "card"
                        :game-type/enabled-elements game-type/memory-enabled-elements}
                       {:game-type/key :go-fish
                        :game-type/name "Go Fish"
                        :game-type/category "card"
                        :game-type/enabled-elements game-type/go-fish-enabled-elements}
                       {:game-type/key :old-maid
                        :game-type/name "Old Maid"
                        :game-type/category "card"
                        :game-type/enabled-elements game-type/old-maid-enabled-elements}
                       {:game-type/key :crazy-eights
                        :game-type/name "Crazy 8s"
                        :game-type/category "card"
                        :game-type/enabled-elements game-type/crazy-eights-enabled-elements}
                       {:game-type/key :rummy
                        :game-type/name "Rummy"
                        :game-type/category "card"
                        :game-type/enabled-elements game-type/rummy-enabled-elements}
                       {:game-type/key :war
                        :game-type/name "War"
                        :game-type/category "card"
                        :game-type/enabled-elements game-type/war-enabled-elements}]}])

(def ^:private card-back-svg
  "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 200 280'><rect width='200' height='280' rx='14' fill='#2c3e50'/><rect x='10' y='10' width='180' height='260' rx='8' fill='none' stroke='#ecf0f1' stroke-width='4'/><path d='M20,20 L180,260 M180,20 L20,260' stroke='#ecf0f1' stroke-width='2' opacity='0.4'/></svg>")

(def ^:private card-front-svg
  "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 200 280'><rect width='200' height='280' rx='14' fill='#f5f5f0'/><rect x='6' y='6' width='188' height='268' rx='10' fill='none' stroke='#333333' stroke-width='3'/></svg>")

(def card-back-hash
  "The bundled 'Card Back' demo prop image's :image/hash -- public (not
   ^:private) so component/panel_props.cljs's 'Add Card Pile (Demo)'
   button can reference the exact same hash seed-props-images seeded,
   without duplicating the SVG or searching the gallery by name."
  (str "data:image/svg+xml," (js/encodeURIComponent card-back-svg)))

(def card-front-hash
  "The bundled 'Card Front (Blank)' demo prop image's :image/hash --
   public for the same reason as card-back-hash."
  (str "data:image/svg+xml," (js/encodeURIComponent card-front-svg)))

(def ^:private seed-props-images
  "Idempotent tx-data (keyed by the unique :image/hash of each bundled
   SVG) that seeds a generic, blank card-front/card-back prop-image
   pair -- proves out the physical prop-copy/pile mechanism
   (:props/create-pile, see events.cljs and panel_props.cljs's 'Add Card
   Pile (Demo)' button) without requiring an upload. Each :image/hash is
   itself a data: URI (see provider.image/url?, widened to recognize
   this), so the image renders directly with no IndexedDB round-trip --
   the same self-referential 'thumbnail equals the full image'
   :image/thumbnail shape :token-images/create-many already uses when a
   caller supplies no distinct thumbnail. Re-transacting this on every
   boot is a safe upsert, matching seed-game-types."
  [{:db/id [:db/ident :root]
    :root/props-images
    [{:image/hash card-back-hash
      :image/name "Card Back"
      :image/size 0
      :image/width 200
      :image/height 280
      :image/thumbnail [:image/hash card-back-hash]}
     {:image/hash card-front-hash
      :image/name "Card Front (Blank)"
      :image/size 0
      :image/width 200
      :image/height 280
      :image/thumbnail [:image/hash card-front-hash]}]}])

(defn initial-data [host]
  (ds/db-with
   (ds/empty-db schema)
   (into
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
    [:db/add -8 :game-type/enabled-elements game-type/gloomhaven-enabled-elements]
    [:db/add -1 :root/game-types -9]
    [:db/add -9 :game-type/key :memory]
    [:db/add -9 :game-type/name "Memory"]
    [:db/add -9 :game-type/category "card"]
    [:db/add -9 :game-type/enabled-elements game-type/memory-enabled-elements]
    [:db/add -1 :root/game-types -10]
    [:db/add -10 :game-type/key :go-fish]
    [:db/add -10 :game-type/name "Go Fish"]
    [:db/add -10 :game-type/category "card"]
    [:db/add -10 :game-type/enabled-elements game-type/go-fish-enabled-elements]
    [:db/add -1 :root/game-types -11]
    [:db/add -11 :game-type/key :old-maid]
    [:db/add -11 :game-type/name "Old Maid"]
    [:db/add -11 :game-type/category "card"]
    [:db/add -11 :game-type/enabled-elements game-type/old-maid-enabled-elements]
    [:db/add -1 :root/game-types -12]
    [:db/add -12 :game-type/key :crazy-eights]
    [:db/add -12 :game-type/name "Crazy 8s"]
    [:db/add -12 :game-type/category "card"]
    [:db/add -12 :game-type/enabled-elements game-type/crazy-eights-enabled-elements]
    [:db/add -1 :root/game-types -13]
    [:db/add -13 :game-type/key :rummy]
    [:db/add -13 :game-type/name "Rummy"]
    [:db/add -13 :game-type/category "card"]
    [:db/add -13 :game-type/enabled-elements game-type/rummy-enabled-elements]
    [:db/add -1 :root/game-types -14]
    [:db/add -14 :game-type/key :war]
    [:db/add -14 :game-type/name "War"]
    [:db/add -14 :game-type/category "card"]
    [:db/add -14 :game-type/enabled-elements game-type/war-enabled-elements]]
    seed-props-images)))

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

(defn ^:private session-carry-tx
  "Tx-data re-asserting whatever session identity `db` already holds --
   for the restore effect below, which replaces the whole database via
   `reset-conn!`.

   A session can be started while the IndexedDB read is still in flight
   (on a cold profile the first open runs `upgradeneeded`, which is not
   fast). The WebSocket itself lives in React state and survives the
   reset untouched -- but the identity and room it is bound to live in
   here and would be wiped, leaving the lobby back at 'Invite your
   friends' with no room code while still connected, and every outgoing
   message stamped `:src nil` (see provider/session's on-send-text).
   Returns an empty vector when no session has started, which is the
   overwhelmingly common case."
  [db]
  (let [user (ds/entity db [:db/ident :user])
        room (:session/room (ds/entity db [:db/ident :session]))]
    (cond-> []
      (:user/uuid user)
      (conj [:db/add [:db/ident :user] :user/uuid (:user/uuid user)])
      (:session/status user)
      (conj [:db/add [:db/ident :user] :session/status (:session/status user)])
      (:session/last-room user)
      (conj [:db/add [:db/ident :user] :session/last-room (:session/last-room user)])
      room
      (conj [:db/add [:db/ident :session] :session/room room]))))

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
                   cat [seed-game-types seed-props-images])]
         (.then (read VERSION)
                (fn [record]
                  (if (nil? record)
                    (ds/transact! conn tx-data)
                    (-> (t/read reader (.-data record))
                        (ds/conn-from-datoms schema)
                        (ds/db)
                        (ds/db-with tx-data)
                        (ds/db-with (session-carry-tx @conn))
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

(ns ogres.app.component.panel-character
  "The 'Character' panel/tab -- generic level/experience/gold/item
   tracking directly on a roster player entity (see events.cljs's
   ';; --- Character Profile ---' section), gated by :tool/character-
   profile (see panel.cljs's visible-tabs). Deliberately NOT Gloomhaven-
   specific -- any game-type with leveling/currency can opt in, though
   Gloomhaven's own seeded template pulls it in by default alongside its
   attack-deck system (see game_type.cljs's gloomhaven-enabled-elements).

   :root/players is already root-scoped (not scene-scoped) and already
   sits inside this app's automatic whole-DB save -- these fields
   persist across scene changes and browser reloads for free. No XP-
   threshold table or item slot/equip-restriction rules are enforced --
   plain editable numbers and a flat named-item list, matching this
   app's general 'trust the humans, don't automate rules resolution'
   stance. Per-character export/import (a small .character.edn blob)
   mirrors the Game Builder's own export-game-type!/import-game-type!
   exactly -- letting a player carry ONE character between separate
   hosts/campaigns, not just within one save."
  (:require [cljs.reader :refer [read-string]]
            [ogres.app.component :refer [icon]]
            [ogres.app.hooks :as hooks]
            [ogres.app.player :as player]
            [ogres.app.util :as util]
            [uix.core :as uix :refer [defui $]]))

(def ^:private query
  [{:root/user [:user/uuid :user/host]}
   {:root/players
    [:db/id :player/name :player/color
     {:player/controller [:user/uuid]}
     [:player/level :default nil]
     [:player/experience :default nil]
     [:player/gold :default nil]
     {:player/items [:db/id :item/name [:item/description :default nil] [:item/equipped? :default false]]}
     {:player/attack-deck
      [{:deck/cards [:card/rank [:card/effect :default nil] [:card/effect-amount :default nil]]}]}]}
   {:root/session [{:session/conns [:user/uuid]}]}])

(defn ^:private character-state
  [result]
  (let [{uuid :user/uuid host :user/host} (:root/user result)
        connected (into #{} (map :user/uuid) (:session/conns (:root/session result)))]
    {:host host
     :players (:root/players result)
     :authorized?
     (fn [player]
       (player/authority? uuid host connected (get-in player [:player/controller :user/uuid])))}))

(defn ^:private export-character!
  "Downloads a small .character.edn blob for `player` -- name/level/
   experience/gold/items/attack-deck composition (NOT its draw/discard
   split -- see :player/import-character's own docstring for why).
   Mirrors panel_game_type_builder.cljs's export-game-type! exactly."
  [player]
  (let [data {:name (:player/name player)
              :level (:player/level player)
              :experience (:player/experience player)
              :gold (:player/gold player)
              :items (mapv (fn [i] {:name (:item/name i)
                                     :description (:item/description i)
                                     :equipped? (:item/equipped? i)})
                            (:player/items player))
              :deck (mapv (fn [c] {:rank (:card/rank c)
                                    :effect (:card/effect c)
                                    :amount (:card/effect-amount c)})
                          (:deck/cards (:player/attack-deck player)))}
        blob (js/Blob. #js [(pr-str data)] #js {"type" "application/edn"})]
    (util/download blob (str (:player/name player) ".character.edn"))))

(defn ^:private import-character!
  [dispatch file]
  (-> (.text file)
      (.then
       (fn [text]
         (try
           (let [data (read-string text)]
             (dispatch :player/import-character data))
           (catch :default e
             (js/console.error "Failed to import character:" e)))))))

(defui ^:private item-row [{:keys [dispatch item disabled?]}]
  ($ :li.character-item-row
    ($ :label.checkbox
      ($ :input
        {:type "checkbox" :disabled disabled? :checked (boolean (:item/equipped? item))
         :on-change (fn [event] (dispatch :player/toggle-item-equipped (:db/id item) (.. event -target -checked)))})
      (:item/name item)
      (if (seq (:item/description item)) ($ :span.character-item-description (str " -- " (:item/description item)))))
    ($ :button.button.button-danger
      {:type "button" :disabled disabled? :on-click #(dispatch :player/remove-item (:db/id item))}
      ($ icon {:name "trash3-fill" :size 14}))))

(defui ^:private add-item-form [{:keys [dispatch player-id disabled?]}]
  (let [[name set-name] (uix/use-state "")
        [description set-description] (uix/use-state "")]
    ($ :form.character-add-item-form
      {:on-submit
       (fn [event]
         (.preventDefault event)
         (if (seq name)
           (do (dispatch :player/add-item player-id name description)
               (set-name "") (set-description ""))))}
      ($ :input.text
        {:type "text" :placeholder "Item name" :disabled disabled?
         :value name :on-change (fn [event] (set-name (.. event -target -value)))})
      ($ :input.text
        {:type "text" :placeholder "Description (optional)" :disabled disabled?
         :value description :on-change (fn [event] (set-description (.. event -target -value)))})
      ($ :button.button.button-neutral {:type "submit" :disabled (or disabled? (empty? name))} "Add Item"))))

(defui ^:private character-row [{:keys [dispatch player authorized?]}]
  (let [[editing? set-editing] (uix/use-state false)
        disabled? (not authorized?)
        {id :db/id name :player/name color :player/color
         level :player/level experience :player/experience gold :player/gold
         items :player/items} player]
    ($ :li.character-row
      ($ :.character-row-header
        ($ :span.character-row-name {:data-color color} name)
        ($ :button.button.button-neutral
          {:type "button" :disabled (nil? (:player/attack-deck player))
           :on-click #(export-character! player)}
          ($ icon {:name "box-arrow-up-right" :size 14}) "Export"))
      ($ :.character-row-stats
        ($ :label.character-stat-label "Level"
          ($ :input.text.character-stat-input
            {:type "number" :disabled disabled? :value (or level 0)
             :on-change (fn [event] (dispatch :player/set-level id (js/Number (.. event -target -value))))}))
        ($ :label.character-stat-label "XP"
          ($ :input.text.character-stat-input
            {:type "number" :disabled disabled? :value (or experience 0)
             :on-change (fn [event] (dispatch :player/set-experience id (js/Number (.. event -target -value))))}))
        ($ :label.character-stat-label "Gold"
          ($ :input.text.character-stat-input
            {:type "number" :disabled disabled? :value (or gold 0)
             :on-change (fn [event] (dispatch :player/set-gold id (js/Number (.. event -target -value))))})))
      ($ :button.button.button-neutral
        {:type "button" :on-click #(set-editing not)}
        (if editing? "Hide Items" (str "Items (" (count items) ")")))
      (if editing?
        ($ :<>
          (if (seq items)
            ($ :ul.character-item-list
              (for [item items]
                ($ item-row {:key (:db/id item) :dispatch dispatch :item item :disabled? disabled?})))
            ($ :.form-notice "No items yet."))
          ($ add-item-form {:dispatch dispatch :player-id id :disabled? disabled?}))))))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        import-input (uix/use-ref)
        {:keys [host players authorized?]} (character-state result)]
    ($ :.form-character
      ($ :header
        ($ :h2 "Character")
        (if host
          ($ :button.button.button-neutral
            {:type "button" :on-click #(.click (deref import-input))}
            ($ icon {:name "door-open" :size 14}) "Import"
            ($ :input
              {:ref import-input
               :type "file"
               :hidden true
               :accept ".edn,text/plain"
               :on-change
               (fn [event]
                 (if-let [file (first (array-seq (.. event -target -files)))]
                   (import-character! dispatch file))
                 (set! (.. event -target -value) ""))}))))
      (if (seq players)
        ($ :ul.character-list
          (for [player players]
            ($ character-row
              {:key (:db/id player) :dispatch dispatch :player player
               :authorized? (authorized? player)})))
        ($ :.form-notice "No players on the roster yet.")))))

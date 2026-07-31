(ns ogres.app.component.panel
  (:require [ogres.app.component :refer [icon]]
            [ogres.app.component.panel-attack-deck :as attack-deck]
            [ogres.app.component.panel-character :as character]
            [ogres.app.component.panel-crazy-eights :as crazy-eights]
            [ogres.app.component.panel-data :as data]
            [ogres.app.component.panel-decks :as decks]
            [ogres.app.component.panel-dice :as dice]
            [ogres.app.component.panel-game-type-builder :as game-type-builder]
            [ogres.app.component.panel-go-fish :as go-fish]
            [ogres.app.component.panel-initiative :as initiative]
            [ogres.app.component.panel-lobby :as lobby]
            [ogres.app.component.panel-memory :as memory]
            [ogres.app.component.panel-old-maid :as old-maid]
            [ogres.app.component.panel-roster :as roster]
            [ogres.app.component.panel-rummy :as rummy]
            [ogres.app.component.panel-scene :as scene]
            [ogres.app.component.panel-tokens :as tokens]
            [ogres.app.component.panel-props :as props]
            [ogres.app.component.panel-war :as war]
            [ogres.app.hooks :as hooks]
            [uix.core :refer [defui $]]))

(def ^:private query
  [[:user/host :default true]
   [:user/mode :default :setup]
   [:panel/selected :default :tokens]
   [:panel/expanded :default true]
   {:user/camera
    [{:camera/scene
      [{:scene/game-type
        [[:game-type/enabled-elements :default #{}]]}]}]}])

(def ^:private query-status
  [{:root/user [:user/host [:session/status :default :initial]]
    :root/session [:session/conns :session/room]}])

(def ^:private status-icon
  ($ icon {:name "globe-americas" :size 16}))

(defui status []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query-status [:db/ident :root])
        {{host :user/host status :session/status} :root/user
         {code  :session/room
          conns :session/conns} :root/session} result
        connected (cond-> (count conns) host (inc))]
    (case status
      :initial      ($ :button.button {:on-click #(dispatch :session/request)} status-icon "Start online game")
      :connecting   ($ :button.button {:disabled true} status-icon "Connecting...")
      :connected    ($ :button.button {:disabled true} status-icon "Connected / " code " / [ " connected " ]")
      :disconnected ($ :button.button {:disabled true} status-icon "Disconnected")
      ($ :button.button {:disabled true} status-icon "Status not known"))))

(def ^:private mode-options
  [[:builder "file-text" "Game Builder"]
   [:setup "easel" "Setup"]
   [:play "play-fill" "Play"]])

(defn ^:private next-mode
  "The mode that follows the given mode in the Builder -> Setup -> Play
   cycle used by the mode tab's single click-to-advance button."
  [mode]
  (case mode :builder :setup :setup :play :play :builder :setup))

(def ^:private data
  {:data       {:icon "floppy" :label "Manage local data" :size 26}
   :initiative {:icon "hourglass-split" :label "Turn Order"}
   :lobby      {:icon "people-fill" :label "Online options"}
   :scene      {:icon "images" :label "Scene options"}
   :tokens     {:icon "pawn" :label "Token images"}
   :props      {:icon "rock" :label "Prop images" :size 26}
   :decks      {:icon "suit-spade-fill" :label "Decks"}
   :dice       {:icon "dice-5-fill" :label "Dice"}
   :roster     {:icon "person-circle" :label "Players"}
   :memory     {:icon "card-front" :label "Memory"}
   :go-fish    {:icon "suit-heart-fill" :label "Go Fish"}
   :old-maid   {:icon "skull" :label "Old Maid"}
   :crazy-eights {:icon "magic" :label "Crazy 8s"}
   :rummy      {:icon "suit-diamond-fill" :label "Rummy"}
   :war        {:icon "fist" :label "War"}
   :attack-deck {:icon "suit-spade-fill" :label "Attack Decks"}
   :character  {:icon "person-circle" :label "Character"}
   :game-type-builder {:icon "sliders" :label "Game builder"}})

(def ^:private components
  {:data       {:form data/panel}
   :initiative {:form initiative/panel :footer initiative/actions}
   :lobby      {:form lobby/panel :footer lobby/actions}
   :scene      {:form scene/panel}
   :tokens     {:form tokens/panel :footer tokens/actions}
   :props      {:form props/panel :footer props/actions}
   :decks      {:form decks/panel :footer decks/actions}
   :dice       {:form dice/panel :footer dice/actions}
   :roster     {:form roster/panel :footer roster/actions}
   :memory     {:form memory/panel :footer memory/actions}
   :go-fish    {:form go-fish/panel :footer go-fish/actions}
   :old-maid   {:form old-maid/panel :footer old-maid/actions}
   :crazy-eights {:form crazy-eights/panel :footer crazy-eights/actions}
   :rummy      {:form rummy/panel :footer rummy/actions}
   :war        {:form war/panel :footer war/actions}
   :attack-deck {:form attack-deck/panel :footer attack-deck/actions}
   :character  {:form character/panel}
   :game-type-builder {:form game-type-builder/panel}})

(defn ^:private visible-tabs
  "The ordered list of visible panel tab keys for the given host status,
   interface mode, and the active scene's game-type enabled-elements set.
   Builder mode shows only the game-type editor and local data management,
   decoupled from any scene. Setup mode is scene-editing only. Play mode
   drops Scene (no more layout changes once play has started) but keeps
   Props (still placeable/adjustable mid-session) and is the only mode
   with the Turn Order tab and Lobby, since those are both live-session
   concerns. The Turn Order tab additionally requires the active
   game-type to have :unit/initiative enabled -- the base per-token
   'participates in the turn tracker' flag every game-type starts with
   (see ogres.app.game-type/default-enabled-elements), making turn
   tracking baseline rather than something only specific game modules
   (e.g. D&D 5e's d20 roll) unlock. The Decks tab, in both Setup and Play,
   likewise requires :tool/cards -- the generic card/deck system is opt-in
   the same way, not enabled by any seeded template yet. The Players
   (roster) tab is baseline like Tokens/Props -- no gating element,
   host-only (roster management is a GM/setup concern, same as Scene).
   The Memory, Go Fish, Old Maid, Crazy 8s, Rummy, and War tabs --
   example games built on the generic prop-copy/shared-toggle and
   card/deck-hand mechanisms -- are gated the same way Decks is,
   behind their own :memory/game/:go-fish/game/:old-maid/game/:crazy-
   eights/game/:rummy/game/:war/game elements (see game-type/games/
   memory.cljs, game-type/games/go-fish.cljs, game-type/games/old-
   maid.cljs, game-type/games/crazy-eights.cljs, game-type/games/
   rummy.cljs, and game-type/games/war.cljs), true for their seeded
   templates and any custom template that enables them. Unlike
   Roster, all six are visible to guests as well: seeing turn order
   and scores, and acting on your own turn, is exactly what every
   connected participant needs, not just the host. The Dice tab, gated
   on :tool/dice, is Play-mode-only (and guest-visible, same as the six
   card games) -- deliberately absent from Setup, unlike Decks, since
   rolling dice is a play-time action, not a scenario-construction one;
   D&D 5e's own advantage/disadvantage/per-player controls layer onto
   this SAME tab (see component/panel_dice.cljs), gated on its own
   :dnd5e/dice-roller element rather than getting a tab of their own.
   The Attack Decks tab, gated on :gloomhaven/attack-deck, is visible in
   BOTH Setup and Play (unlike Dice) -- creating a personal deck for
   each player and applying perk/item composition edits is naturally a
   setup-time activity too, not a play-only one -- and guest-visible
   same as every other opt-in tab. The Character tab, gated on :tool/
   character-profile, follows the exact same Setup + Play + guest-
   visible shape as Attack Decks -- leveling up, tracking gold, and
   managing items are all things a player does between and during
   scenarios, not just mid-play."
  [host mode enabled-elements]
  (let [cards?       (contains? enabled-elements :tool/cards)
        dice?        (contains? enabled-elements :tool/dice)
        attack-deck? (contains? enabled-elements :gloomhaven/attack-deck)
        character?   (contains? enabled-elements :tool/character-profile)
        memory?      (contains? enabled-elements :memory/game)
        go-fish?     (contains? enabled-elements :go-fish/game)
        old-maid?    (contains? enabled-elements :old-maid/game)
        crazy-eights? (contains? enabled-elements :crazy-eights/game)
        rummy?       (contains? enabled-elements :rummy/game)
        war?         (contains? enabled-elements :war/game)]
    (cond
      (not host) (cond-> [:tokens :initiative :lobby]
                   dice? (conj :dice) attack-deck? (conj :attack-deck) character? (conj :character)
                   memory? (conj :memory) go-fish? (conj :go-fish) old-maid? (conj :old-maid)
                   crazy-eights? (conj :crazy-eights) rummy? (conj :rummy) war? (conj :war))
      (= mode :builder) [:game-type-builder :data]
      (= mode :play)
      (into (cond-> [:tokens :roster :props]
              cards? (conj :decks) dice? (conj :dice) attack-deck? (conj :attack-deck) character? (conj :character)
              memory? (conj :memory) go-fish? (conj :go-fish) old-maid? (conj :old-maid)
              crazy-eights? (conj :crazy-eights) rummy? (conj :rummy) war? (conj :war))
            (if (contains? enabled-elements :unit/initiative)
              [:initiative :lobby]
              [:lobby]))
      :else (cond-> [:scene :props :tokens :roster]
              cards? (conj :decks) attack-deck? (conj :attack-deck) character? (conj :character)
              memory? (conj :memory) go-fish? (conj :go-fish) old-maid? (conj :old-maid)
              crazy-eights? (conj :crazy-eights) rummy? (conj :rummy) war? (conj :war)))))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query query)
        {host :user/host
         mode :user/mode
         selected :panel/selected
         expanded :panel/expanded
         {{{enabled-elements :game-type/enabled-elements}
           :scene/game-type} :camera/scene} :user/camera} result
        tabs     (visible-tabs host mode enabled-elements)
        selected (if ((set tabs) selected) selected (first tabs))]
    ($ :.panel
      {:data-expanded expanded}
      (if expanded
        ($ :.panel-status
          ($ status)))
      ($ :ul.panel-tabs
        {:role "tablist"
         :aria-controls "form-panel"
         :aria-orientation "vertical"}
        (if host
          (let [[_ icon-name label] (some #(when (= (first %) mode) %) mode-options)]
            ($ :li.panel-tabs-mode
              {:role "tab"}
              ($ :button
                {:type "button"
                 :aria-label (str "Interface mode: " label ". Click to switch mode.")
                 :data-tooltip label
                 :on-click #(dispatch :user/change-mode (next-mode mode))}
                (if (= mode :builder)
                  ($ :span {:style {:display "inline-flex" :transform "translateY(3px) scale(1.15)"}}
                    ($ icon {:name icon-name :size 22}))
                  ($ icon {:name icon-name :size 22}))))))
        (for [[key data] (map (juxt identity data) tabs)
              :let [selected (= selected key)]]
          ($ :li.panel-tabs-tab
            {:key key :role "tab" :aria-selected (and expanded selected)}
            ($ :label {:aria-label (:label data)}
              ($ :input
                {:type "radio"
                 :name "panel"
                 :value key
                 :checked (and expanded selected)
                 :on-change #(dispatch :user/select-panel key)})
              ($ icon {:name (:icon data) :size (:size data 22)}))))
        ($ :li.panel-tabs-control
          {:role "tab" :on-click #(dispatch :user/toggle-panel)}
          ($ :button {:type "button" :aria-label "Collapse or expand"}
            ($ icon {:name (if expanded "chevron-double-right" "chevron-double-left")}))))
      (if expanded
        ($ :.form
          {:id "form-panel"
           :role "tabpanel"
           :data-form (name selected)}
          ($ :.form-container
            ($ :.form-content
              (if-let [component (get-in components [selected :form])]
                ($ :.form-body ($ component)))
              (if-let [component (get-in components [selected :footer])]
                ($ :.form-footer ($ component))))))))))

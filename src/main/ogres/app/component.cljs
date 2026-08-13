(ns ogres.app.component
  (:require [ogres.app.const :refer [PATH]]
            [ogres.app.hooks :as hooks]
            [uix.core :as uix :refer [defui $]]
            [uix.dom :as dom]))

(defn ^:private create-range
  "Returns a vector of page indexes or spacing sentinels suitable for use
   in a pagination component. Sentinels include `:spacel` and `:spacer`.
   The vector will contain anywhere between 1 and 7 elements.
   ```
   (create-range 1 78 23) ;; [1 :spacel 22 23 24 :spacer 78]
   ```"
  [min max val]
  (cond (< (- max min) 7)
        (range min (inc max))
        (<= val (+ min 3))
        [min (+ min 1) (+ min 2) (+ min 3) (+ min 4) :spacel max]
        (>= val (- max 3))
        [min :spacel (- max 4) (- max 3) (- max 2) (- max 1) max]
        :else
        [min :spacel (- val 1) val (+ val 1) :spacer max]))

(defui icon
  "Renders the `<svg>` definition found in `icons.svg` matching the given name.
   ```
   ($ icon {:name 'arrow-right-short' :size 16})
   ```
   `color`, when given, overrides the icon's fill (defaults to inheriting
   the surrounding text color) -- for the rare icon whose color is
   itself meaningful (e.g. a wild card's star), not a general theming
   knob."
  [{:keys [name size color] :or {size 22}}]
  ($ :svg
    {:fill (or color "currentColor")
     :role "presentation"
     :class "icon"
     :width size
     :height size}
    ($ :use {:href (str PATH "/icons.svg" "#icon-" name)})))

(defui paginated
  "Accepts a collection as `data` and calls the given render function
   with the current page of that collection as part of its argument.
   ```
   ($ paginated {:data [1 2 3 4 5 6] :page-size 3}
     (fn [{:keys [data pages page on-change]}]
       (for [idx data] ;; data => [1 2 3]}
         ($ :div idx))))
   ```"
  [props]
  (let [{:keys [data page-size children]} props
        [page set-page] (uix/use-state 1)
        data  (vec data)
        pages (js/Math.ceil (/ (count data) page-size))
        start (max (* (dec (min page pages)) page-size) 0)
        end   (min (+ start page-size) (count data))]
    (children
     {:data (subvec data start end)
      :page (max (min pages page) 1)
      :pages (max pages 1)
      :on-change set-page})))

(defui pagination
  "Renders a `<nav>` element that provides a means to navigate through
   a paginated resource, including buttons to go backward, forward,
   and directly to different pages.
   ```
   ($ pagination
     {:name 'token-gallery'
      :pages 3
      :value 1
      :on-change (fn [page] ...)})
   ```"
  [{:keys [class-name name label pages value on-change]
    :or   {pages 10 value 1 on-change identity}}]
  ($ :nav {:role "navigation" :aria-label label}
    ($ :ol.pagination
      {:class (if class-name (str "pagination-" class-name))}
      ($ :li
        ($ :button
          {:aria-disabled (= value 1)
           :aria-label "Previous page"
           :on-click
           (fn []
             (if (not (= value 1))
               (on-change (dec value))))}
          ($ icon {:name "chevron-left" :size 16})))
      (for [term (create-range 1 pages value)]
        ($ :li {:key term}
          (if (or (= term :spacel) (= term :spacer))
            ($ :label {:role "presentation"} \…)
            ($ :label {:aria-label value :aria-current (= term value)}
              ($ :input
                {:type "radio"
                 :name name
                 :value value
                 :checked (= term value)
                 :on-change #(on-change term)}) term))))
      ($ :li
        ($ :button
          {:aria-disabled (= value pages)
           :aria-label "Next page"
           :on-click
           (fn []
             (if (not (= value pages))
               (on-change (inc value))))}
          ($ icon {:name "chevron-right" :size 16}))))))

(defui image
  "Renders the image identified by the given hash. Accepts a
   render function as its children which is passed a URL that
   references the image resource.
   ```
   ($ image {:hash 'fa7b887b1ce364732beb9ac70892775a'}
     (fn [url]
       ($ :img {:src url})))
   ```"
  [{:keys [hash children]}]
  (let [url (hooks/use-image hash)]
    (children url)))

(defui image-url-form
  "A small button + popover form for adding an image by pasting a URL,
   alongside local file upload -- see hooks/use-image-url-adder for the
   :token/:scene/:props type convention this mirrors. Host-only, matching
   this app's other host-only gallery actions (e.g. 'Remove all').
   ```
   ($ image-url-form {:type :token})
   ```"
  [{:keys [type]}]
  (let [{host :user/host} (hooks/use-query [:user/host])
        [open? set-open form] (hooks/use-modal)
        add (hooks/use-image-url-adder {:type type})
        input (uix/use-ref)
        [error set-error] (uix/use-state nil)]
    ($ :.image-url-form-wrapper
      ($ :button.button.button-neutral.image-url-form-trigger
        {:type "button"
         :disabled (not host)
         :title "Link images"
         :on-click
         (fn [event]
           (.stopPropagation event)
           (set-error nil)
           (set-open not))}
        ($ icon {:name "globe-americas" :size 16})
        "Link")
      (if open?
        ($ :form.image-url-form
          {:ref form
           :on-submit
           (fn [event]
             (.preventDefault event)
             (let [value (.-value (deref input))]
               (-> (add value)
                   (.then (fn [] (set-error nil) (set-open false)))
                   (.catch (fn [err] (set-error (.-message err)))))))}
          ($ :input.text.text-ghost
            {:type "url"
             :ref input
             :auto-focus true
             :placeholder "https://example.com/image.png"
             :required true
             :aria-label "Image URL"})
          ($ :button {:type "submit"}
            ($ icon {:name "check"}))
          (if error ($ :.image-url-form-error error)))))))

(defui fullscreen-dialog [props]
  (let [element (.querySelector js/document "#root")
        dialog  (uix/use-ref nil)]
    (hooks/use-shortcut ["Escape"]
      (:on-close props))
    (dom/create-portal
     ($ :.fullscreen-dialog
       {:ref dialog
        :role "dialog"
        :tab-index -1
        :on-click
        (fn [event]
          (if (= (.-target event) (deref dialog))
            ((:on-close props) event)))}
       ($ :.fullscreen-dialog-body {:role "document"}
         ($ :.fullscreen-dialog-content
           (:children props)))) element)))

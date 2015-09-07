(ns todomvc.core
  (:require [reagent.core :as r]))

(defonce todo-data (r/atom {:edit nil
                            :show :all
                            :todos (sorted-map)}))


;;; Event handler: (old state, event) -> new state

(defn todo-handler [todos [id x y]]
  (case id
    :add (assoc todos (-> todos rseq ffirst inc)
                {:text x :done false})
    :delete (dissoc todos x)
    :set-text (assoc-in todos [x :text] y)
    :set-done (assoc-in todos [x :done] y)
    :clear-done (reduce-kv (fn [m k v]
                             (if (:done v) (dissoc m k) m))
                           todos todos)
    :toggle-all (reduce-kv (fn [m k _]
                             (assoc-in m [k :done] x))
                           todos todos)))

(defn app-handler [state [id x :as event]]
  (case id
    (:edit :show) (assoc state id x)
    (update-in state [:todos] todo-handler event)))

(defn dispatch [event]
  ;; (js/console.log (str "Handling event: " event))
  (swap! todo-data app-handler event))


;;; Helper components

(defn todo-input [{:keys [title on-save on-stop]}]
  (let [val (r/atom (str title))
        stop #(do (reset! val "")
                  (when on-stop (on-stop)))
        save #(let [v (clojure.string/trim @val)]
                (when-not (empty? v) (on-save v))
                (stop))]
    (fn [props]
      [:input (merge props {:type "text" :value @val :on-blur save
                            :on-change #(reset! val (.-target.value %))
                            :on-key-down #(case (.-which %)
                                            13 (save)
                                            27 (stop)
                                            nil)})])))

(def todo-edit
  (r/create-class {:reagent-render todo-input
                   :component-did-mount #(-> % r/dom-node .select)}))


;;; Pure function components

(defn header []
  [:header#header
   [:h1 "todos"]
   [todo-input {:id 'new-todo :placeholder "What needs to be done?"
                :on-save #(dispatch [:add %])}]])

(defn filter-li [showing to-show text]
  [:li>a {:class (when (= to-show showing) 'selected)
          :on-click #(dispatch [:show to-show])} text])

(defn footer [show active done]
  [:footer#footer
   [:span#todo-count
    [:strong active] " item" (case active 1 "" "s") " left"]
   [:ul#filters
    [filter-li show :all "All"]
    [filter-li show :active "Active"]
    [filter-li show :done "Completed"]]
   (when (pos? done)
     [:button#clear-completed {:on-click #(dispatch [:clear-done])}
      "Clear completed " done])])

(defn todo-item [id {:keys [text done]} edit]
  [:li {:class (str (when done "completed ")
                    (when edit "editing"))}
   [:div.view
    [:input.toggle {:type 'checkbox :checked done
                    :on-change #(dispatch [:set-done id (not done)])}]
    [:label {:on-double-click #(dispatch [:edit id])} text]
    [:button.destroy {:on-click #(dispatch [:delete id])}]]
   (when edit
     [todo-edit {:class 'edit :value text
                 :on-save #(dispatch [:set-text id %])
                 :on-stop #(dispatch [:edit nil])}])])

(defn main-section [{:keys [todos edit show]}]
  (let [done (->> todos vals (filter :done) count)
        active (- (count todos) done)
        pred (case show
               :active (complement :done)
               :done :done
               :all identity)]
    [:div
     [:section#main
      [:input#toggle-all {:type 'checkbox :checked (zero? active)
                          :on-change #(dispatch [:toggle-all (pos? active)])}]
      [:label {:for 'toggle-all} "Mark all as complete"]
      [:ul#todo-list (for [[k v] todos :when (pred v)]
                       ^{:key k} [todo-item k v (= k edit)])]]
     [footer show active done]]))

(defn todo-main [state]
  [:div
   [:section#todoapp
    [header]
    (when (-> state :todos seq)
      [main-section state])]
   [:footer#info
    [:p "Double-click to edit a todo"]]])


;;; App

(defn todo-app []
  [todo-main @todo-data])

(defn ^:export run []
  (r/render [todo-app] (js/document.getElementById "app")))

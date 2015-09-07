(ns todomvc.app
  (:require [reagent.core :as r]
            [todomvc.core :as core]
            [secretary.core :as secretary :refer-macros [defroute]]
            [goog.events :as events]
            [goog.history.EventType :as etype])
  (:import goog.History))


;;; Navigation handling

(defroute "/" [] (core/dispatch [:show :all]))
(defroute show-path "/:id" [id] (core/dispatch [:show (keyword id)]))

(defonce history
  (doto (History.)
    (events/listen etype/NAVIGATE #(secretary/dispatch! (.-token %)))))

(defn nav-handler [state [id val]]
  (case id
    :show (.setToken history (show-path {:id (name val)}))
    nil)
  state)


;;; Storage handling

(def ls-key "reagent-todomvc-key")

(defn read-local-storage [state]
  (some->> (.getItem js/localStorage ls-key)
           (cljs.reader/read-string)
           (into (:todos state))
           (assoc state :todos)))

(defn write-local-storage [state]
  (->> state :todos str
       (.setItem js/localStorage ls-key))
  state)


;;; App

(defn app-handler [state event]
  (-> state
      (core/view-handler event)
      (nav-handler event)
      (write-local-storage)))

(set! core/app-handler app-handler)

(defn ^:export run []
  (swap! core/todo-data read-local-storage)
  (.setEnabled history true)
  (r/render [core/todo-app] (js/document.getElementById "app")))

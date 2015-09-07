(ns todomvc.app
  (:require [reagent.core :as r]
            [todomvc.core :as core :refer [dispatch]]
            [secretary.core :as secretary :refer-macros [defroute]]
            [goog.events :as events]
            [goog.history.EventType :as etype])
  (:import goog.History))

(defroute "/" [] (dispatch [:show :all]))
(defroute show-path "/:show" [show] (dispatch [:show (keyword show)]))

(defonce history
  (doto (History.)
    (events/listen etype/NAVIGATE #(secretary/dispatch! (.-token %)))))

(defn nav-handler [state [id val :as evt]]
  (case id
    :show (.setToken history (show-path {:show (name val)}))
    nil)
  state)

(defn app-handler [state [id val :as event]]
  (-> state
      (nav-handler event)
      (core/view-handler event)))

(set! core/app-handler app-handler)

(defn ^:export run []
  (.setEnabled history true)
  (r/render [core/todo-app] (js/document.getElementById "app")))

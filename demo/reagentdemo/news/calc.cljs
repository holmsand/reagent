(ns reagentdemo.news.calc
  (:require [reagent.core :as r]
            [reagent.debug :refer-macros [dbg]]
            [reagentdemo.syntax :as s]
            [sitetools.core :as tools :refer [link]]
            [cljs.reader :as reader]
            [reagentdemo.common :as common :refer [demo-component]]))

(def url "/news/calc.html")
(def title "Calc")


(defonce calcs (r/atom ["(+ 1 2 3)"
                        "(+ %0 1 4 6)"
                        "(- %1 2)"]))

(defn handle-event [old [action k v]]
  (case action
    :set-calc (assoc old k v)
    old))

(defn emit [evt]
  (swap! calcs handle-event evt))


(defn input-n [n]
  (nth @calcs n ""))

(defn input-count []
  (let [c @calcs
        n (count c)]
    ;; Add empty input
    (if (or (zero? n) (-> c last count pos?))
      (inc n)
      n)))

(defn parse [n]
  (reader/read-string @(r/track input-n n)))

(declare expand)

(defn result [n]
  (try
    (expand @(r/track parse n))
    (catch :default e
      (.-message e))))

(def funs {'+ +
           '- -
           '* *
           '/ /})

(defn expand [x]
  (or (and (list? x)
           (-> x first funs)
           (let [y (doall (map expand (rest x)))]
             (if (every? number? y)
               (-> x first funs (apply y))
               (cons (first x) y))))
      (and (symbol? x)
           (re-matches #"%[0-9]+" (name x))
           (let [i (-> x name (subs 1) js/parseInt)]
             @(r/track result i)))
      x))


(defn calc-input [n]
  [:input {:type 'text
           :value @(r/track input-n n)
           :on-change #(emit [:set-calc n (.-target.value %)])
           :style {:font-family 'courier
                   :width "100%"}}])

(defn calc-output [n]
  (let [val @(r/track result n)]
    (cond
      (string? val) [:code {:style {:color 'red}} val]
      (some? val)   [:code " = " (pr-str val)])))

(defn calculations []
  [:table {:style {:width "100%"}}
   [:tbody
    (doall (for [i (range @(r/track input-count))]
             ^{:key i} [:tr
                        [:td i]
                        [:td [calc-input i]]
                        [:td [calc-output i]]]))]])


(defn story-summary []
  [:div.demo-text
   [:p "summary"]])

(defn story []
  [:div.demo-text
   [:p "Story"]
   [demo-component {:comp calculations
                    :src (s/src-of [:calculations])}]])

(defn main [{:keys [summary]}]
  [:div.reagent-demo
   [:h1 [link {:href url} title]]
   (when summary [link {:href url :class 'news-read-mode} "Read more"])
   (when-not summary
     [story])])

(tools/register-page url [#'main] title)

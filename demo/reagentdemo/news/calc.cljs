(ns reagentdemo.news.calc
  (:require [reagent.core :as r]
            [reagent.debug :refer-macros [dbg]]
            [reagentdemo.syntax :as s]
            [sitetools.core :as tools :refer [link]]
            [cljs.reader :as reader]
            [reagentdemo.common :as common :refer [demo-component]]))

(def url "/news/calc.html")
(def title "Calc")

(def calcs (r/atom ["(+ 1 2 3)"
                    "(- '1 4 6)"]))

(defn handle [old [action k v]]
  (case action
    :set (assoc old k v)
    old))

(defn emit [evt]
  (r/rswap! calcs handle evt))

(defn input-n [n]
  (dbg n)
  (nth @calcs n ""))

(defn input-count []
  (let [c @calcs
        n (count c)]
    (if (and (pos? n) (-> c last count pos?))
      (inc n)
      n)))

(defn read-n [n]
  (dbg n)
  (reader/read-string @(r/track input-n n)))

(declare expand)

(defn result [n]
  (expand @(r/track read-n n)))

(def funs {'+ +
           '- -
           '* *
           '/ /})

(defn expand [x]
  (cond (and (list? x)
             (-> x first funs))
        (let [y (map expand (rest x))]
          (if (every? number? y)
            (apply (funs (first x)) y)
            y))

        (and (list? x)
             (= (first x) 'quote))
        @(r/track result (nth x 1))

        :else x))

(defn calc-input [n]
  [:input {:type 'text
           :value @(r/track input-n n)
           :on-change #(emit [:set n (.-target.value %)])
           :style {:font-family 'courier
                   :width "100%"}}])

(defn calc-output [n]
  (try
    (let [val @(r/track result n)]
      (when val
        [:pre " = " (pr-str val)]))
    (catch :default e
      [:p {:style {:color 'red}}
       "Error: " (.-message e)])))

(defn calc-table []
  [:table {:style {:width "100%"}}
   [:tbody
    (doall
      (for [i (range @(r/track input-count))]
        ^{:key i} [:tr
                   [:td i]
                   [:td [calc-input i]]
                   [:td [calc-output i]]
                   [:td @(r/track input-n i)]]))]])

(defn calculations []
  [:div
   (pr-str @calcs)
   [calc-table]])

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

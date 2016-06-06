(ns reagent.impl.batching
  (:refer-clojure :exclude [flush])
  (:require [reagent.debug :refer-macros [dbg]]
            [reagent.interop :refer-macros [$ $!]]
            [reagent.impl.util :refer [is-client]]
            [clojure.string :as string]))

;;; Update batching

(defonce mount-count 0)

(defn next-mount-count []
  (set! mount-count (inc mount-count)))

(defn fake-raf [f]
  (js/setTimeout f 16))

(def next-tick
  (if-not is-client
    fake-raf
    (let [w js/window]
      (or ($ w :requestAnimationFrame)
          ($ w :webkitRequestAnimationFrame)
          ($ w :mozRequestAnimationFrame)
          ($ w :msRequestAnimationFrame)
          fake-raf))))

(defn compare-mount-order [c1 c2]
  (- ($ c1 :cljsMountOrder)
     ($ c2 :cljsMountOrder)))

(defn run-queue [a]
  ;; sort components by mount order, to make sure parents
  ;; are rendered before children
  (.sort a compare-mount-order)
  (dotimes [i (alength a)]
    (let [c (aget a i)]
      (when (true? ($ c :cljsIsDirty))
        ($ c forceUpdate)))))


;; Set from ratom.cljs
(defonce ratom-flush (fn []))

(deftype RenderQueue [^:mutable ^boolean scheduled?
                      ^:mutable ^boolean running]
  Object
  (enqueue [this k f]
    (assert (some? f))
    (when (nil? (aget this k))
      (aset this k (array)))
    (.push (aget this k) f)
    (.schedule this))

  (run-funs [this k]
    (when-some [fs (aget this k)]
      (aset this k nil)
      (dotimes [i (alength fs)]
        ((aget fs i)))))

  (schedule [this]
    (when-not scheduled?
      (set! scheduled? true)
      (when-not running
        (next-tick #(.run-queues this)))))

  (queue-render [this c]
    (.enqueue this "componentQueue" c))

  (add-before-flush [this f]
    (.enqueue this "beforeFlush" f))

  (add-after-render [this f]
    (.enqueue this "afterRender" f))

  (run-queues [this]
    (set! running true)
    (set! scheduled? false)
    (let [start (.now js/Date)
          _ (try
              (.flush-queues this)
              (set! running false)
              (finally
                (when running
                  (set! running false)
                  (set! scheduled? false))))
          end (.now js/Date)]
      (when scheduled?
        (let [rendtime (- end start)]
          (if (<= (- end start) 16)
            (next-tick #(.run-queues this))
            ;; Skip frame to let browser catch up
            (js/setTimeout (fn [] (next-tick #(.run-queues this)))
                           16))))))

  (flush-after-render [this]
    (.run-funs this "afterRender"))

  (flush-queues [this]
    (.run-funs this "beforeFlush")
    (ratom-flush)
    (when-some [cs (aget this "componentQueue")]
      (aset this "componentQueue" nil)
      (run-queue cs))
    (.flush-after-render this)))

(def render-queue (RenderQueue. false false))

(defn flush []
  (.flush-queues render-queue))

(defn flush-after-render []
  (.flush-after-render render-queue))

(defn queue-render [c]
  (when-not ($ c :cljsIsDirty)
    ($! c :cljsIsDirty true)
    (.queue-render render-queue c)))

(defn mark-rendered [c]
  ($! c :cljsIsDirty false))

(defn do-before-flush [f]
  (.add-before-flush render-queue f))

(defn do-after-render [f]
  (.add-after-render render-queue f))

(defn schedule []
  (when (false? (.-scheduled? render-queue))
    (.schedule render-queue)))

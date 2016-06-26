(ns reagent.impl.batching
  (:refer-clojure :exclude [flush])
  (:require [reagent.debug :refer-macros [dbg]]
            [reagent.interop :refer-macros [$ $!]]
            [reagent.impl.util :refer [is-client]]
            [clojure.string :as string])
  (:import [goog.async.nextTick]))

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

(defn update-components [a]
  ;; sort components by mount order, to make sure parents
  ;; are rendered before children
  (.sort a compare-mount-order)
  (dotimes [i (alength a)]
    (let [c (aget a i)]
      (when (true? ($ c :cljsIsDirty))
        ($ c forceUpdate)))))


;; Set from ratom.cljs
(defonce ratom-flush (fn []))

(defn set-immediate [f]
  ;; Like (js/setTimeout f 0), but without the delay
  (goog.async.nextTick f))

(deftype RenderQueue [^:mutable ^boolean scheduled?
                      ^:mutable ^boolean waiting]
  Object
  (enqueue [this k f]
    (assert (some? f))
    (if-some [q (aget this k)]
      (.push q f)
      (aset this k (array f)))
    (.schedule this))

  (run-funs [this k]
    (when-some [fs (aget this k)]
      (aset this k nil)
      (dotimes [i (alength fs)]
        ((aget fs i)))))

  (schedule [this]
    (when-not scheduled?
      (set! scheduled? true)
      (when-not waiting
        (set! waiting true)
        ;; Don't use raf on first schedule, to avoid Safari delay
        (set-immediate #(.run-queues this))))
    nil)

  (queue-render [this c]
    (.enqueue this "componentQueue" c))

  (add-before-flush [this f]
    (.enqueue this "beforeFlush" f))

  (add-after-render [this f]
    (.enqueue this "afterRender" f))

  (run-queues [this]
    (try
      (.flush-queues this)
      (set! waiting false)
      (finally
        (when waiting
          (set! waiting false)
          (set! scheduled? false))))
    (when (set! waiting scheduled?)
      (set! scheduled? false)
      ;; Wrap call to raf to allow browser time to process events
      (set-immediate (fn [] (next-tick #(.run-queues this))))))

  (flush-after-render [this]
    (.run-funs this "afterRender"))

  (flush-queues [this]
    (.run-funs this "beforeFlush")
    (ratom-flush)
    (when-some [cs (aget this "componentQueue")]
      (aset this "componentQueue" nil)
      (update-components cs))
    (.flush-after-render this)))

(def render-queue (->RenderQueue false false))

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
  (.schedule render-queue))

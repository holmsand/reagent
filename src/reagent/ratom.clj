(ns reagent.ratom
  (:refer-clojure :exclude [run!])
  (:require [reagent.debug :as d]))

(defmacro reaction [& body]
  `(reagent.ratom/make-reaction
    (fn [] ~@body)))

(defmacro run!
  "Runs body immediately, and runs again whenever atoms deferenced in the body change. Body should side effect."
  [& body]
  `(let [co# (reagent.ratom/make-reaction (fn [] ~@body)
                                         :auto-run true)]
     (deref co#)
     co#))

(defmacro with-let [bindings & body]
  (assert (vector? bindings))
  (let [v (gensym "with-let")
        k (keyword v)
        init (gensym "init")
        bs (into [init `(zero? (alength ~v))]
                 (map-indexed (fn [i x]
                                (if (even? i)
                                  x
                                  (let [j (quot i 2)]
                                    `(if ~init
                                       (aset ~v ~j ~x)
                                       (aget ~v ~j)))))
                              bindings))
        [forms destroy] (let [fin (last body)]
                          (if (and (list? fin)
                                   (= 'finally (first fin)))
                            [(butlast body) (rest fin)]
                            [body nil]))
        have-destroy (some? destroy)
        asserting (if *assert* true false)]
    `(let [~v (reagent.ratom/with-let-values ~k)
           c# reagent.ratom/*ratom-context*]
       (when ~asserting
         (when (some? c#)
           (when (== (.-let-generation ~v) (.-execGen c#))
             (d/error "Warning: The same with-let is being used more "
                      "than once in the same reactive context."))
           (set! (.-let-generation ~v) (.-execGen c#))))
       (let ~bs
         (let [res# (do ~@forms)]
           (when ~have-destroy
             (if-not (nil? c#)
               (when (nil? (.-destroy ~v))
                 (set! (.-destroy ~v) (fn [] ~@destroy)))
               (do ~@destroy)))
           res#)))))

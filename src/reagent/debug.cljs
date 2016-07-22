(ns reagent.debug
  (:require-macros [reagent.debug]))

(defonce inline-errors true)

(deftype FakeConsole []
  Object
  (log [_])
  (warn [_])
  (error [_]))

(def real-console (if (exists? js/console) js/console (->FakeConsole)))

(def console real-console)

(def warnings (atom nil))

(deftype TrackConsole []
  Object
  (log [& args] (.log real-console (apply str args)))
  (warn [& args] (swap! warnings update-in [:warn] conj
                        (apply str (js/Array.prototype.slice.call
                                     (js-arguments)))))
  (error [& args] (swap! warnings update-in [:error] conj
                         (apply str (js/Array.prototype.slice.call
                                      (js-arguments))))))

(defn track-warnings [f]
  (set! console (->TrackConsole))
  (reset! warnings nil)
  (try
    (f)
    @warnings
    (finally
      (reset! warnings nil)
      (set! console real-console))))

(def last-exception nil)

(defn exception [save e msg]
  (cond
    (reagent.debug/dev?) (do
                           (.warn console msg e)
                           (when save
                             (.error console (or (.-stack e) e))
                             (set! last-exception e)))
    save (.error console msg e)))

(defn clear-errors []
  (set! last-exception nil))

(defn report-errors []
  (when-some [e last-exception]
    (clear-errors)
    (throw e)))

(defonce ^boolean has-dom false)

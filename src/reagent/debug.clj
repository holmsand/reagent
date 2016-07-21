(ns reagent.debug
  (:refer-clojure :exclude [prn println time]))

(defmacro log
  "Print with console.log, if it exists."
  [& forms]
  `(.log reagent.debug/console ~@forms))

(defmacro warn
  "Print with console.warn."
  [& forms]
  (when *assert*
    `(.warn reagent.debug/console ~@forms)))

(defmacro warn-unless
  [cond & forms]
  (when *assert*
    `(when (not ~cond)
       (warn ~@forms))))

(defmacro error
  "Print with console.error."
  [& forms]
  (when *assert*
    `(.error reagent.debug/console ~@forms)))

(defmacro println
  "Print string with console.log"
  [& forms]
  `(log ~@forms))

(defmacro prn
  "Like standard prn, but prints using console.log (so that we get
nice clickable links to source in modern browsers)."
  [& forms]
  `(log (pr-str ~@forms)))

(defmacro dbg
  "Useful debugging macro that prints the source and value of x,
as well as package name and line number. Returns x."
  [x]
  (let [ns (str cljs.analyzer/*cljs-ns*)]
    `(let [x# ~x]
       (println (str "dbg "
                     ~ns ":"
                     ~(:line (meta &form))
                     ": "
                     ~(pr-str x)
                     ": "
                     (pr-str x#)))
       x#)))

(defmacro dev?
  "True if assertions are enabled."
  []
  (if *assert* true false))

(defmacro time [& forms]
  (let [ns (str cljs.analyzer/*cljs-ns*)
        label (str ns ":" (:line (meta &form)))]
    `(let [label# ~label
           res# (do
                  (js/console.time label#)
                  ~@forms)]
       (js/console.timeEnd label#)
       res#)))

(defmacro with-tracked-warnings [& forms]
  `(reagent.debug/track-warnings (fn [] ~@forms)))

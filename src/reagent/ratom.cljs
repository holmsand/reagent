(ns reagent.ratom
  (:refer-clojure :exclude [atom])
  (:require-macros [reagent.ratom :refer [with-let]])
  (:require [clojure.set :as s]
            [reagent.debug :refer-macros [dbg log warn error dev? time]]
            [reagent.impl.batching :as batch]
            [reagent.impl.util :as util]))

(declare ^:dynamic *ratom-context*)
(defonce ^boolean debug false)
(defonce ^:private ^number generation 1)
(defonce ^:private ^number with-let-gen 1)
(defonce ^:private ^number -nwatches 0)

(defn ^boolean reactive? []
  (some? *ratom-context*))


;;; Utilities

(defn running [] -nwatches)

(declare ^:dynamic *-captured*)

(defn- deref-capture [r f update check shallow]
  (binding [*ratom-context* r
            *-captured* {}]
    (._captured-exec r f update check shallow)))

(defn- notify-deref-watcher! [derefed]
  (when-not (nil? *ratom-context*)
    (set! *-captured* (assoc *-captured* derefed nil))))

(defn- add-w [this key f]
  (set! (.-watches this) (assoc (.-watches this) key f)))

(defn- remove-w [this key]
  (let [ws (dissoc (.-watches this) key)]
    (set! (.-watches this) (when (-> ws count pos?) ws))))

(defn- notify-w [this old new]
  (when-some [ws (.-watches this)]
    (doseq [[key f] ws]
      (f key this old new))))

(defn- check-watches [old new]
  (when debug (set! -nwatches (+ -nwatches (- (count new) (count old)))))
  new)

(def ^:private -empty-array (array))

(defn- coll-array [c]
  (if (-> c count zero?)
    -empty-array
    (if-some [a (.-ratomCollArray c)]
      a
      (set! (.-ratomCollArray c)
            (into-array (if debug (shuffle c) c))))))

(defn- map-key-array [m]
  (if (-> m count zero?)
    -empty-array
    (if-some [a (.-ratomMapKeyArray m)]
      a
      (set! (.-ratomMapKeyArray m)
            (into-array (if debug (shuffle (keys m)) (keys m)))))))

(defn- add-r [this r]
  (let [w (or (.-reactions this) #{})]
    (set! (.-reactions this) (check-watches w (conj w r)))))

(defn- remove-r [this r]
  (let [w (.-reactions this)
        w' (disj w r)
        w' (when (-> w' count pos?) w')]
    (set! (.-reactions this) (check-watches w w'))))

(defn- notify-r [this ^boolean shallow]
  (when-some [rs (.-reactions this)]
    (let [a (coll-array rs)
          age (.-age this)]
      ;; mark children as dirty first, so that _refresh always works
      (dotimes [i (alength a)] (._mark-dirty (aget a i) age))
      (when-not shallow
        (dotimes [i (alength a)] (._handle-change (aget a i)))))))

(defn- pr-atom [a writer opts s]
  (-write writer (str "#<" s " "))
  (pr-writer (binding [*ratom-context* nil] (-deref a)) writer opts)
  (-write writer ">"))


;;; Queueing

(defonce ^:private ratom-queue nil)
(defonce ^:private ^number flush-generation -1)
(defonce ^:private -no-value #js {})
(defonce ^:private -unique-value #js {})

(defn- flush-atoms []
  (assert (== flush-generation -1) "Can't flush in flush")
  (set! flush-generation generation)
  (when-some [q ratom-queue]
    (set! ratom-queue nil)
    (dotimes [i (alength q)]
      (._notify (aget q i)))))

(defn flush! []
  (try (flush-atoms)
       (finally (set! flush-generation -1))))

(defn- ^number atom-generation []
  (if (== flush-generation -1) generation flush-generation))

(set! batch/ratom-flush flush!)


;;; Atom

(defprotocol IReactiveAtom)


(deftype RAtom [^:mutable state meta validator ^:mutable watches
                ^:mutable oldstate ^:mutable ^number age]
  IAtom
  IReactiveAtom

  IEquiv
  (-equiv [o other] (identical? o other))

  IDeref
  (-deref [this]
    (notify-deref-watcher! this)
    state)

  Object
  (_add-reaction [this r]
    (when-not (identical? oldstate -no-value)
      ;; oldstate can no longer be trusted
      (set! oldstate -unique-value))
    (add-r this r))

  (_remove-reaction [this r] (remove-r this r))

  (_enqueue [a old]
    (set! age (set! generation (inc generation)))
    (when (identical? oldstate -no-value)
      (set! oldstate old)
      (when (nil? ratom-queue)
        (set! ratom-queue (array))
        (batch/schedule))
      (.push ratom-queue a)))

  (_notify [a]
    (try
      (when (not= oldstate state)
        (set! oldstate -no-value)
        (notify-r a false))
      (finally
        (set! oldstate -no-value))))

  (_refresh [a compare]
    (cond
      (>= compare age) false
      ;; would flush trigger an update?
      (identical? oldstate -no-value) false
      (= oldstate state) (do (set! oldstate state) false)
      :else true))

  IReset
  (-reset! [a new-value]
    (when-not (nil? validator)
      (assert (validator new-value) "Validator rejected reference state"))
    (let [old-value state]
      (set! state new-value)
      (when-not (identical? old-value new-value)
        (._enqueue a old-value))
      (notify-w a old-value new-value)
      new-value))

  ISwap
  (-swap! [a f]          (-reset! a (f state)))
  (-swap! [a f x]        (-reset! a (f state x)))
  (-swap! [a f x y]      (-reset! a (f state x y)))
  (-swap! [a f x y more] (-reset! a (apply f state x y more)))

  IMeta
  (-meta [_] meta)

  IPrintWithWriter
  (-pr-writer [a w opts] (pr-atom a w opts "Atom:"))

  IWatchable
  (-notify-watches [this old new] (notify-w this old new))
  (-add-watch [this key f]        (add-w this key f))
  (-remove-watch [this key]       (remove-w this key))

  IHash
  (-hash [this] (goog/getUid this)))

(defn atom
  "Like clojure.core/atom, except that it keeps track of derefs."
  ([x] (->RAtom x nil nil nil -no-value -1))
  ([x & {:keys [meta validator]}] (->RAtom x meta validator nil -no-value -1)))


;;; track

(declare make-reaction)

(def ^:private cache-key "reagReactionCache")

(defn- cached-reaction [f o k obj destroy]
  (let [m (aget o cache-key)
        r (get m k nil)]
    (cond
      (some? r) (-deref r)
      (nil? *ratom-context*) (f)
      :else (let [r (make-reaction
                     f :on-dispose (fn [r s]
                                     (when debug (set! -nwatches (dec -nwatches)))
                                     (as-> (aget o cache-key) _
                                       (dissoc _ k)
                                       (aset o cache-key _))
                                     (when (some? obj)
                                       (set! (.-reaction obj) nil))
                                     (when (some? destroy)
                                       (destroy s))))
                  v (-deref r)]
              (aset o cache-key (assoc (if (nil? m) {} m) k r))
              (when debug (set! -nwatches (inc -nwatches)))
              (when (some? obj)
                (set! (.-reaction obj) r))
              v))))

(deftype Track [f args ^:mutable reaction]
  IReactiveAtom

  IDeref
  (-deref [this]
    (if-some [r reaction]
      (-deref r)
      (cached-reaction #(apply f args) f args this nil)))

  IEquiv
  (-equiv [_ other]
    (and (instance? Track other)
         (= f (.-f other))
         (= args (.-args other))))

  IHash
  (-hash [_] (hash [f args]))

  IPrintWithWriter
  (-pr-writer [a w opts] (pr-atom a w opts "Track:")))

(defn make-track [f args]
  (->Track f args nil))

(defn make-track! [f args]
  (let [t (make-track f args)
        r (make-reaction #(-deref t)
                         :auto-run true)]
    @r
    r))

(defn track [f & args]
  {:pre [(ifn? f)]}
  (make-track f args))

(defn track! [f & args]
  {:pre [(ifn? f)]}
  (make-track! f args))

;;; cursor

(deftype RCursor [ratom path ^:mutable reaction
                  ^:mutable state ^:mutable watches]
  IAtom
  IReactiveAtom

  IEquiv
  (-equiv [_ other]
    (and (instance? RCursor other)
         (= path (.-path other))
         (= ratom (.-ratom other))))

  Object
  (_peek [this]
    (binding [*ratom-context* nil]
      (-deref this)))

  (_set-state [this oldstate newstate]
    (when-not (identical? oldstate newstate)
      (set! state newstate)
      (notify-w this oldstate newstate)))

  IDeref
  (-deref [this]
    (let [oldstate state
          newstate (if-some [r reaction]
                     (-deref r)
                     (let [f (if (satisfies? IDeref ratom)
                               #(get-in @ratom path)
                               #(ratom path))]
                       (cached-reaction f ratom path this nil)))]
      (._set-state this oldstate newstate)
      newstate))

  IReset
  (-reset! [this new-value]
    (let [oldstate state]
      (._set-state this oldstate new-value)
      (if (satisfies? IDeref ratom)
        (if (= path [])
          (reset! ratom new-value)
          (swap! ratom assoc-in path new-value))
        (ratom path new-value))
      new-value))

  ISwap
  (-swap! [a f]          (-reset! a (f (._peek a))))
  (-swap! [a f x]        (-reset! a (f (._peek a) x)))
  (-swap! [a f x y]      (-reset! a (f (._peek a) x y)))
  (-swap! [a f x y more] (-reset! a (apply f (._peek a) x y more)))

  IPrintWithWriter
  (-pr-writer [a w opts] (pr-atom a w opts (str "Cursor: " path)))

  IWatchable
  (-notify-watches [this old new] (notify-w this old new))
  (-add-watch [this key f]        (add-w this key f))
  (-remove-watch [this key]       (remove-w this key))

  IHash
  (-hash [_] (hash [ratom path])))

(defn cursor
  [src path]
  (assert (or (satisfies? IReactiveAtom src)
              (and (ifn? src)
                   (not (vector? src))))
          (str "src must be a reactive atom or a function, not "
               (pr-str src)))
  (->RCursor src path nil nil nil))


;;; with-let support

(defn with-let-destroy [v]
  (when-some [f (.-destroy v)]
    (f)))

(defn with-let-values [key]
  (if-some [c *ratom-context*]
    (cached-reaction array c key
                     nil with-let-destroy)
    (array)))


;;;; reaction

(defprotocol IDisposable
  (dispose! [this])
  (add-on-dispose! [this f]))

(defprotocol IRunnable
  (run [this]))

(def recursion-error "Recursion in Reaction not allowed")
(def updating -2)

(deftype ReactionEx [error])

(deftype Reaction [f ^:mutable state ^:mutable auto-run ^:mutable on-dispose
                   ^:mutable ^number age
                   ^:mutable reactions ^:mutable watches ^:mutable watching]
  IAtom
  IReactiveAtom

  IWatchable
  (-notify-watches [this old new] (notify-w this old new))
  (-add-watch [this key f]        (add-w this key f))
  (-remove-watch [this key]       (remove-w this key))

  IReset
  (-reset! [a newval]
    (assert (some? (.-on-set a)) "Reaction is read only.")
    (let [old state]
      (set! state newval)
      ((.-on-set a) old newval)
      newval))

  ISwap
  (-swap! [a f]          (-reset! a (f (._peek-at a))))
  (-swap! [a f x]        (-reset! a (f (._peek-at a) x)))
  (-swap! [a f x y]      (-reset! a (f (._peek-at a) x y)))
  (-swap! [a f x y more] (-reset! a (apply f (._peek-at a) x y more)))

  Object
  (_peek-at [this]
    (binding [*ratom-context* nil]
      (-deref this)))

  (_add-reaction [this r] (add-r this r))

  (_remove-reaction [this r]
    (let [rs reactions]
      (remove-r this r)
      (when (and (nil? auto-run) (some? rs) (nil? reactions))
        (dispose! this))))

  (_mark-dirty [this otherage]
    (when (> otherage age)
      (set! age -1)))

  (_handle-change [this]
    (when (and (== age -1) (some? watching))
      (if (or (nil? auto-run) (true? auto-run))
        (deref-capture this f true (nil? auto-run) false)
        (auto-run this))))

  (_update-watching [this derefed]
    (let [new (-> derefed keys set)
          old (-> watching keys set)]
      (set! watching derefed)
      (doseq [w (s/difference new old)]
        (._add-reaction w this))
      (doseq [w (s/difference old new)]
        (._remove-reaction w this))))

  (_unchecked-exec [this f gen]
    (set! age updating)
    (let [res (f)]
      (set! age gen)
      res))

  (_try-exec [this f gen]
    (try
      (._unchecked-exec this f gen)
      (catch :default e
        (set! age gen)
        (when (= recursion-error (.-message e)) (throw e))
        (error "Error in Reaction: " e)
        (->ReactionEx e))))

  (_maybe-notify [this old new shallow]
    (when-not (or (and (nil? reactions) (nil? watches))
                  (= old new))
      (notify-w this old new)
      (notify-r this shallow)))

  (_handle-result [this res derefed shallow]
    (let [old state
          caching (not (identical? old -no-value))]
      (when caching
        (set! state res))
      (when-not (or (nil? derefed)
                    (= derefed watching))
        (._update-watching this derefed))
      (when caching
        (._maybe-notify this old res shallow)))
    res)

  (_captured-exec [r f ^boolean update ^boolean check shallow]
    (when (dev?) (set! (.-execGen r) (set! with-let-gen (inc with-let-gen))))
    (let [res (if check
                (._try-exec r f (atom-generation))
                (._unchecked-exec r f (atom-generation)))]
      (if update
        (._handle-result r res *-captured* shallow)
        [res *-captured*])))

  (_run-reactive [this]
    (deref-capture this f true false true))

  (_run-refresh [this check]
    (if (and (nil? watching) (nil? *ratom-context*) (nil? auto-run))
      (._handle-result this (._unchecked-exec this f (atom-generation)) nil true)
      (deref-capture this f true check true)))

  (_refresh-watching [this compare]
    (let [ks (map-key-array watching)
          len (alength ks)]
      (loop [i 0]
        (when (< i len)
          (if ^boolean (._refresh (aget ks i) compare)
            (set! age -1)
            (when-not (neg? age) ; did parent mark us dirty?
              (recur (inc i))))))
      (neg? age)))

  (_refresh [this compare]
    (let [gen (atom-generation)
          a age
          dirty (cond
                  (>= a gen) false
                  (neg? a) true
                  (nil? watching) true
                  :else ^boolean (._refresh-watching this a))]
      (if dirty
        (._run-refresh this (not (== compare -1)))
        (set! age gen))
      false))

  (_set-opts [this {:keys [auto-run on-set on-dispose no-cache]}]
    (when (some? auto-run)   (set! (.-auto-run this) auto-run))
    (when (some? on-set)     (set! (.-on-set this) on-set))
    (when (true? no-cache)   (set! state -no-value))
    (when (some? on-dispose) (add-on-dispose! this on-dispose)))

  IRunnable
  (run [this]
    (._run-reactive this))

  IDeref
  (-deref [this]
    (notify-deref-watcher! this)
    (when (instance? ReactionEx state) (throw (.-error state)))
    (when (== age updating) (throw (js/Error. recursion-error)))
    (._refresh this -1)
    state)

  IDisposable
  (dispose! [this]
    (let [s state]
      (doseq [w (keys watching)]
        (._remove-reaction w this))
      (set! watching nil)
      (set! state nil)
      (set! auto-run nil)
      (set! age -1)
      (when-some [a on-dispose]
        (dotimes [i (alength a)]
          ((aget a i) this s)))))

  (add-on-dispose! [this f]
    ;; f is called with the reaction and last state as arguments when
    ;; it is no longer active
    (if-some [od on-dispose]
      (.push od f)
      (set! on-dispose (array f))))

  IEquiv
  (-equiv [o other] (identical? o other))

  IPrintWithWriter
  (-pr-writer [a w opts] (pr-atom a w opts (str "Reaction " (hash a) ":")))

  IHash
  (-hash [this] (goog/getUid this)))

(defn make-reaction [f & {:keys [auto-run on-set on-dispose]}]
  {:pre [(ifn? f)
         (or (nil? on-set) (fn? on-set))
         (or (nil? auto-run) (true? auto-run) (false? auto-run) (fn? auto-run))
         (or (nil? on-dispose) (fn? on-dispose))]}
  (let [ar (if (false? auto-run) nil auto-run)
        od (and on-dispose (array on-dispose))
        r (->Reaction f nil ar od -1 nil nil nil)]
    (when on-set (._set-opts r {:on-set on-set}))
    r))


(def ^:private temp-reaction (make-reaction (fn [])))

(defn run-in-reaction [f obj key run opts]
  (let [r temp-reaction
        res (deref-capture r f true false true)]
    (when (-> r .-watching count pos?)
      (set! temp-reaction (make-reaction (fn [])))
      (._set-opts r opts)
      (set! (.-f r) f)
      (set! (.-auto-run r) #(run obj))
      (aset obj key r))
    res))

(def ^:private check-reaction (make-reaction (fn [])))

(defn check-derefs [f]
  (let [[res captured] (deref-capture check-reaction f false false true)]
    [res (-> captured count pos?)]))


;;; wrap

(deftype Wrapper [^:mutable state callback ^:mutable ^boolean changed
                  ^:mutable watches]

  IAtom

  IDeref
  (-deref [this]
    (when (dev?)
      (when (and changed (some? *ratom-context*))
        (warn "derefing stale wrap: "
              (pr-str this))))
    state)

  IReset
  (-reset! [this newval]
    (let [oldval state]
      (set! changed true)
      (set! state newval)
      (notify-w this oldval newval)
      (callback newval)
      newval))

  ISwap
  (-swap! [a f]          (-reset! a (f state)))
  (-swap! [a f x]        (-reset! a (f state x)))
  (-swap! [a f x y]      (-reset! a (f state x y)))
  (-swap! [a f x y more] (-reset! a (apply f state x y more)))

  IEquiv
  (-equiv [_ other]
    (and (instance? Wrapper other)
         ;; If either of the wrappers have changed, equality
         ;; cannot be relied on.
         (not changed)
         (not (.-changed other))
         (= state (.-state other))
         (= callback (.-callback other))))

  IWatchable
  (-notify-watches [this old new] (notify-w this old new))
  (-add-watch [this key f]        (add-w this key f))
  (-remove-watch [this key]       (remove-w this key))

  IPrintWithWriter
  (-pr-writer [a w opts] (pr-atom a w opts "Wrap:")))

(defn make-wrapper [value callback-fn args]
  (->Wrapper value
             (util/partial-ifn. callback-fn args nil)
             false nil))




#_(do
  (defn ratom-perf []
    (set! debug false)
    (dotimes [_ 10]
      (let [nite 100000
            a (atom 0)
            f (fn []
                (quot @a 10))
            mid (make-reaction f)
            res (track! (fn []
                          ;; (with-let [x 1])
                          ;; @(track f)
                          (inc @mid)
                          ))]
        @res
        (time (dotimes [x nite]
                (swap! a inc)
                ;; @res #_
                (flush!)))
        (dispose! res))))
  (ratom-perf))

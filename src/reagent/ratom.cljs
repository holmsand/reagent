(ns reagent.ratom
  (:refer-clojure :exclude [atom])
  (:require-macros [reagent.ratom :refer [with-let]])
  (:require [reagent.impl.util :as util]
            [reagent.debug :refer-macros [dbg log warn error dev? time]]
            [reagent.impl.batching :as batch]
            [clojure.set :as s]))

(declare ^:dynamic *ratom-context*)
(defonce ^boolean debug false)
(defonce ^:private generation 0)
(defonce ^:private with-let-gen 0)
(defonce ^:private -running (clojure.core/atom 0))

(defn ^boolean reactive? []
  (some? *ratom-context*))


;;; Utilities

(defn running []
  (+ @-running))

(defn- ^number arr-len [x]
  (if (nil? x) 0 (alength x)))

(defn- ^boolean arr-eq [x y]
  (let [len (arr-len x)]
    (and (== len (arr-len y))
         (loop [i 0]
           (or (== i len)
               (if (identical? (aget x i) (aget y i))
                 (recur (inc i))
                 false))))))

(declare ^:dynamic -captured)

(defn- check-depth [r limit]
  (when-some [d (.-rundepth r)]
    (when (> d limit)
      (set! (.-rundepth r) 0)
      (throw (js/Error. (str "Recursion limit in Reaction exceeded "
                             limit " " (.-f r)))))))

(defn- in-context [obj f ^boolean update ^boolean check ^boolean notify]
  (binding [*ratom-context* obj
            -captured nil]
    (let [res
          (if check
            (._try-exec obj f)
            (f))]
      (if update
        (let [c -captured]
          (set! *ratom-context* nil)
          (set! -captured nil)
          (._handle-result obj res c notify))
        [res -captured]))))

(defn- deref-capture [f r ^boolean check notify]
  (when (dev?)
    (set! (.-ratomGeneration r) (set! with-let-gen (inc with-let-gen))))
  (let [dep (if-some [d (.-rundepth r)] d 0)]
    (set! (.-rundepth r) (inc dep))
    (try
      (in-context r f true check notify)
      (finally
        (set! (.-rundepth r) dep)))))

(defn- notify-deref-watcher! [derefed]
  (when-some [r *ratom-context*]
    (if (nil? -captured)
      (set! -captured (array derefed))
      (.push -captured derefed))))

(defn- add-w [this key f]
  (set! (.-watches this) (assoc (.-watches this) key f)))

(defn- remove-w [this key]
  (set! (.-watches this) (dissoc (.-watches this) key)))

(defn- notify-w [this old new]
  (doseq [[key f] (.-watches this)]
    (f key this old new)))

(defn- check-watches [old new]
  (when debug
    (swap! -running + (- (count new) (count old))))
  new)

(defn- add-r [this r]
  (let [w (.-reactions this)
        w' (if (nil? w) #{r} (conj w r))]
    (set! (.-reactions this) (check-watches w w'))
    (set! (.-reactionsArr this) nil)))

(defn- remove-r [this r]
  (let [w (.-reactions this)]
    (set! (.-reactions this) (check-watches w (disj w r)))
    (set! (.-reactionsArr this) nil)))

(defn- notify-r [this]
  (let [w (.-reactionsArr this)
        a (if (nil? w)
            ;; Copy watches to array for speed
            (set! (.-reactionsArr this) (into-array (.-reactions this)))
            w)
        len (alength a)]
    (dotimes [i len]
      (._handle-change (aget a i)))))

(defn- pr-atom [a writer opts s]
  (-write writer (str "#<" s " "))
  (pr-writer (binding [*ratom-context* nil] (-deref a)) writer opts)
  (-write writer ">"))


;;; Queueing

(defonce ^:private ratom-queue nil)

(defn- ratom-enqueue [a old]
  (set! generation (inc generation))
  (set! (.-age a) generation)
  (when-not (true? (.-queued a))
    (when (nil? ratom-queue)
      (set! ratom-queue (array))
      (batch/schedule))
    (set! (.-queued a) true)
    (.push ratom-queue a)
    (.push ratom-queue old)))

(defn flush! []
  (let [q ratom-queue]
    (when-not (nil? q)
      (set! ratom-queue nil)
      (let [len (alength q)]
        (loop [i 0]
          (when (< i len)
            (let [a (aget q i)
                  old (aget q (inc i))
                  new (.-state a)]
              (set! (.-queued a) false)
              (when (not= old new)
                (notify-r a)))
            (recur (+ 2 i))))))))

(set! batch/ratom-flush flush!)


;;; Atom

(defprotocol IReactiveAtom)

(defprotocol IReactionWatchable
  (-add-reaction [this r])
  (-remove-reaction [this r]))

(deftype RAtom [^:mutable state meta validator ^:mutable watches]
  IAtom
  IReactiveAtom

  IEquiv
  (-equiv [o other] (identical? o other))

  IDeref
  (-deref [this]
    (notify-deref-watcher! this)
    state)

  IReset
  (-reset! [a new-value]
    (when-not (nil? validator)
      (assert (validator new-value) "Validator rejected reference state"))
    (let [old-value state]
      (set! state new-value)
      (when-not (identical? old-value new-value)
        (ratom-enqueue a old-value))
      (when-not (nil? watches)
        (notify-w a old-value new-value))
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

  IReactionWatchable
  (-add-reaction [this r]         (add-r this r))
  (-remove-reaction [this r]      (remove-r this r))

  IWatchable
  (-notify-watches [this old new] (notify-w this old new))
  (-add-watch [this key f]        (add-w this key f))
  (-remove-watch [this key]       (remove-w this key))

  IHash
  (-hash [this] (goog/getUid this)))

(defn atom
  "Like clojure.core/atom, except that it keeps track of derefs."
  ([x] (RAtom. x nil nil nil))
  ([x & {:keys [meta validator]}] (RAtom. x meta validator nil)))


;;; track

(declare make-reaction)

(def ^{:private true :const true} cache-key "reagReactionCache")

(defn- cached-reaction [f o k obj destroy]
  (let [m (aget o cache-key)
        m (if (nil? m) {} m)
        r (m k nil)]
    (cond
      (some? r) (-deref r)
      (nil? *ratom-context*) (f)
      :else (let [r (make-reaction
                     f :on-dispose (fn [x]
                                     (when debug (swap! -running dec))
                                     (as-> (aget o cache-key) _
                                       (dissoc _ k)
                                       (aset o cache-key _))
                                     (when (some? obj)
                                       (set! (.-reaction obj) nil))
                                     (when (some? destroy)
                                       (destroy x))))
                  v (-deref r)]
              (aset o cache-key (assoc m k r))
              (when debug (swap! -running inc))
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
  (Track. f args nil))

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
      (when (some? watches)
        (notify-w this oldstate newstate))))

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
  (RCursor. src path nil nil nil))


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
  (dispose! [this]))

(defprotocol IRunnable
  (run [this]))

(defprotocol IReaction
  (add-on-dispose! [this f]))

(deftype Reaction [f ^:mutable state ^boolean nocache?
                   ^:mutable watching ^:mutable watches ^:mutable auto-run
                   ^:mutable caught ^:mutable ^number age
                   ^:mutable ^number last-update]
  IAtom
  IReactiveAtom

  IReactionWatchable
  (-add-reaction [this r]
    (add-r this r))
  (-remove-reaction [this r]
    (let [was-empty (and (empty? (.-reactions this))
                         (empty? watches))]
      (remove-r this r)
      (when (and (not was-empty)
                 (empty? (.-reactions this))
                 (empty? watches)
                 (nil? auto-run))
        (dispose! this))))

  IWatchable
  (-notify-watches [this old new] (notify-w this old new))
  (-add-watch [this key f]        (add-w this key f))
  (-remove-watch [this key]
    (let [was-empty (empty? watches)]
      (remove-w this key)
      (when (and (not was-empty)
                 (empty? watches)
                 (nil? auto-run))
        (dispose! this))))

  IReset
  (-reset! [a newval]
    (assert (fn? (.-on-set a)) "Reaction is read only.")
    (let [oldval state]
      (set! state newval)
      (.on-set a oldval newval)
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

  (_dirty? [this]
    (not (== age generation)))

  (_check-dirty? [this notify]
    (let [dirty (cond
                  (== age -1) true
                  (not (._dirty? this)) false
                  :else
                  (some
                   (fn [r]
                     (if (some? (.-_check-dirty? r))
                       (._check-dirty? r false)
                       (< age (.-age r))))
                   watching))]
      (if dirty
        (let [os state]
          (do (._exec this notify)
              (or (== age last-update)
                  (not= state os))))
        (do (set! age generation)
            false))))

  (_handle-change [this]
    (if (nil? auto-run)
      (when-not (== age -1)
        (._run this true true))
      (if (true? auto-run)
        (._run this false true)
        (auto-run this))))

  (_update-watching [this derefed]
    (let [new (set derefed)
          old (set watching)]
      (set! watching derefed)
      (doseq [w (s/difference new old)]
        (check-depth this 2)
        (-add-reaction w this))
      (doseq [w (s/difference old new)]
        (check-depth this 2)
        (-remove-reaction w this))))

  (_try-exec [this f]
    (try
      (set! caught nil)
      (f)
      (catch :default e
        (set! state e)
        (set! caught e)
        (set! age -1))))

  (_maybe-notify [this oldstate newstate]
    (let [has-w (some? watches)
          has-r (some? (.-reactions this))]
      (when (or has-w has-r)
        (set! last-update age)
        (when (not= oldstate newstate)
          (when has-w
            (notify-w this oldstate newstate))
          (when has-r
            (notify-r this))))))

  (_handle-result [this res derefed notify]
    (let [oldstate state]
      (set! age generation)
      (when-not nocache?)
      (set! state res)
      (if-not (arr-eq derefed watching)
        ;; Optimize common case where derefs occur in same order
        (._update-watching this derefed))
      (when (and notify (not nocache?))
        (._maybe-notify this oldstate res)))
    res)

  (_run [this ^boolean check notify]
    (deref-capture f this check notify))

  (_set-opts [this {:keys [auto-run on-set on-dispose no-cache]}]
    (when (some? auto-run)
      (set! (.-auto-run this) auto-run))
    (when (some? on-set)
      (set! (.-on-set this) on-set))
    (when (some? on-dispose)
      (set! (.-on-dispose this) on-dispose))
    (when (some? no-cache)
      (set! (.-nocache? this) no-cache)))

  (_exec [this notify]
    (let [non-reactive (nil? *ratom-context*)]
      (if (and non-reactive (nil? auto-run))
        (let [oldstate state]
          (set! state (f))
          (._maybe-notify this oldstate state))
        (._run this true notify))))

  IRunnable
  (run [this]
    (._run this false true))

  IDeref
  (-deref [this]
    (check-depth this 5)
    (when-some [e caught]
      (set! caught nil)
      (set! age (dec age))
      (throw e))
    (when-not (nil? *ratom-context*)
      (notify-deref-watcher! this))
    (._check-dirty? this true)
    state)

  IReaction
  (add-on-dispose! [this f]
    ;; f is called without arguments when Reaction is no longer active
    (if-some [a (.-on-dispose-arr this)]
      (.push a f)
      (set! (.-on-dispose-arr this) (array f))))

  IDisposable
  (dispose! [this]
    (let [s state
          wg watching]
      (set! watching nil)
      (set! state nil)
      (set! auto-run nil)
      (set! age -1)
      (doseq [w (set wg)]
        (-remove-reaction w this))
      (when (some? (.-on-dispose this))
        (.on-dispose this s))
      (when-some [a (.-on-dispose-arr this)]
        (dotimes [i (alength a)]
          ((aget a i))))))

  IEquiv
  (-equiv [o other] (identical? o other))

  IPrintWithWriter
  (-pr-writer [a w opts] (pr-atom a w opts (str "Reaction " (hash a) ":")))

  IHash
  (-hash [this] (goog/getUid this)))


(defn make-reaction [f & {:keys [auto-run on-set on-dispose]}]
  (let [reaction (Reaction. f nil false nil nil nil nil -1 0)]
    (._set-opts reaction {:auto-run auto-run
                          :on-set on-set
                          :on-dispose on-dispose})
    reaction))



(def ^:private temp-reaction (make-reaction nil))

(defn run-in-reaction [f obj key run opts]
  (let [r temp-reaction
        res (deref-capture f r false true)]
    (when-not (nil? (.-watching r))
      (set! temp-reaction (make-reaction nil))
      (._set-opts r opts)
      (set! (.-f r) f)
      (set! (.-auto-run r) #(run obj))
      (aset obj key r))
    res))

(defn check-derefs [f]
  (let [[res captured] (in-context #js{} f false false false)]
    [res (some? captured)]))


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
      (when (some? watches)
        (notify-w this oldval newval))
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
  (Wrapper. value
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
                (flush!)))
        (dispose! res))))
  (ratom-perf))

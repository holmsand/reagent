(ns reagenttest.testtrack
  (:require [cljs.test :as t :refer-macros [is deftest testing]]
            [reagent.ratom :as rv :refer [track] :refer-macros [run! reaction]]
            [reagent.debug :as debug :refer-macros [dbg]]
            [reagent.core :as r]))

(defn fixture [f]
  (r/flush)
  (set! rv/debug true)
  (f)
  (set! rv/debug false))

(t/use-fixtures :once fixture)

(defn running []
  (rv/running))

(def testite 10)

(defn dispose [v]
  (rv/dispose! v))

(defn sync []
  (r/flush))

(enable-console-print!)


(deftest basic-ratom
  (let [runs (running)
        start (rv/atom 0)
        svf (fn [] @start)
        sv (track svf)
        compf (fn [x] @sv (+ x @sv))
        comp (track compf 2)
        c2f (fn [] (inc @comp))
        count (rv/atom 0)
        out (rv/atom 0)
        resf (fn []
               (swap! count inc)
               (+ @sv @(track c2f) @comp))
        res (track resf)
        const (run!
               (reset! out @res))]
    (is (= @count 1) "constrain ran")
    (is (= @out 5))
    (reset! start 1)
    (r/flush)
    (is (= @out 8))
    (is (= @count 2))

    (is (= @const 8))
    (is (= @count 2))

    (is (= @res 8))
    (is (= @count 2))
    (is (= (resf) 8))
    (dispose const)
    (is (= (running) runs))))

(deftest test-equal
  (let [runs (running)
        start (rv/atom {:n 0})
        svf (fn [] @start)
        sv (track svf)
        compf (fn [x] @sv {:n (+ x (:n @sv))})
        comp (track compf 2)
        c2f (fn [] {:n (inc (:n @comp))})
        count (rv/atom 0)
        out (rv/atom 0)
        resf (fn []
               (swap! count inc)
               (+ (:n @sv) (:n @(track c2f)) (:n @comp)))
        res (track resf)
        const (run!
               (reset! out @res))]
    (is (= @count 1) "constrain ran")
    (is (= @out 5))
    (reset! start {:n 1})
    (r/flush)
    (is (= @out 8))
    (is (= @count 2))

    (is (= @const 8))
    (is (= @count 2))

    (is (= @res 8))
    (is (= @count 2))
    (is (= (resf) 8))
    (is (= @count 3))

    (reset! start {:n 1})
    (r/flush)
    (is (= @count 3))

    (reset! start {:n 2})
    (r/flush)
    (is (= @count 4))
    (is (= (resf) @out))
    (is (= @count 5))
    (is (= @out @const))
    (is (= @count 5))

    (reset! start {:n 3})
    (is (= 14 @const))
    (is (= @count 6))
    (is (= (resf) @out))
    (is (= @count 7))
    (is (= @out @const))
    (is (= @count 7))
    (r/flush)
    (is (= @count 7))

    (dispose const)
    (is (= (running) runs))))

(deftest test-track!
  (sync)
  (let [runs (running)
        start (rv/atom 0)
        svf (fn [] @start)
        sv (track svf)
        compf (fn [x] @sv (+ x @sv))
        comp (track compf 2)
        c2f (fn [] (inc @comp))
        count (rv/atom 0)
        out (rv/atom 0)
        resf (fn []
               (swap! count inc)
               (+ @sv @(track c2f) @comp))
        res (track resf)
        const (rv/track!
               #(reset! out @res))]
    (is (= @count 1) "constrain ran")
    (is (= @out 5))
    (reset! start 1)
    (is (= @count 1))
    (sync)
    (is (= @out 8))
    (is (= @count 2))
    (dispose const)
    (swap! start inc)
    (sync)
    (is (= @count 2))
    (is (= @const 11))
    (is (= @count 3))
    (is (= (running) runs))))

(deftest repeat-tests
  (dotimes [_ testite]
    (basic-ratom)
    (test-equal)
    (test-track!)))

(deftest double-dependency
  (let [runs (running)
        start (rv/atom 0)
        c3-count (rv/atom 0)
        c1f (fn [] @start 1)
        c2f (fn [] @start)
        c3 (rv/make-reaction
            (fn []
              (swap! c3-count inc)
              (+ @(track c1f) @(track c2f)))
            :auto-run true)]
    (is (= @c3-count 0))
    (is (= @c3 1))
    (is (= @c3-count 1) "t1")
    (swap! start inc)
    (sync)
    (is (= @c3-count 2) "t2")
    (is (= @c3 2))
    (is (= @c3-count 2) "t3")
    (dispose c3)
    (is (= (running) runs))))

(deftest test-from-reflex
  (let [runs (running)]
    (let [!x (rv/atom 0)
          f #(inc @!x)
          !co (run! @(track f))]
      (is (= 1 @!co) "CO has correct value on first deref")
      (swap! !x inc)
      (is (= 2 @!co) "CO auto-updates")
      (dispose !co))
    (is (= runs (running)))))


(deftest test-unsubscribe
  (dotimes [x testite]
    (let [runs (running)
          a (rv/atom 0)
          af (fn [x] (+ @a x))
          a1 (track af 1)
          a2 (track af 0)
          b-changed (rv/atom 0)
          b-saved (atom 0)
          c-changed (rv/atom 0)
          mf (fn [v x spy]
               (swap! spy inc)
               (+ @v x))
          res (run!
               (if (< @a2 1)
                 @(track mf a1 1 b-changed)
                 @(track mf a2 10 c-changed)))]
      (is (= @res (+ 2 @a)))
      (is (= @b-changed 1))
      (is (= @c-changed 0))

      (reset! a -1)
      (is (= @res (+ 2 @a)))
      (is (= @b-changed 2))
      (is (= @c-changed 0))

      (reset! a 2)
      (is (= @res (+ 10 @a)))
      (is (<= 2 @b-changed 3))
      (reset! b-saved @b-changed)
      (is (= @c-changed 1))

      (reset! a 3)
      (is (= @res (+ 10 @a)))
      (is (= @b-changed @b-saved))
      (is (= @c-changed 2))

      (reset! a 3)
      (is (= @res (+ 10 @a)))
      (is (= @b-changed @b-saved))
      (is (= @c-changed 2))

      (reset! a -1)
      (is (= @res (+ 2 @a)))
      (dispose res)
      (is (= runs (running))))))

(deftest maybe-broken
  (let [runs (running)]
    (let [runs (running)
          a (rv/atom 0)
          f (fn [x] (+ x @a))
          b (track f 1)
          c (track f -1)
          d (track #(str @b))
          res (rv/atom 0)
          cs (run!
              (reset! res @d))]
      (is (= @res "1"))
      (dispose cs))
    ;; should be broken according to https://github.com/lynaghk/reflex/issues/1
    ;; but isnt
    (let [a (rv/atom 0)
          f (fn [x] (+ x @a))
          b (track f 1)
          d (run! [@b @(track f -1)])]
      (is (= @d [1 -1]))
      (dispose d))
    (let [a (rv/atom 0)
          f (fn [x] (+ x @a))
          c (track f -1)
          d (run! [@(track f 1) @c])
          res (rv/atom 0)]
      (is (= @d [1 -1]))
      (let [e (run! (reset! res @d))]
        (is (= @res [1 -1]))
        (dispose e))
      (dispose d))
    (is (= runs (running)))))

(deftest non-reactive-deref
  (let [runs (running)
        a (rv/atom 0)
        b (track #(+ 5 @a))]
    (is (= @b 5))
    (is (= runs (running)))

    (reset! a 1)
    (is (= @b 6))
    (is (= runs (running)))))

(deftest catching
  (let [runs (running)
        a (rv/atom false)
        catch-count (atom 0)
        b (track #(if @a (throw (js/Error. "fail"))))
        c (run! (try @b (catch :default e
                          (swap! catch-count inc))))]
    (debug/track-warnings
     (fn []
       (is (= @catch-count 0))
       (reset! a false)
       (sync)
       (is (= @catch-count 0))
       (reset! a true)
       (sync)
       (is (= @catch-count 1))
       (reset! a false)
       (sync)
       (is (= @catch-count 1))))
    (dispose c)
    (is (= runs (running)))))

(deftest track-equality
  (let [f1 (fn [])
        f2 (fn [])
        t r/track]
    (is (= (t f1) (t f1)))
    (is (not= (t f1) (t f2)))
    (is (not= (hash (t f1)) (hash (t f2))))
    (is (= (t f1 "foo") (t f1 "foo")))
    (is (not= (t f2 "foo") (t f1 "foo")))
    (is (not= (t f1 "foo") (t f1 "foobar")))
    (is (= (t f1 "foo" 1) (t f1 "foo" 1)))
    (is (not= (t f1 "foo" 2) (t f1 "foo" 1)))
    (is (= (t f1 2 "foo" 1) (t f1 2 "foo" 1)))
    (is (not= (t f1 2 "foo" 1) (t f2 2 "foo" 1)))
    (is (not= (t f1 2 "foo" 1) (t f1 2 "foo" 3)))
    (is (not= (hash (t f1 2 "foo" 1)) (hash (t f1 2 "foo" 3))))))

(deftest track-identity
  (let [runs (running)
        ts (atom {})
        t r/track
        trigger (r/atom 1)
        f1 (fn [& args]
             (r/with-let [k [:f1 args]
                          _ (is (nil? (@ts k)))
                          _ (swap! ts assoc k k)]
               @trigger
               (finally
                 (is (= k (@ts k)))
                 (swap! ts dissoc k))))
        f2' (fn [& args]
              (r/with-let [k [(t f1) args]
                           _ (is (nil? (@ts k)))
                           _ (swap! ts assoc k k)]
                @trigger
                (finally
                  (is (= k (@ts k)))
                  (swap! ts dissoc k))))
        f2 (fn [& args]
             @(apply t f2' args))
        refs (r/atom nil)
        run (r/track! #(doseq [i @refs]
                         @i))
        check (fn [n & args]
                (reset! refs args)
                (r/flush)
                (is (= (count @ts) n))
                (swap! trigger inc)
                (r/flush)
                (is (= (count @ts) n)))]
    (check 1 (t f1))
    (check 1 (t f1) (t f1))
    (check 2 (t f1) (t f1 1))
    (check 2 (t f1 1) (t f1 1) (t f1 2))
    (check 2 (t f1 1) (t f1 1) (t f2 1))
    (check 0)
    (check 2 (t f2 1) (t f2 1) (t f1 1))
    (check 2 (t f2 2) (t f2 2) (t f1 2))
    (check 2 (t f2 2 3) (t f2 2 3) (t f1 2 3))
    (check 1 (t f2 2 3) (t f2 2 3) (t f2 2 3))
    (check 5 (t f1) (t f1 1) (t f1 2) (t f1 1 2) (t f1 1 2 3))
    (check 4 (t f1) (t f1 1) (t f1 2) (t f1 1 2) (t f1 1 2))

    (r/dispose! run)
    (is (= 0 (count @ts)))
    (is (= runs (running)))))

(deftest multi-depend
  (let [runs (running)
        a (r/atom {:foo 1})
        f1 (fn [] @a)
        f1b (fn [] @a)
        f2 (fn [] (:foo @a))
        f3 (fn [] [(f1) (f2)])
        count (atom 0)
        spy (atom nil)
        res (r/track! (fn []
                        (swap! count inc)
                        @(track f1b) @a
                        (reset! spy [@(track f1) @(track f2)])))]
    (is (= @spy (f3)))
    (is (= @res (f3)))
    (is (= @count 1))

    (swap! a assoc :foo 2)
    (sync)
    (is (= @count 2))
    (is (= @spy (f3)))
    (is (= @res (f3)))

    (swap! a assoc :foo 3)
    (is (= @res (f3)))
    (is (= @count 3))
    (sync)
    (is (= @count 3))

    (reset! a {:foo 3})
    (sync)
    (is (= @count 3))
    (is (= @res (f3)))
    (is (= @count 3))

    (reset! a @a)
    (is (= @spy (f3)))
    (is (= @count 3))

    (reset! a {:foo 3})
    (is (= @res (f3)))
    (is (= @count 3))

    (is (= @res (f3)))
    (is (= @count 3))

    (reset! a {:foo 4})
    (sync)
    (is (= @spy (f3)))
    (is (= @count 4))

    (is (= @res (f3)))
    (is (= @count 4))))

(deftest multi-depend-2
  (let [runs (running)
        a0 (r/atom 1)
        a1 (r/atom 10)
        atoms [a0 a1]
        funs (atom {})
        nexe (atom {})
        a (fn [n] @(atoms n))
        t (fn [n] (if rv/*ratom-context*
                    @(r/track (@funs n))
                    ((@funs n))))
        fundef {0 #(+ (a 0))
                1 #(+ (a 1))
                2 #(+ 2 (t 0) (t 1))
                3 #(if (== (mod (a 1) 2) 0) (a 0) (a 1))
                4 #(quot (t 1) 2)
                5 #(+ 5 (if (== (mod (t 2) 2) 0) (t 3) (t 4)))
                6 #(+ 6 (t 0) (t 1) (t 7) (a 0) (t 5) (t 4) (t 3))
                7 #(if (== (mod (t 4) 2) 0) (t 1) (t 5))
                8 #(+ 8 (t 0) (t 2) (t 3) (t 2) (t 3) (t 5) (t 6) (t 7))
                9 #(+ 9 (a 0) (t 0) (t 2) (t 4) (t 8))
                10 #(+ 10 (a 0) (a 1) (t 6) (t 0) (t 2) (t 4) (t 5) (t 9))
                11 #(+ 11 (a 1))
                12 #(+ 12 (t 11))
                13 #(+ 13 (t 12))
                14 #(+ 14 (t 13))
                15 #(+ 15 (t 14) (t 10))
                16 #(+ 16 (t 15) (a 0) (a 1))}
        _ (reset! funs (reduce-kv
                        (fn [m k v]
                          (assoc m k
                                 (fn []
                                   (let [res (v)]
                                     (when rv/*ratom-context*
                                       (swap! nexe update-in [k] inc))
                                     res))))
                        {} fundef))
        fnum (count fundef)
        exe (fn [n track?]
              (if track?
                (binding [rv/*ratom-context* nil]
                  (t n))
                (t n)))
        checkn (fn [n]
                 (let [v1 (exe n true)
                       v2 (exe n false)]
                   (is (= v1 v2))))
        reset-count (fn [] (reset! nexe {}))
        all (r/track! (fn []
                        (assert rv/*ratom-context*)
                        (doseq [i (->> fnum range shuffle (drop 2))]
                          (checkn i))
                        (is (= #{1} (-> @nexe vals set)))))]
    (dotimes [_ testite]
      (reset-count)
      (swap! a0 inc)
      (rv/flush!)

      (reset-count)
      (swap! a1 inc)
      (rv/flush!)
      (is (= ((@funs 0)) @a0))
      (is (= @(r/track (@funs 0)) @a0))

      (reset-count)
      (swap! a0 inc)
      (swap! a1 inc)
      (rv/flush!)

      (reset-count)
      (reset! a0 @a0)
      (reset! a1 @a1)
      (rv/flush!)
      (is (= @nexe {}))

      (reset-count)
      (let [o0 @a0
            o1 @a1]
        (reset! a0 12345)
        (reset! a1 54321)
        (reset! a0 o0)
        (reset! a1 o1)
        (is (= @nexe {}))
        (rv/flush!)
        (is (= @nexe {})))

      (reset-count)
      (swap! a1 inc)
      @all
      (is (= ((@funs 1)) @a1))
      (is (= @(r/track (@funs 1)) @a1))

      (reset-count)
      (swap! a0 inc)
      @all

      (reset-count)
      (swap! a1 inc)
      (swap! a0 inc)
      @all

      (reset-count)
      (let [o0 @a0
            o1 @a1]
        (rv/flush!)
        (is (= @nexe {}))
        (reset! a0 12345)
        (reset! a1 54321)
        (reset! a0 o0)
        (reset! a1 o1)
        (is (= @nexe {}))
        @all
        (is (= @nexe {})))

      (doseq [i (-> fnum range shuffle)]
        (checkn i)))

    (r/dispose! all)
    (is (= runs (running)))))

(deftest multi-depend-2-equal
  (let [runs (running)
        a0 (r/atom {:foo 1})
        a1 (r/atom {:foo 10})
        atoms [a0 a1]
        funs (atom {})
        nexe (atom {})
        a (fn [n] (:foo @(atoms n)))
        t (fn [n] (:foo (if rv/*ratom-context*
                          @(r/track (@funs n))
                          ((@funs n)))))
        fundef {0 #(+ (a 0))
                1 #(+ (a 1))
                2 #(+ 2 (t 0) (t 1))
                3 #(if (== (mod (a 1) 2) 0) (a 0) (a 1))
                4 #(quot (t 1) 2)
                5 #(+ 5 (if (== (mod (t 2) 2) 0) (t 3) (t 4)))
                6 #(+ 6 (t 0) (t 1) (t 7) (a 0) (t 5) (t 4) (t 3))
                7 #(if (== (mod (t 4) 2) 0) (t 1) (t 5))
                8 #(+ 8 (t 0) (t 2) (t 3) (t 2) (t 3) (t 5) (t 6) (t 7))
                9 #(+ 9 (a 0) (t 0) (t 2) (t 4) (t 8))
                10 #(+ 10 (a 0) (a 1) (t 6) (t 0) (t 2) (t 4) (t 5) (t 9))
                11 #(+ 11 (a 1))
                12 #(+ 12 (t 11))
                13 #(+ 13 (t 12))
                14 #(+ 14 (t 13))
                15 #(+ 15 (t 14) (t 10))
                16 #(+ 16 (t 15) (a 0) (a 1))}
        _ (reset! funs (reduce-kv
                        (fn [m k v]
                          (assoc m k
                                 (fn []
                                   (let [res (v)]
                                     (when rv/*ratom-context*
                                       (swap! nexe update-in [k] inc))
                                     res))))
                        {} fundef))
        fnum (count fundef)
        exe (fn [n track?]
              (if track?
                (binding [rv/*ratom-context* nil]
                  (t n))
                (t n)))
        checkn (fn [n]
                 (let [v1 (exe n true)
                       v2 (exe n false)]
                   (is (= v1 v2))))
        reset-count (fn [] (reset! nexe {}))
        all (r/track! (fn []
                        (assert rv/*ratom-context*)
                        (doseq [i (->> fnum range shuffle (drop 2))]
                          (checkn i))
                        (is (= #{1} (-> @nexe vals set)))))]
    (dotimes [_ testite]
      (reset-count)
      (swap! a0 update :foo inc)
      (rv/flush!)

      (reset-count)
      (swap! a1 update :foo inc)
      (rv/flush!)
      (is (= ((@funs 0)) (:foo @a0)))
      (is (= @(r/track (@funs 0)) (:foo @a0)))

      (reset-count)
      (swap! a0 update :foo inc)
      (swap! a1 update :foo inc)
      (rv/flush!)

      (reset-count)
      (reset! a0 {:foo (:foo @a0)})
      (reset! a1 {:foo (:foo @a1)})
      (rv/flush!)
      (is (= @nexe {}))

      (reset-count)
      (let [o0 @a0
            o1 @a1]
        (reset! a0 12345)
        (reset! a1 54321)
        (reset! a0 o0)
        (reset! a1 o1)
        (is (= @nexe {}))
        (rv/flush!)
        (is (= @nexe {})))

      (reset-count)
      (swap! a1 update :foo inc)
      @all
      (is (= ((@funs 1)) (:foo @a1)))
      (is (= @(r/track (@funs 1)) (:foo @a1)))

      (reset-count)
      (swap! a0 update :foo inc)
      @all

      (reset-count)
      (swap! a1 update :foo inc)
      (swap! a0 update :foo inc)
      @all

      (reset-count)
      (let [o0 @a0
            o1 @a1]
        (rv/flush!)
        (is (= @nexe {}))
        (reset! a0 12345)
        (reset! a1 54321)
        (reset! a0 o0)
        (reset! a1 o1)
        (is (= @nexe {}))
        @all
        (is (= @nexe {})))

      (doseq [i (-> fnum range shuffle)]
        (checkn i)))

    (r/dispose! all)
    (is (= runs (running)))))

(deftest repeat-depend-multi
  (dotimes [_ 2]
    (multi-depend-2)
    (multi-depend-2-equal)))

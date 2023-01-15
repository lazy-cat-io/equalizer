(ns equalizer.core-test
  (:require
    #?@(:clj [[clojure.test :as t]]
        :cljs [[cljs.test :as t]])
    [equalizer.core :as sut]))


(t/deftest matchers-test
  (t/testing "equality-matcher"
    (t/testing "same values"
      (doseq [x [nil
                 true false
                 42 36.6 #?(:clj 42/7)
                 \newline "string"
                 'symbol 'qualified/symbol
                 :keyword :qualified/keyword
                 '(1 2 3)
                 [1 2 3]
                 #{1 2 3}
                 {:foo "bar"}
                 (random-uuid)
                 #'pos-int?
                 pos-int?]]
        (let [matcher (sut/into-matcher x)]
          (t/is (true? (sut/match? matcher x)))
          (t/is (true? (matcher x))))))

    (t/testing "different values"
      (let [matcher (sut/into-matcher true)]
        (t/is (false? (sut/match? matcher false)))
        (t/is (false? (matcher false))))))


  (t/testing "fn-matcher"
    (t/testing "fn"
      (let [matcher (sut/into-matcher pos-int?)]
        (t/is (true? (sut/match? matcher 42)))
        (t/is (true? (sut/match? matcher pos-int?)))
        (t/is (false? (sut/match? matcher -42)))
        (t/is (true? (matcher 42)))
        (t/is (true? (matcher pos-int?)))
        (t/is (false? (matcher -42)))))

    (t/testing "set"
      (let [matcher (sut/into-matcher #{42})]
        (t/is (true? (sut/match? matcher #{42})))
        (t/is (false? (sut/match? matcher #{"42"})))
        (t/is (true? (sut/match? matcher 42)))
        (t/is (false? (sut/match? matcher "42")))
        (t/is (true? (matcher #{42})))
        (t/is (false? (matcher #{"42"})))
        (t/is (true? (matcher 42)))
        (t/is (false? (matcher "42")))))

    (t/testing "keyword"
      (let [matcher (sut/into-matcher ::foo)]
        (t/is (true? (sut/match? matcher {::foo "bar"})))
        (t/is (true? (matcher {::foo "bar"})))
        ;; the current implementation returns `false` result for the following cases
        ;; maybe we should only check for the presence of a value using keyword-matcher?
        (t/is (false? (sut/match? matcher {::foo false})))
        (t/is (false? (sut/match? matcher {::foo nil})))
        (t/is (false? (matcher {::foo false})))
        (t/is (false? (matcher {::foo nil})))))

    (t/testing "map"
      (let [matcher (sut/into-matcher {::foo "bar"})]
        (t/is (true? (sut/match? matcher {::foo "bar"})))
        (t/is (false? (sut/match? matcher ::foo)))
        (t/is (false? (sut/match? matcher ::bar)))
        (t/is (true? (matcher {::foo "bar"})))
        (t/is (false? (matcher ::foo)))
        (t/is (false? (matcher ::bar))))))


  (t/testing "regexp-matcher"
    (let [re #"\d+"
          matcher (sut/into-matcher re)]
      (t/is (true? (sut/match? matcher "42")))
      (t/is (true? (sut/match? matcher re)))
      (t/is (false? (sut/match? matcher "foo42bar")))
      (t/is (true? (matcher "42")))
      (t/is (true? (matcher re)))
      (t/is (false? (matcher "foo42bar")))))


  (t/testing "wildcard-matcher"
    (let [sym '_
          matcher (sut/into-matcher sym)]
      (t/is (true? (sut/match? matcher sym)))
      (t/is (true? (sut/match? matcher nil)))
      (t/is (true? (sut/match? matcher 42)))
      (t/is (true? (matcher sym)))
      (t/is (true? (matcher nil)))
      (t/is (true? (matcher 42)))))


  (t/testing "map-matcher"
    (t/is (true? (sut/match {:foo "bar"} {:foo "bar"})))
    (t/is (true? (sut/match {:status 200, :body {:username string?, :age #(<= 18 %)}}
                   {:status 200
                    :body {:username "@john.doe", :age 42}})))
    (t/is (true? (sut/match {:status 200, :body {:username string?, :age #(<= 18 %)}}
                   {:status 200
                    :body {:username "@john.doe", :age 42, :id (random-uuid)}})))))



(t/deftest combinators-test
  (t/testing "logical combinators"
    (t/testing "not"
      (t/is (true? (sut/match? (sut/not 42) -42)))
      (t/is (false? (sut/match? (sut/not 42) 42)))
      (t/is (true? ((sut/not 42) -42)))
      (t/is (false? ((sut/not 42) 42))))


    (t/testing "and"
      (t/is (true? (sut/match? (sut/and string? #"\d+") "42")))
      (t/is (false? (sut/match? (sut/and string? #"\d+") 42)))
      (t/is (true? ((sut/and string? #"\d+") "42")))
      (t/is (false? ((sut/and string? #"\d+") 42))))


    (t/testing "or"
      (t/is (true? (sut/match? (sut/or string? pos-int?) "42")))
      (t/is (true? (sut/match? (sut/or string? pos-int?) 42)))
      (t/is (false? (sut/match? (sut/or string? pos-int?) -42)))
      (t/is (true? ((sut/or string? pos-int?) "42")))
      (t/is (true? ((sut/or string? pos-int?) 42)))
      (t/is (false? ((sut/or string? pos-int?) -42)))))


  (t/testing "collections"
    (t/testing "tuple"
      (t/is (false? (sut/match? (sut/tuple pos-int? string?) [])))
      (t/is (true? (sut/match? (sut/tuple pos-int? string?) [42 "foo"])))
      (t/is (false? (sut/match? (sut/tuple pos-int? string?) [42 "foo" :bar])))
      (t/is (false? ((sut/tuple pos-int? string?) [])))
      (t/is (true? ((sut/tuple pos-int? string?) [42 "foo"])))
      (t/is (false? ((sut/tuple pos-int? string?) [42 "foo" :bar]))))


    (t/testing "coll-of"
      (t/is (true? (sut/match? (sut/coll-of pos-int?) [])))
      (t/is (true? (sut/match? (sut/coll-of pos-int?) [1 2 3])))
      (t/is (true? (sut/match? (sut/coll-of pos-int?) (list 1 2 3))))
      (t/is (false? (sut/match? (sut/coll-of pos-int?) [1 2 3 -4])))
      (t/is (false? (sut/match? (sut/coll-of pos-int?) (list 1 2 3 -4))))
      (t/is (true? ((sut/coll-of pos-int?) [])))
      (t/is (true? ((sut/coll-of pos-int?) [1 2 3])))
      (t/is (true? ((sut/coll-of pos-int?) (list 1 2 3))))
      (t/is (false? ((sut/coll-of pos-int?) [1 2 3 -4])))
      (t/is (false? ((sut/coll-of pos-int?) (list 1 2 3 -4)))))


    (t/testing "map-of"
      (t/is (true? (sut/match? (sut/map-of string? pos-int?) {})))
      (t/is (true? (sut/match? (sut/map-of string? pos-int?) {"foo" 42})))
      (t/is (false? (sut/match? (sut/map-of string? pos-int?) {"foo" 42, "bar" -42})))
      (t/is (true? (sut/match? (sut/map-of qualified-keyword? pos-int?) {::foo 64, ::bar 128, ::baz 256})))
      (t/is (false? (sut/match? (sut/map-of qualified-keyword? pos-int?) {::foo 64, ::bar 128, :baz 256}))))))

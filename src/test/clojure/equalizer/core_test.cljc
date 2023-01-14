(ns equalizer.core-test
  (:require
    #?@(:clj [[clojure.test :as t]]
        :cljs [[cljs.test :as t]])
    [equalizer.core :as sut]))


(t/deftest equalizer-test

  (t/testing "matchers"
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
      (let [matcher (sut/into-matcher pos-int?)]
        (t/is (true? (sut/match? matcher 42)))
        (t/is (true? (sut/match? matcher pos-int?)))
        (t/is (false? (sut/match matcher -42)))
        (t/is (true? (matcher 42)))
        (t/is (true? (matcher pos-int?)))
        (t/is (false? (matcher -42)))))


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
        (t/is (true? (matcher 42))))))



  (t/testing "matcher combinators"
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
      (t/is (false? ((sut/or string? pos-int?) -42))))))

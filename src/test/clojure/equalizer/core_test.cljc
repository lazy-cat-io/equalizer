(ns equalizer.core-test
  (:require
    #?@(:clj [[clojure.test :as t]]
        :cljs [[cljs.test :as t]])
    [equalizer.core :as sut]))


(t/deftest validator:kind-test
  (t/is (= :nil (sut/kind nil)))

  (t/is (= :object (sut/kind true)))
  (t/is (= :object (sut/kind 42.0)))
  (t/is (= :object (sut/kind 42)))
  (t/is (= :object (sut/kind 42/42)))
  (t/is (= :object (sut/kind \newline)))
  (t/is (= :object (sut/kind "foo")))
  (t/is (= :object (sut/kind :foo)))
  (t/is (= :object (sut/kind ::foo)))
  (t/is (= :object (sut/kind 'foo)))
  (t/is (= :object (sut/kind 'foo/bar)))
  (t/is (= :object (sut/kind #'pos-int?)))
  (t/is (= :object (sut/kind '())))
  (t/is (= :object (sut/kind [])))
  (t/is (= :object (sut/kind #{})))
  (t/is (= :object (sut/kind {})))
  (t/is (= :object (sut/kind #uuid "deadbeaf-dead-beaf-dead-beafdeadbeaf")))

  (t/is (= :regexp (sut/kind #"\d+")))

  (t/is (= :fn (sut/kind pos-int?)))
  (t/is (= :fn (sut/kind #(pos-int? %))))

  (t/is (= :not (sut/kind (sut/not string?))))
  (t/is (= :and (sut/kind (sut/and pos-int? (partial >= 18)))))
  (t/is (= :or (sut/kind (sut/or pos-int? string?))))

  (t/is (= :maybe (sut/kind (sut/maybe string?))))
  (t/is (= :tuple (sut/kind (sut/tuple pos-int? string?))))
  (t/is (= :coll-of (sut/kind (sut/coll-of int?))))
  (t/is (= :map-of (sut/kind (sut/map-of string? pos-int?))))
  (t/is (= :map (sut/kind (sut/map {:foo pos-int?})))))



(t/deftest validator:valid?-test
  (t/testing "general validators"
    (t/testing "nil"
      (t/is (true? (sut/valid? nil nil)))
      (t/is (false? (sut/valid? nil 42))))

    (t/testing "boolean"
      (t/is (true? (sut/valid? true true)))
      (t/is (false? (sut/valid? true false))))

    (t/testing "number"
      (t/is (true? (sut/valid? 42.0 42.0)))
      (t/is (false? (sut/valid? 42.0 42.424242)))
      (t/is (true? (sut/valid? 42 42)))
      (t/is (false? (sut/valid? 42 -42)))
      #?@(:clj
          [(t/is (true? (sut/valid? 42/42 42/42)))
           (t/is (false? (sut/valid? 42/42 42/43)))]))

    (t/testing "char and string"
      (t/is (true? (sut/valid? \newline \newline)))
      (t/is (false? (sut/valid? \newline \tab)))
      (t/is (true? (sut/valid? "foo" "foo")))
      (t/is (false? (sut/valid? "foo" "bar"))))

    (t/testing "keyword and symbol"
      (t/is (true? (sut/valid? :foo :foo)))
      (t/is (false? (sut/valid? :foo :bar)))
      (t/is (true? (sut/valid? ::foo ::foo)))
      (t/is (false? (sut/valid? ::foo ::bar)))
      (t/is (true? (sut/valid? 'foo 'foo)))
      (t/is (false? (sut/valid? 'foo 'bar)))
      (t/is (true? (sut/valid? 'foo/bar 'foo/bar)))
      (t/is (false? (sut/valid? 'foo/bar 'foo/baz))))

    (t/testing "list"
      (t/is (true? (sut/valid? '() '())))
      (t/is (true? (sut/valid? '(1 2 3) '(1 2 3))))
      (t/is (false? (sut/valid? '(1 2 3) '(3 2 1)))))

    (t/testing "vector"
      (t/is (true? (sut/valid? [] [])))
      (t/is (true? (sut/valid? [1 2 3] [1 2 3])))
      (t/is (false? (sut/valid? [1 2 3] [3 2 1]))))

    (t/testing "set"
      (t/is (true? (sut/valid? #{} #{})))
      (t/is (true? (sut/valid? #{1 2 3} #{1 2 3})))
      (t/is (true? (sut/valid? #{1 2 3} #{3 2 1}))))

    (t/testing "map"
      (t/is (true? (sut/valid? {} {})))
      (t/is (true? (sut/valid? {:foo "bar"} {:foo "bar"})))
      (t/is (false? (sut/valid? {:foo "bar"} {:foo "bar", :bar "baz"}))))

    (t/testing "uuid"
      (t/is (true? (sut/valid? #uuid "deadbeaf-dead-beaf-dead-beafdeadbeaf"
                               #uuid "deadbeaf-dead-beaf-dead-beafdeadbeaf"))))

    (t/testing "regexp"
      (t/is (true? (sut/valid? #"^\d+$" "42")))
      (t/is (false? (sut/valid? #"^\d+$" "42d")))
      ;; regexp can't be compared
      ;; (= #"\d+" #"\d+") => false
      ;; (= (str #"\d+") (str #"\d+")) => true
      (t/is (false? (sut/valid? #"^\d+$" #"^\d+$"))))

    (t/testing "function"
      (t/is (true? (sut/valid? pos-int? 42)))
      (t/is (false? (sut/valid? pos-int? -42)))
      (t/is (false? (sut/valid? pos-int? pos-int?)))
      (t/is (true? (sut/valid? uuid? (random-uuid))))))



  (t/testing "validator combinators"
    (t/testing "not"
      (t/is (false? (sut/valid? (sut/not 42) 42)))
      (t/is (true? (sut/valid? (sut/not string?) 42))))

    (t/testing "and"
      (t/is (true? (sut/valid? (sut/and string? #"^\d+$") "42")))
      (t/is (false? (sut/valid? (sut/and string? #"^\d+$") "foo"))))

    (t/testing "or"
      (t/is (true? (sut/valid? (sut/or pos-int?) 42)))
      (t/is (false? (sut/valid? (sut/or neg-int?) 42)))
      (t/is (true? (sut/valid? (sut/or :foo :bar :baz) :bar)))
      (t/is (true? (sut/valid? (sut/or #"^\d+$") "42")))
      (t/is (true? (sut/valid? (sut/or [1 2 3]) [1 2 3])))
      (t/is (false? (sut/valid? (sut/or [1 2 3]) [1 2 4]))))

    (t/testing "maybe"
      (t/is (true? (sut/valid? (sut/maybe string?) "42")))
      (t/is (true? (sut/valid? (sut/maybe string?) nil)))
      (t/is (false? (sut/valid? (sut/maybe string?) 42))))

    (t/testing "tuple"
      (t/is (true? (sut/valid? (sut/tuple pos-int? neg-int?) [1 -2])))
      (t/is (false? (sut/valid? (sut/tuple pos-int? neg-int?) [1 2])))
      (t/is (true? (sut/valid? (sut/tuple pos-int? neg-int?) '(1 -2))))
      (t/is (false? (sut/valid? (sut/tuple pos-int? neg-int?) '(1 2)))))

    (t/testing "coll-of"
      (t/is (true? (sut/valid? (sut/coll-of pos-int?) [1 2 3])))
      (t/is (false? (sut/valid? (sut/coll-of pos-int?) [1 -2 3])))
      (t/is (true? (sut/valid? (sut/coll-of 42) [42 42 42])))
      (t/is (false? (sut/valid? (sut/coll-of 42) [43 42 42]))))

    (t/testing "map-of"
      (t/is (true? (sut/valid? (sut/map-of string? pos-int?) {"foo" 42, "bar" 42})))
      (t/is (false? (sut/valid? (sut/map-of string? pos-int?) {"foo" 42, "bar" -42}))))

    (t/testing "map"
      (t/is (true? (sut/valid? (sut/map {:foo :bar}) {:foo :bar})))
      (t/is (false? (sut/valid? (sut/map {:foo :bar}) {:foo :baz})))
      (t/is (true? (sut/valid? (sut/map {:status 200}) {:status 200, :body {:foo :bar}})))
      (t/is (true? (sut/valid? (sut/map {:status 200, :body map?}) {:status 200, :body {:foo :bar}})))
      (t/is (false? (sut/valid? (sut/map {:status 200, :body {:foo {:bar string?}}}) {:status 200, :body {:foo {:bar 42}}})))))


  (t/testing "nested validators"
    (t/is (true? (sut/valid? (sut/map {:x boolean?, :y (sut/maybe int?), :z string?})
                             {:x true, :y 1, :z "foo"})))))

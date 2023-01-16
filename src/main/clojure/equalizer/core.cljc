(ns equalizer.core
  (:refer-clojure :exclude [not and or cat])
  (:require
    [clojure.core :as c]
    #?(:clj [equalizer.helpers :as helpers]))
  #?(:cljs
     (:require-macros
       [equalizer.helpers :as helpers]))
  #?(:clj
     (:import
       (clojure.lang
         IFn
         PersistentArrayMap
         PersistentHashMap
         PersistentList
         PersistentVector
         Symbol
         Var)
       (java.util.regex
         Pattern))))


;;
;; Protocols
;;

(defprotocol IMatcherBuilder
  (-matcher [this]))


(defprotocol IMatcher
  (-match? [this x]))


(defprotocol IKind
  (-kind [x]))



;;
;; Matcher
;;

(defrecord Matcher
  [kind predicate matcher explainer]
  IMatcherBuilder
  (-matcher [this]
    this)

  IMatcher
  (-match? [_ x]
    (helpers/safe
      (boolean (predicate x))
      (constantly false)))

  IKind
  (-kind [_]
    kind)

  IFn
  #?(:clj
     (invoke
       [this x]
       (-match? this x))

     :cljs
     (-invoke
       [this x]
       (-match? this x))))


(defn as-matcher
  {:arglists '([{:keys [kind predicate]}])}
  [m]
  (map->Matcher m))



;;
;; Matchers
;;

(defn equality-matcher
  [expected]
  (as-matcher
    {:kind :equality
     :predicate (fn predicate
                  [x]
                  (= expected x))}))


(defn fn-matcher
  [f]
  (as-matcher
    {:kind :fn
     :predicate (fn predicate
                  [x]
                  (c/or
                    (= f x)
                    (boolean (f x))))}))


(defn regexp-matcher
  [re]
  (as-matcher
    {:kind :regexp
     :predicate (fn predicate
                  [?string]
                  (c/or
                    (= re ?string)
                    (c/and (string? ?string) (boolean (re-matches re ?string)))))}))


(defn wildcard-matcher
  []
  (as-matcher
    {:kind :wildcard
     :predicate (fn predicate
                  [_]
                  true)}))


(defn map-matchers
  [m]
  (letfn [(walk
            [acc ks m]
            (reduce-kv
              (fn [acc k v]
                (if (map? v)
                  (walk acc (conj ks k) v)
                  (conj acc [(conj ks k) (-matcher v)])))
              acc m))]
    (walk [] [] m)))


(defn map-matcher
  [m]
  (let [matchers (map-matchers m)]
    (as-matcher
      {:kind :map
       :predicate (fn predicate
                    [?map]
                    (c/or
                      (= m ?map)
                      (c/and
                        (map? ?map)
                        (reduce
                          (fn [acc [path matcher]]
                            (if (matcher (get-in ?map path))
                              acc
                              (reduced false)))
                          true matchers))))})))


;;
;; Combinators
;;

(defn not
  [?matcher]
  (let [matcher (-matcher ?matcher)]
    (as-matcher
      {:kind :not
       :predicate (fn predicate
                    [x]
                    (c/not (matcher x)))})))


(defn and
  [& ?matchers]
  (let [matchers (mapv -matcher ?matchers)
        f (apply every-pred matchers)]
    (as-matcher
      {:kind :and
       :predicate (fn predicate
                    [x]
                    (boolean (f x)))})))


(defn or
  [& ?matchers]
  (let [matchers (mapv -matcher ?matchers)
        f (apply some-fn matchers)]
    (as-matcher
      {:kind :or
       :predicate (fn predicate
                    [x]
                    (boolean (f x)))})))



;;
;; Collections
;;

(defn tuple
  [& ?matchers]
  (let [matchers (mapv -matcher ?matchers)
        size (count matchers)]
    (as-matcher
      {:kind :tuple
       :predicate (fn predicate
                    [?coll]
                    (c/or
                      (= ?matchers ?coll)
                      (c/and
                        (sequential? ?coll)
                        (= size (count ?coll))
                        (loop [acc true
                               [matcher & matchers] matchers
                               [x & xs] ?coll]
                          (if (c/or (false? acc) (nil? matcher))
                            (boolean acc)
                            (recur (matcher x) matchers xs))))))})))


(defn coll-of
  [?matcher]
  (let [matcher (-matcher ?matcher)]
    (as-matcher
      {:kind :coll-of
       :predicate (fn predicate
                    [?coll]
                    (c/or
                      (= ?matcher ?coll)
                      (c/and
                        (sequential? ?coll)
                        (every? matcher ?coll))))})))


(defn enum
  [& ?matchers]
  (let [matchers (mapv -matcher ?matchers)
        f (apply some-fn matchers)]
    (as-matcher
      {:kind :enum
       :predicate (fn predicate
                    [x]
                    (boolean (some f x)))})))


(defn map-of
  [?key-matcher ?value-matcher]
  (let [entry-matcher (tuple ?key-matcher ?value-matcher)]
    (as-matcher
      {:kind :map-of
       :predicate (fn predicate
                    [?map]
                    (c/and
                      (map? ?map)
                      (reduce
                        (fn [acc entry]
                          (if (entry-matcher entry)
                            acc
                            (reduced false)))
                        true ?map)))})))



;;
;; Sequences
;;

(defn cat
  [& ?pairs]
  (let [matchers (->> ?pairs
                      (partition-all  2)
                      (mapv (fn [[k v]] [k (-matcher v)])))]
    (as-matcher
      {:kind :cat
       :predicate (fn predicate
                    [?seq]
                    (c/and
                      (sequential? ?seq)
                      (loop [acc true
                             [[_ matcher] & matchers] matchers
                             [x & xs] ?seq]
                        (if (c/or (false? acc) (nil? matcher))
                          (boolean acc)
                          (recur (matcher x) matchers xs)))))})))



;; 
;; Default behaviour
;;

(extend-type nil
  IMatcherBuilder
  (-matcher [_]
    (equality-matcher nil))

  IKind
  (-kind [_]
    nil))


(extend-type #?(:clj Object, :cljs default)
  IMatcherBuilder
  (-matcher [obj]
    (if (fn? obj)
      (fn-matcher obj)
      (equality-matcher obj)))

  IKind
  (-kind [obj]
    (-kind (-matcher obj))))


(extend-type #?(:clj Pattern, :cljs js/RegExp)
  IMatcherBuilder
  (-matcher [re]
    (regexp-matcher re)))


(extend-type #?(:clj Var, :cljs cljs.core/Var)
  IMatcherBuilder
  (-matcher [var]
    (or (equality-matcher var)
        (-matcher @var))))


(extend-type #?(:clj Symbol, :cljs cljs.core/Symbol)
  IMatcherBuilder
  (-matcher [sym]
    (if (= '_ sym)
      (wildcard-matcher)
      (equality-matcher sym))))


(extend-type #?(:clj PersistentArrayMap, :cljs cljs.core/PersistentArrayMap)
  IMatcherBuilder
  (-matcher [m]
    (map-matcher m)))


(extend-type #?(:clj PersistentHashMap, :cljs cljs.core/PersistentHashMap)
  IMatcherBuilder
  (-matcher [m]
    (map-matcher m)))


(extend-type #?(:clj PersistentList, :cljs cljs.core/List)
  IMatcherBuilder
  (-matcher [coll]
    (apply tuple coll)))


(extend-type #?(:clj PersistentVector, :cljs cljs.core/PersistentVector)
  IMatcherBuilder
  (-matcher [coll]
    (apply tuple coll)))



;;
;; Public API
;;

(defn match
  [?matcher data]
  ((-matcher ?matcher) data))

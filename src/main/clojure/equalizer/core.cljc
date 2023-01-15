(ns equalizer.core
  (:refer-clojure :exclude [not and or])
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
         Symbol
         Var)
       (java.util.regex
         Pattern))))


;;
;; Protocols
;;

(defprotocol IMatcherBuilder
  (into-matcher [this]))


(defprotocol IMatcher
  (match? [this x]))


(defprotocol ICombinator
  (matchers [this]))


(defprotocol IKind
  (kind [x]))



;;
;; Matcher
;;

(defrecord Matcher
  [kind matchers predicate]
  IMatcherBuilder
  (into-matcher [this]
    this)

  IMatcher
  (match? [_ x]
    (helpers/safe
      (boolean (predicate x))
      (constantly false)))

  ICombinator
  (matchers [_]
    (c/or matchers []))

  IKind
  (kind [_]
    kind)

  IFn
  #?(:clj
     (invoke
       [this x]
       (match? this x))

     :cljs
     (-invoke
       [this x]
       (match? this x))))


(defn as-matcher
  {:arglists '([{:keys [kind matchers predicate]}])}
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
                  (c/or (= f x)
                        (boolean (f x))))}))


(defn regexp-matcher
  [re]
  (as-matcher
    {:kind :regexp
     :predicate (fn predicate
                  [s]
                  (c/or (= re s)
                        (c/and (string? s) (boolean (re-matches re s)))))}))


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
                  (conj acc [(conj ks k) (into-matcher v)])))
              acc m))]
    (walk [] [] m)))


(defn map-matcher
  [m]
  (let [matchers (map-matchers m)]
    (as-matcher
      {:kind :map
       :matchers matchers
       :predicate (fn predicate
                    [x]
                    (c/or (= m x)
                          (reduce
                            (fn [acc [path matcher]]
                              (if (matcher (get-in x path))
                                acc
                                (reduced false)))
                            true matchers)))})))



;;
;; Combinators
;;

(defn not
  [?matcher]
  (let [matcher (into-matcher ?matcher)]
    (as-matcher
      {:kind :not
       :matchers [matcher]
       :predicate (fn predicate
                    [x]
                    (c/not (matcher x)))})))


(defn and
  [& ?matchers]
  (let [matchers (mapv into-matcher ?matchers)
        f (apply every-pred matchers)]
    (as-matcher
      {:kind :and
       :matchers matchers
       :predicate (fn predicate
                    [x]
                    (boolean (f x)))})))


(defn or
  [& ?matchers]
  (let [matchers (mapv into-matcher ?matchers)
        f (apply some-fn matchers)]
    (as-matcher
      {:kind :or
       :matchers matchers
       :predicate (fn predicate
                    [x]
                    (boolean (f x)))})))



;; 
;; Default behaviour
;;

(extend-type nil
  IMatcherBuilder
  (into-matcher [_]
    (equality-matcher nil))

  IKind
  (kind [_]
    nil))


(extend-type #?(:clj Object, :cljs default)
  IMatcherBuilder
  (into-matcher [obj]
    (if (ifn? obj)
      (fn-matcher obj)
      (equality-matcher obj)))

  IKind
  (kind [_]
    nil))


(extend-type #?(:clj Pattern, :cljs js/RegExp)
  IMatcherBuilder
  (into-matcher [re]
    (regexp-matcher re)))


(extend-type #?(:clj Var, :cljs cljs.core/Var)
  IMatcherBuilder
  (into-matcher [var]
    (or (equality-matcher var)
        (into-matcher @var))))


(extend-type #?(:clj Symbol, :cljs cljs.core/Symbol)
  IMatcherBuilder
  (into-matcher [sym]
    (if (= '_ sym)
      (wildcard-matcher)
      (equality-matcher sym))))


(extend-type #?(:clj PersistentArrayMap, :cljs cljs.core/PersistentArrayMap)
  IMatcherBuilder
  (into-matcher [m]
    (map-matcher m)))


(extend-type #?(:clj PersistentHashMap, :cljs cljs.core/PersistentHashMap)
  IMatcherBuilder
  (into-matcher [m]
    (map-matcher m)))



;;
;; Public API
;;

(defn match
  [?matcher data]
  ((into-matcher ?matcher) data))

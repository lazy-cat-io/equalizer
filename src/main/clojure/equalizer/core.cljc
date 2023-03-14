(ns equalizer.core
  (:refer-clojure :exclude [and or not map])
  (:require
    [clojure.core :as c]
    #?(:clj [equalizer.helpers :as helpers]))
  #?(:cljs
     (:require-macros
       [equalizer.helpers :as helpers]))
  #?(:clj
     (:import
       (clojure.lang
         Fn)
       (java.util.regex
         Pattern))))


;;
;; Protocols
;;

(defprotocol Builder
  (-validator [this]))


(defprotocol Validator
  (-kind [validator])
  (-valid? [validator x]))



;;
;; Public API
;;

(defn kind
  [?validator]
  (-kind (-validator ?validator)))


(defn valid?
  [?validator x]
  (-valid? (-validator ?validator) x))



;;
;; General validators
;;

(extend-type nil
  Builder
  (-validator [_]
    (reify Validator
      (-kind [_] :nil)
      (-valid? [_ x]
        (nil? x)))))


(extend-type #?(:clj Object, :cljs default)
  Builder
  (-validator [obj]
    (reify Validator
      (-kind [_] :object)
      (-valid? [_ x]
        (= obj x)))))


(extend-type #?(:clj Fn, :cljs function)
  Builder
  (-validator [f]
    (reify Validator
      (-kind [_] :fn)
      (-valid? [_ x]
        (helpers/safe
          (boolean (f x))
          (constantly false))))))


(extend-type #?(:clj Pattern, :cljs js/RegExp)
  Builder
  (-validator [re]
    (let [pattern (re-pattern re)]
      (reify Validator
        (-kind [_] :regexp)
        (-valid? [_ x]
          (c/and
            (string? x)
            (boolean (re-find pattern x))))))))



;;
;; Combinators
;;

(defn not
  [?validator]
  (let [validator (delay (-validator ?validator))]
    (reify
      Builder
      (-validator [v] v)

      Validator
      (-kind [_] :not)
      (-valid? [_ x]
        (c/not (-valid? @validator x))))))


(defn and
  [& ?validators]
  (let [validators (->> ?validators
                        (c/map #(partial -valid? (-validator %)))
                        (apply every-pred)
                        (delay))]
    (reify
      Builder
      (-validator [v] v)

      Validator
      (-kind [_] :and)
      (-valid? [_ x]
        (boolean (@validators x))))))


(defn or
  [& ?validators]
  (let [validators (->> ?validators
                        (c/map #(partial -valid? (-validator %)))
                        (apply some-fn)
                        (delay))]
    (reify
      Builder
      (-validator [v] v)

      Validator
      (-kind [_] :or)
      (-valid? [_ x]
        (boolean (@validators x))))))


(defn maybe
  [?validator]
  (let [validator (delay (or ?validator nil?))]
    (reify
      Builder
      (-validator [v] v)

      Validator
      (-kind [_] :maybe)
      (-valid? [_ x]
        (-valid? @validator x)))))


(defn tuple
  [& ?validators]
  (let [validators (delay (c/map -validator ?validators))
        size (count ?validators)]
    (reify
      Builder
      (-validator [v] v)

      Validator
      (-kind [_] :tuple)
      (-valid? [_ ?coll]
        (c/and
          (sequential? ?coll)
          (= size (count ?coll))
          (loop [acc true
                 [v & vs] @validators
                 [x & xs] ?coll]
            (if (c/or (false? acc) (nil? v))
              acc
              (recur (-valid? v x) vs xs))))))))


(defn coll-of
  [?validator]
  (let [validator (delay (-validator ?validator))]
    (reify
      Builder
      (-validator [v] v)

      Validator
      (-kind [_] :coll-of)
      (-valid? [_ ?coll]
        (c/and
          (sequential? ?coll)
          (every? (partial -valid? @validator) ?coll))))))


(defn map-of
  [?key-validator ?value-validator]
  (let [validator (delay (tuple ?key-validator ?value-validator))]
    (reify
      Builder
      (-validator [v] v)

      Validator
      (-kind [_] :map-of)
      (-valid? [_ ?map]
        (c/and
          (map? ?map)
          (reduce
            (fn [acc entry]
              (if (-valid? @validator entry)
                acc
                (reduced false)))
            true ?map))))))


(defn map-validators
  [m]
  (letfn [(walk
            [acc ks m]
            (reduce-kv
              (fn [acc k v]
                (if (map? v)
                  (walk acc (conj ks k) v)
                  (conj acc [(conj ks k) (-validator v)])))
              acc m))]
    (walk [] [] m)))


(defn map
  [?validator]
  (let [validators (delay (map-validators ?validator))]
    (reify
      Builder
      (-validator [v] v)

      Validator
      (-kind [_] :map)
      (-valid? [_ ?map]
        (c/and
          (map? ?map)
          (reduce
            (fn [acc [path validator]]
              (if (-valid? validator (get-in ?map path))
                acc
                (reduced false)))
            true @validators))))))

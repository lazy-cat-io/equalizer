(ns equalizer.helpers
  (:refer-clojure :exclude [format])
  #?(:clj
     (:require
       [clojure.core :as c])
     :cljs
     (:require
       [goog.string :as gstr]
       [goog.string.format])))


(def format
  "Formats a string."
  #?(:clj  c/format
     :cljs gstr/format))


#?(:clj
   (defn cljs?
     "Checks &env in macro and returns `true` if that cljs env. Otherwise `false`."
     {:added "0.0.1"}
     [env]
     (boolean (:ns env))))


#?(:clj
   (defmacro safe
     "Extended version of try-catch."
     {:added "0.0.1"}
     ([body]
      `(try
         ~body
         (catch ~(if-not (cljs? &env) 'Exception :default) _#
           nil)))

     ([body handler]
      `(try
         ~body
         (catch ~(if-not (cljs? &env) 'Exception :default) error#
           (~handler error#))))))

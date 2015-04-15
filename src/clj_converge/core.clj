(ns clj-converge.core
  (:require [clojure.math.numeric-tower :as math]))

(set! *warn-on-reflection* true)

(defn- check-threshold [values threshold]
  (let [[old new] values]
    (<= (math/abs (- old new)) threshold)))

(defn CljConverge
  ([modfn arg threshold]
   (let [initial (modfn arg)]
    (first (map second (drop-while #(not (check-threshold % threshold))
                       (iterate (fn [[p n]] [n (modfn n)]) [initial (modfn initial)]))))))
  ([modfn threshfn arg threshold]
   (let [initial (modfn arg)]
    (first (map second (drop-while #(not (threshfn % threshold))
                       (iterate (fn [[p n]] [n (modfn n)]) [initial (modfn initial)])))))))

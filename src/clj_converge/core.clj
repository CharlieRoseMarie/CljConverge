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

(defn SeqConverge [seq-function initial inc-function joining-function threshold]
    (loop [previous-value (seq-function initial)
           current-nth (inc-function (inc-function initial))
           current-value (joining-function previous-value (seq-function (inc-function initial)))]
      (if (check-threshold [previous-value current-value] threshold)
        current-value
        (recur current-value (inc-function current-nth) (joining-function current-value (seq-function current-nth))))))

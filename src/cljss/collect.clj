(ns cljss.collect
  (:require [cljss.utils :refer [build-css]]))

(defn dynamic? [[_ value]]
  (not (or (string? value)
           (number? value))))

(defn varid [cls idx [rule]]
  [rule (str "--var-" cls "-" idx)])

(defn collect-styles
  ([cls styles rule-index] (collect-styles cls styles rule-index nil))
  ([cls styles rule-index tail-class]
   (let [dynamic (filterv dynamic? styles)
         static  (filterv (comp not dynamic?) styles)
         [vars rule-index] (reduce
                             (fn [[vars idx] ds]
                               (let [ret (conj vars (varid cls idx ds))]
                                 [ret (inc idx)]))
                             [[] rule-index]
                             dynamic)
         vals    (mapv (fn [[_ var] [_ exp]] [var exp]) vars dynamic)
         static  (->> vars
                      (map (fn [[rule var]] [rule (str "var(" var ")")]))
                      (concat static)
                      (build-css (str cls tail-class)))]
     [static vals rule-index])))

(defn collect-dynamic-styles [rule-index rules cls tail-class-g]
  (loop
    [idx rule-index
     acc []
     coll rules]
    (if (not (seq coll))
      [acc idx]
      (let [[rule styles] (first coll)
            [static vals nxt-idx] (collect-styles cls styles idx (tail-class-g rule))]
        (recur nxt-idx
               (conj acc [static vals])
               (rest coll))))))

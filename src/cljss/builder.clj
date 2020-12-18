(ns cljss.builder
  (:require [cljss.media :refer [build-media]]
            [cljss.collect :as c]
            [cljss.utils :as utils]))

(defn status? [[rule value]]
  (and (re-matches #"^.*\?$" (name rule))
       (map? value)))

(defn build-styles [cls styles]
  (let [rule-index 0
        pseudo (filterv utils/pseudo? styles)
        nested (->> styles
                    (filterv (comp not utils/pseudo?))
                    (filterv utils/nested?))
        [mstatic mvals mrule-index] (some-> styles :cljss.core/media ((partial build-media cls rule-index)))
        rule-index (or mrule-index rule-index)
        styles (dissoc styles :cljss.core/media)
        styles (filterv #(and (not (utils/pseudo? %)) (not (utils/nested? %))) styles)

        [static vals rule-index] (c/collect-styles cls styles rule-index)
        [pstyles rule-index] (c/collect-dynamic-styles
                               rule-index
                               pseudo
                               cls
                               (fn [rule] (subs (name rule) 1)))
        [nstyles rule-index] (c/collect-dynamic-styles
                               rule-index
                               nested
                               cls
                               (fn [rule] (str " " rule)))

        vals (->> pstyles
                  (mapcat second)
                  (into vals)
                  (concat mvals)
                  (into []))
        vals (->> nstyles
                  (mapcat second)
                  (into vals))
        static (into [static] (map first pstyles))
        static (into static (map first nstyles))
        static (if mstatic
                 (conj static mstatic)
                 static)]
    [cls static vals]))

(comment
  (build-styles
    "hello"
    {"&:first-child" {:color "red"}
     "a" {:color "blue"}}))

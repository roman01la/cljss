(ns cljss.reagent
  (:require [cljss.core :refer [->styled var->cls-name var->cmp-name]]))

(defmacro defstyled [var tag styles]
  (let [cls-name# (var->cls-name var)
        cmp-name# (var->cmp-name var)
        [tag# static# vals# attrs#] (->styled tag styles cls-name#)
        create-element# `#(apply reagent.core/create-element ~tag# (cljs.core/clj->js %1) (map reagent.core/as-element %2))]
    `(do
       (def ~var
         (cljss.reagent/styled ~cls-name# ~static# ~vals# ~attrs# ~create-element#))
       (set! ~var ~'-displayName ~cmp-name#))))

(ns cljss.sheet
  (:require [goog.object :as gobj]
            [goog.dom :as dom]
            [cljss.utils :refer [dev?]]))

(def ^:private limit 65534)

(defn- make-style-tag []
  (let [tag (dom/createElement "style")
        head (aget (dom/getElementsByTagNameAndClass "head") 0)]
    (gobj/set tag "type" "text/css")
    (dom/appendChild tag (dom/createTextNode ""))
    (dom/appendChild head tag)
    tag))


(defprotocol ISheet
  (insert! [this css cls-name])
  (flush! [this])
  (filled? [this]))

(deftype Sheet [tag sheet cache]
  ISheet
  (insert! [this rule cls-name]
    (when (filled? this)
      (throw (js/Error. (str "A stylesheet can only have " limit " rules"))))
    (when-not (@cache cls-name)
      (swap! cache conj cls-name)
      (let [rule (if (ifn? rule) (rule) rule)]
        (dom/appendChild tag (dom/createTextNode rule)))))
  (flush! [this]
    (-> tag
        .-parentNode
        (.removeChild tag)))
  (filled? [this]
    (= (count @cache) limit)))

(defn create-sheet []
  (let [tag (make-style-tag)
        sheet (gobj/get tag "sheet")]
    (Sheet. tag sheet (atom #{}))))

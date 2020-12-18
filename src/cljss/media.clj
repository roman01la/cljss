(ns cljss.media
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as cstr]
            [cljss.utils :refer [literal?]]
            [cljss.collect :as c]
            [cljss.utils :as utils]))

;;
;; CSS Media Queries Level 4 spec
;;

(s/def ::modifiers #{:not :only})
(s/def ::media-types #{:all :print :screen :speech})

(s/def ::media-feature-name
  #{:aspect-ratio :color-gamut :color :color-index :any-hover :monochrome
    :grid :width :orientation :update :scripting :resolution :hover
    :inverted-colors :pointer :display-mode :any-pointer :scan
    :overflow-block :overflow-inline :light-level :height
    :max-width :min-width :max-height :min-height
    :max-aspect-ratio :min-aspect-ratio :max-resolution :min-resolution
    :max-color :min-color :max-color-index :min-color-index
    :max-monochrome :min-monochrome :device-height :device-width
    :max-device-height :max-device-width :min-device-height :min-device-width
    :device-aspect-ratio :max-device-aspect-ratio :min-device-aspect-ratio})

(s/def ::media-feature-value some?)

(s/def ::media-feature-plain
  (s/cat
    :feature-name ::media-feature-name
    :feature-value ::media-feature-value))

(s/def ::media-feature-boolean ::media-feature-name)

(s/def ::media-feature-range-basic-operand
  (s/alt
    :feature-name ::media-feature-name
    :feature-value ::media-feature-value))

(s/def ::media-feature-range-basic
  (s/cat
    :left ::media-feature-range-basic-operand
    :operator #{'= '< '<= '> '>=}
    :right ::media-feature-range-basic-operand))

(s/def ::media-feature-range-complex-left-dir
  (s/cat
    :left ::media-feature-value
    :left-operator #{'< '<=}
    :feature-name ::media-feature-name
    :right-operator #{'< '<=}
    :right ::media-feature-value))

(s/def ::media-feature-range-complex-right-dir
  (s/cat
    :left ::media-feature-value
    :left-operator #{'> '>=}
    :feature-name ::media-feature-name
    :right-operator #{'> '>=}
    :right ::media-feature-value))

(s/def ::media-feature-range-complex
  (s/alt
    :left-dir ::media-feature-range-complex-left-dir
    :right-dir ::media-feature-range-complex-right-dir))

(s/def ::media-feature-range
  (s/alt
    :basic-range ::media-feature-range-basic
    :complex-range ::media-feature-range-complex))

(s/def ::media-feature
  (s/alt
    :plain ::media-feature-plain
    :boolean ::media-feature-boolean
    :range ::media-feature-range))

(s/def ::media-condition
  (s/alt
    :m-modifier (s/? ::media-not)
    :condition (s/cat
                 :in-parens ::media-in-parens
                 :conditions (s/* (s/alt
                                    :m-modifier ::media-and
                                    :m-modifier ::media-or)))))

(s/def ::media-in-parens
  (s/alt
    :condition-in-parens (s/spec ::media-condition)
    :feature ::media-feature))

(s/def ::media-not
  (s/cat
    :logical #{:not}
    :in-parens ::media-in-parens))

(s/def ::media-and
  (s/cat
    :logical #{:and}
    :in-parens ::media-in-parens))

(s/def ::media-or
  (s/cat
    :logical #{:or}
    :in-parens ::media-in-parens))

(s/def ::media-condition-without-or
  (s/alt
    :m-modifiers ::media-not
    :condition (s/cat
                 :in-parens ::media-in-parens
                 :m-modifiers (s/* ::media-and))))

(s/def ::media-query
  (s/alt
    :mq-condition ::media-condition
    :query (s/cat
             :modifier (s/? ::modifiers)
             :media-type ::media-types
             :condition (s/?
                          (s/cat
                            :logical #{:and}
                            :cond-without-or ::media-condition-without-or)))))

(s/def ::media-query-list
  (s/or
    :media-query ::media-query
    :media-query-list (s/coll-of ::media-query)))


;;
;; AST compiler
;;

(defn compile-media-query-dispatch [ast]
  (first ast))

(defmulti compile-media-query #'compile-media-query-dispatch)

(defmethod compile-media-query :media-query-list [[_ media-query-list]]
  (->> media-query-list
       (map compile-media-query)
       (interpose ", ")
       flatten))

(defmethod compile-media-query :media-query [[_ media-query]]
  (flatten (compile-media-query media-query)))

(defmethod compile-media-query :query [[_ query]]
  (->> (seq query) (map compile-media-query)))

(defmethod compile-media-query :modifier [[_ modifier]]
  (name modifier))

(defmethod compile-media-query :m-modifier [[_ m-modifier]]
  (map compile-media-query m-modifier))

(defmethod compile-media-query :m-modifiers [[_ m-modifiers]]
  (mapcat
    #(map compile-media-query %)
    m-modifiers))

(defmethod compile-media-query :media-type [[_ media-type]]
  (name media-type))

(defmethod compile-media-query :condition [[_ condition]]
  (->> (seq condition) (map compile-media-query)))

(defmethod compile-media-query :mq-condition [[_ condition]]
  (compile-media-query condition))

(defmethod compile-media-query :conditions [[_ conditions]]
  (mapcat compile-media-query conditions))

(defmethod compile-media-query :condition-in-parens [[_ condition-in-parens]]
  (compile-media-query condition-in-parens))

(defmethod compile-media-query :logical [[_ logical]]
  (if (= logical :or)
    ","
    (name logical)))

(defmethod compile-media-query :cond-without-or [[_ condition]]
  (compile-media-query condition))

(defmethod compile-media-query :in-parens [[_ in-parens]]
  (compile-media-query in-parens))

(defmethod compile-media-query :feature [[_ feature]]
  (compile-media-query feature))

(defmethod compile-media-query :feature-name [[_ feature-name]]
  (name feature-name))

(defmethod compile-media-query :feature-value [[_ feature-value]]
  (cond
    (keyword? feature-value) (name feature-value)
    :else feature-value))

(defmethod compile-media-query :plain [[_ {:keys [feature-name feature-value]}]]
  (let [feature-name (compile-media-query [:feature-name feature-name])
        feature-value (compile-media-query [:feature-value feature-value])]
    (str "(" feature-name ":" feature-value ")")))

(defmethod compile-media-query :range [[_ range]]
  (compile-media-query range))

(defmethod compile-media-query :boolean [[_ bool]]
  (str "(" (name bool) ")"))

(defmethod compile-media-query :basic-range [[_ {:keys [left operator right]}]]
  (let [left (compile-media-query left)
        right (compile-media-query right)
        operator (name operator)]
    (str "(" left " " operator " " right ")")))

(defmethod compile-media-query :complex-range
  [[_ [_ {:keys [left left-operator feature-name right-operator right]}]]]
  (let [left (compile-media-query [:feature-value left])
        feature-name (compile-media-query [:feature-name feature-name])
        right (compile-media-query [:feature-value right])
        left-operator (name left-operator)
        right-operator (name right-operator)]
    (str "(" left " " left-operator " " feature-name " " right-operator " " right ")")))

(defn -compile-media-query [query]
  (let [ret (s/conform ::media-query-list query)
        ret (if (= ::s/invalid ret)
              (throw (Error. (s/explain ::media-query-list query)))
              (compile-media-query ret))]
    (->> (flatten ret)
         (clojure.string/join " ")
         (str "@media "))))

(defn compile-media-dispatch [styles _ _]
  (cond
    (contains? styles :media) :media
    (contains? styles :styles) :styles))

(defmulti compile-media #'compile-media-dispatch)

(defmethod compile-media :media [{media :media} cls rule-index]
  (->> (seq media)
       (reduce
         (fn [[sstyles svalues nxt-idx] [query styles]]
           (let [[static values nxt-idx] (compile-media {:styles styles} cls nxt-idx)
                 query (-compile-media-query query)]
             [(str sstyles query static) (concat svalues values) nxt-idx]))
         ["" [] rule-index])))

(defmethod compile-media :styles [{styles :styles} cls rule-index]
  (let [pseudo (filterv utils/pseudo? styles)
        [pstyles rule-index] (c/collect-dynamic-styles rule-index pseudo cls (fn [rule] (subs (name rule) 1)))

        styles (filterv (comp not utils/pseudo?) styles)
        [static values rule-index] (c/collect-styles cls styles rule-index)
        values (->> pstyles
                    (mapcat second)
                    (into values))]
    [(str "{" (apply str static (map first pstyles)) "}")
     values
     rule-index]))

(defn build-media [cls rule-index styles]
  (compile-media {:media styles} cls rule-index))

(comment
  (build-media
    "class"
    {[[:only :screen :and [:min-width "300px"]]
      [:print :and [:color]]]
     {:font-size 'p
      :&:hover   {:color 'g}}}))

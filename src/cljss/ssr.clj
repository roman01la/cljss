(ns cljss.ssr
  (:require [clojure.string :as cstr]
            [rum.server-render :as rsr]
            [cljss.builder :as builder])
  (:import (clojure.lang ISeq IPersistentVector)))

(def ^:dynamic *ssr-ctx*)

(defn add-css
  ([class styles]
   (add-css class styles []))
  ([class styles vars]
   (->> vars
        (reduce (fn [ret [cls value]]
                  (cstr/replace ret (str "var(" cls ")") value))
                (cstr/join styles))
        vector
        (swap! *ssr-ctx* assoc-in [:styles class]))))

(defn compile-class [classes styles]
  (let [cls (str "css-" (hash styles))
        [class styles] (builder/build-styles cls styles)]
    (add-css class styles)
    (cstr/join [classes class])))

(defn normalize-element [[first second & rest]]
  (when-not (or (keyword? first)
                (symbol? first)
                (string? first))
    (throw (ex-info "Expected a keyword as a tag" {:tag first})))
  (let [[tag tag-id tag-classes] (rsr/parse-selector first)
        [attrs children] (if (or (map? second)
                                 (nil? second))
                           [second rest]
                           [nil (cons second rest)])
        attrs-classes (:class attrs)
        classes       (if (and tag-classes attrs-classes)
                        [tag-classes attrs-classes]
                        (or tag-classes attrs-classes))]
    [tag tag-id classes attrs children]))

;;
;; =============================

(defmulti walk-hiccup type)

(defmethod walk-hiccup :default [form]
  form)

(defmethod walk-hiccup IPersistentVector [element]
  (let [[_ _ classes attrs children] (normalize-element element)
        [tag] element
        styles (:css attrs)]
    (if (map? styles)
      (let [class (compile-class classes styles)
            attrs (-> attrs
                      (assoc :class class)
                      (dissoc :css))]
        `[~tag ~attrs ~@(map walk-hiccup children)])
      `[~tag ~attrs ~@(map walk-hiccup children)])))

(defmethod walk-hiccup ISeq [elements]
  (map walk-hiccup elements))

(defn ctx->css-str [ctx]
  (->> ctx :styles vals (map cstr/join) cstr/join))

(defn render-css [html]
  (let [html (walk-hiccup html)
        css  (ctx->css-str @*ssr-ctx*)]
    [html css]))
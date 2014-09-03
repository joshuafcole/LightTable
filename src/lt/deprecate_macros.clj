(ns lt.deprecate-macros
  (:refer-clojure :exclude [namespace]))

(defn full-name
  ([ns-key sym]
   (let [ns-str (clojure.core/namespace ns-key)]
     (str ns-str
          (when sym (str "/" sym)))))
  ([ns-key] (full-name ns-key nil)))

(defn ns-parts [name]
  (clojure.string/split
   (first (clojure.string/split name #"/"))
   #"\."))

;; Functions
(defn wrap-bodies [old-name new-name body]
  (let [wrapper (fn [item]
                  `(~(first item)
                     (lt.util.deprecate/mark-activated :fn ~old-name ~new-name)
                     ~(second item)))
        body (if (vector? (first body))
               (list body)
               body)
        body (map wrapper body)]
    body))

(defmacro function [ns-key old-name new-name & body]
  (let [namespaced (clojure.core/namespace old-name)
        full-old-name (if namespaced (str old-name) (full-name ns-key old-name))
        full-new-name (full-name ns-key new-name)
        old-name-parts (conj (ns-parts full-old-name) (name old-name))]
    `(do
       (lt.util.deprecate/mark-deprecated :fn ~full-old-name nil)
       ~(conj body new-name 'defn)
       (apply aset
              js/window
              ~(conj old-name-parts
                    (conj (wrap-bodies full-old-name full-new-name body) 'fn)))


)))

;; Ex.
;; (macroexpand-1 '(function ::ns old-name new-name [x] x))
;; (macroexpand-1 '(function ::ns bob/old-name new-name [x] x))


;; Variables
(defmacro variable [ns-key old-name new-name val]
  (let [namespaced (clojure.core/namespace old-name)
        full-old-name (if namespaced (str old-name) (full-name ns-key old-name))
        full-new-name (full-name ns-key new-name)
        old-ns-parts (ns-parts full-old-name)]

    `(let [old-ns-obj# (apply aget (cons js/window ~old-ns-parts))]
       (lt.util.deprecate/mark-deprecated :var ~full-old-name nil)
       (def ~new-name ~val)
       (.__defineGetter__
        old-ns-obj# ~(name old-name)
        (fn []
          (lt.util.deprecate/mark-activated :var ~full-old-name ~full-new-name)
          ~new-name)))))

;; Ex.
;; (macroexpand-1 '(variable ::ns old-var new-var 5))
;; (macroexpand-1 '(variable ::ns bob/old-var new-var 5))


;; Namespaces
(defmacro namespace [old-name new-name]
  `(do
     (lt.util.deprecate/mark-deprecated :ns ~(str old-name) nil)
     (let [old-proxy# ~(clojure.string/split (str old-name) #"\.")
           old-root# (butlast old-proxy#)
           old-tail# (last old-proxy#)
           mk-ns# (fn [memo# v#]
                    (when-not (aget memo# v#)
                      (aset memo# v# (clojure.core/js-obj)))
                    (aget memo# v#))
           old-obj# (reduce mk-ns# js/window old-root#)]

       (.__defineGetter__
        old-obj# old-tail#
        (fn []
          (lt.util.deprecate/mark-activated :ns ~(str old-name) ~(str new-name))
          ~new-name)))))

;; Ex.
;; (macroexpand-1 '(namespace lt.blues.clues lt.util.deprecate))



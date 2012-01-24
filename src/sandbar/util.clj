;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.util
  "Utility functions for sandbar."
  (:use [clojure.string :only [split]])
  (:import java.io.File))

;;
;; Misc
;; ====
;;

(defn name-with-attributes
  "To be used in macro definitions.
   Handles optional docstrings and attribute maps for a name to be defined
   in a list of macro arguments. If the first macro argument is a string,
   it is added as a docstring to name and removed from the macro argument
   list. If afterwards the first macro argument is a map, its entries are
   added to the name's metadata map and the map is removed from the
   macro argument list. The return value is a vector containing the name
   with its extended metadata map and the list of unprocessed macro
   arguments."
  [name macro-args]
  (let [[docstring macro-args] (if (string? (first macro-args))
                                 [(first macro-args) (next macro-args)]
                                 [nil macro-args])
    [attr macro-args]          (if (map? (first macro-args))
                                 [(first macro-args) (next macro-args)]
                                 [{} macro-args])
    attr                       (if docstring
                                 (assoc attr :doc docstring)
                                 attr)
    attr                       (if (meta name)
                                 (conj (meta name) attr)
                                 attr)]
    [(with-meta name attr) macro-args]))

(defn append-to-keyword [k s]
  (keyword (str (name k) s)))

(defn to-keywords [coll]
  (map #(keyword %) coll))

(defn path-to-seq [path]
  (filter #(not (.equals % "")) (split path #"/")))

(defn seq-to-path [coll]
  (apply str (interleave (repeat "/") coll)))

(defn random-between [lo hi]
  (let [r (java.util.Random.)
        lo (if (char? lo) (int lo) lo)
        hi (if (char? hi) (int hi) hi)
        n (+ 1 (- hi lo))]
    (+ lo (Math/abs (mod (. r nextInt) n)))))

(defn random-string [lo hi]
  (loop [length (random-between lo hi)
         v []]
    (if (> length 0)
      (let [j (random-between 1 3)]
        (recur (dec length)
               (conj v
                     (cond (= j 1) (char (random-between \a \z))
                           (= j 2) (char (random-between \A \Z))
                           (= j 3) (char (random-between \1 \9))))))
      (apply str v))))

(defn index-by [k coll]
  (reduce (fn [a b]
            (let [v (k b)]
              (assoc a v b)))
          {}
          coll))

;;
;; Files
;; =====
;;

(defn without-ext [s]
  (apply str (interpose "." (reverse (rest (reverse (split s #"[.]")))))))

(defn remove-file-ext [file-name]
  (let [index (.lastIndexOf file-name ".")]
    (if (> index -1)
      (apply str (first (split-at index file-name)))
      file-name)))
 
(defn file-to-ns-string [f root-dir]
  (let [f-sep File/separator
        test-dir-pattern (re-pattern (str f-sep root-dir f-sep))]
    (replace (remove-file-ext
              (last (split (.getAbsolutePath f) test-dir-pattern)))
             (re-pattern f-sep)
             ".")))
 
(defn file-seq-map-filter [dir mf ff]
  (filter ff (map mf (file-seq (File. dir)))))

;;
;; Formats
;; =======
;;

(defn format-csv [coll-of-rows]
  (let [data (map #(interpose "," %) coll-of-rows)]
    (apply str
           (map #(if (nil? %) "\"\""  (if (and (not (= % "\n"))
                                               (not (= % ",")))
                                        (str "\"" % "\"")
                                        %))
                           (reduce (fn [a b] (concat a b ["\n"])) [] data)))))

(defn date-string
  ([] (date-string (java.util.Date.)))
  ([d]
     (let [formatter (java.text.SimpleDateFormat. "yyyy-MM-dd")]
       (.format formatter d))))

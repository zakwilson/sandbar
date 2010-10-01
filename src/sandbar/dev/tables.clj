;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.dev.tables
  "HTML tables which may be filtered, sorted and paged."
  (:use (ring.util [codec :only (url-encode)])
        (clojure.contrib [json :only (json-str)])
        (hiccup core page-helpers)
        (sandbar core stateful-session)))

(declare *table-id*)

(defprotocol ResourceList
  (find-resources [this filters page-and-sort])
  (fields [this]))

(defprotocol PagedResources
  (page-size [this])
  (total-resource-count [this filters]))

(defprotocol Labels
  (label [this key]))

(defn merge-table-state-vecs [old new]
  (if (number? old)
    new
    (let [order (distinct
                 (concat (map first (partition 2 old))
                         (map first (partition 2 new))))
          m (reduce (fn [a b]
                      (assoc a (first b) (last b)))
                    {}
                    (partition 2 (concat old new)))]
      (vec
       (apply concat
              (filter #(not (= :remove (last %)))
                      (map #(vector % (m %)) order)))))))

(defn update-table-state! [params]
  (-> (update-session!
       (fn [a b]
         (let [current-state (or (-> a :table-state *table-id*) {})]
           (-> a
               (assoc-in [:table-state *table-id*]
                         (merge-with merge-table-state-vecs
                                     current-state
                                     b)))))
       (let [s
             (hash-map :sort
                       (vec (concat
                             (if-let [sort (get-param params :sort-asc)]
                               [(keyword sort) :asc])
                             (if-let [sort (get-param params :sort-desc)]
                               [(keyword sort) :desc])
                             (if-let [sort (get-param params :remove-sort)]
                               [(keyword sort) :remove]))))
             f
             (hash-map :filter
                       (vec (concat
                             (if-let [filter (get-param params :filter)]
                               [(keyword filter)
                                (get-param params :filter-value)])
                             (if-let [filter (get-param params :remove-filter)]
                               [(keyword filter) :remove]))))
             p
             (cond (or (not (empty? (concat (:filter f) (:sort s)))))
                   {:page 0}
                   :else (if-let [page (get-param params :page)]
                           {:page (Integer/valueOf page)}))]
         (merge s f p)))
      :table-state *table-id*))

(defn build-page-and-sort-map [adapter table-state-map]
  (let [pas (assoc {} :sort
                   (vec
                    (apply concat
                           (map #(list (last %) (name (first %)))
                                (partition 2 (:sort table-state-map))))))]
    (if (satisfies? PagedResources adapter)
      (assoc pas :page (or (:page table-state-map) 0)
                 :page-size (page-size adapter))
      pas)))

(defn build-filter-map [table-state-map]
  (reduce (fn [a b] (assoc a (first b) (last b)))
          {}
          (partition 2 (:filter table-state-map))))

(defn current-filters! [params]
 (let [t-state (update-table-state! params)]
    (build-filter-map t-state)))

(defn current-page-and-sort! [adapter params]
  (let [t-state (update-table-state! params)]
    (build-page-and-sort-map adapter t-state)))

(defn get-column-name [column-spec-row]
  (if (keyword? column-spec-row)
    column-spec-row
    (:column column-spec-row)))

(defn table-cell [map-or-value & values]
  (vec
   (filter #(not (nil? %))
           (if (map? map-or-value)
             [:td (:attr map-or-value)
              (if (contains? (:actions map-or-value) :filter)
                (link-to-js (addFilter (name (:column map-or-value))
                                       (url-encode (:value map-or-value)))
                            (:value map-or-value)
                            *table-id*)
                (:value map-or-value))]
             (if (seq values)
               (vec (concat [:td map-or-value] values))
               [:td map-or-value])))))

(defn table-row
  ([coll] [:tr (doall (map table-cell coll))])
  ([coll row-class]
     [:tr {:class row-class} (doall (map table-cell coll))]))

(defn table-header [coll]
  [:tr (map #(vector :th {:nowrap ""} %) coll)])

(defn standard-table [props columns column-fn data]
  [:table {:class "list"}
      (table-header (map #(if-let [p (props %)] p %) columns))
   (map
    (fn [row-data class]
      (table-row (doall (map #(column-fn % row-data) columns)) class))
    data (cycle ["odd" "even"]))])

(defn get-table-state [table-name]
  (session-get [:table-state table-name]))

(defn opposite-sort-dir [d]
  (cond (= d :asc) :desc
        (= d :desc) :asc
        :else d))

(defn table-column-names [column-spec]
  (map get-column-name column-spec))

(defn table-sort-columns [column-spec]
  (set
   (map #(if (contains? (:actions %) :sort) (:column %))
        (filter map? column-spec))))

(defn sort-table-header [adapter column-spec]
  (let [t-state (:sort (get-table-state *table-id*))
        sort-dir-map (reduce
                      (fn [a b]
                        (assoc a (first b) (last b)))
                      {}
                      (partition 2 t-state))
        sort-columns (table-sort-columns column-spec)]
    [:tr (doall
          (map
           #(let [sort-dir (sort-dir-map %)
                  opp-sort-dir (name (if-let [sd (opposite-sort-dir sort-dir)]
                                       sd
                                       :asc))]
              (vector :th {:nowrap ""}
                      (if (contains? sort-columns %)
                        (link-to-js (sortColumn opp-sort-dir (name %))
                                    (label adapter %)
                                    *table-id*)
                        (label adapter %))
                      "&nbsp;"
                      (cond (= sort-dir :asc) (image "sort_ascending.png")
                            (= sort-dir :desc) (image "sort_descending.png")
                            :else (image "blank16.gif"))))
           (table-column-names column-spec)))]))

(defn- create-saf-table-control [t-state k title link-fn data-fn]
  (let [t-state (k t-state)]
      (if (seq t-state)
        (vec
         (concat
          [:div title]
          (interpose
           ", "
           (apply vector (map link-fn (data-fn t-state))))))
        "")))

(defn create-table-sort-and-filter-controls [adapter]
  (let [current-state (get-table-state *table-id*)]
    (vec
     (conj
      [:div {:class "filter-and-sort-controls"}]
      (create-saf-table-control current-state :sort "Remove sort: "
                               #(link-to-js (removeSort (name %))
                                            (label adapter (keyword %))
                                            *table-id*)
                               #(map first (partition 2 %)))
      (create-saf-table-control current-state :filter "Remove filter: "
                                #(let [c (first %)]
                                   (link-to-js (removeFilter (name c))
                                               (str
                                                (label adapter (keyword c))
                                                " = "
                                                (last %))
                                               *table-id*))
                                #(partition 2 %))))))

(defn- page-controls [view content]
  (if-let [page-size (:page-size view)]
    (let [{:keys [last page available column-count]} view
          next-page (+ page 1)
          previous-page (- page 1)]
      [:tr
       [:td {:colspan column-count}
        [:table {:width "100%" :cellspacing "0" :cellpadding "0" :border "0"}
         [:tr
          [:td {:width "40px" :align "left"}
           (if (>= previous-page 0)
             (link-to-js (page previous-page)
                         "back"
                         *table-id*))]
          content
          [:td {:align "right"}
           (if (< last available)
             (link-to-js (page next-page)
                         "next"
                         *table-id*))]]]]])))

(defn- page-control-header [name view]
  (page-controls view
                 (if (:page-size view)
                   (let [{:keys [first last available]} view]
                    [:td.filter-table-summary
                     "showing "
                     name " "
                     [:b (+ first 1)]
                     " through "
                     [:b last]
                     " of "
                     [:b available]]))))

(defn- page-control-footer [view]
  (page-controls view [:td]))

(defn make-table-view
  "Create the current view of the data that will be displayed in the table.
   This includes paging information."
  [adapter column-spec params]
  (let [page-and-sort (current-page-and-sort! adapter params)
        filters (current-filters! params)
        table-data (find-resources adapter filters page-and-sort)
        view {:data table-data
              :column-count (count column-spec)}]
    (if-let [page-size (:page-size page-and-sort)]
      (let [page (get page-and-sort :page 0)
            first (* page page-size)
            visible (count table-data)
            last (+ visible first)]
        (merge view {:page-size page-size
                     :page page
                     :first first
                     :last last
                     :available (total-resource-count adapter filters)
                     :visible visible}))
      view)))

(defmulti display-table-cell (fn [type k data] [type k]))

(defmethod display-table-cell :default [type k data]
           (or (k data) ""))

(defn build-row
  "Build one table row."
  [adapter column-spec row-data css-class]
  (let [next-row (table-row
                  (map #(let [cell-data
                              (display-table-cell (:type adapter)
                                                  (get-column-name %)
                                                  row-data)
                              cell-data (if (map? cell-data)
                                          cell-data
                                          {:value cell-data})]
                          (merge
                           {:column (get-column-name %)
                            :value nil
                            :attr (merge {:align :left}
                                         (:attr %))
                            :actions (:actions %)}
                           cell-data))
                       column-spec)
                  css-class)]
    next-row))

(defn filter-and-sort-table [adapter column-spec params]
  (binding [*table-id* (keyword (str (name (:type adapter)) "-table"))]
    (let [table-view (make-table-view adapter column-spec params)]
      [:div {:id *table-id* :class (or (:class adapter)
                                       "filter-and-sort-table")}
       (create-table-sort-and-filter-controls adapter)
       [:table {:class "list"}
        (page-control-header (label adapter *table-id*)
                             table-view)
        (sort-table-header adapter column-spec)
        (doall
         (map (partial build-row adapter column-spec)
              (:data table-view)
              (cycle ["odd" "even"])))
        (page-control-footer table-view)]
       (include-js (str (cpath "/js/sandbar/table/")
                        (name *table-id*)
                        ".js"))])))

(defn table-as-json [table]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (json-str {:html table})})

;; Use Scriptjure for generating javascript.

(defmulti js-ajax (fn [js-lib _ _] js-lib))

(defmethod js-ajax :prototype [js-lib qualifier table-id]
  (str "
function updateTable_" qualifier "(uri) {
  new Ajax.Request(uri, {
    onSuccess: function(response) {
      var data = response.responseJSON;
      displayResults_" qualifier "(data);
    }
  });
}

function displayResults_" qualifier "(data) {
  $('" table-id "').replace(data['html']);
}"))

(defmethod js-ajax :jquery [js-lib qualifier table-id]
  (str "
function updateTable_" qualifier "(uri) {
  $.ajax({
    type: 'post',
    dataType: 'json',
    url: uri,
    success: function(data) {
      $('#" table-id "').html(data['html']);
    }
  });
}"))

(defn js [table-id table-uri js-lib]
  (let [q (.replaceAll table-id "-" "_")]
    (str "
function page_" q "(n) {
  updateTable_" q "('" table-uri "?page' + '=' + n);
}

function sortColumn_" q "(dir, column) {
  updateTable_" q "('" table-uri "?sort-' + dir + '=' + column);
}

function removeSort_" q "(column) {
  updateTable_" q "('" table-uri "?remove-sort=' + column);
}

function addFilter_" q "(column, value) {
  updateTable_" q "('" table-uri "?filter=' + column + '&filter-value=' + value);
}

function removeFilter_" q "(column) {
  updateTable_" q "('" table-uri "?remove-filter=' + column);
}" "\n" (js-ajax js-lib q table-id))))

(defn wrap-table-js
  [handler js-uri-map js-lib]
  (fn [request]
    (let [uri (:uri request)]
      (if (.startsWith uri "/js/sandbar/table/")
        (let [table-id (.substring uri 18 (- (count uri) 3))]
          {:status 200
           :headers {"Content-Type" "text/javascript"}
           :body (js table-id ((keyword table-id) js-uri-map) js-lib)})
        (handler request)))))

;;
;; Functions for adapting this table to carte backend
;;

(defn remove-path-from-keyword [path k]
  (keyword (subs (name k)
                 (if (= path "") 0 (+ 1 (count path))))))

(defn sorts-on-path [path sort]
  (flatten
   (filter #(= (.indexOf (name (last %)) ".") -1)
           (map #(vector (first %)
                          (remove-path-from-keyword path (last %)))
                 (filter (if (= path "")
                           #(= (.indexOf (name (last %)) ".") -1)
                           #(.startsWith (name (last %)) (str path ".")))
                         (partition 2 sort))))))

(defn filters-on-path [path filters]
  (reduce (fn [a b]
            (let [k (remove-path-from-keyword path (key b))]
              (if (= (.indexOf (name k) ".") -1)
                (assoc a k (val b))
                a)))
          {}
          (select-keys filters
                       (filter (if (= path "")
                                 #(= (.indexOf (name %) ".") -1)
                                 #(.startsWith (name %) (str path ".")))
                               (keys filters)))))

(defn- new-root-path [path next]
  (let [n (name next)]
    (if (= path "")
      n
      (str path
           "."
           n))))

(defn carte-query
  ([table filters sort]
     (let [query [table]
           query (if (empty? filters) query (conj query filters))
           query (if (and sort
                          (not (empty? sort)))
                   (vec
                    (concat query
                            [:order-by]
                            (map #(let [[field dir] (reverse %)]
                                    [(keyword field) dir])
                                 (partition 2 sort))))
                   query)]
       query))
  ([root-path table joins filters sort]
     (let [query (carte-query table
                              (filters-on-path root-path filters)
                              (sorts-on-path root-path sort))
           join-queries (map #(if (coll? %)
                                (carte-query (new-root-path root-path (first %))
                                             (first %)
                                             (rest %)
                                             filters
                                             sort)
                                (let [p (new-root-path root-path %)]
                                  (carte-query %
                                               (filters-on-path p filters)
                                               (sorts-on-path p sort))))
                             joins)]
       (if (not (empty? join-queries))
             (concat query [:with] join-queries)
             query))))

(defn carte-table-adapter
  "Transform filter and sort information from a filter-and-sort table into
   a query that carte can understand."
  [table filters sort-and-page]
  (let [{:keys [sort page page-size]} sort-and-page
        tables (if (keyword? table) [table] table)
        query (carte-query "" (first tables) (rest tables) filters sort)]
    (if page-size
      (concat query [:page page page-size])
      query)))

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
        (clojure.contrib (json :only (json-str)))
        (hiccup core page-helpers)
        (sandbar core stateful-session)))

(declare *table-id*)

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
                             (if-let [sort (get params "sort-asc")]
                               [(keyword sort) :asc])
                             (if-let [sort (get params "sort-desc")]
                               [(keyword sort) :desc])
                             (if-let [sort (get params "remove-sort")]
                               [(keyword sort) :remove]))))
             f
             (hash-map :filter
                       (vec (concat
                             (if-let [filter (get params "filter")]
                               [(keyword filter) (get params "filter-value")])
                             (if-let [filter (get params "remove-filter")]
                               [(keyword filter) :remove]))))
             p
             (cond (or (not (empty? (concat (:filter f) (:sort s)))))
                   {:page 0}
                   :else (if-let [page (get params "page")]
                           {:page (Integer/valueOf page)}))]
         (merge s f p)))
      :table-state *table-id*))

(defn build-page-and-sort-map [page-size table-state-map]
  (let [pas (assoc {} :sort
                   (vec
                    (apply concat
                           (map #(list (last %) (name (first %)))
                                (partition 2 (:sort table-state-map))))))]
    (if page-size
      (assoc pas :page (or (:page table-state-map) 0) :page-size page-size)
      pas)))

(defn build-filter-map [table-state-map]
  (reduce (fn [a b] (assoc a (first b) (last b)))
          {}
          (partition 2 (:filter table-state-map))))

(defn current-filters! [params]
 (let [t-state (update-table-state! params)]
    (build-filter-map t-state)))

(defn current-page-and-sort! [page-size params]
  (let [t-state (update-table-state! params)]
    (build-page-and-sort-map page-size t-state)))

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

(defn sort-table-header [props column-spec]
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
                                    (props %)
                                    *table-id*)
                        (props %))
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

(defn create-table-sort-and-filter-controls [props]
  (let [current-state (get-table-state *table-id*)]
    (vec
     (conj
      [:div {:class "filter-and-sort-controls"}]
      (create-saf-table-control current-state :sort "Remove sort: "
                               #(link-to-js (removeSort (name %))
                                            ((keyword %) props %)
                                            *table-id*)
                               #(map first (partition 2 %)))
      (create-saf-table-control current-state :filter "Remove filter: "
                                #(let [c (first %)]
                                   (link-to-js (removeFilter (name c))
                                               (str
                                                ((keyword c) props c)
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

(defprotocol FilterAndSortTable
  (load-table-data [this filters page-and-sort])
  (create-cell [this name data])
  (total-row-count [this filters]))

(defn make-table-view
  "Create the current view of the data that will be displayed in the table.
   This includes paging information."
  [adapter column-spec]
  (let [{:keys [params props page-size]} adapter
        page-and-sort (current-page-and-sort! page-size params)
        filters (current-filters! params)
        table-data (load-table-data adapter filters page-and-sort)
        view {:props props
              :data table-data
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
                     :available (total-row-count adapter filters)
                     :visible visible}))
      view)))

(defn build-row
  "Build one table row."
  [adapter column-spec row-data css-class]
  (table-row
   (map #(let [cell-data
               (create-cell adapter
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
   css-class))

(defn filter-and-sort-table [adapter column-spec]
  (binding [*table-id* (keyword (str (name (:type adapter)) "-table"))]
    (let [table-view (make-table-view adapter column-spec)
          props (:props table-view)]
      [:div {:id *table-id* :class "filter-and-sort-table"}
       (create-table-sort-and-filter-controls props)
       [:table {:class "list"}
        (page-control-header (property-lookup props *table-id*)
                             table-view)
        (sort-table-header props column-spec)
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

(defn carte-table-adapter
  "Transform filter and sort information from a filter-and-sort table into
   a query that carte can understand."
  [table filters sort-and-page]
  (let [{:keys [sort page page-size]} sort-and-page
        query [table]
        query (if (empty? filters) query (conj query filters))
        query (if (and sort
                       (not (empty? sort)))
                (vec
                 (concat query
                         [:order-by]
                         (map #(let [[field dir] (reverse %)]
                                 [(keyword field) dir])
                              (partition 2 sort))))
                query)
        query (if page-size
                (concat query [:page page page-size])
                query)]
    query))

(ns sandbar.example.database
  "In memory user database to support other examples.")

(defonce db (atom {:id 1
                   :users {}
                   :roles #{:admin :user}
                   :regions #{{:id 1 :value "North America"}
                              {:id 2 :value "South America"}
                              {:id 3 :value "Europe"}
                              {:id 4 :value "Australia"}}}
                  :languages #{{:id 1 :name "Clojure"}
                               {:id 2 :name "Ruby"}
                               {:id 3 :name "Java"}
                               {:id 4 :name "C"}
                               {:id 5 :name "Go"}}))

(defn all-roles []
  (:roles @db))

(defn all-users []
  (map val (:users @db)))

(defn all-regions []
  (:regions @db))

(defn store-user [user]
  (swap! db
         (fn [old u]
           (let [id (Integer/valueOf (if (:id u) (:id u) (inc (:id old))))]
             (if (:id u)
               (assoc-in old [:users id] u)
               (-> old
                   (assoc :id id)
                   (assoc-in [:users id] (assoc u :id id))))))
         user))

(defn find-user [id]
  (get (:users @db) (Integer/valueOf id)))

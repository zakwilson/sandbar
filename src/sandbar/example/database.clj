(ns sandbar.example.database
  "In memory user database to support other examples.")

(defonce db (atom {:id 1
                   :users {}
                   :roles #{:admin :user}}))

(defn all-roles []
  (:roles @db))

(defn all-users []
  (map val (:users @db)))

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

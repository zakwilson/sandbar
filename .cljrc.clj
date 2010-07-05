(set! *print-length* 103)
(set! *print-level* 15)

(defn exit [] (. System exit 0))

(def deview-server (atom nil))

(require 'deview.server)
(defn start-deview []
  (swap! deview-server (fn [a b] b) (deview.server/start 9001)))

(defn stop-deview []
  (deview.server/stop @deview-server))

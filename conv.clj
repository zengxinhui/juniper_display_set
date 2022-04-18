(ns conv
  (:require [clojure.string :as s]))

;; /\*.*?\*/ matches annotate
;; #[^\n]+\n matches ignored comment
;; [^\n;{}]+ matches any line
;; ;|\{|} matches ; or { or }

(defn process [s]
  (let [f (fn [{:keys [tags temp] :as r} piece]
            (cond
              (= [\/ \*] (take 2 piece)) (cond-> r
                                           :do (update :anno conj "top")
                                           :do (assoc :anno-msg (str " \"" (subs piece 3 (- (count piece) 3)) "\""))
                                           (< 0 (count tags)) (update :anno conj (s/join " " (cons "edit" tags))))
              (= \# (first piece)) r
              (= "{" piece) (-> r (update :tags conj temp) (assoc :temp nil))
              (= "}" piece) (-> r (update :tags pop))
              (= ";" piece) (-> r
                                (update :lines conj (s/join " " (cons "set" (conj tags temp))))
                                (assoc :temp nil))
              (= "inactive:" (apply str (take 9 piece))) (-> r
                                                             (update :lines conj (s/join " " (cons "deactivate" (conj tags (subs piece 10)))))
                                                             (assoc :temp (subs piece 10)))
              (= "protect:" (apply str (take 8 piece))) (-> r
                                                            (update :lines conj (s/join " " (cons "protect" (conj tags (subs piece 9)))))
                                                            (assoc :temp (subs piece 9)))
              :else (cond-> r
                      (:anno-msg r) (update :anno conj (str "annotate " piece (:anno-msg r)))
                      :then (dissoc :anno-msg)
                      :then (update :temp #(if % (str % " " piece) piece)))))
        {:keys [lines anno]} (->> (re-seq #"/\*.*?\*/|#[^\n]+\n|[^\n;{}]+|;|\{|}" s)
                                  (map s/trim)
                                  (remove #{""})
                                  (reduce f {:tags [], :anno [], :lines [], :temp nil}))]
    (->> (lazy-cat lines anno) (s/join "\n"))))

(if (not= 1 (count *command-line-args*))
  (println "Usage: clj conv.clj <filename>")
  (->> (first *command-line-args*) slurp process println))

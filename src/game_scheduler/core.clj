(ns game-scheduler.core)
(require '[clojure.string :as str])
(require '[clojure.set :refer [intersection]])

; The general idea is to sort the possible opponents for each group.
; The group that has the least number of possible opponents gets to select it opponent first.
; If two or more groups has the same number of possible opponents the first one gets to make the selection (every thing is sorted/ordered)
; It will always select the first opponent in its list of possible opponents
; The solution is recursive and removes available groups and possible opponents as the loop progresses
; The structure of the data that runs in the loop looks just the valid-opponents-by-group

; So given ABCD the structur will look like this in the first run (in the loop)

(def groups #{\A \B \C \D})

(def valid-opponents-by-group {\A #{\C \D \E}
                               \B #{\A \C \D}
                               \C #{\A \B \F}
                               \D #{\B \E \F}})

(def senarios ["ABCD"
               "ABCE"
               "ABCF"
               "ABDE"
               "ABDF"
               "ABEF"
               "ACDE"
               "ACDF"
               "ACEF"
               "ADEF"
               "BCDE"
               "BCDF"
               "BCEF"
               "BDEF"
               "CDEF"])

(defrecord Match [group opponents])
(defn match-to-record [match]
  (let [[group opponents] match]
    (->Match group opponents)))
; (match-to-record (first {\B #{\D \C}}))

(defn sort-accending-by-number-of-possible-opponents [matches]
  (sort-by #(count (:opponents (match-to-record %))) matches))
; (sort-accending-by-number-of-possible-opponents {\B #{\D \C} \C #{\A}})

; Removes groups that has no possible opponents left
(defn remove-empty-groups [groups]
  (filter #(not (empty? (last %))) groups))

(defn possible-maches [groups best-thirds]
  (map
   #(list % (intersection (get valid-opponents-by-group %) best-thirds))
   groups))
; (possible-maches groups (set (seq "ABCD")))

(defn find-next-match [groups best-thirds]
  (->
   (possible-maches groups best-thirds)
   (remove-empty-groups)
   (sort-accending-by-number-of-possible-opponents)
   (first)))
; (find-next-match groups (set (seq "ABCD")))

(defn create-schedule [groups best-thirds result]
  (let [next-match (match-to-record (find-next-match groups best-thirds))]
    (if (empty? best-thirds)
      (map last (sort-by #(first %) result))
      (let [group (:group next-match)
            third (first (:opponents next-match))]
        (create-schedule (disj groups group)
                         (disj best-thirds third)
                         (conj result (hash-map group third)))))))

(defn create-schedule-for-senario [best-thirds]
  (create-schedule groups (set (seq best-thirds)) []))

(println (create-schedule-for-senario "ABCD"))

; Manual test
(for [senario senarios]
  (println senario (apply str (map last (create-schedule-for-senario senario)))))

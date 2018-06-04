(ns game-scheduler.core
  (:require [clojure.string :as str]
            [clojure.set :refer [intersection]]))

; Each group has a given set of possible/valid opponents. The opponents are also called best-thirds in this code.
; The group that has the least number of possible opponents gets to select it's opponent first.
; If two or more groups has the same number of possible opponents the first one gets to make the selection (everything is sorted/ordered)
; It will always select the first opponent in its list of possible opponents
; The solution is recursive and removes available groups and possible opponents as the loop progresses
; The structure of the data that runs in the loop looks just the valid-opponents-by-group

; So given ABCD as best thirds the possible-matches will look like this in the first run (in the loop)
; ((\A #{\C \D})
;  (\B #{\A \C \D}) 
;  (\C #{\A \B}) 
;  (\D #{\B}))
; Since D only can play B, that match is given.

; Group D and opponent B are removed from the data set and the loop recurs and possible-matches are:
; ((\A #{\C \D})) 
;  (\B #{\A \C \D}) 
;  (\C #{\A}))
; 
; Group C has only one possible opponents \A, which leaves us with:
; ((\A #{\C \D}) 
;  (\B #{\C \D}))
; Now both A and B has two possible opponent.
; In this case the first group \A selects its only possible opponent \C

; The commands to produce the above data structures are:
; (possible-matches groups (set (seq "ABCD")))
; (possible-matches (disj groups \D) (set (seq "ACD")))
; (possible-matches (disj groups \D \C) (set (seq "CD")))


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

(defn sort-by-opponents-count [matches]
  (sort-by #(count (:opponents (apply ->Match %)))
           matches))

; Removes groups that has no possible opponents left
; TODO: The (last %) is not very clear, it means :opponents
(defn remove-empty-groups [groups]
  (filter #(not (empty? (last %)))
          groups))

(defn possible-matches [groups best-thirds]
  (map
   #(list % (intersection (get valid-opponents-by-group %)
                          best-thirds))
   groups))

(defn find-next-match [groups best-thirds]
  (->
   (possible-matches groups best-thirds)
   (remove-empty-groups)
   (sort-by-opponents-count)
   (first)))

(defn create-schedule [groups best-thirds result]
  (if (empty? best-thirds)
    ; Where done and return the result
    ; The opponents will at this stage only contain one team per group
    (str/join (map :opponents (sort-by #(:group %) result)))
    ; Where not done, lets continue..
    (let [next-match (apply ->Match (find-next-match groups best-thirds))
          group (:group next-match)
          third (first (:opponents next-match))]
      (create-schedule (disj groups group)
                       (disj best-thirds third)
                       (conj result (->Match group third))))))

(defn create-schedule-for-senario [best-thirds]
  (create-schedule groups (set (seq best-thirds)) []))

;(create-schedule-for-senario "ABCD")


(defn -main []
  (doall
   (for [senario senarios]
     (println senario "will play" (create-schedule-for-senario senario)))))

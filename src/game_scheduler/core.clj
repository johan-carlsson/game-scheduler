(ns game-scheduler.core
  (:require [clojure.string :as str]
            [clojure.set :refer [intersection]]))

; Each group has a given set of possible/valid opponents. The opponents are also called best-thirds in this code.
; The group that has the least number of possible opponents, in a given scenario,  gets to select it's opponent first.
; If two or more groups has the same number of possible opponents the first one gets to make the selection (everything is sorted/ordered)
; It will always select the first opponent in its list of possible opponents
; The solution is recursive and removes available groups and possible opponents as the loop progresses

; So given ABCD as best thirds the possible-matches will look like this in the first iteration
; ({:group \A, :opponents #{\C \D}} 
;  {:group \B, :opponents #{\A \C \D}} 
;  {:group \C, :opponents #{\A \B}} 
;  {:group \D, :opponents #{\B}})

; Since D only can play B, that match is given.

; Group D and opponent B are removed and the loop recurs and possible-matches are:
; ({:group \A, :opponents #{\C \D}} 
;  {:group \B, :opponents #{\A \C \D}} 
;  {:group \C, :opponents #{\A}})
; 
; Group C has only one possible opponent \A, which leaves us with:
; ({:group \A, :opponents #{\C \D}} 
;  {:group \B, :opponents #{\C \D}})

; Now both A and B has two possible opponents.
; In this case the first group: \A selects its first opponent: \C

; The commands to produce the above data structures are:
; (possible-matches groups (set (seq "ABCD")))
; (possible-matches (disj groups \D) (set (seq "ACD")))
; (possible-matches (disj groups \D \C) (set (seq "CD")))


(def groups (sorted-set \A \B \C \D))

(def valid-opponents-by-group {\A (sorted-set \C \D \E)
                               \B (sorted-set \A \C \D)
                               \C (sorted-set \A \B \F)
                               \D (sorted-set \B \E \F)})

(def scenarios ["ABCD"
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

(defn count-opponents [match]
  (count (:opponents match)))

(defn sort-by-opponents-count [matches]
  (sort-by count-opponents
           matches))

(defn possible-opponents [group best-thirds]
  (intersection (get valid-opponents-by-group group)
                best-thirds))

(defn possible-matches [groups best-thirds]
  (map
   #(->Match % (possible-opponents % best-thirds))
   groups))

(defn find-next-match [groups best-thirds]
  (->
   (possible-matches groups best-thirds)
   (sort-by-opponents-count)
   (first)))

(defn create-schedule [groups best-thirds result]
  (if (empty? best-thirds)
    ; Where done and return the result
    ; The opponents will at this stage only contain one team per group
    (str/join (map :opponents (sort-by :group result)))
    ; Where not done, lets continue..
    (let [{group :group opponents :opponents} (find-next-match groups best-thirds)
          best-third (first opponents)]
      (recur  (disj groups group)
              (disj best-thirds best-third)
              (conj result (->Match group best-third))))))

(defn create-schedule-for-scenario [best-thirds]
  (create-schedule groups (set (seq best-thirds)) []))

;(create-schedule-for-scenario "ABCD")


(defn -main []
  (doall
   (for [scenario scenarios]
     (println scenario "will play" (create-schedule-for-scenario scenario)))))

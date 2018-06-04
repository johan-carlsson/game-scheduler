(ns game-scheduler.core)
(require '[clojure.string :as str])
(require '[clojure.set :refer [intersection]])

; The group that has the least number of possible opponents gets to select it's opponent first.
; If two or more groups has the same number of possible opponents the first one gets to make the selection (everything is sorted/ordered)
; It will always select the first opponent in its list of possible opponents
; The solution is recursive and removes available groups and possible opponents as the loop progresses
; The structure of the data that runs in the loop looks just the valid-opponents-by-group

; So given ABCD as best thirds the possible-maches will look like this in the first run (in the loop)
; ((\A #{\C \D})
;  (\B #{\A \C \D}) 
;  (\C #{\A \B}) 
;  (\D #{\B}))
;
;
; Since D only can play B, that match is given.
; Group D and opponent B are removed from the data set and the loop recurs and possible-maches are:
; ((\A #{\C}) 
;  (\C #{\A \B}) 
;  (\D #{\B}))
; 
; Now both A and D has one possible opponent.
; In this case the first group \A selects its only possible opponent \C

; Given ACBE each group will have 2 possible opponents:
; ((\A #{\C \E})
;  (\B #{\A \C}) 
;  (\C #{\A \B}) 
;  (\D #{\B \E}))
;
; In this case the first group \A will select it's first posible opponent \C
; This process continues until all matches are made 

; The commands to produce the above data structures are:
; (possible-maches groups (set (seq "ABCD")))
; (possible-maches (disj groups \B) (set (seq "ABC")))


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
; (apply match-to-record {\B #{\D \C}})

(defn sort-accending-by-number-of-possible-opponents [matches]
  (sort-by #(count (:opponents (match-to-record %))) matches))

; (sort-accending-by-number-of-possible-opponents {\B #{\D \C} \C #{\A}})

; Removes groups that has no possible opponents left
(defn remove-empty-groups [groups]
  (filter #(not (empty? (last %))) groups))

; TODO: Should this be renamed to possible-groups
(defn possible-maches [groups best-thirds]
  (map
   #(list % (intersection (get valid-opponents-by-group %) best-thirds))
   groups))

; (possible-maches groups (set (seq "ABCD")))

; TODO: Should i rename match to game next-game
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
      ; Where done and return the result
      (map last (sort-by #(first %) result))
      ; Where not done, lets continue
      (let [group (:group next-match)
            third (first (:opponents next-match))]
        (create-schedule (disj groups group)
                         (disj best-thirds third)
                         (conj result (hash-map group third)))))))

(defn create-schedule-for-senario [best-thirds]
  (create-schedule groups (set (seq best-thirds)) []))

(println (create-schedule-for-senario "ABCD"))

; Manual test
(defn -main []
  (doall
   (for [senario senarios]
     (println senario "will play" (apply str (map last (create-schedule-for-senario senario)))))))

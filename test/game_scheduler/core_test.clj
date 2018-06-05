(ns game-scheduler.core-test
  (:require [clojure.test :refer :all]
            [game-scheduler.core :refer :all]))

(deftest count-opponents-test
  (let [expected 2]
    (is (=  (count-opponents '(\A #{\C \D}))
            expected))))

(deftest sort-by-opponents-count-test
  (let [expected '([\D #{\B}] [\C #{\A \B}] [\B #{\A \C \D}])]
    (is (= (sort-by-opponents-count {\B #{\A \C \D} \C #{\A \B} \D #{\B}})
           expected))))

(deftest possible-opponents-test
  (let [expected #{\D}]
    (is (= (possible-opponents \A #{\A \B \D})
           expected))))

(deftest possible-matches-test
  (let [expected '((\A #{\C \D}) (\B #{\A \C \D}) (\C #{\A \B}) (\D #{\B}))]
    (is (= (possible-matches groups (set (seq "ABCD")))
           expected))))

(deftest sort-ascending-by-number-of-possible-opponents-test
  (let [expected '([\C #{\A}] [\B #{\C \D}])]
    (is (= (sort-by-opponents-count {\B #{\D \C} \C #{\A}})
           expected))))

(deftest find-next-match-test
  (let [expected '(\D #{\B})]
    (is (= (find-next-match groups (set (seq "ABCD")))
           expected))))

(deftest scenario-cdef-test
  (let [expected "CDFE"]
    (is (= (create-schedule-for-scenario "CDEF")
           expected))))

(def scenarios-with-solution {"ABCD" "CDAB"
                              "ABCE" "CABE"
                              "ABCF" "CABF"
                              "ABDE" "DABE"
                              "ABDF" "DABF"
                              "ABEF" "EABF"
                              "ACDE" "CDAE"
                              "ACDF" "CDAF"
                              "ACEF" "CAFE"
                              "ADEF" "DAFE"
                              "BCDE" "CDBE"
                              "BCDF" "CDBF"
                              "BCEF" "ECBF"
                              "BDEF" "EDBF"
                              "CDEF" "CDFE"})

(deftest create-schedules-for-all-scenarios-test
  (doall
   (for [[scenario solution]  scenarios-with-solution]
     (do
       (is (= (create-schedule-for-scenario scenario)
              solution))))))

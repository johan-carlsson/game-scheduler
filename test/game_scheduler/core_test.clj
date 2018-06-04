(ns game-scheduler.core-test
  (:require [clojure.test :refer :all]
            [game-scheduler.core :refer :all]))

(deftest possible-maches-test
  (let [expected '((\A #{\C \D}) (\B #{\A \C \D}) (\C #{\A \B}) (\D #{\B}))]
    (is (= (possible-maches groups (set (seq "ABCD"))) expected))))

(deftest sort-ascending-by-number-of-possible-opponents-test
  (let [expected '([\C #{\A}] [\B #{\C \D}])]
    (is (= (sort-by-opponents-count {\B #{\D \C} \C #{\A}}) expected))))

(deftest remove-empty-groups-test
  (let [expected '([\A #{\B \C}])]
    (is (= (filter-empty {\A #{\B \C} \D #{}}) expected))))

(deftest find-next-match-test
  (let [expected '(\D #{\B})]
    (is (= (find-next-match groups (set (seq "ABCD"))) expected))))

(def senarios-with-solution {"ABCD" "CDAB"
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

(deftest create-schedules-for-all-senarios-test
  (doall
   (for [[senario solution]  senarios-with-solution]
     (do
       (is (= (create-schedule-for-senario senario))) solution))))

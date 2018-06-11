(ns game-scheduler.core-test
  (:require [clojure.test :refer :all]
            [game-scheduler.core :refer :all]))

(deftest count-opponents-test
  (is (=  2
          (count-opponents (->Match \A #{\C \D})))))

(deftest sort-by-opponents-count-test
  (is (= (map #(apply ->Match %)
              {\D #{\B} \C #{\A \B} \B #{\A \C \D}})
         (sort-by-opponents-count (map #(apply ->Match %) {\B #{\A \C \D} \C #{\A \B} \D #{\B}})))))

(deftest possible-opponents-test
  (is (= #{\D}
         (possible-opponents \A #{\A \B \D}))))

(deftest possible-matches-test
  (is (= (map #(apply ->Match %)
              {\A #{\C \D} \B #{\A \C \D} \C #{\A \B} \D #{\B}})
         (possible-matches groups (set (seq "ABCD"))))))

(deftest find-next-match-test
  (is (= (->Match \D #{\B})
         (find-next-match groups (set (seq "ABCD"))))))

(deftest scenario-cdef-test
  (is (= "CDFE"
         (create-schedule-for-scenario "CDEF"))))

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
       (is (= solution
              (create-schedule-for-scenario scenario)))))))

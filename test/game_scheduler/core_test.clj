(ns game-scheduler.core-test
  (:require [clojure.test :refer :all]
            [game-scheduler.core :refer :all]))

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

(deftest senarios-test
  (doall
   (for [[senario solution]  senarios-with-solution]
     (do
       (is (= (apply str (map last (create-schedule-for-senario senario))) solution))))))

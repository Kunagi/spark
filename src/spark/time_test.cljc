(ns spark.time-test
  (:require
   [tick.core :as tick]
   [spark.time :as sut]
   [clojure.test :refer [deftest testing is]]
   ))

(deftest test-date
  (is (= (tick/date "2023-01-01") (sut/date "2023-01-01")))
  (is (= (tick/date "2023-01-01") (tick/date "2023-01-01"))))

(deftest test-time
  (is (= (tick/time "09:23") (sut/time "09:23")))
  (is (= (tick/time "17:23") (sut/time "17:23")))
  #_(is (= "07:00" (str (tick/time "7:00")))))

(deftest test-date-time
  (is (= (tick/date-time "2023-01-01T09:23") (sut/date-time "2023-01-01T09:23")))
  (is (= (tick/date-time "2023-01-01T17:23") (sut/date-time "2023-01-01T17:23")))
  #_(is (= "07:00" (str (tick/time "7:00")))))

(deftest millis-test
  (is (nil? (sut/millis nil)))
  (is (= 1672585200000  (sut/millis (tick/date-time "2023-01-01T10:00:00"))))
  (is (= 1672585200000 (sut/millis "2023-01-01T10:00:00"))))

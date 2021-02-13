(ns simple-lang.core-test
  (:require [clojure.test :refer :all]
            [simple-lang.core :refer :all]))

(deftest Number-e
  (let [exp (->Number-e 5)]
    (testing "is not reducible"
      (is (= false (reducible? exp))))
    (testing "reducing yields the value"
      (is (= 5 (reduce-e exp {}))))))

(deftest Add-e
  (let [exp (->Add-e (->Number-e 7) (->Number-e 8))]
    (testing "is reducible"
      (is (= true (reducible? exp))))
    (testing "reducing yields correct number"
      (is (= (->Number-e (+ 7 8)) (reduce-e exp {}))))))

(deftest Mult-e
  (let [exp (->Mult-e (->Number-e 10) (->Number-e 11))]
    (testing "is reducible"
      (is (= true (reducible? exp))))
    (testing "reducing yields correct number"
      (is (= (->Number-e (* 10 11)) (reduce-e exp {}))))))

(deftest run-machine
  (testing "running gives expected result"
    (let [exp (->Add-e
               (->Add-e
                (->Number-e 1)
                (->Mult-e
                 (->Number-e 2)
                 (->Number-e 3)))
               (->Number-e 4))]
      (is (= (->Number-e 11) (run exp))))))
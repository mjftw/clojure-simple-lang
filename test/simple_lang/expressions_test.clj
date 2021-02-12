(ns simple-lang.expressions-test
  (:require [clojure.test :refer :all]
            [simple-lang.expressions :refer :all]))

(deftest Number-e
  (let [exp (->Number-e 5)]
    (testing "is not reducible"
      (is (= false (reducible? exp))))
    (testing "reducing yields the value"
      (is (= 5 (reduce-e exp))))))

(deftest Add-e
  (let [exp (->Add-e (->Number-e 7) (->Number-e 8))]
    (testing "is reducible"
      (is (= true (reducible? exp))))
    (testing "reducing yields correct number"
      (is (= (->Number-e (+ 7 8)) (reduce-e exp))))))

(deftest Mult-e
  (let [exp (->Mult-e (->Number-e 10) (->Number-e 11))]
    (testing "is reducible"
      (is (= true (reducible? exp))))
    (testing "reducing yields correct number"
      (is (= (->Number-e (* 10 11)) (reduce-e exp))))))

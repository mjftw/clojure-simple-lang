(ns simple-lang.core-test
  (:require [clojure.test :refer :all]
            [simple-lang.core :refer :all]))

(deftest Number-e
  (let [exp (->Number-e 5)]
    (testing "is not reducible"
      (is (= false (reducible? exp))))
    (testing "reducing yields the value"
      (is (= 5 (first (reduce-e exp {})))))))

(deftest Add-e
  (let [exp (->Add-e (->Number-e 7) (->Number-e 8))]
    (testing "is reducible"
      (is (= true (reducible? exp))))
    (testing "reducing yields correct number"
      (is (= (->Number-e (+ 7 8)) (first (reduce-e exp {})))))))

(deftest Mult-e
  (let [exp (->Mult-e (->Number-e 10) (->Number-e 11))]
    (testing "is reducible"
      (is (= true (reducible? exp))))
    (testing "reducing yields correct number"
      (is (= (->Number-e (* 10 11)) (first (reduce-e exp {})))))))

(deftest Assign-e
  (testing "causes env change"
    (let [num (->Number-e 42)]
      (= num (second (->Assign-e "foo" num))))))

(deftest Assign-e
  (testing "reduces then causes env change"
    (let [exp (->Mult-e (->Number-e 10) (->Number-e 11))]
      (= (->Number-e (* 10 11)) (second (->Assign-e "foo" exp))))))

(deftest run-machine
  (testing "running gives expected result"
    (let [exp (->Add-e
               (->Add-e
                (->Number-e 1)
                (->Mult-e
                 (->Number-e 2)
                 (->Number-e 3)))
               (->Number-e 4))]
      (is (= (->Number-e 11) (run exp)))))

  (testing "this causes an error - why?"
    (run
     (->If-e (->LessThan-e (->Variable-e "bar") (->Number-e 5))
             (->DoNothing-e)
             (->DoNothing-e))
     {"bar" (->Number-e 3)})))

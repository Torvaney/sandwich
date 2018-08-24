(ns sandwich.core-test
  (:require [clojure.test :refer :all]
            [sandwich.core :refer :all]))


;; Example objective functions

(defn- sq [x] (Math/pow x 2))

(defn- build-rosenbrock
  [a b]
  (fn [[x y]]
    (+ (sq (- a x))
       (* b (sq (- y (sq x)))))))

(defn- build-quadratic
  [a b c]
  (fn [[x]]
    (+ (* a (sq x))
       (* b x)
       c)))


;; Tests and helper fns

(defn- close? [tolerance x y]
  (< (Math/abs (- x y)) tolerance))

(defn- test-optim
  [f op x-true x0]
  (let [result (op f x0)
        x-est  (:x result)]
    (->> (mapv #(close? 0.001 %1 %2) x-true x-est)
         (every? true?))))

(deftest nelder-mead-test
  (testing "Nelder-Mead minimisation"
    (is (test-optim (build-rosenbrock 1 100) nelder-mead [1 1] [0     0]))
    (is (test-optim (build-rosenbrock 1 100) nelder-mead [1 1] [5    -1]))
    (is (test-optim (build-rosenbrock 1 100) nelder-mead [1 1] [15    2]))
    (is (test-optim (build-rosenbrock 1 100) nelder-mead [1 1] [-9   -9]))
    (is (test-optim (build-rosenbrock 1 100) nelder-mead [1 1] [5.2 1.2]))
    (is (test-optim (build-rosenbrock 1 100) nelder-mead [1 1] [10    2]))
    (is (test-optim (build-rosenbrock 1 100) nelder-mead [1 1] [-1   -1]))
    (is (test-optim (build-quadratic 1  0 0) nelder-mead [0]  [-1]))
    (is (test-optim (build-quadratic 3  0 8) nelder-mead [0]  [10]))
    (is (test-optim (build-quadratic 1 -4 0) nelder-mead [2]  [1000]))
    (is (test-optim (build-quadratic 1  4 3) nelder-mead [-2] [-18]))
    (is (test-optim (build-quadratic 5 10 1) nelder-mead [-1] [10]))
    (is (test-optim (build-quadratic 2  4 2) nelder-mead [-1] [-10]))
    (is (test-optim (build-quadratic 1 -4 1) nelder-mead [2]  [7.8]))))

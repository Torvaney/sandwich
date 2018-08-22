(ns sandwich.core-test
  (:require [clojure.test :refer :all]
            [sandwich.core :refer :all]))

(defn- sq [x] (Math/pow x 2))

(defn- build-rosenbrock
  [a b]
  (fn [[x y]]
    (+ (sq (- a x))
       (* b (sq (- y (sq x)))))))

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
    (is (test-optim (build-rosenbrock 1 100) nelder-mead [1 1] [-1   -1]))))

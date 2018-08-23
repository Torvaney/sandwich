(ns sandwich.core
  (:require [clojure.math.combinatorics]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as ms]))


;; Initialise simplex

(defn- find-b
  [c n]
  (* (/ c (* n (Math/sqrt 2)))
     (- (Math/sqrt (+ n 1)) 1)))

(defn- find-a
  [c b]
  (+ b (/ c (Math/sqrt 2))))

(defn- init-simplex
  "Initialise an equilateral n+1 dimensional simplex."
  [x]
  (let [c 1
        n (count x)
        b (find-b c n)
        a (find-a c b)]
    (->> (cons a (repeat (- n 1) b))
         clojure.math.combinatorics/permutations
         (mapv #(m/add x %))
         (cons (m/matrix x)))))


;; Simplex operations (finding new points)
; Arguments are named according to the following convention:
; xa: average of all points aside from the worst
; xb: best point
; xl: second worst ("lousy") point
; xw: worst point

(defn- reflect
  "Reflect simplex and return the reflected point."
  [p xa xw]
  (m/add xa (m/mul p (m/sub xa xw))))

(defn- expand
  "Expand simplex and return the expanded point."
  [p xr xa]
  (m/add xr (m/mul p (m/sub xr xa))))

(defn- contract-inside
  "Contract simplex inside and return the contracted point."
  [p xa xw]
  (m/sub xa (m/mul p (m/sub xa xw))))

(defn- contract-outside
  "Contract simplex outside and return the contracted point."
  [p xa xw]
  (m/add xa (m/mul p (m/sub xa xw))))


;; Evaluate the points on the simplex

(defn- eval-point
  "Evaluate a single point"
  [f xi]
  (assoc {} :x xi :f (f xi)))

(defn- eval-simplex
  "Evaluate a function at each point of a simplex."
  [f x]
  (mapv #(eval-point f %) x))


;; Update the simplex

(defn- shrink-point
  "Shrink a single point on the simplex."
  [f p xb xi]
  (->> (m/add xb (m/mul p (m/sub xi xb)))
       (eval-point f)))

(defn- shrink
  "Shrink simplex."
  [f p [xb & xs]]
  (->> (cons xb (map #(shrink-point f p (:x xb) (:x %)) xs))
       (sort-by :f)))

(defn- accept-point
  [sorted xi]
  (->> (cons xi (drop-last sorted))
       (sort-by :f)))

(defn- expand-and-update
  "Expand simplex and update accordingly"
  [f p xa xb xr sorted]
  (let [xe (eval-point f (expand p (:x xr) (:x xa)))]
    (cond
      (< (:f xr) (:f xe)) (accept-point sorted xr)
      (< (:f xe) (:f xb)) (accept-point sorted xe)
      true                (accept-point sorted xr))))

(defn- contract-inside-and-update
  "Contract simplex inside and update accordingly"
  [f p xa xw sorted]
  (let [xc (eval-point f (contract-inside p (:x xa) (:x xw)))]
    (if (< (:f xc) (:f xw))
      (accept-point sorted xc)
      (shrink f p sorted))))

(defn- contract-outside-and-update
  "Contract simplex inside and update accordingly"
  [f p xa xr xw sorted]
  (let [xc (eval-point f (contract-outside p (:x xa) (:x xw)))]
    (if (<= (:f xc) (:f xr))
      (accept-point sorted xc)
      (shrink f p sorted))))


;; Iterate

(defn- iter-simplex
  "Find the next iteration of the simplex."
  [f sorted]
  (let [xw     (last sorted)
        xl     (second (reverse sorted))
        xb     (first sorted)
        xa     (->> sorted drop-last (map :x) ms/mean m/matrix (eval-point f))
        xr     (eval-point f (reflect 1 (:x xa) (:x xw)))]
  (cond
    (<  (:f xr) (:f xb)) (expand-and-update f 1 xa xb xr sorted)
    (<= (:f xr) (:f xl)) (accept-point sorted xr)
    (>  (:f xr) (:f xw)) (contract-inside-and-update f 0.5 xa xw sorted)
    true                 (contract-outside-and-update f 0.5 xa xr xw sorted))))


;; Do the thing

(defn- converged?
  [tol sorted]
  (let [xb (first sorted)
        xw (last sorted)]
    (< (- (:f xw) (:f xb)) tol)))

(defn nelder-mead
  "Optimise f"
  [f x0]
  (let [x      (init-simplex x0)
        sorted (->> x (eval-simplex f) (sort-by :f))]
    (loop [s sorted]
      (if (converged? 0.00000001 s)
        (first s)
        (recur (iter-simplex f s))))))

(ns sandwich.genetic)

;; Single objective optimisation

; (defn- initialise-population
;   []
;   (...))


; (defn- determine-mating-pool
;   []
;   (...))


; (defn- generate-offspring
;   []
;   (...))


; (defn- mutation
;   []
;   (...))


; (defn- compute-offspring-fitness
;   []
;   (...))


; (defn- tournament
;   []
;   (...))


; (defn- identify-best-member
;   []
;   (...))


(defn optimise
  [f x0]
  (let [fitness (map #({:f (f %) :x %}) x0)]
    [1]))

(ns dnf.core
  (:gen-class)
  (:require [clojure.set :as cset]
            [clojure.test :refer [deftest testing is]])
)

;; boolean константы
(def True 
  "True boolean constant"
  (list :true)
)

(def False
  "False boolean constant"
  (list :false)
)

(defn True?
  "Check that expr is true boolean constant"
  [expr]
  (= (first expr) :true)
)

(defn False?
  "Check that expr is false boolean constant"
  [expr]
  (= (first expr) :false)
)

(defn const?
  "Check that expr is boolean constant"
  [expr]
  (or (True? expr) (False? expr))
)

;; Переменные
(defn Var
  "Variable definition"
  [varName]
  (list :v varName)  
)

(defn Var?
  "Check that expr is variable"
  [expr]
  (= (first expr) :v)  
)

(defn Var-name 
  "Extract variable name"
  [expr]
  (if (Var? expr)
    (second expr)
    nil
  )
)

;;Операции

(defn negation?
  "Check that expr is negation"
  [expr]
  (= (first expr) :neg)
)

(defn negation
  "Negation definition"
  [expr]
  (list :neg expr)
)

(defn conjuction 
  "Conjuction definition"
  [expr & rest]
  (cons :conj (cons expr rest))
)

(defn conjuction? 
  "Check that expr is conjuction"
  [expr]
  (= (first expr) :conj)
)

(defn disjunction
  "Disjunction definition"
  [expr & rest]
  (cons :disj (cons expr rest))
)

(defn disjunction?
   "Check that expr is disjunction"
  [expr]
  (= (first expr) :disj)
)

(defn var-const? 
  "Check that expr is variable or constant"
  [expr]
  (or (const? expr) (Var? expr))
)

(defn single-value-expr? 
  "Check that expr is single value"
  [expr]
  (or (var-const? expr) (and (negation? expr) (var-const? (second expr))))
)

(defn elementary-conjuction? 
  "Check that expr is elementary conjuction"
  [expr]
  (and (conjuction? expr) (every? single-value-expr? (rest expr))) 
)

(defn dnf?
  "Check that expr is dnf"
  [expr]
  (or 
   (single-value-expr? expr)
   (elementary-conjuction? expr)
   (and (disjunction? expr) (every? dnf? (rest expr)))
  )
)

(defn neg-conjuction?
  "Check that expr is neg(A*B)"  
  [expr]
  (and (negation? expr) (conjuction? (second expr)))
)

(defn neg-disjunction?
  "Check that expr is neg(A+B)"  
  [expr]
  (and (negation? expr) (disjunction? (second expr)))
)

(defn remove-double-neg
  "Remove double neg of expr. neg(neg(expr)) = expr"
  [expr]
  (if (and (negation? expr) (negation? (second expr)))
    (second (second expr))
    expr
  )
)

  
(defn de-morgan 
  "Transform expr following De Morgan rule"
  [expr]
  (cond
    (neg-conjuction? expr) ( let [conjuctArgs (rest (second expr))]
                     (apply disjunction (map negation conjuctArgs))
    )
    (neg-disjunction? expr) ( let [disjunctArgs (rest (second expr))]
                     (apply conjuction (map negation disjunctArgs))
    )
    :else expr  
  )
)


(deftest true-check-test
  (is (True? True))
  (is (not (True? False)))
)

(deftest false-check-test
  (is (False? False))
  (is (not (False? True)))
)

(deftest const-check-test
  (is (const? False))
  (is (const? True))
  (is (not (const? (Var :a))))
)

(deftest var-test
  (let [varA (Var :a)]
    (is (= :v (first varA)))
    (is (= :a (second varA)))
  )
)


(deftest var-check-test
  (let [varA (Var :a)]
    (is (Var? varA))
    (is (not (Var? True)))
  )
)

(deftest var-name-test
  (let [varA (Var :a)]
    (is (= :a (Var-name varA)))
    (is (nil? (Var-name True)))
  )
)

(deftest negation-test
  (let [
        varA (Var :a)
        negVar (negation varA)
        ]
    (is (= :neg (first negVar)))
    (is (= varA (second negVar)))
  )
)

(deftest negation-check-test
  (let [
        varA (Var :a)
        negVar (negation varA)
        ]
    (is (negation? negVar))
    (is (not (negation? varA)))
  )
)

(deftest conjuction-test
  (let [
        varA (Var :a)
        conjuct (conjuction varA True)
        ]
    (is (= :conj (first conjuct)))
    (is (= varA (second conjuct)))
    (is (True? (last conjuct)))
  )
)

(deftest conjuction-check-test
  (let [
        varA (Var :a)
        conjuct (conjuction varA True)
        ]
    (is (conjuction? conjuct))
    (is (not (conjuction? varA)))
    (is (not (conjuction? (negation conjuct))))
  )
)

(deftest disjunction-test
  (let [
        varA (Var :a)
        disjunct (disjunction varA True)
        ]
    (is (= :disj (first disjunct)))
    (is (= varA (second disjunct)))
    (is (True? (last disjunct)))
  )
)

(deftest disjunction-check-test
  (let [
        varA (Var :a)
        disjunct (disjunction varA True)
        ]
    (is (disjunction? disjunct))
    (is (not (disjunction? varA)))
    (is (not (disjunction? (negation disjunct))))
  )
)

(deftest var-const-check-test
  (is (var-const? False))
  (is (var-const? True))
  (is (var-const? (Var :a)))
  (is (not (var-const? (conjuction False True))))
)

(deftest single-value-expr-check-test
  (is (single-value-expr? False))
  (is (single-value-expr? True))
  (is (single-value-expr? (Var :a)))
  (is (single-value-expr? (negation False)))
  (is (single-value-expr? (negation True)))
  (is (single-value-expr? (negation (Var :a))))
  (is (not (single-value-expr? (conjuction False True))))
)

(deftest elementary-conjuction-check-test
  (is (elementary-conjuction? (conjuction True (Var :a))))
  (is (elementary-conjuction? (conjuction True (negation (Var :a)))))
  (is (elementary-conjuction? (conjuction True (negation (Var :a)) (Var :b) False)))
  (is (not (elementary-conjuction? False)))
  (is (not (elementary-conjuction? (Var :a))))
  (is (not (elementary-conjuction? (disjunction True (negation (Var :a))))))
  (is (not (elementary-conjuction? (conjuction (conjuction True (Var :b)) (negation (Var :a))))))
)

(deftest dnf-check-test
  (is (dnf? (conjuction True (Var :a))))
  (is (dnf? (conjuction True (negation (Var :a)))))
  (is (dnf? (conjuction True (negation (Var :a)) (Var :b) False)))
  (is (dnf? False))
  (is (dnf? (Var :a)))
  (is (dnf? (negation (Var :a))))
  (is (dnf? (disjunction (conjuction True (Var :a)) (Var :b))))
  (is (dnf? (disjunction (conjuction True (Var :a)) (Var :b) (disjunction True (Var :c)))))
  (is (not (dnf? (conjuction (conjuction True (Var :b)) (negation (Var :a))))))
  (is (not (dnf? (negation (disjunction (conjuction True (Var :a)) (Var :b))))))
)

(deftest neg-conjuction-check-test
  (is (neg-conjuction? (negation (conjuction True (Var :a)))))
  (is (not (neg-conjuction? (conjuction True (negation (Var :a))))))
  (is (not (neg-conjuction? (disjunction (conjuction True (Var :a)) (Var :b)))))
  (is (not (neg-conjuction? (negation (disjunction(Var :b) (Var :c))))))
)

(deftest neg-disjunction-check-test
  (is (neg-disjunction? (negation (disjunction True (Var :a)))))
  (is (not (neg-disjunction? (disjunction True (negation (Var :a))))))
  (is (not (neg-disjunction? (conjuction (conjuction True (Var :a)) (Var :b)))))
  (is (not (neg-disjunction? (negation (conjuction(Var :b) (Var :c))))))
)

(deftest remove-double-neg-test
  (let [
        varA (Var :a)
        negVarA (negation varA)
        ]
    (is (= varA (remove-double-neg (negation negVarA))))
    (is (= varA (remove-double-neg varA)))
    (is (= negVarA (remove-double-neg negVarA)))
  )
)

(deftest de-morgan-test
  (let [
        vA (Var :a)
        vB (Var :b)
        vC (Var :c)
        vD (Var :D)
        conjuct (conjuction vA vB vC vD)
        disjunct (disjunction vA vB vC vD)
        nA (negation vA)
        nB (negation vB)
        nC (negation vC)
        nD (negation vD)
        ]
    (is (= vA (de-morgan vA)))
    (is (= nA (de-morgan nA)))
    (is (= conjuct (de-morgan conjuct)))
    (is (= disjunct (de-morgan disjunct)))
    (is (= (conjuction nA nB nC nD) (de-morgan (negation disjunct))))
    (is (= (disjunction nA nB nC nD) (de-morgan (negation conjuct))))
  )     
)

(true-check-test)
(false-check-test)
(const-check-test)
(var-test)
(var-check-test)
(var-name-test)
(negation-test)
(negation-check-test)
(conjuction-test)
(conjuction-check-test)
(disjunction-test)
(disjunction-check-test)
(var-const-check-test)
(single-value-expr-check-test)
(elementary-conjuction-check-test)
(dnf-check-test)
(neg-conjuction-check-test)
(neg-disjunction-check-test)
(remove-double-neg-test)
(de-morgan-test)
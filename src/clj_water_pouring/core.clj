;;;; Water pouring problem at the REPL
(ns clj-water-pouring.core
  (:require [clojure.pprint :refer [pprint]]))

;;; I start at the REPL by declaring inital-state

(def initial-state
  [{:capacity 5
    :current 0}
   {:capacity 3
    :current 0}
   ])

;;; Then final-state

(def final-state
  [{:capacity 5
    :current 4}
   {:capacity 3
    :current 0}
   ])

;;; I start to implement pour and fill functions

(defn pour
  ([glass]
   (assoc glass :current 0))
  ([glass quantity]
   (let [{:keys [current]} glass]
     (assoc glass :current (- current
                              (min current quantity))))))
;; Test pour
;(pour {:capacity 5 :current 4})
;(pour {:capacity 8 :current 4} 2)
;(pour {:capacity 8 :current 4} 5)

(defn fill
  ([glass]
   (assoc glass :current (:capacity glass)))
  ([glass quantity]
   (let [{:keys [capacity current]} glass]
     (assoc glass :current (+ current
                              (min (- capacity current) quantity))))))

;; Test `fill`
;(fill {:capacity 5 :current 0})
;(fill {:capacity 8 :current 4} 1)
;(fill {:capacity 8 :current 7} 2)

;;; Now I'd need a function that takes a collection of glasses (state),
;;; apply an action (commonly called a 'move' in the current domain language)
;;; and return a new, updated collection of glasses (new state).

;;; I'm beginning to think that a bit of *Ad-hoc polymorphism* would
;;; be great in order to dispatch on the type of 'move': let's use
;;; *multi-methods*.

(defmulti ->move
  "Apply a move to the given state and return the new `state`."
  (fn [state move] (:type move)))

(defmethod ->move :empty
  [state {:keys [from]}]
  (update-in state [from] pour))

(defmethod ->move :fill
  [state {:keys [to]}]
  (update-in state [to] fill))

(defmethod ->move :pour
  [state {:keys [from to]}]
  (let [quantity (min (get-in state [from :current])
                      (- (get-in state [to :capacity])
                         (get-in state [to :current])))]
    (-> state
        (update-in [from] pour quantity)
        (update-in [to] fill quantity))))

;; Test `->move`

;```(-> initial-state
;        (->move {:type :fill :to 1})```
;        (->move {:type :pour :from 1 :to 0})
;        (->move {:type :fill :to 1})
;        (->move {:type :pour :from 1 :to 0})
;        (->move {:type :empty :from 0})
;        (->move {:type :pour :from 1 :to 0})
;        (->move {:type :fill :to 1})
;        (->move {:type :pour :from 1 :to 0})
;        )
;

;;; I must now implement a function that when given a collection of
;;; glasses will return a collection of possible moves that,
;;; when applied to the collection of glasses in input, would return a
;;; meaningful new collection of glasses.

;;; Meaningful as in:
;;; * not emptying an empty glass,
;;; * not filling a glass that is full, and
;;; * not pouring into 'itself', which would lead to the input

(defn glasses->index
  "Filter the `glasses` by the `filter-fn` function and
  return the index the filtered item had in the
  `glasses` collection."
  [glasses filter-fn]
  (->> glasses
       (map-indexed #(vector %1 %2))
       (filter (fn [[idx value]] (filter-fn value)))
       (map first)))

;; Test `glasses->index`

;(glasses->index final-state (comp pos? :current))

;(glasses->index final-state (comp #(= 3 %) :capacity))

(defn available-moves
  "Return the list of valid moves from current state of glasses."
  [glasses]
  (let [non-empty (glasses->index glasses (comp pos? :current))
        non-full (glasses->index glasses #(< (:current %) (:capacity %)))]
    (concat
      (map #(hash-map :type :empty :from %) non-empty)
      (map #(hash-map :type :fill  :to   %) non-full)
      ; Oh so beautiful cartesian product
      (for [from non-empty to non-full :when (not= from to)]
        {:type :pour :from from :to to}))))

;; Test `available-moves`

;(available-moves initial-state)
;(available-moves final-state)

;;; Explore the next glasses available from the current glasses.

(defn explore
  [glasses moves]
  (map (partial ->move glasses) moves))

;; Test `explore`

;(explore initial-state (available-moves initial-state))

;;; Given a node containing glasses and the sequence of moves leading to
;;; them, return a list of successor nodes.

(defn expand
  [{:keys [glasses moves] :as node}]
  (let [next-moves (available-moves glasses)]
    (->> next-moves
         (map #(hash-map :glasses (->move glasses %)
                         :moves (conj moves %))))))

;; Test `expand`

;(expand
;  {:glasses [{:capacity 5 :current 5} {:capacity 3 :current 0}]
;   :moves [{:type :fill :to 0}]})

;;; Backtract already visited nodes

(defn backtrack
  "Returns true if a node has been visited."
  [visited {:keys [glasses]}]
  (not (contains? visited glasses)))

;; Test `backtrack`

;(backtrack #{[{:capacity 5 :current 0} {:capacity 3 :current 0}]}
;           {:glasses [{:capacity 5 :current 5} {:capacity 3 :current 0}]
;            :moves [{:type :fill :to 0}]})

;;; Identify a solution among a set of candidates

(defn has-solution?
  [target successors]
  (some #(when (= target (:glasses %)) (:moves %))
        successors))

;; Test `has-solution?`

;(has-solution?
;  [{:capacity 5 :current 4} {:capacity 3 :current 0}]
;  [])
;(has-solution?
;  [{:capacity 5 :current 4} {:capacity 3 :current 0}]
;  [{:glasses [{:capacity 5 :current 0}] :moves []}
;   {:glasses [{:capacity 5 :current 4} {:capacity 3 :current 0}] :moves [:foo :bar]}])

;;; Loop over glasses nodes, keeping track of the already visited ones,
;;; searching for a solution leading to the targetted state.

(require '[clojure.set :as set])

(defn solver
  [target]
  (let [has-solution-fn? (partial has-solution? target)]
    (fn [glasses]
      (loop [visited #{} nodes [{:glasses glasses :moves []}]]
        (if-let [solution (has-solution-fn? nodes)]
          solution
          (let [successors (mapcat #(expand %) nodes)
                valid-successors (filter (partial backtrack visited) successors)]
            (recur (clojure.set/union visited (into #{} (map :glasses valid-successors)))
                   valid-successors)))))))

;; Test `solver`

;(def simple-solver (solver [{:capacity 5 :current 4} {:capacity 3 :current 0}]))
;(simple-solver [{:capacity 5 :current 0} {:capacity 3 :current 0}])
;(def less-simple-solver (solver [{:capacity 8 :current 4} {:capacity 5 :current 0} {:capacity 3 :current 0}]))
;(pprint (less-simple-solver #{} [{:capacity 8 :current 0} {:capacity 5 :current 0} {:capacity 3 :current 0}]))

;;; Refactor

;;; Refactor `->move` fn to better reflect domain

(defmulti ->move
  "Apply a move to the given state and return the new state."
  (fn [glasses move] (:type move)))

(defmethod ->move :empty
  [glasses {:keys [from]}]
  (update-in glasses [from] pour))

(defmethod ->move :fill
  [glasses {:keys [to]}]
  (update-in glasses [to] fill))

(defmethod ->move :pour
  [glasses {:keys [from to]}]
  (let [quantity (min (get-in glasses [from :current])
                      (- (get-in glasses [to :capacity])
                         (get-in glasses [to :current])))]
    (-> glasses
        (update-in [from] pour quantity)
        (update-in [to] fill quantity))))

;;; Refactor `expand` by extracting the node builder

(defn make-node
  [glasses moves]
  {:glasses glasses
   :moves moves})

(defn expand-node
  [{:keys [glasses moves]}]
  (let [next-moves (available-moves glasses)]
    (->> next-moves
         (map #(make-node (->move glasses %) (conj moves %)))
         ;(filter #(not= glasses %))
         )))

;;; Refactor `solver` by decomposing functions

(defn make-glass
  ([capacity]
   (make-glass capacity 0))
  ([capacity current]
   {:capacity capacity :current current}))

;; Test `make-glass``

;(make-glass 5)
;(make-glass 8 3)

(defn initialize
  ([capacities]
   (vec (map make-glass capacities)))
  ([capacities quantities]
   (vec (map make-glass capacities quantities))))

;; Test `initialize``

;(initialize [8 5 3])
;(initialize [8 5 3] [4 0 0])

(defn find-successors
  [nodes]
  (mapcat #(expand-node %) nodes))

;; Test `find-successors`

;(find-successors [{:glasses [{:capacity 5 :current 0} {:capacity 3 :current 0}] :moves []}])

(defn filter-successors
  [successors visited]
  (filter (partial backtrack visited) successors))

;; Test `filter-successors`

;(filter-successors
;  [{:moves [{:type :fill :to 0}] :glasses [{:capacity 5, :current 5} {:capacity 3, :current 0}]}
;   {:moves [{:type :fill, :to 1}], :glasses [{:capacity 5, :current 0} {:capacity 3, :current 3}]}]
;  #{[{:capacity 5, :current 5} {:capacity 3, :current 0}]})

(defn distinct-glasses
  [successors]
  (into #{} (map :glasses successors)))

;; Test `distinct-glasses``

;(distinct-glasses
;  [{:moves [{:type :fill :to 0}] :glasses [{:capacity 5, :current 5} {:capacity 3, :current 0}]}
;   {:moves [{:type :fill, :to 1}], :glasses [{:capacity 5, :current 5} {:capacity 3, :current 0}]}])

(defn solver
  [capacities quantities]
  (let [initial (initialize capacities)
        target (initialize capacities quantities)
        has-solution-fn? (partial has-solution? target)]
    (fn []
      (let [first-node (make-node initial [])]
        (loop [visited #{initial} nodes [first-node]]
          (if-let [solution (has-solution-fn? nodes)]
            solution
            (let [successors (find-successors nodes)
                  valid-successors (filter-successors successors visited)
                  unique-glasses (distinct-glasses valid-successors)]
              (recur (clojure.set/union visited unique-glasses)
                     valid-successors))))))))

;; Test `solver`

(def simple-solver (solver [5 3] [4 0]))
(pprint (simple-solver))
;(def less-simple-solver (solver [8 5 3] [4 0 0]))
;(pprint (less-simple-solver))

;;; Implement `-main`

(defn -main
  [& args]
  (assert (even? (count args)))
  (let [input (map #(Integer. (re-find  #"\d+" %)) args)
        solver-fn (apply solver (split-at (/ (count input) 2) input))]
    (pprint (solver-fn))))

(ns clj-water-pouring.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]))

(defn pour
  ([glass]
   (assoc glass :current 0))
  ([glass quantity]
   (let [{:keys [current]} glass]
     (assoc glass :current (- current
                              (min current quantity))))))

(defn fill
  ([glass]
   (assoc glass :current (:capacity glass)))
  ([glass quantity]
   (let [{:keys [capacity current]} glass]
     (assoc glass :current (+ current
                              (min (- capacity current) quantity))))))

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

(defn glasses->index
  "Filter the `glasses` by the `filter-fn` function and
  return the index the filtered item had in the
  `glasses` collection."
  [glasses filter-fn]
  (->> glasses
       (map-indexed #(vector %1 %2))
       (filter (fn [[idx value]] (filter-fn value)))
       (map first)))

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

(defn backtrack
  "Returns true if a node has been visited."
  [visited {:keys [glasses]}]
  (not (contains? visited glasses)))

(defn has-solution?
  [target successors]
  (some #(when (= target (:glasses %)) (:moves %))
        successors))

(defn make-node
  [glasses moves]
  {:glasses glasses
   :moves moves})

(defn expand-node
  [{:keys [glasses moves]}]
  (let [next-moves (available-moves glasses)]
    (->> next-moves
         (map #(make-node (->move glasses %) (conj moves %)))
         )))

(defn make-glass
  ([capacity]
   (make-glass capacity 0))
  ([capacity current]
   {:capacity capacity :current current}))

(defn initialize
  ([capacities]
   (vec (map make-glass capacities)))
  ([capacities quantities]
   (vec (map make-glass capacities quantities))))

(defn find-successors
  [nodes]
  (mapcat #(expand-node %) nodes))

(defn filter-successors
  [successors visited]
  (filter (partial backtrack visited) successors))

(defn distinct-glasses
  [successors]
  (into #{} (map :glasses successors)))

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

(defn -main
  [& args]
  (assert (even? (count args)))
  (let [input (map #(Integer. (re-find  #"\d+" %)) args)
        solver-fn (apply solver (split-at (/ (count input) 2) input))]
    (pprint (solver-fn))))

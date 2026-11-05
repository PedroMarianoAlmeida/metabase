(ns metabase.serialization.protocol.util
  "Helpers that implementations of [[metabase.serialization.protocol/Store]] share."
  (:require
   [metabase.serialization.spec :as serialization.spec]
   [metabase.serialization.util :as serialization.util]))

(set! *warn-on-reflection* true)

(defn with-keys
  "`columns` with `ks` first, or nil for every column when `columns` is nil."
  [columns ks]
  (when columns
    (into [] (distinct) (concat ks columns))))

(defn in-batches
  "The rows `f` returns for each batch of the distinct `xs`."
  [f xs]
  (into [] (comp (distinct) (partition-all serialization.util/batch-size) (mapcat #(f (vec %)))) xs))

(defn identity-tuples
  "The distinct `identities` of `model` as vectors, leaving out those missing their first column, which match no row."
  [identities]
  (into [] (comp (map vec) (filter #(some? (first %))) (distinct)) identities))

(defn identity-filters
  "The `[column values]` of each identity column of `model` that a query for `tuples` filters on: the first one always,
  and each other one whose values are all strings or integers."
  [model tuples]
  (into []
        (keep-indexed (fn [i k]
                        (let [vs (into [] (comp (map #(nth % i)) (distinct)) tuples)]
                          (when (or (zero? i) (every? #(or (string? %) (int? %)) vs))
                            [k vs]))))
        (serialization.spec/identity-columns model)))

(defn matching-identities
  "The `rows` of `model` whose identity columns equal one of `tuples`, oldest first."
  [model tuples rows]
  (let [tuple  (apply juxt (serialization.spec/identity-columns model))
        wanted (set tuples)
        pk     (serialization.spec/primary-key model)]
    (->> rows
         (filter #(contains? wanted (tuple %)))
         (sort-by #(get % pk)))))

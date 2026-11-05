(ns metabase.serialization.parameters
  "Export and import of the `:parameters` and `:parameter_mappings` of Cards, Dashboards and DashboardCards, in bulk."
  (:require
   [medley.core :as m]
   [metabase.serialization.mbql :as mbql]
   [metabase.serialization.resolve :as resolve]))

(set! *warn-on-reflection* true)

(defn walk-export-parameter-mappings
  "The :parameter_mappings field of a `Card` or `DashboardCard`, as a vector of maps, with its ids replaced by what
  `visit` returns for their references, sorted by :parameter_id for stable serialization output."
  [visit mappings]
  (->> mappings
       (sort-by :parameter_id)
       (mapv #(mbql/walk-export visit %))))

(defn walk-import-parameter-mappings
  "The exported :parameter_mappings field with its portable references replaced by what `visit` returns for them."
  [visit mappings]
  (mapv #(-> (mbql/walk-import visit %)
             (m/update-existing :card_id (fn [eid] (resolve/visit-some visit [:fk :model/Card eid]))))
        mappings))

(defn- walk-export-parameter
  "A single parameter with its ids replaced by what `visit` returns for their references. A values source pointing at
  a Card that no longer exists has no portable id, so the source is dropped and the parameter falls back to its
  connected fields — the same shape the app produces when the source Card is archived."
  [visit parameter]
  (if (get-in parameter [:values_source_config :card_id])
    (or (resolve/found-or-nil #(mbql/walk-export visit parameter))
        (mbql/walk-export visit (dissoc parameter :values_source_type :values_source_config)))
    (mbql/walk-export visit parameter)))

(defn walk-export-parameters
  "The :parameters field of a `Card` or `Dashboard` with its ids replaced by what `visit` returns for their
  references, sorted by `:id` (a nil `:id` first) with a `:position` added to preserve display order."
  [visit parameters]
  (->> parameters
       (map-indexed (fn [i p] (assoc p :position i)))
       (sort-by :id)
       (mapv #(walk-export-parameter visit %))))

(defn walk-import-parameters
  "The exported :parameters field with its portable references replaced by what `visit` returns for them, sorted by
  `:position` when every parameter has one."
  [visit parameters]
  (let [params (mapv #(-> (mbql/walk-import visit %)
                          (m/update-existing-in [:values_source_config :card_id]
                                                (fn [eid] (resolve/visit-some visit [:fk :model/Card eid]))))
                     parameters)]
    (if (every? :position params)
      (sort-by :position params)
      params)))

(defn export-parameter-mappings
  "Each of the `:parameter_mappings` lists of `xs` exported by [[walk-export-parameter-mappings]], with the references of
  all of them resolved by `resolver` at once."
  [resolver xs]
  (resolve/export-in-bulk resolver walk-export-parameter-mappings xs))

(defn import-parameter-mappings
  "Each of the exported `:parameter_mappings` lists of `xs` imported by [[walk-import-parameter-mappings]], with the
  references of all of them resolved by `resolver` at once."
  [resolver xs]
  (resolve/import-in-bulk resolver walk-import-parameter-mappings xs))

(defn export-parameters
  "Each of the `:parameters` lists of `xs` exported by [[walk-export-parameters]], with the references of all of them
  resolved by `resolver` at once."
  [resolver xs]
  (resolve/export-in-bulk resolver walk-export-parameters xs))

(defn import-parameters
  "Each of the exported `:parameters` lists of `xs` imported by [[walk-import-parameters]], with the references of all
  of them resolved by `resolver` at once."
  [resolver xs]
  (resolve/import-in-bulk resolver walk-import-parameters xs))

(def parameter-mappings-transform
  "A spec transform of `:parameter_mappings` lists, by [[export-parameter-mappings]] and [[import-parameter-mappings]]."
  {:export (fn [resolver _ xs] (export-parameter-mappings resolver xs))
   :import (fn [resolver _ xs] (import-parameter-mappings resolver xs))})

(def parameters-transform
  "A spec transform of `:parameters` lists, by [[export-parameters]] and [[import-parameters]]."
  {:export (fn [resolver _ xs] (export-parameters resolver xs))
   :import (fn [resolver _ xs] (import-parameters resolver xs))})

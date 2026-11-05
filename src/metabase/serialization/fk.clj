(ns metabase.serialization.fk
  "Bulk translation of foreign keys for spec transforms. Each fn takes a resolver and the values of a column for a
  batch, returns the translated values in the same order, keeps a nil value nil, and throws a
  [[metabase.serialization.resolve/target-not-found]] error for a reference with no target, unless it says otherwise."
  (:require
   [metabase.serialization.resolve :as resolve]))

(set! *warn-on-reflection* true)

(defn- translate
  "The values of `refs`, references or nils, resolved by `resolve-refs` at once."
  [resolve-refs refs]
  (let [resolved (resolve-refs (remove nil? refs))]
    (mapv #(some->> % (resolve/ref-value resolved)) refs)))

(defn- export-values [resolver ->ref xs]
  (translate #(resolve/export-refs resolver %) (mapv #(some-> % ->ref) xs)))

(defn- import-values [resolver ->ref xs]
  (translate #(resolve/import-refs resolver %) (mapv #(some-> % ->ref) xs)))

(defn export-fks
  "The portable ids of the `model` rows with `ids`."
  [model resolver ids]
  (export-values resolver #(vector :fk model %) ids))

(defn export-fks-keyed
  "The `field` values of the `model` rows with `ids`, nil for a missing row."
  [model resolver field ids]
  (export-values resolver #(vector :fk-keyed model field %) ids))

(defn export-users
  "The emails of the Users with `ids`, nil for a missing User."
  [resolver ids]
  (export-fks-keyed :model/User resolver :email ids))

(defn export-databases
  "The names of the Databases with `ids`, nil for a missing Database."
  [resolver ids]
  (export-fks-keyed :model/Database resolver :name ids))

(defn export-tables
  "The `[db-name schema table-name]` references of the Tables with `ids`."
  [resolver ids]
  (export-values resolver #(vector :table %) ids))

(defn export-fields
  "The `[db-name schema table-name & field-names]` references of the Fields with `ids`."
  [resolver ids]
  (export-values resolver #(vector :field %) ids))

(defn import-fks
  "The primary keys of the `model` rows with portable ids `eids`."
  [model resolver eids]
  (import-values resolver #(vector :fk model %) eids))

(defn import-users
  "The ids of the Users with `emails`, created as inactive Users when missing."
  [resolver emails]
  (import-values resolver #(vector :user %) emails))

(defn import-databases
  "The ids of the Databases named `db-names`, created as stubs when missing."
  [resolver db-names]
  (import-values resolver #(vector :database %) db-names))

(defn import-tables
  "The ids of the Tables at `[db-name schema table-name]` `paths`, created as inactive Tables when missing."
  [resolver paths]
  (import-values resolver #(vector :table %) paths))

(defn import-fields
  "The ids of the Fields at `[db-name schema table-name & field-names]` `paths`, created as inactive Fields when
  missing."
  [resolver paths]
  (import-values resolver #(vector :field %) paths))

(defn fk-transform
  "A spec transform between the ids of `model` rows and their portable ids."
  [model]
  {:export (fn [resolver _ ids] (export-fks model resolver ids))
   :import (fn [resolver _ eids] (import-fks model resolver eids))})

(def user-transform
  "A spec transform between User ids and emails."
  {:export (fn [resolver _ ids] (export-users resolver ids))
   :import (fn [resolver _ emails] (import-users resolver emails))})

(def database-transform
  "A spec transform between Database ids and names."
  {:export (fn [resolver _ ids] (export-databases resolver ids))
   :import (fn [resolver _ db-names] (import-databases resolver db-names))})

(def table-transform
  "A spec transform between Table ids and their `[db-name schema table-name]` references."
  {:export (fn [resolver _ ids] (export-tables resolver ids))
   :import (fn [resolver _ paths] (import-tables resolver paths))})

(def field-transform
  "A spec transform between Field ids and their `[db-name schema table-name & field-names]` references."
  {:export (fn [resolver _ ids] (export-fields resolver ids))
   :import (fn [resolver _ paths] (import-fields resolver paths))})

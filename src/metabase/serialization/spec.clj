(ns metabase.serialization.spec
  "The serialization spec of a model, and applying it to a batch of entities in either direction.

  A spec is a map of:

  - `:schema`, the schema of an exported entity; dev and test check every export against it.
  - `:copy`, the columns exported and imported as they are.
  - `:skip`, the columns that are not serialized (`:id`, `:created_at` and `:updated_at` never are).
  - `:transform`, a map from a column to its transform, `{:export f :import g}`, where `f` and `g` are fns of a
    resolver, the batch of entities, and the column's values in that batch, returning the transformed values in the
    same order. A value of `::skip` leaves the key out. An `:as` key exports the column under another key. A transform
    with `:owned [model owner-key]` in place of `:import` exports the entities of `model` its owner owns, sorted by its
    `:order` key fn unless it has an `:export`, and a load of the owner loads them, matching them on their `:identity`
    and deleting the rows of the owner none matches.
  - `:defaults`, the non-nil values a column is left out of the export with.
  - `:identity`, the columns of a deserialized row that find the local row it updates, `[:entity_id]` by default.
  - `:primary-key`, the primary key column of the model, `:id` by default."
  (:require
   [metabase.config.core :as config]
   [metabase.lib.core :as lib]
   [metabase.util.date-2 :as u.date]
   [metabase.util.malli.humanize :as mu.humanize]
   [metabase.util.malli.registry :as mr]
   [toucan2.model :as t2.model]))

(set! *warn-on-reflection* true)

(def skip
  "A transformed value that leaves its key out."
  ::skip)

(defn skip-all
  "A transform fn that leaves the column out."
  [_resolver _entities values]
  (mapv (constantly ::skip) values))

(defn unchanged
  "A transform fn that keeps the values as they are."
  [_resolver _entities values]
  (vec values))

(defn export-dates
  "A transform fn that formats temporal values in the stable serialization format."
  [_resolver _entities values]
  (mapv #(some-> % u.date/format) values))

(defn import-dates
  "A transform fn that parses formatted temporal values back."
  [_resolver _entities values]
  (mapv #(cond-> % (string? %) u.date/parse) values))

(defn export-keywords
  "A transform fn that names keyword values."
  [_resolver _entities values]
  (mapv #(some-> % name) values))

(defn import-keywords
  "A transform fn that keywordizes named values, so comparisons in model hooks see the values they store."
  [_resolver _entities values]
  (mapv #(some-> % keyword) values))

(def import-only
  "A transform of a column that is imported as it is but never exported."
  {:export skip-all :import unchanged})

(def date-transform
  "A transform of temporal values, by [[export-dates]] and [[import-dates]]."
  {:export export-dates :import import-dates})

(def keyword-transform
  "A transform of keyword values, by [[export-keywords]] and [[import-keywords]]."
  {:export export-keywords :import import-keywords})

(defmulti spec
  "The serialization spec of `model`, or nil when the model is not serialized."
  {:arglists '([model])}
  identity)

(defmethod spec :default [model]
  (when (and (qualified-keyword? model) (= "model" (namespace model)))
    (try
      (t2.model/resolve-model model)
      (catch Exception _ nil))
    (when-not (= (get-method spec model) (get-method spec :default))
      (spec model))))

(defn spec-of
  "The serialization spec of `model`, throwing when it has none."
  [model]
  (or (spec model)
      (throw (ex-info (str "No serialization spec defined for model " model) {:model model}))))

(def ^:private unserialized-identities
  "The `:identity` columns of the models that references name but serialization does not export."
  {:model/User [:email]})

(defn identity-columns
  "The columns of a row of `model` that find the local row it matches, in spec order."
  [model]
  (or (:identity (spec model)) (get unserialized-identities model) [:entity_id]))

(defn check-identity!
  "Throws unless the `:identity` columns of `model` are `columns`, the ones a lookup of it matches on."
  [model columns]
  (when-not (= (vec columns) (identity-columns model))
    (throw (ex-info (format "%s is not identified by %s" model (pr-str columns))
                    {:model model :columns columns :identity (identity-columns model)}))))

(defn primary-key
  "The primary key column of `model`."
  [model]
  (or (:primary-key (spec model)) :id))

(defn owners
  "The `[owner-model owner-key]` of each `:owned` transform of a spec naming `model`."
  [model]
  (for [[owner-model f] (dissoc (methods spec) :default)
        [_ {[owned-model owner-key] :owned}] (:transform (f owner-model))
        :when (= owned-model model)]
    [owner-model owner-key]))

(defn owner-key
  "The key of a row of `model` to its owner, that of the one `:owned` transform naming `model`."
  [model]
  (let [[[_ k] & more] (owners model)]
    (when (or (nil? k) more)
      (throw (ex-info (format "Expected one :owned transform naming %s, found %d" model (count (owners model)))
                      {:model model})))
    k))

(defn columns
  "The columns an export of `model` selects: its primary key and the columns its spec copies or transforms, except
  those holding owned entities."
  [model]
  (let [spec (spec-of model)]
    (into [] (distinct) (concat [(primary-key model)]
                                (:copy spec)
                                (keep (fn [[k t]] (when-not (:owned t) k)) (:transform spec))))))

(defn validate-export!
  "In dev and test, throws when `exported`, an entity of `model` exported with `spec`, does not match its schema."
  [model spec exported]
  (when (or config/is-dev? config/is-test?)
    (let [schema (or (:schema spec)
                     (throw (ex-info (format "No :schema in the serialization spec of %s" model)
                                     {:model model})))]
      (when-let [error (some-> (mr/explain schema exported) mu.humanize/humanize)]
        (throw (ex-info (format "Invalid export of %s: %s" model (pr-str error))
                        {:model model :error error}))))))

(defn- transformed
  "The values of column `k` of `entities` transformed by `f`, a fn of `resolver`, the entities and the values."
  [f resolver entities k]
  (let [values (mapv #(get % k) entities)
        result (vec (f resolver entities values))]
    (when-not (= (count values) (count result))
      (throw (ex-info (format "The transform of %s returned %d values for %d" k (count result) (count values))
                      {:key k})))
    result))

(defn serialize-rows
  "The `entities` of `model` as exported maps by its `spec`, transformed with the export `resolver` all at once."
  [model spec resolver entities]
  (let [defaults  (:defaults spec)
        columns   (for [[k transform] (:transform spec)]
                    [(:as transform k) (transformed (:export transform) resolver entities k)])
        exported  (fn [i entity]
                    (reduce (fn [row [k values]]
                              (let [v (nth values i)]
                                (cond-> row
                                  (and (not= v ::skip) (not= v (get defaults k))) (assoc k v))))
                            (-> (into {} (remove (fn [[k v]] (= v (get defaults k)))) (select-keys entity (:copy spec)))
                                (assoc :serdes/meta [{:model (name model)}]))
                            columns))]
    (vec (map-indexed exported entities))))

(defn deserialize-rows
  "The `ingested` entities of `model`, normalized by the `:schema` of its spec but for the columns its import
  transforms normalize themselves, as the rows to insert, transformed with the
  import `resolver` all at once. The columns imported after insert are left out."
  [model resolver ingested]
  (let [spec     (spec-of model)
        raw-keys (into [] (keep (fn [[k transform]] (when (:import transform) (:as transform k)))) (:transform spec))
        ingested (mapv #(merge (lib/normalize (:schema spec) (apply dissoc % raw-keys)) (select-keys % raw-keys))
                       ingested)
        columns  (for [[k transform] (:transform spec)
                       :when (:import transform)]
                   (let [import-k (:as transform k)]
                     [k import-k (transformed (:import transform) resolver ingested import-k)]))
        row      (fn [i entity]
                   (reduce (fn [row [k import-k values]]
                             (let [v (nth values i)]
                               (cond-> row
                                 (and (not= v ::skip)
                                      (or (some? v) (contains? entity import-k)))
                                 (assoc k v))))
                           (merge (:defaults spec) (select-keys entity (:copy spec)))
                           columns))]
    (vec (map-indexed row ingested))))

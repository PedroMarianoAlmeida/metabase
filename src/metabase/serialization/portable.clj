(ns metabase.serialization.portable
  "The portable identity of an exported entity.

  An exported entity's `:serdes/meta` names only its model, `[{:model \"Card\"}]`. Its portable id, the value other
  exported entities refer to it by, comes from its own keys: the exported values of its spec's `:identity` columns,
  like the entity id of a Card, the name of a Database, or the `[db-name schema table-name]` of a Table."
  (:require
   [clojure.string :as str]
   [metabase.serialization.spec :as serialization.spec]))

(set! *warn-on-reflection* true)

(defn meta-of
  "The `:serdes/meta` of an exported entity of `model`."
  [model]
  [{:model (name model)}])

(defn model-of
  "The model of `exported`, an exported entity, named by its `:serdes/meta`."
  [exported]
  (some->> exported :serdes/meta last :model (keyword "model")))

(defn- table-ref
  "The `[db-name schema table-name]` reference of the Table on a full `:serdes/meta` `path`, with a nil schema for a
  schemaless Table."
  [path]
  (let [id-of (fn [model] (some #(when (= model (:model %)) (:id %)) path))]
    [(id-of "Database") (id-of "Schema") (id-of "Table")]))

(defn- field-ref
  "The `[db-name schema table-name & field-names]` reference of the Field on a full `:serdes/meta` `path`."
  [path]
  (into (table-ref path) (keep #(when (= "Field" (:model %)) (:id %))) path))

(def ^:private path-identity
  "For each model whose files kept their identity only in a full `:serdes/meta` path up to release 64, a fn of the path
  of its parent to its identity columns."
  {"FieldValues"       (fn [parent] {:field_id (field-ref parent)})
   "FieldUserSettings" (fn [parent] {:field_id (field-ref parent)})
   "Dimension"         (fn [parent] {:field_id (field-ref parent)})
   "TableUserSettings" (fn [parent] {:table_id (table-ref parent)})
   "OsiAiContext"      (fn [parent]
                         (let [{:keys [model id]} (peek parent)]
                           (if (= "Table" model)
                             {:entity_type "table" :entity_local_id (table-ref parent)}
                             {:entity_type     (get {"Card" "card" "Measure" "measure" "Segment" "segment"} model model)
                              :entity_local_id id})))})

(defn restore-identity
  "`ingested`, an ingested entity, with the identity columns that an export up to release 64 kept only in its full
  `:serdes/meta` path filled in from that path."
  [ingested]
  (let [path (vec (:serdes/meta ingested))]
    (if-let [identity-of (and (next path) (path-identity (:model (peek path))))]
      (reduce-kv (fn [entity k v] (cond-> entity (nil? (get entity k)) (assoc k v)))
                 ingested
                 (identity-of (pop path)))
      ingested)))

(defmulti portable-id
  "The portable id of `exported`, an exported entity of `model`: the exported value of its single `:identity` column,
  or the vector of the values of several."
  {:arglists '([model exported])}
  (fn [model _exported] model))

(defmethod portable-id :default [model exported]
  (let [spec   (serialization.spec/spec-of model)
        values (for [k (serialization.spec/identity-columns model)]
                 (let [v (get exported (get-in spec [:transform k :as] k))]
                   (cond-> v (string? v) str/trim)))]
    (if (next values) (vec values) (first values))))

(defn key-of
  "The `[model portable-id]` key of `exported`, an exported entity, which the dependencies of other entities name."
  [exported]
  (let [model (model-of exported)]
    [model (portable-id model exported)]))

(defn portable-key
  "The `[model portable-id]` key `k`, a reference, names: its own, or the entity id of a nested entity it names by
  `[owner-entity-id entity-id]`."
  [[model id :as k]]
  (if (and (vector? id) (= [:entity_id] (:identity (serialization.spec/spec model) [:entity_id])))
    [model (peek id)]
    k))

(defn log-str
  "`exported`, an exported entity, as a string for logs and errors."
  [exported]
  (let [[model id] (key-of exported)]
    (str (name model) " " (pr-str id))))

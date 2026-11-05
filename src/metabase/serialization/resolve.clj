(ns metabase.serialization.resolve
  "Resolution of the portable references serialization replaces, a whole collection per query of a store (see
  [[metabase.serialization.protocol/Store]]), where a reference is a vector naming what to resolve, its input last:

  - export: `[:fk model id]`, `[:fk-keyed model field id]`, `[:table id]`, `[:field id]`;
  - import: `[:fk model eid]`, `[:fk-keyed model field value]`, `[:user email]`, `[:database db-name]`,
    `[:table path]`, `[:field path]`."
  (:require
   [metabase.serialization.protocol :as protocol]
   [metabase.serialization.spec :as serialization.spec]
   [metabase.serialization.util :as serialization.util]
   [metabase.util.i18n :refer [tru]]))

(set! *warn-on-reflection* true)

(defmulti nested-owner
  "The `[owner-model owner-key]` of `model` when exports name its entities by `[owner-entity-id entity-id]`, or nil."
  {:arglists '([model])}
  identity)

(defmethod nested-owner :default [_] nil)

(defn resolver
  "The resolver of references against `store` with the options `opts`, which the walks it serves read (see [[option]]):
  `:create-missing?` creates the inactive Users, stub Databases and inactive Tables and Fields an import refers to but
  `store` lacks, `:lenient?` resolves an import reference with no target to nil, `:numeric-ids-allowed?` accepts bare
  numeric ids alongside portable references in an imported form, and `:skip-schema-validation?` keeps an imported
  query that this instance's query schema rejects."
  ([store]
   (resolver store {}))
  ([store opts]
   (assoc opts :store store)))

(defn store
  "The store `resolver` resolves references against."
  [resolver]
  (:store resolver))

(defn options
  "The options of `resolver` (see [[resolver]])."
  [resolver]
  (dissoc resolver :store))

(defn lenient
  "`resolver` resolving an import reference with no target to nil instead of failing on it."
  [resolver]
  (assoc resolver :lenient? true))

(defn- distinct-inputs
  "The distinct non-nil `xs`."
  [xs]
  (into [] (comp (remove nil?) (distinct)) xs))

(defn- select-by-pks [resolver model pks columns]
  (let [pks (distinct-inputs pks)]
    (if (seq pks) (protocol/select-by-pks (store resolver) model pks columns) [])))

(defn- select-by-identity [resolver model identities columns]
  (let [identities (distinct-inputs identities)]
    (if (seq identities) (protocol/select-by-identity (store resolver) model identities columns) [])))

(defn- insert-missing!
  "The map from each of the distinct `inputs` that `found` lacks to the id of the `model` row inserted for it by
  `->row` when `resolver` has `:create-missing?`, merged into `found`."
  [resolver model found inputs ->row]
  (let [missing (into [] (comp (distinct) (remove #(contains? found %))) inputs)]
    (if (and (:create-missing? resolver) (seq missing))
      (into found (map vector missing (map :id (protocol/insert-rows! (store resolver) model (mapv ->row missing)))))
      found)))

;;; ------------------------------------------------------ Export ------------------------------------------------------

(defn field-values
  "The map from each of `ids` to the `field` value of its `model` row, leaving out the ids with no row."
  [resolver model field ids]
  (let [pk (serialization.spec/primary-key model)]
    (into {} (map (juxt #(get % pk) field)) (select-by-pks resolver model ids [pk field]))))

(defn entity-ids
  "The map from each of `ids` to the portable id of its `model` row, its entity id or, for a nested model, the vector of
  its owner's entity id and its own, leaving out the ids with no such row."
  [resolver model ids]
  (let [pk                (serialization.spec/primary-key model)
        [owner owner-key] (nested-owner model)
        rows              (filter :entity_id (select-by-pks resolver model ids (cond-> [pk :entity_id] owner-key (conj owner-key))))]
    (if owner
      (let [owner-eids (entity-ids resolver owner (map owner-key rows))]
        (into {}
              (keep (fn [row]
                      (when-let [owner-eid (get owner-eids (get row owner-key))]
                        [(get row pk) [owner-eid (:entity_id row)]])))
              rows))
      (into {} (map (juxt #(get % pk) :entity_id)) rows))))

(defn table-refs
  "The map from each of `table-ids` to the `[db-name schema table-name]` reference of its Table, leaving out the ids with
  no Table."
  [resolver table-ids]
  (let [tables   (select-by-pks resolver :model/Table table-ids [:id :db_id :schema :name])
        db-names (field-values resolver :model/Database :name (map :db_id tables))]
    (into {}
          (map (fn [{:keys [id db_id schema name]}] [id [(db-names db_id) schema name]]))
          tables)))

(defn- fields-with-ancestors
  "The map from id to row of the Fields with `field-ids` and all their ancestors, one nesting level per query."
  [resolver field-ids]
  (loop [rows {} pending (distinct-inputs field-ids)]
    (if (empty? pending)
      rows
      (let [fetched (select-by-pks resolver :model/Field pending [:id :name :parent_id :table_id])
            rows    (into rows (map (juxt :id identity)) fetched)]
        (recur rows (into [] (comp (keep :parent_id) (remove rows) (distinct)) fetched))))))

(defn- ancestry
  "The rows of the Field with `field-id` and its ancestors in `rows`, root first."
  [rows field-id]
  (loop [id field-id chain ()]
    (if-let [row (get rows id)]
      (recur (:parent_id row) (conj chain row))
      (vec chain))))

(defn field-refs
  "The map from each of `field-ids` to the `[db-name schema table-name & field-names]` reference of its Field, leaving
  out the ids with no Field."
  [resolver field-ids]
  (let [rows   (fields-with-ancestors resolver field-ids)
        chains (into {} (keep (fn [id] (when (contains? rows id) [id (ancestry rows id)]))) (distinct-inputs field-ids))
        tables (table-refs resolver (map (comp :table_id first) (vals chains)))]
    (into {}
          (keep (fn [[id chain]]
                  (when-let [table-ref (get tables (:table_id (first chain)))]
                    [id (into table-ref (map :name) chain)])))
          chains)))

;;; ------------------------------------------------------ Import ------------------------------------------------------

(defn- entity-id-of
  "The entity id a portable id names: itself, or the last element of a nested model's path ids."
  [eid]
  (if (vector? eid) (last eid) eid))

(defn entity-pks
  "The map from each of `eids`, portable ids of `model` rows or, on a surface that accepts them, their numeric ids, to
  the primary key of its row, leaving out the ones with no row."
  [resolver model eids]
  (let [pk                                  (serialization.spec/primary-key model)
        {numeric true portable false}       (group-by int? (distinct-inputs eids))
        _                                   (when (seq portable) (serialization.spec/check-identity! model [:entity_id]))
        by-eid                              (serialization.util/index-first :entity_id #(get % pk)
                                                                            (select-by-identity resolver model
                                                                                                (map (comp vector entity-id-of) portable)
                                                                                                [pk :entity_id]))
        by-pk                               (into #{} (map #(get % pk)) (select-by-pks resolver model numeric [pk]))]
    (merge (into {} (keep (fn [eid] (when-some [id (get by-eid (entity-id-of eid))] [eid id]))) portable)
           (into {} (comp (filter by-pk) (map (juxt identity identity))) numeric))))

(defn pks-by-field
  "The map from each of `values` to the primary key of the oldest `model` row whose `field`, its identity, has that
  value, leaving out the values with no row."
  [resolver model field values]
  (serialization.spec/check-identity! model [field])
  (let [pk (serialization.spec/primary-key model)]
    (serialization.util/index-first field #(get % pk) (select-by-identity resolver model (map vector (distinct-inputs values)) [pk field]))))

(defn user-ids
  "The map from each of `emails` to the id of its User, an inactive one created when `resolver` has `:create-missing?`."
  [resolver emails]
  (insert-missing! resolver :model/User (pks-by-field resolver :model/User :email emails) (distinct-inputs emails)
                   (fn [email] {:email email :is_active false})))

(defn database-ids
  "The map from each of `db-names` to the id of the Database it names, a stub created when `resolver` has
  `:create-missing?`."
  [resolver db-names]
  (insert-missing! resolver :model/Database (pks-by-field resolver :model/Database :name db-names) (distinct-inputs db-names)
                   (fn [db-name] {:name                db-name
                                  :engine              :postgres
                                  :details             {}
                                  :is_stub             true
                                  :initial_sync_status "complete"})))

(defn table-ids
  "The map from each of the `[db-name schema table-name]` `paths` to the id of its Table, an inactive one created when
  `resolver` has `:create-missing?`."
  [resolver paths]
  (let [paths  (distinct-inputs paths)
        db-ids (database-ids resolver (map first paths))
        tuple  (fn [[db-name schema table-name]] (some-> (get db-ids db-name) (vector schema table-name)))
        tuples (keep tuple paths)
        found  (serialization.util/index-first (juxt :db_id :schema :name) :id
                                               (select-by-identity resolver :model/Table tuples [:id :db_id :schema :name]))
        ids    (insert-missing! resolver :model/Table found tuples
                                (fn [[db-id schema table-name]] {:db_id db-id :schema schema :name table-name :active false}))]
    (into {} (keep (fn [path] (when-some [id (some->> (tuple path) (get ids))] [path id]))) paths)))

(defn- check-field-path!
  "Throws when `path` names no Field, holding no more than a Table reference."
  [path]
  (when (< (count path) 4)
    (throw (ex-info (tru "Portable field FK {0} must have at least 4 elements." (pr-str path))
                    {:status-code 400
                     :error       :invalid-field-fk
                     :path        path}))))

(defn field-ids
  "The map from each of the `[db-name schema table-name & field-names]` `paths` to the id of its Field, resolved one
  nesting level per query, with each missing node an inactive Field created when `resolver` has `:create-missing?`."
  [resolver paths]
  (let [paths     (mapv vec (distinct-inputs paths))
        _         (run! check-field-path! paths)
        table-ids (table-ids resolver (map #(subvec % 0 3) paths))]
    (loop [depth 4 ids {}]
      (let [prefixes (into [] (comp (filter #(>= (count %) depth)) (map #(subvec % 0 depth)) (distinct)) paths)]
        (if (empty? prefixes)
          (into {} (keep (fn [path] (when-some [id (get ids path)] [path id]))) paths)
          (let [parent  #(when (> depth 4) (get ids (pop %)))
                tuple   #(vector (get table-ids (subvec % 0 3)) (parent %) (peek %))
                ready   (filter #(and (get table-ids (subvec % 0 3)) (or (= depth 4) (parent %))) prefixes)
                found   (serialization.util/index-first (juxt :table_id :parent_id :name) :id
                                                        (select-by-identity resolver :model/Field (map tuple ready)
                                                                            [:id :table_id :parent_id :name]))
                by-node (insert-missing! resolver :model/Field found (map tuple ready)
                                         (fn [[table-id parent-id field-name]]
                                           {:table_id      table-id
                                            :parent_id     parent-id
                                            :name          field-name
                                            :active        false
                                            :base_type     :type/*
                                            :database_type "NULL"}))]
            (recur (inc depth)
                   (into ids (keep (fn [prefix] (when-some [id (get by-node (tuple prefix))] [prefix id]))) ready))))))))

;;; ---------------------------------------------------- References ----------------------------------------------------

(defn- grouped-refs
  "The `[kind inputs]` pairs of `refs`, a kind being a reference without its input, in order of first appearance."
  [refs]
  (let [refs   (distinct refs)
        groups (group-by pop refs)]
    (for [kind (distinct (map pop refs))]
      [kind (mapv peek (get groups kind))])))

(defn- resolve-refs
  "The map from each of `refs` to what `resolve-kind`, a fn of a kind and its inputs, finds for it."
  [resolve-kind refs]
  (into {}
        (mapcat (fn [[kind inputs]]
                  (let [found (resolve-kind kind inputs)]
                    (for [input inputs
                          :when (contains? found input)]
                      [(conj kind input) (get found input)]))))
        (grouped-refs refs)))

(defn export-refs
  "The map from each of the export `refs` to what `resolver` resolves it to, leaving out the refs with no target, with
  one lookup per kind of reference."
  [resolver refs]
  (resolve-refs (fn [[k model field] inputs]
                  (case k
                    :fk       (entity-ids resolver model inputs)
                    :fk-keyed (field-values resolver model field inputs)
                    :table    (table-refs resolver inputs)
                    :field    (field-refs resolver inputs)))
                refs))

(defn import-refs
  "The map from each of the import `refs` to what `resolver` resolves it to, leaving out the refs with no target unless
  `resolver` is [[lenient]], with one lookup per kind of reference."
  [resolver refs]
  (resolve-refs (fn [[k model field] inputs]
                  (let [found (case k
                                :fk       (entity-pks resolver model inputs)
                                :fk-keyed (pks-by-field resolver model field inputs)
                                :user     (user-ids resolver inputs)
                                :database (database-ids resolver inputs)
                                :table    (table-ids resolver inputs)
                                :field    (field-ids resolver inputs))]
                    (cond-> found
                      (:lenient? resolver) (into (comp (remove #(contains? found %)) (map (fn [x] [x nil]))) inputs))))
                refs))

(defn target-not-found
  "The error of an export or import reference `ref` whose target does not exist."
  [[k model :as ref]]
  (let [input        (peek ref)
        target-model (case k
                       (:fk :fk-keyed) model
                       :table          :model/Table
                       :field          :model/Field
                       :user           :model/User
                       :database       :model/Database)]
    (ex-info (format "%s %s was not found" (name target-model) (pr-str input))
             {:model target-model
              :id    input
              ::type :target-not-found})))

(defn target-not-found?
  "Whether `e` is a [[target-not-found]] error."
  [e]
  (= :target-not-found (::type (ex-data e))))

(defn ref-value
  "The value `resolved` holds for `ref`: nil for a missing keyed lookup, and a [[target-not-found]] error for any other
  missing target."
  [resolved [k :as ref]]
  (cond
    (contains? resolved ref) (get resolved ref)
    (= k :fk-keyed)          nil
    :else                    (throw (target-not-found ref))))

(defn found-or-nil
  "What `f` returns, or nil when it throws a [[target-not-found]] error."
  [f]
  (try
    (f)
    (catch clojure.lang.ExceptionInfo e
      (when-not (target-not-found? e)
        (throw e)))))

(defn walk-in-bulk
  "Each of `xs` rewritten by `walk`, a fn of a visitor and a form that replaces every reference in the form with what
  the visitor returns for it, and `opts` the [[options]] the walk reads with [[option]]. A first walk collects the references, returning each input unresolved; `resolve`, a fn
  of a collection of references like [[export-refs]], resolves them all at once; and a second walk turns each
  resolution into a value with [[ref-value]]. A reference the first walk misses, because it threw or took a path that
  depends on resolved values, is resolved when the second walk meets it."
  ([walk resolve xs]
   (walk-in-bulk walk resolve {} xs))
  ([walk resolve opts xs]
   (let [collected (volatile! [])
         collect   (with-meta (fn [ref] (vswap! collected conj ref) (peek ref)) {::collecting true ::options opts})
         _         (run! #(try (walk collect %) (catch Exception _ nil)) xs)
         refs      (distinct @collected)
         resolved  (volatile! (resolve refs))
         tried     (volatile! (set refs))
         visit     (with-meta (fn [ref]
                                (when-not (contains? @tried ref)
                                  (vswap! tried conj ref)
                                  (vswap! resolved merge (resolve [ref])))
                                (ref-value @resolved ref))
                              {::options opts})]
     (mapv #(walk visit %) xs))))

(defn export-in-bulk
  "Each of `xs` rewritten by `walk` (see [[walk-in-bulk]]), with the export references resolved by `resolver`."
  [resolver walk xs]
  (walk-in-bulk walk #(export-refs resolver %) (options resolver) xs))

(defn import-in-bulk
  "Each of `xs` rewritten by `walk` (see [[walk-in-bulk]]), with the import references resolved by `resolver`."
  [resolver walk xs]
  (walk-in-bulk walk #(import-refs resolver %) (options resolver) xs))

(defn option
  "The value of the option `k` (see [[options]]) of the resolver whose references `visit`, a visitor of
  [[walk-in-bulk]], resolves."
  [visit k]
  (get (::options (meta visit)) k))

(defn collecting?
  "Whether `visit` is the visitor of the first walk of [[walk-in-bulk]], which returns each input unresolved, so a
  walk can skip the steps that need resolved values."
  [visit]
  (boolean (::collecting (meta visit))))

(defn visit-some
  "What `visit` returns for `ref`, or nil when the input of `ref` is nil."
  [visit ref]
  (when (some? (peek ref))
    (visit ref)))

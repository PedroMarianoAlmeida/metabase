(ns metabase.serialization.protocol.stub
  "A [[metabase.serialization.protocol/Store]] answering lookups with stub fns of the references they serve, for tests
  that should not touch the app DB."
  (:require
   [metabase.serialization.protocol :as protocol]
   [metabase.serialization.resolve :as resolve]
   [metabase.serialization.spec :as serialization.spec]))

(defn- unstubbed [k]
  (fn [& _]
    (throw (ex-info (str "No stub for " k) {:stub k}))))

(defn- placeholder-db
  "The stub of `:database` when there is none: an opaque id that keeps the name, so Table paths still resolve."
  [db-name]
  [::db db-name])

(defn- placeholder-table
  "The stub of `:table` when there is none: an opaque id that keeps the path, so Field paths still resolve."
  [path]
  [::table path])

(defn- field-row
  "The row of the Field with `id` at the `[db-name schema table-name & field-names]` `path`."
  [id path]
  {:id        id
   :name      (peek path)
   :parent_id (when (> (count path) 4) [::field (pop path)])
   :table_id  [::table (subvec path 0 3)]})

(defn- row-by-pk
  "The row of `model` with primary key `pk` that the stubs answer, or nil, a nested row owned by the owner of its own id."
  [stub model pk columns]
  (let [[tag x] (when (vector? pk) pk)]
    (case tag
      ::db    {:id pk :name x}
      ::table {:id pk :db_id [::db (first x)] :schema (second x) :name (nth x 2)}
      ::field (field-row pk x)
      (case model
        :model/Table (when-let [[db-name schema table-name] ((stub :table) pk)]
                       {:id pk :db_id [::db db-name] :schema schema :name table-name})
        :model/Field (some->> ((stub :field) pk) vec (field-row pk))
        (let [k               (serialization.spec/primary-key model)
              col             (first (remove #{k} columns))
              [_ owner-key]   (resolve/nested-owner model)]
          (when-some [v (if (= col :entity_id)
                          ((stub :fk) pk model)
                          ((stub :fk-keyed) pk model col))]
            (cond-> {k pk col v}
              owner-key (assoc owner-key pk))))))))

(defn stub-store
  "The store answering with the fns of `stubs`, a nil answer meaning no row, each recorded in the atom `:calls` as
  `[op model]` when there is one: `:fk` of an id and a model, `:fk-keyed` of an id, a model and a column, `:table` of
  a Table id and `:field` of a Field id on export; `:fk` of an entity id and a model, `:fk-keyed` of a value, a model
  and a column, `:user` of an email, `:database` of a Database name, `:table` of a Table path and `:field` of a Field
  path on import; or `:import`, a fn of any import reference's input answering every kind."
  [stubs]
  (let [stub      (fn [k] (or (get stubs k)
                              (when-let [f (:import stubs)] (fn [x & _] (f x)))
                              (case k :database placeholder-db :table placeholder-table (unstubbed k))))
        db-names  (atom {})
        paths     (atom {})
        record!   (fn [op model] (some-> (:calls stubs) (swap! conj [op model])))]
    (reify protocol/Store
      (select-by-pks [_ model pks columns]
        (record! :select-by-pks model)
        (into [] (keep #(row-by-pk stub model % columns)) (distinct pks)))
      (select-by-identity [_ model identities _columns]
        (record! :select-by-identity model)
        (into []
              (keep (fn [[a b c]]
                      (case model
                        :model/Database (when-some [id ((stub :database) a)]
                                          (swap! db-names assoc id a)
                                          {:id id :name a})
                        :model/User     (some->> ((stub :user) a) (hash-map :email a :id))
                        :model/Table    (let [path [(if (vector? a) (second a) (get @db-names a)) b c]]
                                          (when-some [id ((stub :table) path)]
                                            (swap! paths assoc [:table id] path)
                                            {:id id :db_id a :schema b :name c}))
                        :model/Field    (let [parent (if b (get @paths [:field b]) (get @paths [:table a]))
                                              path   (conj (vec parent) c)]
                                          (when-some [id ((stub :field) path)]
                                            (swap! paths assoc [:field id] path)
                                            {:id id :table_id a :parent_id b :name c}))
                        (let [k (serialization.spec/primary-key model)
                              [col] (serialization.spec/identity-columns model)]
                          (when-some [id (if (= col :entity_id)
                                           ((stub :fk) a model)
                                           ((stub :fk-keyed) a model col))]
                            {k id col a})))))
              (distinct identities)))
      (select-by-fks [_ model _fks _columns]
        (throw (ex-info "No stub for owned rows" {:model model})))
      (insert-rows! [_ model _rows]
        (throw (ex-info "No stub for inserts" {:model model})))
      (update-row! [_ model _pk _changes]
        (throw (ex-info "No stub for updates" {:model model})))
      (delete-rows! [_ model _pks]
        (throw (ex-info "No stub for deletes" {:model model})))
      (transact [_ _opts f]
        (f)))))

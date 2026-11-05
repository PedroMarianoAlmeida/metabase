(ns metabase.serialization.load
  "Import: exported entities loaded into the app DB, a batch at a time, in one transaction."
  (:refer-clojure :exclude [import load])
  (:require
   [clojure.walk :as walk]
   [metabase.models.interface :as mi]
   [metabase.serialization.dependencies :as serialization.dependencies]
   [metabase.serialization.portable :as serialization.portable]
   [metabase.serialization.protocol :as protocol]
   [metabase.serialization.protocol.app-db :as protocol.app-db]
   [metabase.serialization.resolve :as resolve]
   [metabase.serialization.spec :as serialization.spec]
   [metabase.serialization.util :as serialization.util]
   [metabase.util.log :as log])
  (:import
   (java.time OffsetDateTime ZonedDateTime)))

(set! *warn-on-reflection* true)

(defmulti before-load
  "The load `items` of `model`, each `{:ingested map :row row :local local}`: the exported map, the row it
  deserializes to, and the local row its `:identity` matches, or nil when it is new. Returns the items to load, so a
  model can leave items out, adjust their `:row` against their `:local` row, adjust the `:ingested` map whose
  after-insert columns it imports, or throw to refuse them, much like a t2 `before-update`. By default, the items."
  {:arglists '([model items])}
  (fn [model _items] model))

(defmethod before-load :default [_ items] items)

(defmulti after-load
  "Runs the side effects of loading `items` of `model`, each a load item (see [[before-load]]) with the loaded
  `:instance`. By default, none."
  {:arglists '([model items])}
  (fn [model _items] model))

(defmethod after-load :default [_ _items] nil)

(def ^:private import-resolver
  "The resolver of an import into the app DB, which creates the Users, Databases, Tables and Fields it lacks."
  (resolve/resolver protocol.app-db/store {:create-missing? true}))

(defn- matching-rows
  "The local row of `model` in `store` each of `rows` matches on the model's `:identity` columns, or nil, where a row
  missing its first identity column, like one without an entity id, matches none, and of several matches the oldest
  wins, as it does when references resolve."
  [store model rows]
  (let [[k :as ks] (serialization.spec/identity-columns model)
        tuple      (apply juxt ks)
        tuples     (into [] (comp (filter #(some? (get % k))) (map tuple) (distinct)) rows)
        locals     (when (seq tuples)
                     (serialization.util/index-first tuple identity (protocol/select-by-identity store model tuples nil)))]
    (mapv #(when (some? (get % k)) (get locals (tuple %))) rows)))

(defn- comparable
  "`x` as an import compares it with a local value: without the uuids and metadata providers of MBQL clauses, which
  every import of a query regenerates, and with temporals as instants."
  [x]
  (walk/postwalk (fn [y]
                   (cond
                     (map? y)                       (dissoc y :lib/uuid :lib/metadata)
                     (instance? OffsetDateTime y)   (.toInstant ^OffsetDateTime y)
                     (instance? ZonedDateTime y)    (.toInstant ^ZonedDateTime y)
                     :else                          y))
                 x))

(defn- same?
  "Whether `x`, an imported value, equals `local`, the value of the local row it updates."
  [x local]
  (or (= x local) (= (comparable x) (comparable local))))

(defn- changes
  "The columns of `row`, a row of `model`, whose values differ from those of `local`, the row it updates. A column
  `local` does not select, like an internal one a model's `after-select` drops, is unchanged at its spec default."
  [model row local]
  (let [defaults (:defaults (serialization.spec/spec-of model))]
    (into {}
          (remove (fn [[k v]]
                    (if (contains? local k)
                      (same? v (get local k))
                      (and (contains? defaults k) (= v (get defaults k))))))
          row)))

(defn- write!
  "Inserts the `items` with no local row and updates the others with the columns of their `:row` that changed, but
  those their entity loads without to break a cycle, returning each with its loaded `:instance`."
  [store model items]
  (let [pk       (serialization.spec/primary-key model)
        new-idxs (into [] (keep-indexed (fn [i {:keys [local]}] (when-not local i))) items)
        inserted (when (seq new-idxs)
                   (zipmap new-idxs (protocol/insert-rows! store model (mapv #(:row (items %)) new-idxs))))]
    (vec (map-indexed (fn [i {:keys [row local ingested] :as item}]
                        (if local
                          (let [row     (apply dissoc row (:serdes/stripped ingested))
                                changed (changes model row local)]
                            (when (seq changed)
                              (protocol/update-row! store model (get local pk) changed))
                            (assoc item :instance (merge local row)))
                          (assoc item :instance (get inserted i))))
                      items))))

(defn- prepare-items
  "The load items of the `ingested` entities of `model` (see [[before-load]]): deserialized with `resolver`, matched
  to local rows, and passed through [[before-load]]."
  [model resolver ingested]
  (let [rows (serialization.spec/deserialize-rows model resolver ingested)]
    (vec (before-load model (mapv (fn [ingested row local] {:ingested ingested :row row :local local})
                                  ingested rows (matching-rows (resolve/store resolver) model rows))))))

(declare ^:private load-owned!)

(defn- load-items!
  "Writes the load `items` of `model`, loads the entities they own (see [[metabase.serialization.spec]]), and passes
  them to [[after-load]], with `resolver`. Returns the loaded instances."
  [model resolver items]
  (let [items (write! (resolve/store resolver) model items)]
    (doseq [[k {[owned-model owner-key] :owned :as transform}] (:transform (serialization.spec/spec-of model))
            :when owned-model]
      (load-owned! owned-model owner-key resolver items (:as transform k)))
    (after-load model items)
    (mapv :instance items)))

(defn- load-owned!
  "Loads the entities under `k` in the ingested maps of the loaded owner `items` as rows of `model` whose `owner-key`
  is their owner's id, deleting first the rows of these owners that none matches on the `:identity` of `model`. An
  owner whose map lacks `k`, like one loading without the keys that close a dependency cycle, keeps its rows."
  [model owner-key resolver items k]
  (let [items  (filter #(contains? (:ingested %) k) items)
        ids    (mapv (comp :id :instance) items)
        nested (vec (for [{:keys [instance ingested]} items
                          x                           (get ingested k)]
                      (assoc x owner-key (:id instance))))]
    (when (seq ids)
      (let [store    (resolve/store resolver)
            pk       (serialization.spec/primary-key model)
            prepared (prepare-items model resolver nested)
            kept     (into #{} (keep #(some-> (:local %) (get pk))) prepared)
            stale    (into [] (comp (map #(get % pk)) (remove kept))
                           (protocol/select-by-fks store model ids [pk]))]
        (when (seq stale)
          (protocol/delete-rows! store model stale))
        (load-items! model resolver prepared)))))

(defn load
  "Loads `ingested`, exported entities of `model`, into the app DB with `resolver` in one batch: each updates the local
  row its `:identity` matches, or is inserted. Returns the loaded instances."
  ([model ingested]
   (load model import-resolver ingested))
  ([model resolver ingested]
   (binding [mi/*deserializing?* true]
     (load-items! model resolver (prepare-items model resolver (vec ingested))))))

(defn import
  "The rows that `ingested`, exported entities of `model`, import to, with their references resolved at once."
  [model ingested]
  (serialization.spec/deserialize-rows model import-resolver (vec ingested)))

(defn local-rows
  "The local row of `model` each of `ingested`, exported entities, matches on the model's `:identity`, or nil, looking
  its references up without creating what they refer to."
  [model ingested]
  (let [rows (serialization.spec/deserialize-rows model (resolve/lenient (resolve/resolver protocol.app-db/store)) (vec ingested))]
    (matching-rows protocol.app-db/store model rows)))

;;; ------------------------------------------------------ Order -------------------------------------------------------

(def ^:private circular-keys
  "The keys that break the dependency cycles a model can be part of: an entity in a cycle loads without them first,
  then again whole. A dashboard question's card points at its dashboard, whose dashcards point at the card."
  {:model/Dashboard #{:dashcards :parameters}
   :model/Document  #{:document}
   :model/Card      #{:dashboard_id :document_id :parameters}})

(def ^:private strip-priority
  "The models whose entities break a cycle first: the keys a card loads without matter least."
  {:model/Card 0 :model/Document 1 :model/Dashboard 2})

(defn- batch-references
  "The `[k {:all refs :stripped refs :owned refs}]` of each `[k entity]` of `batch`, exported entities with their keys:
  the keys of the entities it and those it owns depend on, whole and without the keys that close its dependency
  cycles, and the keys of those it owns."
  [batch]
  (for [[model pairs] (group-by ffirst batch)
        :let [ks       (mapv first pairs)
              entities (mapv second pairs)
              strip    (circular-keys model)
              all      (serialization.dependencies/import-dependencies-with-owned model entities)
              stripped (when strip (serialization.dependencies/import-dependencies-with-owned model (mapv #(apply dissoc % strip) entities)))]
        [i k] (map-indexed vector ks)]
    [k {:all      (cond-> (nth all i) (not strip) (disj k))
        :stripped (when strip (disj (nth stripped i) k))
        :owned    (serialization.dependencies/owned-keys model (nth entities i))}]))

(defn- prerequisites
  "The map from each of `ks`, the `[model portable-id]` keys of exported entities, to `{:all keys :stripped keys}`:
  the keys among them of the entities that the entity `read` returns for it, and those it owns, refer to, whole and
  without the keys that close its dependency cycles. A reference to an owned entity is to its owner. `read` is a fn of
  a batch of keys, returning their entities, and only the references of the entities stay in memory."
  [ks read]
  (let [refs    (into {}
                      (comp (partition-all serialization.util/batch-size)
                            (mapcat (fn [batch] (batch-references (map vector batch (read (vec batch)))))))
                      ks)
        owners  (into {} (for [[k {:keys [owned]}] refs, owned-key owned] [owned-key k]))
        among   (fn [ks-refs] (into #{} (comp (map #(get owners % %)) (filter refs)) ks-refs))]
    (update-vals refs (fn [{:keys [all stripped]}]
                        {:all      (among all)
                         :stripped (some-> stripped among)}))))

(defn load-order
  "The keys `ks` of the exported entities `read` returns for a batch of them, `[model portable-id]`, as batches to
  load in order, each `{:model model :keys [...] :strip keys}`: every entity after the entities among them it refers to. A dependency
  cycle is broken by one of its entities, which comes first in a batch without the `:strip` keys that close the
  cycle, and again later whole."
  [ks read]
  (let [prereqs (prerequisites ks read)
        batches (fn [ks strip]
                  (for [[model ks] (group-by first (sort-by pr-str ks))]
                    {:model model :keys (vec ks) :strip strip}))]
    (loop [remaining (set ks) loaded #{} stripped #{} order []]
      (if (empty? remaining)
        order
        (let [ready (filter #(every? loaded (:all (prereqs %))) remaining)]
          (if (seq ready)
            (recur (reduce disj remaining ready) (into loaded ready) stripped (into order (batches ready nil)))
            (let [[k] (->> remaining
                           (filter #(when-let [deps (and (not (stripped %)) (:stripped (prereqs %)))]
                                      (every? (some-fn loaded stripped) deps)))
                           (sort-by (juxt (comp strip-priority first) pr-str)))]
              (when-not k
                (throw (ex-info "Circular dependency that no stripped keys break" {:keys remaining})))
              (recur remaining (conj loaded k) (conj stripped k)
                     (into order (batches [k] (circular-keys (first k))))))))))))

;;; --------------------------------------------------- Transaction ----------------------------------------------------

(defn load-error
  "The error of `entity`, an exported entity of `model`, failing to load with `e`. Its ex-data names the entity by
  `:model`, `:id`, its portable id, and `:name`, with the `:stripped` keys it loaded without, if any."
  [model entity e]
  (let [[_ id] (serialization.portable/key-of entity)]
    (ex-info (format "Failed to load %s" (serialization.portable/log-str entity))
             (cond-> {::type :load-error :model model :id id :name (when (string? (:name entity)) (:name entity))}
               (:serdes/stripped entity) (assoc :stripped (:serdes/stripped entity)))
             e)))

(defn load-error?
  "Whether `e` is a [[load-error]]."
  [e]
  (= :load-error (::type (ex-data e))))

(defn- load-batch!
  "Loads `entities` of `model` in a savepoint; when they fail, rolls back to it and loads them one at a time, each in
  its own savepoint, to tell the [[load-error]] of each entity that fails. Throws the first of them unless
  `continue-on-error`, and returns them otherwise."
  [model resolver entities continue-on-error]
  (let [load (fn [entities]
               (protocol/transact (resolve/store resolver) {} #(load model resolver entities)))]
    (try
      (load entities)
      []
      (catch Exception e
        (let [errors (into []
                           (keep (fn [entity]
                                   (try
                                     (load [entity])
                                     nil
                                     (catch Exception e
                                       (load-error model entity e)))))
                           entities)]
          (when-not continue-on-error
            (throw (or (first errors) e)))
          (doseq [error errors]
            (log/warnf "Skipping %s: %s" (ex-message error) (ex-message (ex-cause error))))
          errors)))))

(defn load-all
  "Loads the exported entities with the `[model portable-id]` keys `ks` into the app DB in one transaction, every
  entity after what it depends on. `read` is a fn of a batch of keys returning their entities, called a batch at a
  time so that the whole import need not fit in memory. Retries the whole import on a transient DB error. With
  `continue-on-error`, an entity that fails to load is left out and its error returned; otherwise the first failure
  rolls the whole import back. With `skip-schema-validation`, queries load even when this instance's query schema
  rejects them. With `store`, it loads into that store rather than the app DB. Returns the errors."
  [ks read & {:keys [continue-on-error skip-schema-validation store] :or {store protocol.app-db/store}}]
  (let [order    (load-order ks read)
        resolver (resolve/resolver store {:create-missing?         true
                                          :skip-schema-validation? (boolean skip-schema-validation)})]
    (protocol/transact
     store
     {:retries 3}
     (fn []
       (into []
             (mapcat (fn [{:keys [model keys strip]}]
                       (eduction (comp (partition-all serialization.util/batch-size)
                                       (mapcat (fn [batch]
                                                 (load-batch! model
                                                              resolver
                                                              (mapv #(cond-> %
                                                                       strip (-> (as-> e (apply dissoc e strip))
                                                                                 (assoc :serdes/stripped strip)))
                                                                    (read (vec batch)))
                                                              continue-on-error))))
                                 keys)))
             order)))))

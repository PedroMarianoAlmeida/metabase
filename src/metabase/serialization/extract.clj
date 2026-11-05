(ns metabase.serialization.extract
  "Extraction: entities of the app DB as portable maps, a batch at a time."
  (:require
   [metabase.serialization.protocol :as protocol]
   [metabase.serialization.resolve :as resolve]
   [metabase.serialization.spec :as serialization.spec]))

(set! *warn-on-reflection* true)

(defn- extraction-error [model entity e]
  (ex-info (format "Error extracting %s %s" (name model) (:id entity))
           (assoc (ex-data e) :model model :id (:id entity))
           e))

(defn- isolated
  "What `f`, a fn of a batch of `entities` returning one result for each, returns for `entities`, retrying a failed
  batch one entity at a time so that an entity that fails comes back as its error and the rest still succeed."
  [f model entities]
  (try
    (f entities)
    (catch Exception e
      (if (<= (count entities) 1)
        (mapv #(extraction-error model % e) entities)
        (into [] (mapcat #(isolated f model [%])) entities)))))

(declare ^:private exporting-spec)

(defn extract-batch
  "The `entities` of `model` exported with `resolver`, all their references resolved at once. An entity that fails
  to export comes back as its error."
  [model resolver entities]
  (let [spec (exporting-spec model)]
    (mapv (fn [entity exported]
            (if (instance? Exception exported)
              exported
              (try
                (serialization.spec/validate-export! model spec exported)
                exported
                (catch Exception e
                  (extraction-error model entity e)))))
          entities
          (isolated #(serialization.spec/serialize-rows model spec resolver %) model entities))))

(defn export-owned
  "For each of `owners`, its `rows` of `model`, those whose `owner-key` is its id, exported with `resolver` all at
  once in the order of `rows`; throws the error of the first row that fails. For the export of a transform with
  `:owned`."
  [model owner-key resolver owners rows]
  (let [rows     (vec rows)
        exported (mapv #(if (instance? Exception %) (throw %) %) (extract-batch model resolver rows))
        grouped  (group-by (comp owner-key rows) (range (count rows)))]
    (mapv (fn [{:keys [id]}] (mapv exported (get grouped id))) owners)))

(defn- export-owned-rows
  "The export fn of a transform with `:owned [model owner-key]` and no `:export`: the rows of `model` of each owner,
  sorted by the transform's `:order` key fn."
  [{[model owner-key] :owned order :order :or {order :entity_id}}]
  (fn [resolver owners _]
    (export-owned model owner-key resolver owners
                  (sort-by order (when-let [ids (seq (keep :id owners))]
                                   (protocol/select-by-fks (resolve/store resolver) model (vec ids) nil))))))

(defn- exporting-spec
  "The spec of `model`, each transform with `:owned` and no `:export` exporting the rows its owners own."
  [model]
  (update (serialization.spec/spec-of model) :transform update-vals
          #(cond-> % (and (:owned %) (not (:export %))) (assoc :export (export-owned-rows %)))))

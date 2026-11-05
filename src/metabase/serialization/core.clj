(ns metabase.serialization.core
  "Serialization (SerDes v3): exporting entities into portable maps and importing them back, in bulk.

  Exported entities refer to each other by portable references rather than database ids; a resolver (see
  [[metabase.serialization.resolve]]) translates the references of a whole batch at once."
  (:refer-clojure :exclude [import load])
  (:require
   [metabase.serialization.dependencies :as serialization.dependencies]
   [metabase.serialization.export :as serialization.export]
   [metabase.serialization.extract :as serialization.extract]
   [metabase.serialization.fk :as serialization.fk]
   [metabase.serialization.load :as serialization.load]
   [metabase.serialization.mbql :as serialization.mbql]
   [metabase.serialization.parameters :as serialization.parameters]
   [metabase.serialization.portable :as serialization.portable]
   [metabase.serialization.protocol :as protocol]
   [metabase.serialization.protocol.app-db :as protocol.app-db]
   [metabase.serialization.resolve :as resolve]
   [metabase.serialization.spec :as serialization.spec]
   [metabase.serialization.storage :as serialization.storage]
   [metabase.serialization.util :as serialization.util]
   [metabase.serialization.visualization-settings :as serialization.viz]
   [metabase.util.namespaces :as shared.ns]))

(shared.ns/import-fns
 [serialization.dependencies
  export-dependencies
  export-escapes
  import-escapes
  import-dependencies
  export-mbql-dependencies
  export-parameters-dependencies
  export-visualization-settings-dependencies
  import-mbql-dependencies
  import-parameters-dependencies
  import-visualization-settings-dependencies]
 [serialization.export
  export
  export-batch
  with-export-transaction]
 [serialization.extract
  export-owned]
 [serialization.fk
  database-transform
  export-databases
  export-fields
  export-fks
  export-tables
  export-users
  field-transform
  fk-transform
  import-databases
  import-fields
  import-fks
  import-tables
  import-users
  table-transform
  user-transform]
 [serialization.spec
  columns
  date-transform
  import-only
  keyword-transform
  spec
  skip-all
  spec-of
  unchanged]
 [resolve
  nested-owner
  collecting?
  export-in-bulk
  found-or-nil
  import-in-bulk
  lenient
  resolver
  target-not-found
  target-not-found?
  visit-some]
 [serialization.load
  after-load
  before-load
  import
  load
  load-all
  load-error
  load-error?
  load-order
  local-rows]
 [serialization.mbql
  card-entity-id
  card-template-tag-export-rename
  card-template-tag-import-rename
  card-template-tag-renames
  export-mbql
  import-mbql
  mbql-transform
  recording-card-entity-ids
  serialized-query-source-table
  walk-export
  walk-import
  walk-export-refs
  walk-import-refs]
 [serialization.parameters
  export-parameter-mappings
  export-parameters
  import-parameter-mappings
  import-parameters
  parameter-mappings-transform
  parameters-transform]
 [serialization.viz
  export-visualization-settings
  import-visualization-settings
  import-visualizer-settings
  link-card-model->toucan-model
  visualization-settings-transform]
 [serialization.storage
  collection-namespace-folder
  resource-paths
  storable
  storage-default-collection-path
  batch-storage-paths
  storage-path-prefixes
  storage-paths]
 [serialization.portable
  key-of
  log-str
  model-of
  portable-id
  restore-identity])

(def batch-size
  "The number of rows, ids or entities that serialization queries or processes together."
  serialization.util/batch-size)

(def app-db-store
  "The store of the application database."
  protocol.app-db/store)

(def export-resolver
  "The export resolver of the application database."
  (resolve/resolver app-db-store))

(def lookup-resolver
  "The import resolver of the application database that only looks references up, creating nothing."
  (resolve/resolver app-db-store))

(def import-resolver
  "The import resolver of the application database, which creates the Users, Databases, Tables and Fields an import
  refers to but this instance lacks."
  (resolve/resolver app-db-store {:create-missing? true}))

(defn select-owned
  "The rows of `model` owned by the rows with ids `owner-ids`, with only `columns` unless nil, from the store of
  `resolver`."
  [resolver model owner-ids columns]
  (protocol/select-by-fks (resolve/store resolver) model owner-ids columns))

(def skip
  "A transformed value that leaves its key out of the result."
  serialization.spec/skip)

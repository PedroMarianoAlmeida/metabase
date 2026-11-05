(ns metabase.serialization.core
  "Serialization (SerDes v3): exporting entities into portable maps and importing them back, in bulk.

  Exported entities refer to each other by portable references rather than database ids; a resolver (see
  [[metabase.serialization.resolve]]) translates the references of a whole batch at once."
  (:refer-clojure :exclude [import load])
  (:require
   [metabase.serialization.fk :as serialization.fk]
   [metabase.serialization.protocol :as protocol]
   [metabase.serialization.protocol.app-db :as protocol.app-db]
   [metabase.serialization.resolve :as resolve]
   [metabase.serialization.spec :as serialization.spec]
   [metabase.serialization.util :as serialization.util]
   [metabase.util.namespaces :as shared.ns]))

(shared.ns/import-fns
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
  visit-some])

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

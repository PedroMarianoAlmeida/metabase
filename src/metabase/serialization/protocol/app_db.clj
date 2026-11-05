(ns metabase.serialization.protocol.app-db
  "The [[metabase.serialization.protocol/Store]] of this instance's application database, querying a batch at a time."
  (:require
   [diehard.core :as dh]
   [metabase.app-db.core :as mdb]
   [metabase.app-db.transient-error :as transient-error]
   [metabase.serialization.db :as serialization.db]
   [metabase.serialization.protocol :as protocol]
   [metabase.serialization.protocol.util :as protocol.util]
   [metabase.serialization.spec :as serialization.spec]
   [metabase.serialization.util :as serialization.util]
   [metabase.util.log :as log]))

(set! *warn-on-reflection* true)

(defn- select-by-identity
  "The rows of `model` whose identity columns equal one of the tuples `identities`, with one query per batch."
  [model identities columns]
  (let [pk     (serialization.spec/primary-key model)
        tuples (protocol.util/identity-tuples identities)
        cols   (protocol.util/with-keys columns (cons pk (serialization.spec/identity-columns model)))]
    (protocol.util/matching-identities
     model
     tuples
     (protocol.util/in-batches #(serialization.db/rows-where-in model cols (protocol.util/identity-filters model %) pk)
                               tuples))))

(defn- with-retries
  "What `f` returns, retried up to `retries` times with backoff when it throws a transient DB error, like a deadlock."
  [retries f]
  (let [db-type (mdb/db-type)]
    (dh/with-retry {:max-retries retries
                    :backoff-ms  [200 1600 2.0]
                    :retry-if    (fn [_result e] (transient-error/transient-error? db-type e))
                    :on-retry    (fn [_result e] (log/warnf "Transient DB error, retrying: %s" (ex-message e)))}
      (f))))

(def store
  "The store of the application database."
  (reify protocol/Store
    (select-by-pks [_ model pks columns]
      (let [pk (serialization.spec/primary-key model)]
        (protocol.util/in-batches #(serialization.db/rows-where-in model (protocol.util/with-keys columns [pk]) [[pk %]] pk) (remove nil? pks))))
    (select-by-identity [_ model identities columns]
      (select-by-identity model identities columns))
    (select-by-fks [_ model fks columns]
      (let [pk        (serialization.spec/primary-key model)
            owner-key (serialization.spec/owner-key model)]
        (protocol.util/in-batches #(serialization.db/rows-where-in model (protocol.util/with-keys columns [pk owner-key]) [[owner-key %]] pk)
                                  (remove nil? fks))))
    (insert-rows! [_ model rows]
      (into [] (mapcat #(serialization.db/insert-rows! model (vec %))) (partition-all serialization.util/batch-size rows)))
    (update-row! [_ model pk changes]
      (serialization.db/update-row! model (serialization.spec/primary-key model) pk changes))
    (delete-rows! [_ model pks]
      (let [pk (serialization.spec/primary-key model)]
        (run! #(serialization.db/delete-rows! model pk (vec %)) (partition-all serialization.util/batch-size pks))))
    (transact [_ {:keys [isolation retries]} f]
      (let [run (fn []
                  (if (= isolation :repeatable-read)
                    (serialization.db/do-in-repeatable-read-transaction f)
                    (serialization.db/do-in-transaction f)))]
        (if retries
          (with-retries retries run)
          (run))))))

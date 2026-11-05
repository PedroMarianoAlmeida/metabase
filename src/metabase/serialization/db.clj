(ns metabase.serialization.db
  "Application database queries for the serialization module."
  (:require
   [metabase.util.malli :as mu]
   [metabase.util.malli.registry :as mr]
   [toucan2.core :as t2]))

(set! *warn-on-reflection* true)

(mr/def ::row
  "A row of any serialized model, as its spec imports it; the model's own hooks and schemas check it."
  [:map {:closed false, ::mr/deliberately-open true, :description "row of a serialized model"}])

(mr/def ::value
  "A value a serialized row is looked up by."
  [:maybe [:or :int :string :boolean :keyword]])

(mu/defn rows-where-in :- [:sequential ::row]
  "The `model` rows, with only `columns` unless nil, whose every column of the `[column values]` pairs `in` holds one of
  its values, ordered by `order-by`."
  [model    :- :keyword
   columns  :- [:maybe [:sequential :keyword]]
   in       :- [:sequential [:tuple :keyword [:sequential {:min 1} ::value]]]
   order-by :- :keyword]
  (t2/select (if columns (into [model] columns) model)
             {:where    (into [:and] (map (fn [[k vs]] [:in k [:auto/param (vec vs)]])) in)
              :order-by [[order-by :asc]]}))

(mu/defn insert-rows! :- [:sequential ::row]
  "Insert `rows` of `model`, running its hooks, and return the inserted instances."
  [model :- :keyword
   rows  :- [:sequential ::row]]
  (t2/insert-returning-instances! model rows))

(mu/defn update-row!
  "Update the `model` row whose primary key `pk` is `id` with `changes`, running the model's hooks."
  [model   :- :keyword
   pk      :- :keyword
   id      :- [:or :int :string]
   changes :- ::row]
  (t2/update! model pk [:auto/param id] changes))

(mu/defn delete-rows!
  "Delete the `model` rows whose primary key `pk` is one of `ids`, running the model's hooks."
  [model :- :keyword
   pk    :- :keyword
   ids   :- [:sequential [:or :int :string]]]
  (t2/delete! model pk [:in [:auto/param ids]]))

(defn do-in-transaction
  "Calls `thunk` in a transaction of the app DB, or in a savepoint of the one already open."
  [thunk]
  (t2/with-transaction [_]
    (thunk)))

(defn do-in-repeatable-read-transaction
  "Calls `thunk` in a REPEATABLE READ transaction of the app DB, or in the transaction already open, whose isolation
  can't change without committing."
  [thunk]
  (t2/with-connection [^java.sql.Connection conn]
    (if-not (.getAutoCommit conn)
      (thunk)
      (let [isolation (.getTransactionIsolation conn)]
        (.setTransactionIsolation conn java.sql.Connection/TRANSACTION_REPEATABLE_READ)
        (try
          (t2/with-transaction [_ conn]
            (thunk))
          (finally
            (.setTransactionIsolation conn isolation)))))))

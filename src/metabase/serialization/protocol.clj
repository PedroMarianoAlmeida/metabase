(ns metabase.serialization.protocol
  "The storage serialization reads and writes rows through, in bulk, by the lookup columns the specs declare.")

(defprotocol Store
  "Rows of serialized models read and written in bulk, where a non-nil `columns` names the only columns a caller needs."
  (select-by-pks [store model pks columns]
    "The rows of `model` whose primary key is one of `pks`.")
  (select-by-identity [store model identities columns]
    "The rows of `model` whose `:identity` columns, in spec order, equal one of the tuples `identities`, oldest first.")
  (select-by-fks [store model fks columns]
    "The rows of `model` whose key to their owner, that of the one `:owned` transform naming `model`, is one of `fks`.")
  (insert-rows! [store model rows]
    "Inserts `rows` of `model`, running its hooks, and returns the inserted instances.")
  (update-row! [store model pk changes]
    "Updates the row of `model` whose primary key is `pk` with `changes`, running the model's hooks.")
  (delete-rows! [store model pks]
    "Deletes the rows of `model` whose primary key is one of `pks`, running the model's hooks.")
  (transact [store opts f]
    "What `f` returns when called in one transaction, or a savepoint inside one, with `opts` like `{:isolation :repeatable-read}` or `{:retries 3}`."))

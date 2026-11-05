(ns metabase.serialization.test-util
  "Stub serialization resolvers, for tests that should not touch the app DB."
  (:require
   [metabase.serialization.protocol.stub :as protocol.stub]
   [metabase.serialization.resolve :as resolve]))

(defn stub-export-resolver
  "An export resolver over [[protocol.stub/stub-store]] answering with the fns of `stubs`, with the options `opts`."
  ([stubs]
   (stub-export-resolver stubs {}))
  ([stubs opts]
   (resolve/resolver (protocol.stub/stub-store stubs) opts)))

(defn stub-import-resolver
  "An import resolver over [[protocol.stub/stub-store]] answering with the fns of `stubs`, with the options `opts`."
  ([stubs]
   (stub-import-resolver stubs {}))
  ([stubs opts]
   (resolve/resolver (protocol.stub/stub-store stubs) opts)))

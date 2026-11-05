(ns metabase.serialization.protocol.app-db-test
  (:require
   [clojure.test :refer :all]
   [metabase.serialization.protocol :as protocol]
   [metabase.serialization.protocol.app-db :as protocol.app-db]
   [metabase.serialization.resolve :as resolve]
   [metabase.test :as mt]
   [toucan2.core :as t2]))

(defn- query-count [thunk]
  (t2/with-call-count [call-count]
    (thunk)
    (call-count)))

(def ^:private export-resolver (resolve/resolver protocol.app-db/store))
(def ^:private import-resolver (resolve/resolver protocol.app-db/store {:create-missing? true}))

(deftest entity-ids-test
  (testing "exports the entity ids of existing rows, leaving out missing ones"
    (mt/with-temp [:model/Collection {coll-id :id coll-eid :entity_id} {}]
      (is (= {coll-id coll-eid}
             (resolve/entity-ids export-resolver :model/Collection [coll-id Integer/MAX_VALUE]))))))

(deftest field-values-test
  (mt/with-temp [:model/Database {db-id :id} {:name "serdes-v3-db"}]
    (is (= {db-id "serdes-v3-db"}
           (resolve/field-values export-resolver :model/Database :name [db-id Integer/MAX_VALUE])))))

(deftest table-refs-test
  (mt/with-temp [:model/Database {db-id :id}       {:name "serdes-v3-db"}
                 :model/Table    {table-id :id}    {:db_id db-id :name "users" :schema "public"}
                 :model/Table    {schemaless :id}  {:db_id db-id :name "events" :schema nil}]
    (is (= {table-id   ["serdes-v3-db" "public" "users"]
            schemaless ["serdes-v3-db" nil "events"]}
           (resolve/table-refs export-resolver [table-id schemaless Integer/MAX_VALUE])))))

(deftest field-refs-test
  (testing "exports nested Fields with one query per nesting level, whatever their number"
    (mt/with-temp [:model/Database {db-id :id}    {:name "serdes-v3-db"}
                   :model/Table    {table-id :id} {:db_id db-id :name "events" :schema "public"}
                   :model/Field    {outer-id :id} {:table_id table-id :name "outer"}]
      (let [field-ids (vec (for [i (range 30)]
                             (t2/insert-returning-pk! :model/Field {:table_id      table-id
                                                                    :parent_id     outer-id
                                                                    :name          (str "inner_" i)
                                                                    :base_type     :type/Text
                                                                    :database_type "TEXT"})))
            export    #(resolve/field-refs export-resolver %)]
        (is (= {outer-id          ["serdes-v3-db" "public" "events" "outer"]
                (first field-ids) ["serdes-v3-db" "public" "events" "outer" "inner_0"]}
               (select-keys (export (conj field-ids outer-id Integer/MAX_VALUE))
                            [outer-id (first field-ids) Integer/MAX_VALUE])))
        (is (= (query-count #(export (take 2 field-ids)))
               (query-count #(export field-ids))))))))

(deftest entity-pks-test
  (mt/with-temp [:model/Collection {coll-id :id coll-eid :entity_id} {}]
    (testing "imports entity ids, and a nested model's path ids by their last element, leaving out missing ones"
      (is (= {coll-eid           coll-id
              ["x" coll-eid]     coll-id}
             (resolve/entity-pks import-resolver :model/Collection [coll-eid ["x" coll-eid] "aaaaaaaaaaaaaaaaaaaaa"]))))))

(deftest user-ids-test
  (testing "finds existing Users and creates inactive ones for the missing emails"
    (mt/with-model-cleanup [:model/User]
      (let [rasta-id (mt/user->id :rasta)
            email    "serdes-v3-new@example.com"
            ids      (resolve/user-ids import-resolver ["rasta@metabase.com" email email])]
        (is (= rasta-id (get ids "rasta@metabase.com")))
        (is (= [email false]
               (t2/select-one-fn (juxt :email :is_active) [:model/User :email :is_active] :id (get ids email))))))))

(deftest database-and-table-ids-test
  (testing "finds existing Databases and Tables and creates stubs for missing ones"
    (mt/with-model-cleanup [:model/Database :model/Table]
      (mt/with-temp [:model/Database {db-id :id}    {:name "serdes-v3-db"}
                     :model/Table    {table-id :id} {:db_id db-id :name "users" :schema "public"}]
        (let [ids (resolve/table-ids import-resolver [["serdes-v3-db" "public" "users"]
                                                      ["serdes-v3-db" nil "fresh"]
                                                      ["serdes-v3-missing-db" "public" "users"]])]
          (is (= table-id (get ids ["serdes-v3-db" "public" "users"])))
          (is (=? {:db_id db-id :schema nil :name "fresh" :active false}
                  (t2/select-one :model/Table :id (get ids ["serdes-v3-db" nil "fresh"]))))
          (is (=? {:name "serdes-v3-missing-db" :is_stub true}
                  (t2/select-one :model/Database
                                 :id (:db_id (t2/select-one :model/Table
                                                            :id (get ids ["serdes-v3-missing-db" "public" "users"])))))))))))

(deftest field-ids-test
  (testing "finds existing Fields and creates each missing node of the paths once"
    (mt/with-model-cleanup [:model/Field]
      (mt/with-temp [:model/Database {db-id :id}    {:name "serdes-v3-db"}
                     :model/Table    {table-id :id} {:db_id db-id :name "events" :schema "public"}
                     :model/Field    {outer-id :id} {:table_id table-id :name "outer"}]
        (let [paths    [["serdes-v3-db" "public" "events" "outer"]
                        ["serdes-v3-db" "public" "events" "outer" "x"]
                        ["serdes-v3-db" "public" "events" "outer" "y"]
                        ["serdes-v3-db" "public" "events" "fresh" "z"]]
              resolved (resolve/field-ids import-resolver paths)]
          (is (= outer-id (get resolved (first paths))))
          (is (= #{outer-id}
                 (t2/select-fn-set :parent_id :model/Field :id [:in [(get resolved (nth paths 1))
                                                                     (get resolved (nth paths 2))]])))
          (is (= 5 (t2/count :model/Field :table_id table-id)))
          (testing "and resolves them again without creating anything, in as many queries for few paths as for many"
            (is (= resolved (resolve/field-ids import-resolver paths)))
            (is (= 5 (t2/count :model/Field :table_id table-id)))
            (is (= (query-count #(resolve/field-ids import-resolver (take 2 paths)))
                   (query-count #(resolve/field-ids import-resolver paths))))))))))

(deftest select-by-identity-test
  (testing "matches whole identity tuples, oldest first, in one query per batch"
    (mt/with-temp [:model/Database {db-id :id}  {:name "serdes-v3-db"}
                   :model/Table    {users :id}  {:db_id db-id :name "users" :schema "public"}
                   :model/Table    {other :id}  {:db_id db-id :name "users" :schema "other"}
                   :model/Table    {events :id} {:db_id db-id :name "events" :schema nil}]
      (let [select #(protocol/select-by-identity protocol.app-db/store :model/Table % [:id])]
        (is (= [users events]
               (map :id (select [[db-id "public" "users"] [db-id nil "events"] [db-id "public" "missing"]]))))
        (is (= [other] (map :id (select [[db-id "other" "users"]]))))
        (is (= 1 (query-count #(select [[db-id "public" "users"] [db-id nil "events"]]))))))))

(deftest select-by-fks-test
  (testing "selects the rows owned by the owners with the given ids, by the owner key of the :owned transform"
    (mt/with-temp [:model/Timeline      {timeline-id :id} {}
                   :model/TimelineEvent {event-id :id}    {:timeline_id timeline-id :name "e"
                                                           :timestamp   #t "2024-01-01T00:00:00Z"
                                                           :timezone    "UTC" :time_matters false}]
      (is (= [{:id event-id :timeline_id timeline-id}]
             (map #(select-keys % [:id :timeline_id])
                  (protocol/select-by-fks protocol.app-db/store :model/TimelineEvent [timeline-id] [:id])))))))

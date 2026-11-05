(ns metabase.serialization.extract-test
  (:require
   [clojure.test :refer :all]
   [metabase.serialization.core :as serialization]
   [metabase.serialization.extract :as serialization.extract]
   [metabase.serialization.resolve :as resolve]
   [metabase.serialization.spec :as serialization.spec]
   [metabase.serialization.test-util :as serialization.tu]))

(defmethod serialization/spec :model/SerdesV3TestEntity [_]
  {:schema    [:map {:closed true}
               [:serdes/meta :any]
               [:entity_id :string]
               [:name {:optional true} :string]
               [:archived {:optional true} :boolean]
               [:card {:optional true} [:maybe :string]]
               [:secret {:optional true} :string]]
   :copy      [:entity_id :name :archived]
   :skip      []
   :transform {:card_id {:as     :card
                         :export (fn [resolver _entities ids] (serialization/export-fks :model/Card resolver ids))
                         :import (fn [resolver _entities eids] (serialization/import-fks :model/Card resolver eids))}
               :secret  {:export (fn [_ entities values]
                                   (mapv (fn [entity v] (if (:public entity) v ::serialization.spec/skip))
                                         entities values))
                         :import (fn [_ _ values] values)}
               :size    {:export (fn [_ _ values] values)
                         :import (fn [_ _ values] values)}}
   :defaults  {:archived false}})

(defn- entity [n card-id & {:as extra}]
  (merge {:entity_id (format "%021d" n) :name (str "Entity " n) :archived false :card_id card-id} extra))

(defn- counting-resolver
  "An export resolver naming Card `n` `card-n` and finding no Card 404, counting its store calls in `calls`."
  [calls]
  (serialization.tu/stub-export-resolver
   {:calls calls
    :fk    (fn [id _model] (when-not (= id 404) (str "card-" id)))}))

(deftest ^:parallel extract-batch-test
  (testing "exports a batch with one resolver call, leaving out default values and skipped columns"
    (let [calls (atom [])]
      (is (= [{:serdes/meta [{:model "SerdesV3TestEntity"}]
               :entity_id   (format "%021d" 1)
               :name        "Entity 1"
               :card        "card-7"}
              {:serdes/meta [{:model "SerdesV3TestEntity"}]
               :entity_id   (format "%021d" 2)
               :name        "Entity 2"
               :archived    true
               :secret      "s"}]
             (serialization.extract/extract-batch :model/SerdesV3TestEntity (counting-resolver calls)
                                                  [(entity 1 7 :secret "hidden")
                                                   (entity 2 nil :archived true :secret "s" :public true)])))
      (is (= 1 (count @calls))))))

(deftest ^:parallel extract-batch-isolates-failures-test
  (testing "an entity whose reference has no target fails alone, and the rest of the batch still exports"
    (let [[ok failed] (serialization.extract/extract-batch :model/SerdesV3TestEntity (counting-resolver (atom []))
                                                           [(entity 1 7) (entity 2 404 :id 2)])]
      (is (= "card-7" (:card ok)))
      (is (instance? clojure.lang.ExceptionInfo failed))
      (is (=? {:model :model/SerdesV3TestEntity :id 2} (ex-data failed)))
      (is (resolve/target-not-found? (ex-cause failed))))))

(deftest ^:parallel extract-batch-validates-test
  (testing "an export that does not match the schema of the spec fails"
    (let [[failed] (serialization.extract/extract-batch :model/SerdesV3TestEntity (counting-resolver (atom [])) [(entity 1 7 :size 3)])]
      (is (re-find #"Invalid export of :model/SerdesV3TestEntity" (ex-message (ex-cause failed)))))))

(deftest ^:parallel deserialize-rows-test
  (testing "deserializes a batch, restoring defaults and renamed keys"
    (is (= [{:entity_id (format "%021d" 1) :name "Entity 1" :archived false :card_id 7 :size 3}]
           (serialization.spec/deserialize-rows :model/SerdesV3TestEntity
                                                (serialization.tu/stub-import-resolver {:fk (fn [eid _] (parse-long (subs eid 5)))})
                                                [{:entity_id (format "%021d" 1) :name "Entity 1" :card "card-7" :size 3}])))))

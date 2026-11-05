(ns metabase.serialization.spec-test
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer :all]
   [malli.core :as mc]
   [metabase.app-db.core :as mdb]
   [metabase.config.core :as config]
   [metabase.models.resolution :as models.resolution]
   [metabase.serialization.core :as serialization]
   [metabase.serialization.spec :as serialization.spec]
   [metabase.test :as mt]
   [metabase.test.fixtures :as fixtures]
   [metabase.util :as u]
   [metabase.util.connection :as u.conn]
   [metabase.util.malli.registry :as mr]
   [toucan2.core :as t2]))

(use-fixtures :once
  (fixtures/initialize :db)
  (fn [thunk]
    (doseq [[model model-ns] models.resolution/model->namespace
            :when (or config/ee-available? (not (str/starts-with? (str model-ns) "metabase-enterprise")))]
      (t2/resolve-model model))
    (thunk)))

(defn- schema-keys
  "The keys `schema` declares, across every branch of a `:multi`."
  [schema]
  (let [schema (mc/deref-all (mr/resolve-schema schema))]
    (set (if (= :multi (mc/type schema))
           (for [[_ _ branch] (mc/children schema)
                 [k]          (mc/entries (mc/deref-all branch))]
             k)
           (map first (mc/entries schema))))))

(defn- boolean-default [default-value]
  (when default-value
    (case (u/upper-case-en default-value)
      ("TRUE" "'TRUE'")   true
      ("FALSE" "'FALSE'") false
      nil)))

(defn- model-specs []
  (for [model (keys (dissoc (methods serialization/spec) :default))
        :when (isa? model :metabase/model)]
    [(name model) model (serialization/spec model)]))

(deftest specs-describe-every-column-test
  (mt/with-empty-h2-app-db!
    (doseq [[model-name model spec] (model-specs)]
      (testing model-name
        (let [columns    (u.conn/app-db-column-types (mdb/app-db) (t2/table-name model))
              declared   (concat (:copy spec) (:skip spec) (keys (:transform spec)) [:id :created_at :updated_at])
              serialized (set (concat (:copy spec)
                                      (for [[k t] (:transform spec)
                                            :when (not= serialization/skip-all (:export t))]
                                        (:as t k))))]
          (testing "declares each column once"
            (is (= (count declared) (count (set declared)))))
          (testing "declares every column"
            (is (empty? (set/difference (set (map (comp keyword u/lower-case-en) (keys columns))) (set declared)))))
          (testing "selects only its own columns"
            (is (empty? (set/difference (set (serialization/columns model))
                                        (set (map (comp keyword u/lower-case-en) (keys columns)))))))
          (testing "transforms both ways"
            (doseq [[k transform] (:transform spec)]
              (is (or (ifn? (:export transform)) (:owned transform)) (str k))
              (is (or (ifn? (:import transform)) (:owned transform)) (str k))))
          (testing "declares every serialized key in its schema"
            (is (empty? (remove (schema-keys (:schema spec)) serialized))))
          (testing "declares the boolean defaults of the columns it serializes"
            (doseq [[column {:keys [default]}] columns
                    :let  [k          (keyword (u/lower-case-en column))
                           db-default (boolean-default default)]
                    :when (and (some? db-default) (contains? serialized k))]
              (is (= db-default (get-in spec [:defaults k])) (str k)))))))))

(deftest validate-export!-test
  (let [spec {:schema [:map {:closed true} [:name :string]]}]
    (testing "export matching its spec's schema passes"
      (is (nil? (serialization.spec/validate-export! :model/Model spec {:name "x"}))))
    (testing "export with key its spec's schema does not declare throws"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid export of :model/Model"
                            (serialization.spec/validate-export! :model/Model spec {:name "x" :id 1}))))
    (testing "spec without schema throws"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No :schema"
                            (serialization.spec/validate-export! :model/Model {} {:name "x"}))))))

(deftest owned-models-have-one-owner-test
  (testing "each owned model is named by exactly one :owned transform, whose key is the owner key its lookups use"
    (doseq [[_ model spec] (model-specs)
            [_ {[owned-model owner-key] :owned}] (:transform spec)
            :when owned-model]
      (testing owned-model
        (is (= [[model owner-key]] (serialization.spec/owners owned-model)))
        (is (= owner-key (serialization.spec/owner-key owned-model)))))))

(deftest primary-keys-match-models-test
  (testing "the primary key of each spec is the first primary key of its model"
    (doseq [[model-name model] (model-specs)]
      (testing model-name
        (is (= (first (t2/primary-keys model)) (serialization.spec/primary-key model)))))))

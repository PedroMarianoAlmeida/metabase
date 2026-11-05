(ns metabase.serialization.resolve-test
  (:require
   [clojure.test :refer :all]
   [metabase.serialization.resolve :as resolve]
   [metabase.serialization.test-util :as serialization.tu]))

(defn- recording-resolver
  "An export resolver that names each Card id `Card_<id>`, records every store call in `calls`, and finds no Card 404."
  [calls]
  (serialization.tu/stub-export-resolver
   {:calls    calls
    :fk       (fn [id model] (when-not (= id 404) (str (name model) "_" id)))
    :fk-keyed (fn [id _model _field] (str "user" id "@example.com"))
    :table    (fn [id] ["DB" nil (str "T" id)])
    :field    (fn [id] ["DB" nil "T" (str "F" id)])}))

(deftest ^:parallel export-refs-test
  (testing "resolves each kind of reference with one store call per model, leaving out the references with no target"
    (let [calls (atom [])]
      (is (= {[:fk :model/Card 1]                  "Card_1"
              [:fk :model/Card 2]                  "Card_2"
              [:fk-keyed :model/User :email 7]     "user7@example.com"
              [:table 3]                           ["DB" nil "T3"]}
             (resolve/export-refs (recording-resolver calls)
                                  [[:fk :model/Card 1] [:table 3] [:fk :model/Card 2] [:fk :model/Card 1]
                                   [:fk-keyed :model/User :email 7] [:fk :model/Card 404]])))
      (is (= [[:select-by-pks :model/Card]
              [:select-by-pks :model/Table]
              [:select-by-pks :model/Database]
              [:select-by-pks :model/User]]
             @calls)))))

(deftest ^:parallel ref-value-test
  (let [resolved {[:fk :model/Card 1] "Card_1"}]
    (is (= "Card_1" (resolve/ref-value resolved [:fk :model/Card 1])))
    (testing "missing keyed lookup is nil"
      (is (nil? (resolve/ref-value resolved [:fk-keyed :model/User :email 2]))))
    (testing "any other missing target throws target-not-found error"
      (is (resolve/target-not-found?
           (try (resolve/ref-value resolved [:fk :model/Card 2])
                nil
                (catch clojure.lang.ExceptionInfo e e))))
      (is (=? {:model :model/Table :id 9}
              (ex-data (try (resolve/ref-value resolved [:table 9])
                            nil
                            (catch clojure.lang.ExceptionInfo e e))))))))

(deftest ^:parallel walk-in-bulk-test
  (testing "rewrites every form with one resolution of all their references"
    (let [calls (atom [])
          walk  (fn [visit ids] (mapv #(resolve/visit-some visit [:fk :model/Card %]) ids))]
      (is (= [["Card_1" nil] ["Card_2" "Card_1"]]
             (resolve/walk-in-bulk walk #(resolve/export-refs (recording-resolver calls) %) [[1 nil] [2 1]])))
      (is (= [[:select-by-pks :model/Card]] @calls))))
  (testing "resolves a reference the collecting walk missed because it threw before reaching it"
    (let [calls (atom [])
          walk  (fn [visit [a b]]
                  (let [x (visit [:fk :model/Card a])]
                    (when-not (string? x)
                      (throw (ex-info "needs a resolved value" {})))
                    [x (visit [:fk :model/Card b])]))]
      (is (= [["Card_1" "Card_2"] ["Card_3" "Card_4"]]
             (resolve/walk-in-bulk walk #(resolve/export-refs (recording-resolver calls) %) [[1 2] [3 4]])))
      (is (= [[:select-by-pks :model/Card]
              [:select-by-pks :model/Card]
              [:select-by-pks :model/Card]]
             @calls))))
  (testing "a missing target fails only the form that refers to it"
    (let [walk (fn [visit id]
                 (try (visit [:fk :model/Card id])
                      (catch clojure.lang.ExceptionInfo e (ex-message e))))]
      (is (= ["Card_1" "Card 404 was not found"]
             (resolve/walk-in-bulk walk #(resolve/export-refs (recording-resolver (atom [])) %) [1 404]))))))

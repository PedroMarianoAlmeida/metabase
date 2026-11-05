(ns metabase.serialization.visualization-settings-test
  (:require
   [clojure.test :refer :all]
   [metabase.serialization.resolve :as resolve]
   [metabase.serialization.test-util :as serialization.tu]
   [metabase.serialization.visualization-settings :as viz]))

(defn- export-one [stubs x]
  (first (viz/export-visualization-settings (serialization.tu/stub-export-resolver stubs) [x])))

(defn- import-one [stubs x]
  (first (viz/import-visualization-settings (serialization.tu/stub-import-resolver stubs) [x])))

(deftest ^:parallel export-visualization-settings-test
  (let [export (partial export-one {:field (constantly ["A" "B" "C" "D"])
                                    :fk    (fn [id model] (format "%s___%d" (name model) id))})]
    (is (= {:column_settings
            {"[\"ref\",[\"field\",[\"A\",\"B\",\"C\",\"D\"],null]]"
             {:column_title "Locus"
              :click_behavior
              {:type     "link"
               :linkType "question"
               :targetId "Card___1"
               :parameterMapping
               {"[\"dimension\",[\"field\",[\"A\",\"B\",\"C\",\"D\"],{\"source-field\":[\"A\",\"B\",\"C\",\"D\"]}]]"
                {:id     "[\"dimension\",[\"field\",[\"A\",\"B\",\"C\",\"D\"],{\"source-field\":[\"A\",\"B\",\"C\",\"D\"]}]]"
                 :source {:type "column", :id "Category_ID", :name "Category ID"}
                 :target {:type      "dimension"
                          :id        "[\"dimension\",[\"field\",[\"A\",\"B\",\"C\",\"D\"],{\"source-field\":[\"A\",\"B\",\"C\",\"D\"]}]]"
                          :dimension [:dimension [:field ["A" "B" "C" "D"] {:source-field ["A" "B" "C" "D"]}]]}}}}}}}
           (export
            {:column_settings
             {"[\"ref\",[\"field\",54,null]]"
              {:column_title "Locus"
               :click_behavior
               {:type     "link"
                :linkType "question"
                :targetId 1
                :parameterMapping
                {(keyword "[\"dimension\",[\"field\",54,{\"source-field\":53}]]")
                 {:id     "[\"dimension\",[\"field\",54,{\"source-field\":53}]]"
                  :source {:type "column", :id "Category_ID", :name "Category ID"}
                  :target {:type      "dimension"
                           :id        "[\"dimension\",[\"field\",54,{\"source-field\":53}]]"
                           :dimension ["dimension" [:field 54 {:source-field 53}]]}}}}}}})))))

(deftest ^:parallel import-old-timeline-events-settings-test
  (testing "source-instance numeric IDs cannot select unrelated destination timelines or hide their events"
    (let [settings {:graph.show_values                    true
                    :timeline.selected_timeline_ids       [7]
                    :timeline.excluded_timeline_event_ids [42]}]
      (is (= {:graph.show_values                    true
              :timeline.selected_timeline_ids       []
              :timeline.excluded_timeline_event_ids []}
             (select-keys (import-one {} settings) (keys settings)))))))

(defn- export-timelines [settings]
  (export-one {:fk (fn [id model] (format "%s___%d" (name model) id))} settings))

(deftest ^:parallel export-malformed-timeline-events-settings-test
  (testing "values that are not numeric IDs are dropped on export instead of failing it"
    (let [settings {:timeline.selected_timeline_ids       [1 "abc" nil]
                    :timeline.excluded_timeline_event_ids [2 -1 "xyz"]}]
      (is (= {:timeline.selected_timeline_ids       ["Timeline___1"]
              :timeline.excluded_timeline_event_ids [["Timeline___2" "TimelineEvent___2"]]}
             (select-keys (export-timelines settings) (keys settings)))))))

(deftest ^:parallel scalar-timeline-events-settings-test
  (testing "a setting holding a scalar instead of a list counts as no ids rather than failing export or import"
    (let [settings {:timeline.selected_timeline_ids       7
                    :timeline.excluded_timeline_event_ids true}
          cleared  {:timeline.selected_timeline_ids       []
                    :timeline.excluded_timeline_event_ids []}]
      (is (= cleared (select-keys (export-timelines settings) (keys settings))))
      (is (= cleared (select-keys (import-one {} settings) (keys settings)))))))

(deftest ^:parallel import-viz-settings-test
  (let [import (partial import-one {:field (constantly 3)})]
    (is (= {:column_settings
            {"[\"ref\",[\"field\",3,null]]"
             {:click_behavior
              {:parameterMapping
               {"[\"dimension\",[\"field\",3,{\"source-field\":3}]]"
                {:id     "[\"dimension\",[\"field\",3,{\"source-field\":3}]]"
                 :target {:type      "dimension"
                          :dimension [:dimension [:field 3 {:source-field 3}]]
                          :id        "[\"dimension\",[\"field\",3,{\"source-field\":3}]]"}}}}}}}
           (import
            {:column_settings
             {"[\"ref\",[\"field\",[\"my-db\",null,\"orders\",\"invoice\"],null]]"
              {:click_behavior
               {:parameterMapping
                {"[\"dimension\",[\"field\",[\"my-db\",null,\"orders\",\"invoice\"],{\"source-field\":[\"my-db\",null,\"orders\",\"subtotal\"]}]]"
                 {:id     "[\"dimension\",[\"field\",[\"my-db\",null,\"orders\",\"invoice\"],{\"source-field\":[\"my-db\",null,\"orders\",\"subtotal\"]}]]"
                  :target {:type      "dimension"
                           :dimension [:dimension
                                       [:field
                                        ["my-db" nil "orders" "invoice"]
                                        {:source-field ["my-db" nil "orders" "subtotal"]}]]
                           :id        "[\"dimension\",[\"field\",[\"my-db\",null,\"orders\",\"invoice\"],{\"source-field\":[\"my-db\",null,\"orders\",\"subtotal\"]}]]"}}}}}}})))))

(deftest ^:parallel visualizer-settings-lenient-import-test
  (testing "a lenient resolver leaves a dangling visualizer card reference in its portable form"
    (let [eid      "AAAAAAAAAAAAAAAAAAAAA"
          settings {:visualization {:columnValuesMapping {:COLUMN_1 [{:sourceId (str "card:" eid)}]}}}
          import   (fn [resolver] (first (viz/import-visualizer-settings resolver [settings])))]
      (is (= settings (import (resolve/lenient (serialization.tu/stub-import-resolver {:fk (constantly nil)})))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (import (serialization.tu/stub-import-resolver {:fk (constantly nil)})))))))

(deftest ^:parallel export-visualization-settings-in-bulk-test
  (testing "exporting many visualization settings resolves each kind of reference with one store call per model"
    (let [calls    (atom [])
          resolver (serialization.tu/stub-export-resolver
                    {:calls calls
                     :fk    (fn [id model] (str (name model) "_" id))
                     :field (fn [id] ["DB" "S" "T" (str "F" id)])})
          settings (for [i (range 1 4)]
                     {:column_settings {(str "[\"ref\",[\"field\"," i ",null]]")
                                        {:click_behavior {:type "link" :linkType "question" :targetId i}}}})]
      (is (= {:column_settings {"[\"ref\",[\"field\",[\"DB\",\"S\",\"T\",\"F3\"],null]]"
                                {:click_behavior {:type "link" :linkType "question" :targetId "Card_3"}}}}
             (last (viz/export-visualization-settings resolver settings))))
      (is (= [[:select-by-pks :model/Field] [:select-by-pks :model/Table] [:select-by-pks :model/Database]
              [:select-by-pks :model/Card]]
             @calls)))))

(deftest ^:parallel document-link-card-test
  (let [viz {:virtual_card {:display "link"}
             :link         {:entity {:id 2 :model "document" :name "My document"}}}]
    (testing "export converts linked document's id to portable reference"
      (is (= "Document___2"
             (get-in (export-one {:fk (fn [id model] (format "%s___%d" (name model) id))} viz)
                     [:link :entity :id]))))
    (testing "import resolves portable reference"
      (is (= 7
             (get-in (import-one {:fk (fn [eid _model] (when (= eid "abc") 7))}
                                 (assoc-in viz [:link :entity :id] "abc"))
                     [:link :entity :id]))))))

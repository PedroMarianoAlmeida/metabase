(ns metabase.dashboards.models.dashboard-card-series
  (:require
   [metabase.dashboards.schema :as dashboards.schema]
   [metabase.serialization.core :as serialization]
   [methodical.core :as methodical]
   [toucan2.core :as t2]))

(methodical/defmethod t2/table-name :model/DashboardCardSeries [_model] :dashboardcard_series)

(doto :model/DashboardCardSeries
  (derive :metabase/model)
  (derive :hook/dependencies))

;; Serialization

(defmethod serialization/spec :model/DashboardCardSeries [_]
  {:schema    ::dashboards.schema/external-dashboard-card-series
   :identity  [:dashboardcard_id :position]
   :copy      [:position]
   :transform {:dashboardcard_id serialization/import-only
               :card_id (serialization/fk-transform :model/Card)}})

(ns metabase.serialization.util
  "Helpers that serialization namespaces share.")

(set! *warn-on-reflection* true)

(def batch-size
  "The number of rows, ids or entities that serialization queries or processes together, under database parameter limits."
  1000)

(defn index-first
  "The map from `(key-fn row)` to `(val-fn row)` for `rows`, keeping the first row of each key."
  [key-fn val-fn rows]
  (reduce (fn [m row]
            (let [k (key-fn row)]
              (cond-> m
                (not (contains? m k)) (assoc k (val-fn row)))))
          {}
          rows))

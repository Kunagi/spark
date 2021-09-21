(ns spark.ctx
  (:require
   [spark.utils :as u]
   ))


(defn new-ctx
  ([m]
   (merge m (new-ctx)))
  ([]
   {:ctx.identity :ctx.identity}))


(defn ctx? [ctx]
  (boolean
   (and (map? ctx)
        (-> ctx :ctx.identity (= :ctx.identity)))))

(comment
  (ctx? {})
  (ctx? "nope")
  (ctx? (new-ctx)))


(defn assert-ctx [ctx]
  (u/assert (ctx? ctx)
            (str "Expected ctx, got ") (type ctx))
  ctx)

(comment
  (assert-ctx {})
  (assert-ctx "nope")
  (assert-ctx (new-ctx)))

(defn assert-key [ctx k predicate]
  (u/assert (predicate (get ctx k))
            (str "Predicate for key '" k "' in ctx failed."))
  ctx)

(comment
  (-> (new-ctx)
      (assoc :hello :world)
      (assert-key :hello #(= % :world))))

(defn => [ctx & fns]
  (reduce (fn [ctx-promise f]
            (-> ctx-promise
                (.then (fn [ctx]
                         (assert-ctx ctx)
                         (u/as> (f ctx))))))
          (u/as> ctx) fns))

(defn wrap> [ctx promise]
  (u/=> promise (fn [_] ctx)))

(defn assoc> [ctx ctx-key promise]
   (u/=> promise
         (fn [result]
           (if ctx-key
             (assoc ctx ctx-key result)
             ctx))))

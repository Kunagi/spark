(ns spark.ctx
  (:require
   [spark.utils :as u]
   ))


(defn new-ctx []
  {:ctx.identity :ctx.identity})

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

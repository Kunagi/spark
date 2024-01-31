(ns spark.dev.expectations
  (:require-macros [spark.dev.expectations :refer [expect]])
  (:require
   [spark.utils :as u]))


(defn expect> [expected provided expect-form test-form]
  (if (u/promise? provided)
    (-> provided
        (.then #(expect> expected % expect-form test-form)))
    (js/Promise.resolve
     (cond

       (fn? expected)
       (let [result (expected provided)]
         (if result
           provided
           (throw (ex-info "result does'n meet expectations"
                           {:expected expected
                            :provided provided
                            :predicate-result result
                            :expect-form expect-form
                            :test-form test-form
                            }))))

       :else
       (if (= expected provided)
         provided
         (throw (ex-info "result does'n meet expectations"
                         {:expected expected
                          :provided provided
                          :expect-form expect-form
                          :test-form test-form
                          })))))))

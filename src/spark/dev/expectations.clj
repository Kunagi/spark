(ns spark.dev.expectations)


(defmacro expect [expect-form test-form]
  `(-> (expect> ~expect-form ~test-form '~expect-form '~test-form)
       (.catch ~'devcard-catch)))

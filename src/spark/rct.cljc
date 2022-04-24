(ns spark.rct
  #?(:cljs (:require-macros [spark.rct :refer [def> testform> test> tests]]))
  (:require
   #?(:cljs [cljs.test :refer [deftest]])
   [promesa.core :as p]
   [spark.logging :refer [log]]))

(defonce AUTORUN (atom false))
(defn autorun? [] @AUTORUN)

(defn enable-autorun!
  ([]
   (enable-autorun! true))
  ([autorun-on?]
   (reset! AUTORUN autorun-on?)))

#?(:clj
   (defn compiler-option [k]
     (when cljs.env/*compiler*
       (get-in @cljs.env/*compiler* [:options k]))))

(defmacro def> [symbol init]
  (let [value (gensym "value")]
    `(do
       (def ~symbol nil)
       (p/let [~value ~init]
         (def ~symbol ~value)
         (log ~(keyword (str *ns*) "def>")
              :var '~symbol
              :val ~value
              :ns ~(str *ns*))
         ~value))))

(comment
  (def> _testvar (js/Promise.resolve 42)))

(def TESTED_FORMS (atom '()))

(defmacro testform> [form]
  (let [ret-val (gensym "ret-val_")
        ex (gensym "ex_")
        resolve (gensym "resolve_")
        reject (gensym "reject_")
        ret (gensym "ret_")]
    `(-> (p/create
          (fn [~resolve ~reject]
            (~resolve ~form)))
         (p/handle (fn [~ret-val ~ex]
                     (if ~ex
                       (let [~ret {:result :exception
                                   :exception ~ex
                                   :form '~form
                                   :ns ~(str *ns*)
                                   :id ~(random-uuid)}]
                         (log ::exception
                              :exception ~ex
                              :form '~form)
                         (swap! TESTED_FORMS conj ~ret)
                         ~ret)
                       (let [~ret {:result :success
                                   :val    ~ret-val
                                   :form '~form
                                   :ns ~(str *ns*)
                                   :id ~(random-uuid)}]
                         (log ::success
                              :val ~ret-val
                              :form '~form)
                         (swap! TESTED_FORMS conj ~ret)
                         ~ret)))))))

(comment
  (testform> (str 42))
  (macroexpand '(testform> (str 42))))

(defn- conj-form-result [acc result-p]
  (p/let [result result-p]
    (-> acc
        (update :forms conj result)
        (assoc :failed (-> result :result (not= :success) boolean)))))

(def TESTS (atom '()))

(defmacro test> [symbol & forms]
  (when (compiler-option :spark-tests)
    (let [acc (gensym "acc_")
          forms_ (->> forms
                      (map (fn [form]
                             `(p/then (fn [~acc]
                                        (if (-> ~acc :failed)
                                          (-> ~acc
                                              (update :forms conj
                                                      {:result :skipped
                                                       :form '~form
                                                       :ns ~(str *ns*)
                                                       :id ~(random-uuid)}))
                                          (conj-form-result
                                           ~acc (testform> ~form))))))))]
      `(do
         (cljs.test/deftest ~symbol
           (-> (p/resolved {:forms  []
                            :ns     ~(str *ns*)
                            :id     ~(random-uuid)
                            :failed false})
               ~@forms_
               (p/then (fn [~acc]
                         (swap! TESTS conj ~acc)))))
         (when (autorun?)
           (~symbol))))))


(do
   (log ::test>
        :autorun (autorun?))
  (test> dummy
         (assert (= 1 1))
         (js/console.log "%cyahoo!" "background-color: red; color: white; padding: 16px;")))

(comment
  (autorun?))

#?(:clj (defmacro tests [& body]))

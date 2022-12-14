(ns spark.ui.data-table
  (:require
   ["@mui/material" :as mui]
   [spark.utils :as u]
   [spark.env :as env]
   [spark.browser :as browser]
   [spark.money :as money]
   [spark.ui :as ui :refer [$ def-ui]]
   [clojure.string :as str]))

(def-ui FilterRowCell [col on-change]
  (ui/div
   {:min-width "90px"}
   ($ mui/TextField
      {:size "small"
       :value (or (-> col :filter-value) "")
       :onChange #(-> % .-target .-value on-change)
       :placeholder "Suche"
       :sx (clj->js {"& input" {"padding" "4px 0 4px 8px"
                                "font-size" "16px"}})
       :InputProps (when (u/non-blank-string (-> col :filter-value))
                     (clj->js {:endAdornment ($ mui/InputAdornment
                                                {:position "end"}
                                                ($ mui/IconButton
                                                   {:edge "end"
                                                    :onClick #(on-change nil)}
                                                   (ui/icon {:font-size "16px"
                                                             :color "#aaa"}
                                                            "cancel")))}))})))

(defn record-visible-for-col? [record col]
  (let [searchtext (u/non-blank-string (-> col :filter-value))
        words (->> (str/split searchtext #"\s")
                   (map str/trim)
                   (remove str/blank?)
                   (map str/lower-case))]
    (if (seq words)
      (let [value (get record (-> col :id))
            value (when value ((-> col :format) value))
            value (when value (str/lower-case value))]
        (u/string-includes-all? (str value) words))
      true)))

(defn record-visible-for-cols? [record cols]
  (reduce (fn [_ col]
            (if (record-visible-for-col? record col)
              true
              (reduced false)))
          true cols))

(defn filter-records [records cols]
  (->> records
       (filter #(record-visible-for-cols? % cols))))

(def-ui DataTable [table records
                   csv-filename
                   table-max-height]
  (let [record-id-getter (or (-> table :record-id-getter)
                             :id)
        record-on-click (-> table :record-on-click)
        footers-count (reduce (fn [c col]
                                (max c (-> col :footers count)))
                              0 (-> table :cols))

        [col-filters-values set-col-filters-values] (ui/use-state {})

        ;; prepare cols
        cols (->> table
                  :cols
                  (map-indexed (fn [idx col]
                                 (if (map? col)
                                   (let [type (-> col :type)
                                         align (or (-> col :align)
                                                   (case type
                                                     :eur :right
                                                     :number :right
                                                     :millis :right
                                                     :boolean :center
                                                     :x-on-true :center
                                                     :left))
                                         format (or (when-let [format (-> col :format)]
                                                      (if (fn? format)
                                                        format
                                                        (env/formatter-from-env format)))
                                                    (env/formatter-from-env type))
                                         record-key (or (-> col :record-key)
                                                        (-> col :id))
                                         no-wrap (contains? #{:eur :date :time :date+time :millis}
                                                            (-> col :type))
                                         col-id (or (-> col :id)
                                                    (str "_col_" idx))
                                         filter-value (get col-filters-values col-id)]
                                     (assoc col
                                            :id col-id
                                            :idx idx
                                            :align align
                                            :format format
                                            :record-key record-key
                                            :no-wrap no-wrap
                                            :filter-value filter-value))
                                   {:id (str "_col_" idx)
                                    :idx idx
                                    :label col
                                    :format str
                                    :record-key col}))))

        filter-row? (->> cols
                         (filter :filter)
                         first
                         boolean)

        records (if filter-row?
                  (filter-records records cols)
                  records)

        CsvDownloadButton (when (and csv-filename
                                     (seq records))
                            ($ ui/Button
                               {:text "Download CSV"
                                :on-click #(browser/initiate-text-download
                                            (or csv-filename "data.csv")
                                            (let [header-row (map (fn [col]
                                                                    (if (map? col)
                                                                      (-> col :label)
                                                                      (str col)))
                                                                  cols)
                                                  rows (into [header-row]
                                                             (map (fn [record]
                                                                    (map (fn [[col-idx col]]
                                                                           (let [record-key (-> col :record-key)
                                                                                 value (cond
                                                                                         (map? record) (get record record-key)
                                                                                         (vector? record) (get record col-idx)
                                                                                         :else record)]
                                                                             ((-> col :format) value)))
                                                                         (map-indexed vector cols)))
                                                                  records))]
                                              (u/csv-table rows)))
                                :size :small
                                :variant :text
                                :color :default}))]
    (ui/div
     {:class "DataTable"}
     ($ mui/Paper
        (ui/div
         {:display :grid
          :grid-template-rows "auto max-content"}
         ($ mui/TableContainer
            {:sx (ui/sx {:max-height (or table-max-height
                                         "80vh")})}

            ;; {:component mui/Paper}
            ;; (ui/div {:background-color "yellow" :height "100%"})
            ($ mui/Table
               {:stickyHeader true
                :size "small"}

               ($ mui/TableHead
                  ;; (ui/data footers-count)
                  ($ mui/TableRow
                     (for [col cols]
                       ($ mui/TableCell
                          {:key (or (-> col :id) (-> col :label))}
                          (ui/div
                           {:font-weight 900
                            :text-align (-> col :align)}
                           (-> col :label))))))

               ($ mui/TableBody

                  (when-let [rows (-> table :prefix-rows)]
                    (for [[row-idx row] (map-indexed vector rows)]
                      ($ mui/TableRow
                         {:key row-idx}
                         (for [[col-idx cell] (map-indexed vector row)]
                           ($ mui/TableCell
                              {:key col-idx}
                              (when cell
                                (if-let [component (-> cell :component)]
                                  component
                                  (ui/data cell))))))))

                  (when filter-row?
                    ($ mui/TableRow
                       (for [[col-idx col] (map-indexed vector cols)]
                         ($ mui/TableCell
                            {:key col-idx}
                            (when (-> col :filter)
                              ($ FilterRowCell {:col col
                                                :on-change #(set-col-filters-values
                                                             (assoc col-filters-values
                                                                    (-> col :id) %))}))))))

                  (for [[row-idx record] (map-indexed vector records)]
                    ($ mui/TableRow
                       {:key (or (record-id-getter record)
                                 (keyword "record-index" (str row-idx)))
                        :hover true
                        :onClick (when record-on-click
                                   #(record-on-click record))
                        :className (when record-on-click "CursorPointer")}
                       (for [[col-idx col] (map-indexed vector cols)]
                         (let [record-key (-> col :record-key)
                               value (cond
                                       (map? record) (get record record-key)
                                       (vector? record) (get record col-idx)
                                       :else record)
                               on-click (-> col :on-click)]
                           ($ mui/TableCell
                              {:key (or (-> col :id) (-> col :label))
                               :onClick (when on-click
                                          #(on-click record))
                               :className (when on-click "CursorPointer")}
                              (ui/div
                               {:text-align (-> col :align)
                                :white-space "nowrap"}
                               ((-> col :format) value))))))))

               ($ mui/TableHead
                  (for [footer-idx (range footers-count)]
                    ($ mui/TableRow
                       {:key footer-idx}
                       (for [col cols]
                         (let [footer (-> col :footers (get footer-idx))
                               record-key (-> col :record-key)
                               type (-> col :type)
                               value (-> footer :value)
                               value (cond
                                       (fn? value)
                                       (value records)

                                       (-> footer :type (= :count))
                                       (->> records
                                            (remove (fn [record]
                                                      (nil? (get record record-key))))
                                            count)

                                       (-> footer :type (= :sum))
                                       (let [aggregator (cond

                                                          (= type :millis)
                                                          +

                                                          (= type :number)
                                                          +

                                                          (= type :eur)
                                                          money/+

                                                          :else str)
                                             format (-> col :format)]
                                         (format (reduce (fn [result record]
                                                           (let [value (get record record-key)]
                                                             (aggregator result value)))
                                                         nil records))))]

                           ($ mui/TableCell
                              {:key (or (-> col :id) (-> col :label))}
                              (ui/div
                               {:font-weight 900
                                :text-align (cond
                                              (-> footer :type (= :count)) "right"
                                              :else (-> col :align))}
                               value)))))))))

         (when CsvDownloadButton
           (ui/div
            {:padding "8px 16px"}
            CsvDownloadButton)))))))

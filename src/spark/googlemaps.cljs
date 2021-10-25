(ns spark.googlemaps
  (:require
   [promesa.core :as p]

   ["@material-ui/core" :as mui]
   ["@material-ui/lab" :as mui-lab]
   ["lodash.throttle" :as throttle]
   ["autosuggest-highlight/parse" :as parse]

   [spark.logging :refer [log]]
   [spark.utils :as u]

   [spark.ui :as ui :refer [defnc $ def-ui def-ui-showcase]]

   [clojure.string :as str]))

;; * init

(def zoomlevel-continent 5)
(def zoomlevel-city 10)
(def zoomlevel-streets 15)

(def location-rinteln {:lat 52.1599958
                       :lng 8.9770099})

(defn load [api-key]
  (log ::load
       :api-key api-key)
  (let [script (js/document.createElement "script")]
    (set! (.-src script) (str "https://maps.googleapis.com/maps/api/js?key="
                              api-key
                              "&language=de"
                              "&libraries=places"))
    ;; (set! (.-defer script) true)
    (-> js/document .-head (.appendChild script))))

(defn places-service [target-element]
  (new (-> js/window .-google .-maps
                              .-places
                              .-PlacesService)
       target-element))

(comment
  (-> places-service))

(defn init-map [map-element-id map-config]
  (let [e (js/document.getElementById map-element-id)
        map (js/google.maps.Map. e (clj->js map-config))]
    map))

(defonce marker-atom (atom #{}))

(defn create-marker
  [map props]
  (log ::create-marker
       :map map
       :props props)
  (let [on-click (-> props :on-click)
        props    (-> props
                     (dissoc :on-click)
                     (assoc :map map)
                     (assoc :animation js/google.maps.Animation.DROP))
        marker   (js/google.maps.Marker. (clj->js props))]
    (when on-click
      (-> ^js marker (.addListener "click" on-click)))
    marker))


;; * Lokationen aus Google

(defn find-place-from-phone-number> [phone fields]
  (log ::find-place-from-phone-number
       :phone phone
       :fields fields)
  (u/promise>
   (fn [resolve reject]
     (-> ^js (places-service (js/document.createElement "div"))
         (.findPlaceFromPhoneNumber
          (clj->js {:phoneNumber phone
                    :fields (into #{"place_id"} fields)})
          (fn [results status]
            (log ::find-place-from-phone-number--results
                 :status status
                 :results results)
            (if (= status "OK")
              (resolve (js->clj (first results) :keywordize-keys true))
              (reject status))))))))

(comment
  (-> (find-place-from-phone-number> "+4915774737908" #{"geometry"}))
  )

(defn place-details> [place-id fields]
  (log ::place-details
       :place place-id
       :fields fields)
  (u/promise>
   (fn [resolve reject]
     (-> ^js (places-service (js/document.createElement "div"))
         (.getDetails (clj->js {:placeId place-id
                                :fields fields})
                      (fn [result status]
                        (log ::place-details--result
                             :status status
                             :result result)
                        (if (= status "OK")
                          (resolve (js->clj result :keywordize-keys true))
                          (reject status))
                        ))))))

(comment
  (u/tap>
   (place-details> "ChIJMZ5c1DV7ukcRto8Ta4_ygzs" #{"rating"}))
  )

(defn use-place-details [place-id fields]
  (let [[details set-details] (ui/use-state nil)]

    (ui/use-effect
     [place-id fields]
     (p/let [result (place-details> place-id fields)]
       (set-details result))
     nil)
    details))

(comment
  
  (def testdiv (js/document.createElement "div"))
  (.setAttribute testdiv "id" "testdiv")
  (-> js/document .-body (.appendChild testdiv))
  (def placesService (new (-> js/window .-google .-maps
                              .-places
                              .-PlacesService) testdiv))

  (-> placesService
      (.nearbySearch (clj->js {:location location-rinteln
                               :radius   5000
; :type "restaurant" ;TODO: Better filter later
                               })
                     #(js/console.log
                       "google sent: "
                       (str
                        (->> (js->clj
                              % :keywordize-keys true)
                             (mapv (fn [place]
                                     (select-keys place [:types :name :place_id])))))))))


(defn- google-place-to-marker-props
  ""
  [{:keys [name geometry]}]
  (let [location-obj (:location geometry)]
    {:title    name
     :label    name
     :icon     {:url         "/img/grey_map_marker.png"
                :labelOrigin (new js/google.maps.Point 16 -6)}
     :position {:lat ^js (.lat location-obj) 
                :lng ^js (.lng location-obj)}}))


;; * MapWithPosition
;; https://developers.google.com/maps/documentation/javascript/adding-a-google-map

(defn init-default-map
  ""
  [element-id position]
  (init-map element-id
            {:center            position
             :zoom              zoomlevel-streets
             :streetViewControl false
             :styles            [
                                 {:featureType "poi"
                                  :stylers     [{:visibility "off"}]}
                                 ]}))


(def-ui MapWithPosition
  [id
   height
   markers
   position
   lokationen
   google-types]
  (assert position)
  (let [position                          (if (map? position)
                                            (clj->js position)
                                            position)

        position-marker {:title "Du bist hier"
                         :label "Du"
                         :position position}

        fetch-google-markers
        (ui/use-memo
         [] (throttle
             (fn [placesService callback]
               ^js (.nearbySearch placesService (clj->js {:location position
                                                          :radius   5000})
                                  (fn [results]
                                    (log ::nearby-search--result
                                         :results results)
                                    ;; (->> results js->clj
                                    ;;      (map (fn  [result]
                                    ;;             (js/console.log result))))
                                    (callback results))))))
        map-element-id                    (or id "map")
        [gmap set-gmap]                   (ui/use-state nil)
        [placesService set-placesService] (ui/use-state nil)
        [all-markers set-all-markers]     (ui/use-state nil)]

    (ui/use-effect
     [lokationen]
     (when-not gmap
       (let [new-gmap (init-default-map id position)]
         (set-gmap new-gmap)
         (set-placesService
          (new (-> js/window .-google .-maps
                   .-places .-PlacesService) new-gmap)))))

    (ui/use-effect
     [gmap lokationen]
     (when gmap
       (doseq [marker all-markers]
         ^js (.setMap marker nil))
       (fetch-google-markers
        placesService #(let [google-markers
                             (->> (js->clj % :keywordize-keys true)
                                  (keep
                                   (fn [place]
                                     (if (some google-types
                                               (:types place))
                                       (google-place-to-marker-props place)))))]
                         (set-all-markers (doall
                                           (map
                                            (partial create-marker gmap)
                                            (concat google-markers
                                                    markers
                                                    [position-marker]))))))))

    ($ :div
       {:id    map-element-id
        :style {:height (or height "40vh")}})))

(def-ui-showcase ::MapWithPosition
  ($ :div {:style {:width "400px"}}
     ($ MapWithPosition
        {:id           "test-MapWithPosition"
         :height       "400px"
         :google-types {"restaurant"
                        "bar"
                        "cafe"
                        "meal_takeaway"
                        "meal_delivery" ;TODO: mark as delivery?
                        "night_club"}
         :position     {:lat 52.1875305
                        :lng 9.0788149}
         :markers      [{:title    "Bodega"
                         :label    "Bodega"
                         :position {:lat 52.1875305
                                    :lng 9.0788149}}]})))

;; * Geocoding

(defn- coerce-geocode-results [^js results status]
  (when (= status "OK")
    (when-let [^js result (aget results 0)]
      (let [^js location (-> result .-geometry .-location)]
        {:lat (-> location .lat)
         :lng (-> location .lng)}))))

;; https://developers.google.com/maps/documentation/javascript/reference/geocoder#GeocoderRequest

(defn geocode-place-id> [place-id]
  (log ::geocode-place-id>
       :place-id place-id)
  (js/Promise.
   (fn [resolve reject]
     (let [geocoder (js/google.maps.Geocoder.)
           options  {:placeId place-id}]
       (.geocode
        ^js geocoder
        (clj->js options)
        (fn [^js results status]
          (if (= status "OK")
            (if-let [^js result (aget results 0)]
              (let [^js location (-> result .-geometry .-location)]
                (resolve {:lat (-> location .lat)
                          :lng (-> location .lng)}))
              (reject {:message "no results"}))
            (reject results))))))))

(defn geocode-address> [address]
  (log ::geocode-address
       :address address)
  (js/Promise.
   (fn [resolve reject]
     (let [geocoder (js/google.maps.Geocoder.)
           options  {:address address}]
       (.geocode
        ^js geocoder
        (clj->js options)
        (fn [^js results status]
          (when-let [result (coerce-geocode-results results status)]
            (resolve result)
            (reject results))))))))

(comment
  (u/=> (geocode-address> "Marschenhausweg 3, 27580 Bremerhaven")
        u/tap>))


;; * Standorteingabe

(def AUTOCOMPLETE_SERVICE (atom nil))

(defn autocomplete-service []
  (if-let [as @AUTOCOMPLETE_SERVICE]
    as
    (let [as (new (-> js/window .-google .-maps
                      .-places .-AutocompleteService))]
      (reset! AUTOCOMPLETE_SERVICE as)
      as)))

(defn get-place-predictions [input callback]
  (log ::get-place-predictions>
       :input input)
  (if (-> input str/blank?)
    (callback [])
    (-> ^js (autocomplete-service)
        (.getPlacePredictions
         ;; https://developers.google.com/maps/documentation/javascript/reference/places-autocomplete-service#AutocompletionRequest
         (clj->js {:input                 input
                   :componentRestrictions {:country ["de"]}
                   :language              "de"})
         #(callback (into [] %))))))

(def get-place-predictions-throtteled (throttle get-place-predictions 300))

(comment
  (get-place-predictions "Kö" tap>)
  (get-place-predictions-throtteled "Kö" tap>))

;;
;; Mocking to replace the actual google call if needed
;;

(defn rand-str [len]
  (.toLowerCase
   (apply str (take len (repeatedly #(char (+ (rand 26) 65))))))) 


(defn options-mock-effect 
  ""
  [set-options input-value value fetch]
  (let [c           (count input-value)
        new-options (conj (map #(str input-value %)
                               (take (Math/ceil (/ 6 (if (>= c 1) c 1)))
                                     (repeatedly
                                      #(rand-str (if (< c 6)
                                                   (Math/ceil (- 6 c))
                                                   1))))) "")]
    (set-options
     (into [ ] new-options))))

;; ** Helpers for PositionInput

(defn render-google-option
  "Renders the Dropdown Options containing the google places"
  [option]
  (if (= option "powered-by-google")
    (ui/div
     {:display :flex
      :place-content :center
      :place-items :center
      :padding 4
      :width "100%"}
     ($ :img
        {:src "/img/google/powered_by_google_on_white.png"})
     )
    (let [option
          (js->clj option :keywordize-keys true)
          matches   (get-in option
                            [:structured_formatting
                             :main_text_matched_substrings])
          main-text (get-in option [:structured_formatting
                                    :main_text])
          parts     (js->clj
                     (parse main-text
                            (clj->js
                             (map
                              (fn [{:keys [offset length]}]
                                [offset (+ length offset)])
                              matches))) :keywordize-keys true)]
      ($ mui/Grid
         {:container  true
          :alignItems "center"}
         ($ mui/Grid
            {:item true
             :xs   2}
            (ui/icon "location_on"))
         ($ mui/Grid
            {:item true
             :xs   10}
            (map-indexed (fn [i {:keys [text highlight]}]
                           ($ :span
                              {:key   i
                               :style {:fontWeight (if highlight 700 400)}} text)) parts)
            ($ mui/Typography
               {:variant "body2" :color "textSecondary"}
               (get-in option
                       [:structured_formatting
                        :secondary_text])))))))


(def-ui PositionInput [on-place-selected label placeholder auto-focus]
  (let [[input-value set-input-value] (ui/use-state "")
        [options set-options]         (ui/use-state [])
        update-input-value (fn [value]
                             (set-input-value (when-not (= value "powered-by-google")
                                                value)))]

    ($ mui-lab/Autocomplete
       {:getOptionLabel     #(if (string? %) % (.-description %))
        :options            (clj->js (conj options "powered-by-google")) ; warum manuell?
        :autoComplete       true
        :includeInputInList true
        :getOptionSelected  #(= (get %1 "place_id")
                                (get %2 "place_id"))
        :value              input-value
        :onChange           (fn [^js event ^js new-value]
                              (when (and new-value
                                         (.hasOwnProperty new-value "description")
                                         (-> new-value .-description))
                                (update-input-value (.-description new-value))
                                (when on-place-selected
                                  (on-place-selected
                                   {:place_id    (-> new-value .-place_id)
                                    :description (-> new-value .-description)
                                    }))
                                ))
        :onInputChange      (fn [^js _event new-input-value]
                              (update-input-value new-input-value)
                              (when-not (-> new-input-value str/blank?)
                                (get-place-predictions-throtteled
                                 new-input-value set-options)))
        :fullWidth          true
        :renderInput        (fn [props]

                              ;; Workaround fixes damaging of styles of other components
                              (-> props .-InputProps .-endAdornment (set! js/undefined))

                              (-> props .-InputProps .-startAdornment
                                  (set! (ui/icon
                                         {:color "#9a958f"}
                                         "place")))

                              ($ mui/TextField
                                 {
                                  :label       label
                                  :placeholder placeholder
                                  ;; :onChange #(-> % .-target .-value set-ort)
                                  :variant     "outlined"
                                  :type        "text"
                                  :autoFocus   auto-focus
                                  :&           props}))
        ;; :renderOption #(str (js->clj %))
        :renderOption render-google-option})))

 
(def-ui-showcase ::PositionInput ;TODO: Write Proper test?
  ($ PositionInput {:set-position js/alert}))


;; * Map

(def-ui-showcase ::PositionInput
  ($ PositionInput {:set-position js/alert}))


(def-ui Map
  [id height markers google-types
   force-position-input? lokationen]
  (let [[position set-position] (ui/use-state (when-not force-position-input?
                                                :loading))]

    (ui/use-effect
     :once
     (when-not force-position-input?
       (js/setTimeout
        #(js/navigator.geolocation.getCurrentPosition
          (fn [^js position]
            (log ::Map.position
                 :position position)
            (set-position {:lat (-> position .-coords .-latitude)
                           :lng (-> position .-coords .-longitude)}))
          (fn [error]
            (set-position nil)
            (log ::Map.Error
                 :error error))) 1000))
     nil)

    (case position
      :loading ($ :div
                  {:style {:height        height
                           :display       :flex
                           :place-content "center"
                           :place-items   "center"}}
                  ($ mui/CircularProgress))
      nil      ($ :div
                  {:style {:height        height
                           :display       :flex
                           :place-content "center"
                           :place-items   "center"}}
                  ($ PositionInput
                     {:set-position set-position}))
      ($ MapWithPosition
         {:id           id
          :height       height
          :markers      markers
          :position     position
          :lokationen   lokationen
          :google-types google-types}))))

(def-ui-showcase ::Map
  (ui/stack
   ($ :div {:style {:width "300px"}}
      ($ Map
         {:id "test-Map1"
          :height "200px"
          :markers [{:title "A Title" :label "A Label"
                     :position {:lat 52.1875305 :lng 9.0788149}}]}))
   ($ :div {:style {:width "300px"}}
      ($ Map
         {:id "test-Map2"
          :height "200px"
          :force-position-input? true
          :markers [{:title "A Title" :label "A Label"
                     :position {:lat 52.1875305 :lng 9.0788149}}]}))))


(defn compute-distance-text> [origin destination]
  (let [service (js/google.maps.DistanceMatrixService.)
        config  {:origins      [origin]
                 :destinations [destination]
                 :travelMode   "DRIVING"}]
    (js/Promise.
     (fn [resolve reject]
       (log ::compute-distance-text>
            :origin origin
            :destination destination)
       (.getDistanceMatrix service (clj->js config)
                           (fn [^js response _status]
                             (let [distance (-> response
                                                js->clj
                                                (get "rows")
                                                first
                                                (get "elements")
                                                first
                                                (get "distance")
                                                (get "text"))]
                               (log ::compute-distance.result
                                    :distance distance)
                               (resolve distance))))))))

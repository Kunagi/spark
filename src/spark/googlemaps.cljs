(ns spark.googlemaps
  (:require

   ["@material-ui/core" :as mui]
   ["@material-ui/lab" :as mui-lab]
   ["lodash.throttle" :as throttle]
   ["autosuggest-highlight/parse" :as parse]
   [spark.logging :refer [log]]
    
   [spark.ui :as ui :refer [defnc $ def-ui-test]]
   ))


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


(defn init-map [map-element-id map-config]
  (let [e (js/document.getElementById map-element-id)
        map (js/google.maps.Map. e (clj->js map-config))]
    map))

(def marker-atom (atom #{}))

(defn create-marker
  [map props]
  (log ::create-marker
       :map map
       :props props)
  (let [marker (-> props
                   (assoc :map map)
                   (assoc :animation js/google.maps.Animation.DROP))]
    (js/google.maps.Marker. (clj->js marker))))


;; #########################################
;; ######### Lokationen aus Google #########
;; #########################################



(comment
  
  (def testdiv (js/document.createElement "div"))
  (.setAttribute testdiv "id" "testdiv")
  (-> js/document .-body (.appendChild testdiv))
  (def placesService (new (-> js/window .-google .-maps
                              .-places
                              .-PlacesService) testdiv))

  (-> placesService
      (.nearbySearch (clj->js {:location location-rinteln
                               :radius 5000
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
    {:title name
     :label name
     :icon {:url "/img/grey_map_marker.png"
            :labelOrigin (new js/google.maps.Point 16 -6)}
     :position {:lat ^js (.lat location-obj) 
                :lng ^js (.lng location-obj)}}))


;; #############################
;; ###### MapWithPosition ######
;; ############################# 


(defn init-default-map
  ""
  [element-id position]
  (init-map element-id
                 {:center position
                  :zoom 10
                  :streetViewControl false
                  :styles [
                           {:featureType "poi"
                            :stylers [{:visibility "off"}]}
                           ]}))

(defnc MapWithPosition 
  [{:keys [id
           height
           markers
           position
           lokationen
           google-types]}]
  (assert position)
  (let [map-element-id (or id "map")
        
        [gmap set-gmap] (ui/use-state nil)
        [all-markers set-all-markers] (ui/use-state nil)]

    (ui/use-effect
     [lokationen]
     (if-not gmap (set-gmap (init-default-map id position))))

    (ui/use-effect
     [gmap lokationen]
     (when gmap
       (let [placesService
             (new (-> js/window .-google .-maps
                      .-places .-PlacesService) gmap)]
         (doseq [marker all-markers]
                 ^js (.setMap marker nil))
         ^js (.nearbySearch placesService (clj->js {:location position
                                                :radius 5000})
                        #(let [google-markers (->> (js->clj % :keywordize-keys true)
                                                   (keep
                                                    (fn [place]
                                                      (if (some google-types
                                                                (:types place))
                                                        (google-place-to-marker-props place)))))]
                           (set-all-markers (doall
                                             (map
                                              (partial create-marker gmap)
                                              (concat google-markers markers)))))))))

    ($ :div
       {:id map-element-id
        :style {:height (or height "40vh")}})))

(def-ui-test [MapWithPosition]
  ($ :div {:style {:width "400px"}}
     ($ MapWithPosition
        {:id "test-MapWithPosition"
         :height "400px"
         :google-types {"restaurant"
                        "bar"
                        "cafe"
                        "meal_takeaway"
                        "meal_delivery"  ;TODO: mark as delivery?
                        "night_club"}
         :position {:lat 52.1875305
                    :lng 9.0788149}
         :markers [{:title "Bodega"
                    :label "Bodega"
                    :position {:lat 52.1875305
                               :lng 9.0788149}}]})))


(defn geocode-address> [address]
  (log ::geocode-address
       :address address)
  (js/Promise.
   (fn [resolve reject]
     (let [geocoder (js/google.maps.Geocoder.)
           options {:address address}]
       (.geocode
        ^js geocoder
        (clj->js options)
        (fn [^js results status]
          (if (= status "OK")
            (resolve results)
            (reject results))))))))


;; #############################
;; ###### Standorteingabe ###### 
;; #############################


(def autocompleteService (clj->js {:current nil}))

;; Places-API Experimentation

(comment
 (set! (.. autocompleteService -current) 
       (new (-> js/window .-google .-maps
                .-places .-AutocompleteService)))
 (.-current autocompleteService)
 ^js/window.google.maps.places.AutocompleteService
 (->  
  autocompleteService
  .-current
  (.getPlacePredictions (clj->js {:input "Kö"})
                        #(js/console.log "google Places said " %))))

;;
;; Mocking to replace the actual google call if needed
;;
 
(defn rand-str [len]
  (.toLowerCase
   (apply str (take len (repeatedly #(char (+ (rand 26) 65))))))) 


(defn options-mock-effect 
  ""
  [set-options input-value value fetch]
  (let [c (count input-value)
        new-options (conj (map #(str input-value %)
                          (take (Math/ceil (/ 6 (if (>= c 1) c 1)))
                                (repeatedly
                                 #(rand-str (if (< c 6)
                                              (Math/ceil (- 6 c))
                                              1))))) "")]
    (set-options
     (into [ ] new-options))))

;;
;; Helpers for PositionInput
;; 

(defn options-from-google-effect
  "Effect that makes a call to the google places Autocomplete-API"
  [set-options input-value ort fetch] 
  (fn []
    (let [active? (atom true)]
      (if (and (not (.-current autocompleteService))
               js/window.google.maps.places)
        (set! (.. autocompleteService -current)  ;TODO: factor out the side-effect??
              (new (-> js/window .-google .-maps
                       .-places .-AutocompleteService))))

      (cond
        (not (.-current autocompleteService)) nil
        (empty? input-value) (do (set-options (if ort [ort] []))
                                 nil)
        :else
        (fetch (clj->js {:input input-value
                         :componentRestrictions {:country ["de"]}
                         :language "de"})
               (fn [results] ; this is the callback we hand to the API-call
                 ;; (js/console.log "google said " (js->clj results :keywordize-keys true))
                 (if @active?
                   (set-options
                    (into
                     (if ort [ort] [])
                     results))))))
      #(reset! active? false))))


(defn render-google-option
  "Renders the Dropdown Options containing the google places"
  [option]
  (let [option
        (js->clj option :keywordize-keys true)                                    
        matches (get-in option 
                        [:structured_formatting
                         :main_text_matched_substrings])
        main-text (get-in option [:structured_formatting
                                  :main_text])
        parts (js->clj
               (parse main-text
                      (clj->js
                       (map
                        (fn [{:keys [offset length]}]
                          [offset (+ length offset)])
                        matches))) :keywordize-keys true)]
    ($ mui/Grid
       {:container true
        :alignItems "center"}
       ($ mui/Grid
          {:item true
           :xs 2}
          (ui/icon "location_on"))
       ($ mui/Grid
          {:item true
           :xs 10}                                      
          (map-indexed (fn [i {:keys [text highlight]}]
                         ($ :span
                            {:key i
                             :style {:fontWeight (if highlight 700 400)}} text)) parts)
          ($ mui/Typography
             {:variant "body2" :color "textSecondary"}
             (get-in option 
                     [:structured_formatting
                      :secondary_text])))))) 


(defnc PositionInput [{:keys [set-position]}]
  (let [fetch (ui/use-memo
               [] (throttle 
                   (fn [request callback]
                     ^js (. (.-current autocompleteService)
                            getPlacePredictions request callback)
                              ;; (->  autocompleteService ; Don't know where to put Metadata in threading macro
                              ;;      .-current
                              ;;      (.getPlacePredictions
                              ;;       request callback))
                     ) 200))
        [input-value set-input-value] (ui/use-state "")
        [options set-options] (ui/use-state [])
        [ort set-ort] (ui/use-state
                       (js/window.localStorage.getItem "standort"))]
    
    
    (ui/use-effect
     [ort input-value fetch] ; runs when any of these change
     (options-from-google-effect set-options input-value ort fetch))
    
    ($ :div
       ($ :form
          {:style {:width "300px"}
           :onSubmit (fn [^js event]
                       (-> event .preventDefault)
                       (js/window.localStorage.setItem "standort" ort)
                       (when ort 
                         (-> (geocode-address> ort)
                             (.then #(set-position (-> %
                                                       first
                                                       .-geometry
                                                       .-location
                                                       js->clj)))))
                       false)}          
          ($ mui-lab/Autocomplete
             {:getOptionLabel #(if (string? %) % (.-description %))
              :options (clj->js options) ; warum manuell?
              :autoComplete true
              :includeInputInList true
              :getOptionSelected #(= (get %1 "place_id")
                                     (get %2 "place_id"))
              :value ort
              :onChange (fn [event new-value]
                          (set-options (if new-value (into [new-value] options)
                                           options))
                          (if (.-description new-value)
                           (set-ort (.-description new-value))))
              :onInputChange (fn [event new-input-value]
                               (set-input-value new-input-value))
              :renderInput (fn [params]
                             ($ mui/TextField
                                {:label "Bitte Standort eingeben"
                                 ;; :onChange #(-> % .-target .-value set-ort)
                                 :variant "outlined"
                                 :type "text"
                                 :autoFocus true
                                 :& params}))
              ;; :renderOption #(str (js->clj %))
              :renderOption render-google-option})))))

 
(def-ui-test [PositionInput] ;TODO: Write Proper test?
   ($ PositionInput {:set-position js/alert}))


;; #########
;; ## Map ##
;; #########

(def-ui-test [PositionInput]
  ($ PositionInput {:set-position js/alert}))


(defnc Map
  [{:keys [id height markers google-types
           force-position-input? lokationen]}]
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
                  {:style {:height height
                           :display :flex
                           :place-content "center"
                           :place-items "center"}}
                  ($ mui/CircularProgress))
      nil ($ :div
             {:style {:height height
                      :display :flex
                      :place-content "center"
                      :place-items "center"}}
             ($ PositionInput
                {:set-position set-position}))
      ($ MapWithPosition
         {:id id
          :height height
          :markers markers
          :position position
          :lokationen lokationen
          :google-types google-types}))))

(def-ui-test [Map]
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
                    :position {:lat 52.1875305 :lng 9.0788149}}]})))


(defn compute-distance-text> [origin destination]
  (let [service (js/google.maps.DistanceMatrixService.)
        config {:origins [origin]
                :destinations [destination]
                :travelMode "DRIVING"}]
    (js/Promise.
     (fn [resolve reject]
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

(ns commons.googlemaps
  (:require

   ["@material-ui/core" :as mui]

   [commons.logging :refer [log]]
    
   [commons.mui :as ui :refer [defnc $]]
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
                              api-key))
    ;; (set! (.-defer script) true)
    (-> js/document .-head (.appendChild script))))


(defn init-map [map-element-id map-config]
  (let [e (js/document.getElementById map-element-id)
        map (js/google.maps.Map. e (clj->js map-config))]
    map))


(defn create-marker
  [map props]
  (log ::create-marker
       :map map
       :props props)
  (let [marker (-> props
                   (assoc :map map)
                   (assoc :animation js/google.maps.Animation.DROP))]
    (js/google.maps.Marker. (clj->js marker))))


(defnc MapWithPosition
  [{:keys [id
           height
           zoom
           markers
           position]}]
  (let [map-element-id (or id "map")
        [gmap set-gmap] (ui/use-state nil)
        [old-markers set-old-markers] (ui/use-state nil)]

    (ui/use-effect
     :always
     (when-not (= markers old-markers)
       (set-old-markers markers)
       (log ::Map.init-map)
       (let [gmap (init-map
                   map-element-id
                   {:center position
                    :zoom (or zoom 10)})]
         (set-gmap gmap)
         (doseq [marker markers]
           (create-marker gmap marker))))
     nil)

    ($ :div
       {:id map-element-id
        :style {:height (or height "40vh")}})))


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


(defnc PositionInput [{:keys [set-position]}]
  (let [[ort set-ort] (ui/use-state (js/window.localStorage.getItem "standort"))]
    ($ :div
       ($ :form
          {:onSubmit (fn [^js event]
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
          ($ mui/TextField
             {:label "Bitte Standort eingeben"
              :defaultValue ort
              :onChange #(-> % .-target .-value set-ort)
              :type "text"
              :autoComplete "address-level2"
              :autoFocus true})))))


(defnc Map
  [{:keys [id height zoom markers]}]
  (let [[position set-position] (ui/use-state :loading)]

    (ui/use-effect
     :once
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
               :error error))) 1000)
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
          :zoom zoom
          :markers markers
          :position position}))))


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

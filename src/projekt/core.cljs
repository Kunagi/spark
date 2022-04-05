(ns projekt.core)

(defn story-sprint-id [story]
  (get story :sprint-id "99999"))

(defn story-feature-id [story]
  (get story :feature-id "zzzzz"))

(defn storymap [projekt storys]
  (let [sprints-ids (->> storys
                         (map story-sprint-id)
                         (into #{})
                         (sort (fn [a b]
                                 (compare (js/parseInt a) (js/parseInt b)))))
        features (->> storys (map story-feature-id) (into #{}) sort)
        storys-by-sprint-and-feature (reduce (fn [m story]
                                               (update m [(-> story story-sprint-id)
                                                          (-> story story-feature-id)]
                                                       conj story))
                                             {} storys)
        feature-reihenfolge-map (->> projekt
                                     :feature-reihenfolge
                                     (map-indexed (fn [idx id] [id idx]))
                                     (into {}))
        feature-ids (->> features
                         (sort-by #(get feature-reihenfolge-map % 99999)))
        sprints (->> sprints-ids
                     (reduce (fn [m sprint-id]
                               (assoc m
                                      sprint-id
                                      (or (-> projekt :sprints (get sprint-id))
                                          {:id sprint-id
                                           :db/ref [(-> projekt :db/ref) :sprints sprint-id]})))
                             {}))]
    {:storys storys
     :sprints-ids sprints-ids
     :features features
     :storys-by-sprint-and-feature storys-by-sprint-and-feature
     :feature-ids feature-ids
     :sprints sprints}))

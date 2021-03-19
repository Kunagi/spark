(ns projekt.core
  )

(defn story-sprint-id [story]
  (get story :sprint-id "99999"))

(defn story-feature-id [story]
  (get story :feature-id "zzzzz"))



(defn storymap [projekt storys]
  (let [sprints-ids (->> storys (map story-sprint-id) (into #{}) sort)
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
                         (sort-by #(get feature-reihenfolge-map % 999999)))]
    {:storys storys
     :sprints-ids sprints-ids
     :features features
     :storys-by-sprint-and-feature storys-by-sprint-and-feature
     :feature-ids feature-ids}))

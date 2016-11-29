(ns som.kmeans
  (require [clojure.math.numeric-tower :as math]
           [clojure.core.matrix :as mat]))

;;K Means clustering implementation
(defn normalize [values]
  (let [mn (reduce min values)
        mx (reduce max values)
        factor (float (/ 1 (- mx mn)))]
    (map (fn [v] (* factor (- v mn))) values)))

(defn dist [a b]
  (math/expt (- a b) 2))

(defn average [values]
  (mat/div (reduce mat/add values) (count values)))

;;Map of normalized frequencies to their tokens
(defn normalized-frequency-map [freqs]
  (zipmap (keys freqs) (mat/normalise (vals freqs))))

(defn sample [values n]
  (for [i (range n)] (rand-nth values)))

(defn distinct-rand-sample [values samples k]
  (distinct (concat samples
                    (sample values (- k (count samples))))))

;;Makes sure to have distinct centroids as samples
(defn initial-centroids [freqs k]
  (let [values (vals freqs)
        samples (sample values k)]
    (loop [i 0 samples (distinct samples)]
      (if (or (> i 100) (= k (count samples)))
        samples
        (recur (inc i)
               (distinct-rand-sample values samples k))))))

(defn initial-cluster-map [centroids]
  (zipmap centroids (map (fn [c] []) centroids)))

(defn min-centroid [centroids v]
  (loop [centroids centroids
         mn-pair [(first centroids) (mat/distance (first centroids) v)]]
    (if (empty? centroids) (first mn-pair)
        (recur (rest centroids)
           (let [c (first centroids)]
             (if (< (dist c v) (second mn-pair))
               [c (dist c v)]
               mn-pair))))))

(defn add-to-cluster [clusters c v]
  (assoc clusters c (conj (get clusters c) v)))

(defn clusterify [nfreqs centroids]
  (let [clusters (initial-cluster-map centroids)]
    (loop [nfreqs (into [] nfreqs) clusters clusters]
      (if (empty? nfreqs) clusters
          (recur (rest nfreqs)
                 (let [v (second (first nfreqs))]
                   (add-to-cluster clusters
                                   (min-centroid centroids v)
                                   (first nfreqs))))))))

(defn recalculate-center [cluster n]
  (if (= [] cluster) (if (= n 1) 0 (repeat n 0))
      (mat/div (reduce mat/add (map second cluster)) (count cluster))))

(defn adjust-centroids [clusters-map]
  (let [ks (keys clusters-map)
        size (count (first ks))])
  (map (fn [c] (recalculate-center c size)) (vals clusters-map)))

(defn kmeans [k freqs iterations]
  (let [nfreqs (normalized-frequency-map freqs)]
    (loop [centroids (initial-centroids nfreqs k)
           i iterations clusters {}]
      (if (= i 0) clusters
          (let [clusters (clusterify nfreqs centroids)]
            (recur (adjust-centroids clusters) (dec i) clusters))))))

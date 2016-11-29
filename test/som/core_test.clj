(ns som.core-test
  (:require [clojure.test :refer :all]
            [som.core :refer :all]
            [som.kmeans :refer :all]
            [clojure.data.json :as json]
            [clojure.core.matrix :as mat]
            [som.viz :as viz]))

(def poke-data (keys-to-ints (json/read-str (slurp "poke_base_stats.json"))))

(def input-vectors (map mat/normalise (map vals (vals poke-data))))

(count input-vectors)

(def w 4)
(def h )

(def output (som-map w h input-vectors 10000))

;;Should be output-id-map
;;id could also be index
(def output-map (into {} (map-indexed (fn [i n] [n i]) output)))

;;output = nodes
(def freq-map (loop [freq-map {} ivs input-vectors]
                (if (empty? ivs) freq-map
                    (let [iv (first ivs)
                          bmu (find-bmu output iv)
                          id (output-map bmu)
                          freq-map (if (freq-map id)
                                     (assoc freq-map id (inc (freq-map id)))
                                     (assoc freq-map id 1))]
                      (recur freq-map (rest ivs))))))

(viz/plot-som-with-density w h 1 output freq-map)

(println freq-map)



(ns som.core-test
  (:require [clojure.test :refer :all]
            [som.core :refer :all]
            [som.kmeans :refer :all]
            [clojure.data.json :as json]
            [clojure.core.matrix :as mat]
            [som.viz :as viz]))

(def poke-data (keys-to-ints (json/read-str (slurp "poke_base_stats.json"))))

(def input-vectors (map mat/normalise (map vals (vals poke-data))))

(def w 10)
(def h 10)

(def output (som-map w h input-vectors 10000))

(viz/plot-som w h 0 output)



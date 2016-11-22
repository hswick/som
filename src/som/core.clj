(ns som.core
  (:require [clojure.core.matrix :as mat]
            [clojure.core.matrix.operators :as m]
            [clojure.data.json :as json]))

;;Algorithm taken from http://www.ai-junkie.com/ann/som/

(defn random-coll [n]
  (take n (repeatedly #(rand))))

(defn keys-to-ints [data]
  (into {} (for [[k v] data] [(Integer. (re-find #"\d+" k)) v])))

(def poke-data (keys-to-ints (json/read-str (slurp "poke_base_stats.json"))))

(def input-vectors (map mat/normalise (map vals (vals poke-data))))

(defn node [x y n]
  {:x x :y y :weights (mat/matrix (random-coll n))})

;;w = (width) number of nodes in x direction
;;h = (height) number of nodes in y direction
;;wn = weight number. Size of weight vector, same number as length of input vector
(defn grid [w h wn]
  {:w w :h h
   :nodes (for [x (take w (range))
                y (take h (range))] (node x y wn))})

;;Calculate the best matching node to the input vector
(defn find-bmu [nodes input]
  (apply min-key (fn [node]
                   (mat/distance input (:weights node))) nodes))

;;Get map radius by finding greatest length and dividing by 2
(defn map-radius [w h]
  (/ (max w h) 2))

;;Calculate time constant ratio for learning rate
(defn time-constant [iterations map-radius]
  (/ iterations (mat/log map-radius)))

;;Find radius in which nodes are considered neighbors
(defn neighborhood-radius [map-radius time-constant iter-count]
  (* map-radius (mat/exp (- (/ iter-count time-constant)))))

(defn find-neighbors [neighborhood-radius bmu nodes]
  (let [v1 [(:x bmu) (:y bmu)]
        f (fn [node]
            (let [v2 [(:x node) (:y node)]]
              (< (mat/distance v1 v2) neighborhood-radius)))]
    (filter f nodes)))

;;Add distance data to neighboring nodes
(defn assoc-dists [bmu neighbors]
  (map (fn [node]
         (assoc node :dist (mat/distance [(:x bmu) (:y bmu)]
                                         [(:x node) (:y node)])))
       neighbors))

;;Calculate current learning rate
;;lr-start = initial learning rate
;;num-iters = total number of training iterations
;;iter-coint = number of iterations that have occurred so far
(defn learning-rate [lr-start num-iters iter-count]
  (* lr-start (mat/exp (- (/ iter-count num-iters)))))

;;Theta
;;How much influence a node has on weight based on distance
(defn node-influence [neighborhood-radius dist]
  (mat/exp (- (/ (mat/pow dist 2)
                 (* 2 (mat/pow neighborhood-radius 2))))))

;;Increment weights of the node
;;input = input vector
(defn update-weights [node influence learning-rate input]
  (let [weights (:weights node)]
    (m/+ weights (m/* influence learning-rate (m/- input weights)))))

;;Associate ids with nodes in order to update nodes
(defn assoc-ids [nodes]
  (map-indexed (fn [i node] (assoc node :id i)) nodes))

;;Create an indexed map of nodes
(defn gen-node-map [nodes]
  (reduce (fn [node-map node] (assoc node-map (:id node) node))
          {} nodes))

;;update list of neighbors
(defn update-neighbors [neighbors l-rate input neighborhood-radius]
  (loop [updated [] neighbors neighbors]
    (if (empty? neighbors) updated
        (let [node (first neighbors)
              influence (node-influence neighborhood-radius (:dist node))]
          (recur (conj updated
                       (assoc node :weights
                              (update-weights node influence l-rate input)))
                 (rest neighbors))))))

;;Swap out updated neighbors with old nodes
(defn update-node-map [node-map neighbors]
  (reduce (fn [node-map node] (assoc node-map (:id node) node))
          node-map neighbors))

;;Make sure input vectors are normalised
;;(vals node-map) = nodes
(defn som-map [w h inputs iterations]
  (let [grid (grid w h (count (first inputs)))
        nodes (assoc-ids (:nodes grid))
        init-learning-rate 0.05
        som-radius (map-radius (:w grid) (:h grid))
        time-constant (time-constant iterations som-radius)]
    (loop [iter-count 0 node-map (gen-node-map nodes)]
      (if (= iter-count iterations) (map #(dissoc % :id) (vals node-map))
          (let [input (rand-nth inputs)
                nhbr-radius (neighborhood-radius som-radius time-constant iter-count)
                l-rate (learning-rate init-learning-rate iterations iter-count)
                bmu (find-bmu (vals node-map) input)
                neighbors (assoc-dists bmu (find-neighbors nhbr-radius bmu (vals node-map)))
                neighbors-2 (update-neighbors neighbors l-rate input nhbr-radius)]
            (recur (inc iter-count) (update-node-map node-map neighbors-2)))))))

(def output (som-map 10 10 input-vectors 10000))

;;(spit "pokedata.json" (json/write-str output))





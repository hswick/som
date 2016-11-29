(ns som.viz
  (:require [quil.core :as q]))

(defn heat-map-rgb [stat]
  (if (> stat 0.5)
    [(q/map-range stat 0 1 0 255) 0 0]
    (if (> stat 0.3)
      [255 (q/map-range stat 0 1 0 255) 0]
      [0 0 (q/map-range stat 0 1 0 255)])))

(defn heat-map-rgba [stat max-v freq]
  (conj (if (> stat 0.5)
          [(q/map-range stat 0 1 0 255) 0 0 ]
          [0 0 (q/map-range stat 0 1 0 255)])
        (q/map-range (if (nil? freq) 0 freq) 0 max-v 0 255)))

(defn draw-som [w h index nodes]
  (q/background 255)
  (q/no-stroke)
  (let [nrows w
        ncols h
        cell-w (/ (q/width) ncols)
        cell-h (/ (q/height) nrows)
        n (atom 0)]
    (dotimes [i ncols]
      (dotimes [j nrows]
        (let [node (nth nodes @n)
              weights (node :weights)
              stat (nth weights index)
              rgb (heat-map-rgb stat)]
          (q/fill (nth rgb 0) (nth rgb 1) (nth rgb 2))
          (q/rect (* i cell-w) (* j cell-h) cell-w cell-h)
          (swap! n inc))))))

(defn draw-som-with-density [w h index nodes freq-map]
  (q/background 255)
  (q/no-stroke)
  (let [nrows w
        ncols h
        cell-w (/ (q/width) ncols)
        cell-h (/ (q/height) nrows)
        n (atom 0)
        max-v (reduce max (map val freq-map))]
    (dotimes [i ncols]
      (dotimes [j nrows]
        (let [node (nth nodes @n)
              weights (node :weights)
              stat (nth weights index)
              freq (if (nil? (freq-map @n)) 0 (freq-map @n))
              rgba (heat-map-rgba stat max-v (freq-map @n))]
          (q/fill (nth rgba 0) (nth rgba 1) (nth rgba 2) (nth rgba 3))
          (q/rect (* i cell-w)
                  (* j cell-h)
                  cell-w
                  cell-h)
          (swap! n inc))))))


(defn plot-som [w h index nodes]
  (q/defsketch som-viz
    :title "SOM Visualization"
    :setup (fn [] (draw-som w h index nodes))
    :size [800 800]))

(defn plot-som-with-density [w h index nodes freq-map]
  (q/defsketch som-viz
    :title "SOM Visualization"
    :setup (fn [] (draw-som-with-density w h index nodes freq-map))
    :size [800 800]))

(clojure.java.browse/browse-url "http://clojuredocs.org")

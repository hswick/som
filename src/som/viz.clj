(ns som.viz
  (:require [quil.core :as q]))

(defn heat-map-rgb [stat]
  (if (> stat 0.5)
    [(q/map-range stat 0 1 0 255) 0 0]
    (if (> stat 0.3)
      [255 (q/map-range stat 0 1 0 255) 0]
      [0 0 (q/map-range stat 0 1 0 255)])))

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

(defn plot-som [w h index nodes]
  (q/defsketch som-viz
    :title "SOM Visualization"
    :setup (fn [] (draw-som w h index nodes))
    :size [800 800]))

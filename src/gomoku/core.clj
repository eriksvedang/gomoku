(ns gomoku.core
  (:require [quil.core :as q])
  (:gen-class))

(defn setup [])

(defn scale [pixel-x pixel-y w h factor]
  (let [small (* 0.5 (- 1.0 factor))]
    [(+ pixel-x (* w small))
     (+ pixel-y (* h small))
     (* w factor)
     (* h factor)]))

(defn draw-shape [shape x y cell-size x-offset y-offset scale-factor]
  (apply shape (scale (+ (* x cell-size) x-offset)
                      (+ (* y cell-size) y-offset)
                      cell-size
                      cell-size
                      scale-factor)))

(defn draw-grid [x-dim y-dim cell-size x-offset y-offset]
  (q/stroke 20)
  (q/fill 255)
  (let [cells (for [x (range x-dim)
                    y (range y-dim)]
                [x y])]
    (doseq [[x y] cells]
      (draw-shape q/rect x y cell-size x-offset y-offset 1.0))))

(defn draw-shapes [shape cell-size x-offset y-offset]
  )

(defn draw []
  (q/ellipse-mode :corner)
  (q/background 210)
  (draw-grid 15 10 30 20 20)
  (q/fill 0)
  (draw-shape q/ellipse 3 4 30 20 20 0.7))

(defn create-window []
  (q/defsketch gomoku
               :title "Gomoku"
               :size [800 600]
               :setup setup
               :draw draw))

(defn -main []
  (create-window))

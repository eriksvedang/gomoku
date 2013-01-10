(ns gomoku.graphics
  (:require [quil.core :as q]
            [gomoku.gameplay :as gameplay]))

; Some pixel settings
(def x-offset 15)
(def y-offset 40)
(def cell-size 25)

(defn scale [pixel-x pixel-y w h factor]
  (let [small (* 0.5 (- 1.0 factor))]
    [(+ pixel-x (* w small))
     (+ pixel-y (* h small))
     (* w factor)
     (* h factor)]))

(defn draw-shape [shape x y scale-factor]
  (apply shape (scale (+ (* x cell-size) x-offset)
                      (+ (* y cell-size) y-offset)
                      cell-size
                      cell-size
                      scale-factor)))

(defn draw-grid [x-dim y-dim]
  (q/stroke 20)
  (q/fill 255)
  (let [cells (for [x (range x-dim)
                    y (range y-dim)]
                [x y])]
    (doseq [[x y] cells]
      (draw-shape q/rect x y 1.0))))

(defn draw-shapes [shape coords]
  (doseq [[x y] coords]
    (draw-shape shape x y 0.8)))

(defn draw-moves-for-player [board-state player]
  (draw-shapes q/ellipse (gameplay/get-positions-of-moves-for-player board-state player)))

(defn draw [board-state game-state cell-count-horizontal cell-count-vertical]
  (q/ellipse-mode :corner)
  (q/background 210)
  (draw-grid cell-count-horizontal cell-count-vertical)
  (q/no-stroke)
  (q/fill 255 0 0)
  (draw-moves-for-player board-state :a)
  (q/fill 0 0 255)
  (draw-moves-for-player board-state :b)
  (q/fill 20)
  (q/text (str "Game state: " game-state) 20 20))
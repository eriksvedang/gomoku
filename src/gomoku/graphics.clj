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

(defn draw [board-state game-state cell-count-horizontal cell-count-vertical fraction-of-time-used]
  (q/ellipse-mode :corner)
  (q/background 230 (+ (* (Math/sin (* 0.1 (q/frame-count))) 20) 235) 230)
  (draw-grid cell-count-horizontal cell-count-vertical)
  (q/no-stroke)
  (q/fill 255 0 0)
  (draw-moves-for-player board-state :a)
  (q/fill 0 0 255)
  (draw-moves-for-player board-state :b)
  (q/fill 20)
  (q/text (str "Player a wins: " (get-in game-state [:wins :a]) "   "
               "Player b wins: " (get-in game-state [:wins :b]))
          25 25)
  (let [x 20 y (- (q/height) 25) w (- (q/width) 40) h 10]
    (q/fill 200)
    (q/rect x y w h)
    (q/fill 50)
    (q/rect x y (* fraction-of-time-used w) h)))


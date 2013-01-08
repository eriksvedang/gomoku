(ns gomoku.core
  (:require [quil.core :as q])
  (:gen-class))

(def x-offset 15) ; in pixels

(def y-offset 15) ; in pixels

(def grid-size-x 20) ; nr of cells horizontal

(def grid-size-y 15) ; nr of cells vertical

(def cell-size 25) ; in pixels

(def state (atom [{:player :a :pos [2 3]}
                  {:player :b :pos [4 4]}]))

(defn make-move [player x y]
  (let [all-pos (map #(:pos %) @state)]
    (doseq [pos all-pos]
      (when (= pos [x y])
        (throw (Exception. (str "Can't place move for player " player " on " [x y]))))))
  (swap! state conj {:player player :pos [x y]}))

(defn setup [])

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

(defn get-moves-for-player [state player]
  (filter #(= player (:player %)) state))

(defn draw []
  (q/ellipse-mode :corner)
  (q/background 210)
  (draw-grid grid-size-x grid-size-y)
  (q/no-stroke)
  (q/fill 255 0 0)
  (draw-shapes q/ellipse (map #(:pos %) (get-moves-for-player @state :a)))
  (q/fill 0 0 255)
  (draw-shapes q/rect (map #(:pos %) (get-moves-for-player @state :b))))

(defn create-window []
  (let [x-size (+ (* grid-size-x cell-size) (* 2 x-offset))
        y-size (+ (* grid-size-y cell-size) (* 2 y-offset))]
    (q/defsketch gomoku
                 :title "Gomoku"
                 :size [x-size y-size]
                 :setup setup
                 :draw draw)))

(defn -main []
  (create-window))

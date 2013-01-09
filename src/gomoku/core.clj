(ns gomoku.core
  (:require [quil.core :as q]
            [gomoku.stupid-ai :as stupid])
  (:gen-class))

(def x-offset 15) ; in pixels

(def y-offset 40) ; in pixels

(def grid-size-x 20) ; nr of cells horizontal

(def grid-size-y 15) ; nr of cells vertical

(def cell-size 25) ; in pixels

(def state (atom [{:player :a :pos [2 3]}
                  {:player :b :pos [4 4]}]))

(def game-state (atom :turn-a))

(defn new-game []
  (reset! state [])
  (reset! game-state :turn-a))

(defn stop []
  (swap! game-state :stopped))

(defn other-player [game-state]
  (cond (= game-state :turn-a) :turn-b
        (= game-state :turn-b) :turn-a))

(defn is-cell-occupied? [state pos]
  (some true? (for [cell state]
                (= (:pos cell) pos))))

(defn make-move [player move-pos]
  (cond 
   (>= (count @state) (* grid-size-x grid-size-y)) (reset! game-state :board-is-full)
   (not (nil? move-pos)) (do
                           (if (is-cell-occupied? @state move-pos)
                             (println "Can't place move for player " player " on " move-pos)
                             (swap! state conj {:player player :pos move-pos}))
                           (swap! game-state other-player))
   :else :no-predicate-was-true))

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
  (condp = @game-state
    :turn-a (make-move :a (stupid/get-move @state grid-size-x grid-size-y))
    :turn-b (make-move :b (stupid/get-move @state grid-size-x grid-size-y))
    :do-nothing)
  (q/ellipse-mode :corner)
  (q/background 210)
  (draw-grid grid-size-x grid-size-y)
  (q/no-stroke)
  (q/fill 255 0 0)
  (draw-shapes q/ellipse (map #(:pos %) (get-moves-for-player @state :a)))
  (q/fill 0 0 255)
  (draw-shapes q/ellipse (map #(:pos %) (get-moves-for-player @state :b)))
  (q/stroke 0)
  (q/fill 0)
  (q/text (str "Game state: " @game-state) 20 20))

(defn create-window []
  (let [x-size (+ (* grid-size-x cell-size) (* 2 x-offset))
        y-size (+ (* grid-size-y cell-size) (* 2 y-offset))]
    (q/defsketch gomoku
                 :title "Gomoku"
                 :size [x-size y-size]
                 :setup setup
                 :draw draw)))

(defn -main []
  (new-game)
  (create-window))

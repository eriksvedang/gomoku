(ns gomoku.core
  (:require [quil.core :as q]
            [gomoku.stupid-ai :as stupid])
  (:gen-class))

; Switchboard for what ai functions to use for the two players
(def ai-fns 
  {:a stupid/get-move 
   :b stupid/get-move})

; Some pixel settings
(def x-offset 15)
(def y-offset 40)
(def cell-size 25)

; Game board settings
(def cell-count-horizontal 20)
(def cell-count-vertical 15)

; Contains all the moves
(def board-state (atom []))

; Contains info about winner, active-player, etc
(def game-state (atom :a))

(defn new-game []
  (reset! board-state [])
  (reset! game-state :a))

(defn stop []
  (reset! game-state :stopped))

(defn create-move [player move-pos]
  {:player player :pos move-pos})

(defn other-player [game-state]
  (cond (= game-state :a) :b
        (= game-state :b) :a))

(defn is-cell-occupied? [board pos]
  (some true? (for [cell board]
                (= (:pos cell) pos))))

(defn get-player-moves [player board]
  (map :pos (filter #(= player (:player %)) board)))

(defn add-pos [[x1 y1][x2 y2]]
  [(+ x1 x2)
   (+ y1 y2)])

(defn nr-of-moves-in-dir [moves start-pos dir]
  (loop [counter 0
         pos start-pos]
    (if (contains? (set moves) pos)
      (recur (inc counter) 
             (add-pos pos dir))
      counter)))

(def dirs
  (for [x (range -1 2)
        y (range -1 2)
        :when (not (and (= 0 x) (= 0 y)))]
    [x y]))

(defn get-lengths [player board]
  (let [moves (get-player-moves player board)]
    (for [move moves
          dir dirs]
      (nr-of-moves-in-dir moves move dir))))

(defn has-won? [player board]
  (not (empty? (filter #(>= % 5) (get-lengths player board)))))

(defn make-move! [player move-pos]
  (cond 
   (>= (count @board-state) (* cell-count-horizontal cell-count-vertical)) (reset! game-state :board-is-full)
   (not (nil? move-pos)) (do
                           (if (is-cell-occupied? @board-state move-pos)
                             (println "Can't place move for player " player " on " move-pos)
                             (swap! board-state conj (create-move player move-pos)))
                           (swap! game-state other-player)
                           (when (has-won? player @board-state)
                             (reset! game-state [:won-by player])))
   :else :no-predicate-was-true))

(defn setup []
  (q/frame-rate 60))

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

(defn get-moves-for-player [board player]
  (filter #(= player (:player %)) board))

(defn update-states []
  (let [active-player @game-state]
    (when (contains? #{:a :b} active-player)
      (make-move! active-player ((get ai-fns active-player) @board-state cell-count-horizontal cell-count-vertical)))))

(defn draw []
  (update-states)
  (q/ellipse-mode :corner)
  (q/background 210)
  (draw-grid cell-count-horizontal cell-count-vertical)
  (q/no-stroke)
  (q/fill 255 0 0)
  (draw-shapes q/ellipse (map #(:pos %) (get-moves-for-player @board-state :a)))
  (q/fill 0 0 255)
  (draw-shapes q/ellipse (map #(:pos %) (get-moves-for-player @board-state :b)))
  (q/fill 20)
  (q/text (str "Game state: " @game-state) 20 20))

(defn create-window []
  (let [x-size (+ (* cell-count-horizontal cell-size) (* 2 x-offset))
        y-size (+ (* cell-count-vertical cell-size) (* 2 y-offset))]
    (q/defsketch gomoku
                 :title "Gomoku"
                 :size [x-size y-size]
                 :setup setup
                 :draw draw)))

(defn -main []
  (new-game)
  (create-window))

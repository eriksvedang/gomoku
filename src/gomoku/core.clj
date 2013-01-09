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
                  {:player :a :pos [3 3]}
                  {:player :a :pos [10 3]}
                  {:player :b :pos [4 4]}]))

(def game-state (atom :turn-a))

(defn new-game []
  (reset! state [])
  (reset! game-state :turn-a))

(defn stop []
  (reset! game-state :stopped))

(defn other-player [game-state]
  (cond (= game-state :turn-a) :turn-b
        (= game-state :turn-b) :turn-a))

(defn is-cell-occupied? [state pos]
  (some true? (for [cell state]
                (= (:pos cell) pos))))

(defn get-player-moves [player state]
  (map :pos (filter #(= player (:player %)) state)))

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

(defn get-lengths [player state]
  (let [moves (get-player-moves player state)]
    (for [move moves
          dir dirs]
      (nr-of-moves-in-dir moves move dir))))

;@state
;(doall (get-player-moves :a @state))
;(nr-of-moves-in-dir (get-player-moves :a @state) [3 3] [1 1])
;(doall (get-lengths :a @state))

(defn has-won? [player state]
  (not (empty? (filter #(>= % 5) (get-lengths player state)))))

(defn make-move [player move-pos]
  (cond 
   (>= (count @state) (* grid-size-x grid-size-y)) (reset! game-state :board-is-full)
   (not (nil? move-pos)) (do
                           (if (is-cell-occupied? @state move-pos)
                             (println "Can't place move for player " player " on " move-pos)
                             (swap! state conj {:player player :pos move-pos}))
                           (swap! game-state other-player)
                           (when (has-won? player @state)
                             (reset! game-state [:won-by player])))
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
    :stopped :do-nothing
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

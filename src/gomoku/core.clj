(ns gomoku.core
  (:require [quil.core :as q]
            [gomoku.gameplay :as gameplay]
            [gomoku.graphics :as graphics]
            [gomoku.stupid-ai :as stupid])
  (:gen-class))

; Lookup table for what ai function to use for each player
(def ai-fns 
  {:a stupid/get-move 
   :b stupid/get-move})

; Game board settings
(def cells-horizontal 20)
(def cells-vertical 15)

; The moves
(def board-state (atom []))

; Info about winner, active-player, etc
(def game-state (atom :a))

(defn new-game []
  (reset! board-state [])
  (reset! game-state :a))

(defn stop []
  (reset! game-state :stopped))

(defn board-is-full? []
  (>= (count @board-state) (* cells-horizontal cells-vertical)))

(defn make-move! [player move-pos]
  (if (gameplay/is-cell-occupied? @board-state move-pos)
    (println "Can't place move for player " player " on " move-pos)
    (swap! board-state conj (gameplay/create-move player move-pos))))

(defn let-player-do-turn [player]
  (let [move-pos ((get ai-fns player) @board-state cells-horizontal cells-vertical)]
    (if (nil? move-pos)
      (println "Got invalid move from ai for player " player)
      (make-move! player move-pos))))

(defn no-winner? []
  (contains? #{:a :b} @game-state))

(defn update-states []
  (let [active-player @game-state]
    (cond
     (board-is-full?) (reset! game-state :board-is-full)
     (contains? #{:a :b} active-player) (let-player-do-turn active-player))
    (when (gameplay/has-won? active-player @board-state)
      (reset! game-state [:won-by active-player])))
  (when (no-winner?)
    (swap! game-state gameplay/other-player)))
  
(defn setup []
  (q/frame-rate 60))

(defn draw []
  (update-states)
  (graphics/draw @board-state @game-state cells-horizontal cells-vertical))

(defn create-window []
  (let [x-size (+ (* cells-horizontal graphics/cell-size) (* 2 graphics/x-offset))
        y-size (+ (* cells-vertical graphics/cell-size) (* 2 graphics/y-offset))]
    (q/defsketch gomoku
                 :title "Gomoku"
                 :size [x-size y-size]
                 :setup setup
                 :draw draw)))

(defn -main []
  (new-game)
  (create-window))

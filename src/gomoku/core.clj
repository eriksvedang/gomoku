(ns gomoku.core
  (:require [quil.core :as q]
            [gomoku.gameplay :as gameplay]
            [gomoku.graphics :as graphics]
            [gomoku.stupid-ai :as stupid])
  (:gen-class))

; Switchboard for what ai functions to use for the two players
(def ai-fns 
  {:a stupid/get-move 
   :b stupid/get-move})

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

(defn board-is-full? []
  (>= (count @board-state) (* cell-count-horizontal cell-count-vertical)))

(defn make-move! [player move-pos]
  (cond 
   (board-is-full?) (reset! game-state :board-is-full)
   (not (nil? move-pos)) (do
                           (if (gameplay/is-cell-occupied? @board-state move-pos)
                             (println "Can't place move for player " player " on " move-pos)
                             (swap! board-state conj (gameplay/create-move player move-pos)))
                           (swap! game-state gameplay/other-player)
                           (when (gameplay/has-won? player @board-state)
                             (reset! game-state [:won-by player])))
   :else :no-predicate-was-true))

(defn update-states []
  (let [active-player @game-state]
    (when (contains? #{:a :b} active-player)
      (make-move! active-player ((get ai-fns active-player) @board-state cell-count-horizontal cell-count-vertical)))))

(defn setup []
  (q/frame-rate 60))

(defn draw []
  (update-states)
  (graphics/draw @board-state @game-state cell-count-horizontal cell-count-vertical))

(defn create-window []
  (let [x-size (+ (* cell-count-horizontal graphics/cell-size) (* 2 graphics/x-offset))
        y-size (+ (* cell-count-vertical graphics/cell-size) (* 2 graphics/y-offset))]
    (q/defsketch gomoku
                 :title "Gomoku"
                 :size [x-size y-size]
                 :setup setup
                 :draw draw)))

(defn -main []
  (new-game)
  (create-window))

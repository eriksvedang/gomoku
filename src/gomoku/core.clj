(ns gomoku.core
  (:require [quil.core :as q]
            [gomoku.gameplay :as gameplay]
            [gomoku.graphics :as graphics]
            [gomoku.stupid-ai :as stupid])
  (:gen-class))

; Lookup table for what ai function to use for each player
(def ai-fns 
  {:a stupid/slow-move 
   :b stupid/slow-move})

; Nr of milliseconds the ai has to make it's move or the turn will go to the other player
(def ai-time-limit 1000)

; Game board settings
(def cells-horizontal 20)
(def cells-vertical 15)

; The moves
(def board-state (atom []))

; Info about winner, active-player, etc
(def game-state (atom {}))

(defn random-player []
  (rand-nth [:a :b]))

(defn new-game! []
  (reset! board-state [])
  (reset! game-state {:active-player (random-player) :playing true :wins {:a 0 :b 0}}))

(defn start-next-round! []
  (reset! board-state [])
  (swap! game-state assoc-in [:active-player] (random-player)))

(defn stop! []
  (swap! game-state assoc-in [:playing] false))

(defn resume! []
  (swap! game-state assoc-in [:playing] true))

(defn winner-decided! [player]
  (swap! game-state update-in [:wins player] inc)
  (start-next-round!))

(defn board-is-full? []
  (>= (count @board-state) (* cells-horizontal cells-vertical)))

(defn make-move! [player move-pos]
  (if (or (gameplay/is-cell-occupied? @board-state move-pos)
          (gameplay/is-cell-outside-bounds? move-pos cells-horizontal cells-vertical))
    (println "Can't place move for player" player "on" move-pos)
    (swap! board-state conj (gameplay/create-move player move-pos))))

(defn create-worker-function [player board-state h v]
  "Creates an anonymous function that ignores the argument; the current state of the agent"
  (fn [_] ((get ai-fns player) board-state h v)))

(defn invalid-pos? [move-pos]
  (or (nil? move-pos) (not (vector? move-pos)) (not (= 2 (count move-pos)))))

(defn let-player-do-turn [player]
  (let [worker (agent :TIMED-OUT)
        f (create-worker-function player @board-state cells-horizontal cells-vertical)]
    (send worker f)
    (await-for ai-time-limit worker)
    (let [move-pos @worker]
      (cond
       (= :TIMED-OUT move-pos) (println (str "Player " player " was too slow to make a move"))
       (invalid-pos? move-pos) (println (str "Got invalid move-pos from ai for player " player " " move-pos))
       :else (make-move! player move-pos)))))

(defn get-active-player []
  (:active-player @game-state))

(defn update []
  (let [active-player (get-active-player)]
    (let-player-do-turn active-player)
    (cond 
     (gameplay/has-won? active-player @board-state) (winner-decided! active-player)
     (board-is-full?) (start-next-round!)
     :else (swap! game-state update-in [:active-player] gameplay/other-player))))
      
(defn setup []
  (q/frame-rate 60))

(defn draw []
  (graphics/draw @board-state @game-state cells-horizontal cells-vertical))

(defn create-window []
  (let [x-size (+ (* cells-horizontal graphics/cell-size) (* 2 graphics/x-offset))
        y-size (+ (* cells-vertical graphics/cell-size) (* 2 graphics/y-offset))]
    (q/defsketch gomoku
                 :title "Gomoku"
                 :size [x-size y-size]
                 :setup setup
                 :draw draw)))

(def update-thread-on (atom true))

(defn update-loop []
  (while @update-thread-on
    (when (:playing @game-state)
      (update))))

(defn start-update-loop []
  (reset! update-thread-on true)
  (.start (Thread. #(update-loop))))

(defn stop-update-loop []
  (reset! update-thread-on false))

(defn -main []
  (new-game!)
  (create-window)
  (start-update-loop))

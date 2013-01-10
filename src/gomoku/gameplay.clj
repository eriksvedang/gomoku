(ns gomoku.gameplay)

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
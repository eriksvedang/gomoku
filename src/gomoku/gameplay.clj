(ns gomoku.gameplay)

(defn create-move [player move-pos]
  {:player player :pos move-pos})

(defn other-player [game-state]
  (cond (= game-state :a) :b
        (= game-state :b) :a))

(defn is-cell-occupied? [board pos]
  (some true? (for [cell board]
                (= (:pos cell) pos))))

(defn is-cell-outside-bounds? [[x y] w h]
  (not (and (<= 0 x)
            (<= 0 y)
            (< x w)
            (< y h))))

(defn get-moves-for-player [board player]
  (filter #(= player (:player %)) board))

(defn get-positions-of-moves-for-player [board player]
  (map :pos (get-moves-for-player board player)))

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

(defn get-chain-lengths [player board]
  (let [moves (get-positions-of-moves-for-player board player)]
    (for [move moves
          dir dirs]
      (nr-of-moves-in-dir moves move dir))))

(defn has-won? [player board]
  (not (empty? (filter #(>= % 5) (get-chain-lengths player board)))))
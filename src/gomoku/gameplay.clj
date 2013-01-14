(ns gomoku.gameplay)

(defn create-move [player move-pos]
  {:player player :pos move-pos})

(defn other-player [player]
  ({:a :b :b :a} player))

(defn is-cell-occupied? [state pos]
  (contains? (set (map :pos state)) pos))

(defn is-cell-outside-bounds? [[x y] w h]
  (not (and (<= 0 x)
            (<= 0 y)
            (< x w)
            (< y h))))

(defn get-moves-for-player [board player]
  (filter #(= player (:player %)) board))

(defn get-positions-of-moves-for-player [board player]
  (map :pos (get-moves-for-player board player)))

(defn add-pos [p1 p2]
  (map + p1 p2))

(defn nr-of-moves-in-dir [positions start-pos dir]
  (loop [counter 0
         pos start-pos]
    (if (contains? (set positions) pos)
      (recur (inc counter) 
             (add-pos pos dir))
      counter)))

(def dirs
  (for [x (range -1 2)
        y (range -1 2)
        :when (not (and (= 0 x) (= 0 y)))]
    [x y]))

(defn get-chain-lengths [player board]
  (let [positions (get-positions-of-moves-for-player board player)]
    (for [pos positions
          dir dirs]
      (nr-of-moves-in-dir positions pos dir))))

(defn has-won? [player board]
  (not (empty? (filter #(>= % 5) (get-chain-lengths player board)))))
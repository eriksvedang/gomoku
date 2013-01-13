(ns gomoku.stupid-ai)

(defn get-all-positions [w h]
  (for [x (range w) y (range h)]
    [x y]))

(defn is-cell-occupied? [state pos]
  (contains? (set (map :pos state)) pos))

(defn get-move [state board-width board-height]
  (let [all-positions (get-all-positions board-width board-height)
        possible-moves (remove #(is-cell-occupied? state %) all-positions)]
    (if (empty? possible-moves)
      nil
      (rand-nth possible-moves))))

(defn slow-move [s w h]
  (Thread/sleep (+ 100 (rand-int 600)))
  (get-move s w h))
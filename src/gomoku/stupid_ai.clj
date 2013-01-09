(ns gomoku.stupid-ai)

(defn get-all-positions [w h]
  (for [x (range w) y (range h)]
    [x y]))

(defn is-cell-occupied? [state pos]
  (some true? (for [cell state]
                (= (:pos cell) pos))))

(defn get-move [state board-width board-height]
  (let [all-positions (get-all-positions board-width board-height)
        possible-moves (remove #(is-cell-occupied? state %) all-positions)]
    (if (empty? possible-moves)
      nil
      (rand-nth possible-moves))))

;(def test-state [{:player :a :pos [2 3]} {:player :b :pos [4 4]}])
;(is-cell-occupied? test-state [4 4])
;(get-move test-state 10 10)
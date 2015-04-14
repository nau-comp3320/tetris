(ns tetris.core
  (:gen-class))

(def board-width 10)
(def board-height 20)

(defn new-board
  "Returns a new tetris game board"
  []
  (vec (repeat board-height (vec (repeat board-width 0)))))

(defn row->str
  "Renders a tetris row into a string."
  [row]
  (apply str
         (map (fn [cell]
                (if (zero? cell) \space \X))
              row)))

(defn board->str
  "Renders a tetris board to a string."
  [board]
  (apply str
         (map (fn [row]
                (str (row->str row) \newline))
              board)))

(def tetriminos
  {:I [[0 1] [0 0] [0 -1] [0 -2]]
   :J [[0 -1] [0 0] [1 0] [2 0]]
   :L [[-2 0] [-1 0] [0 0] [0 -1]]
   :O [[0 -1] [0 0] [1 0] [1 -1]]
   :S [[-1 0] [0 0] [0 -1] [1 -1]]
   :Z [[0 1] [0 0] [0 -1] [-1 -1]]
   :T [[0 0] [-1 0] [1 0] [0 -1]]})

(defn new-game
  "Constructs a new instance of the game state."
  []
  (let [board (new-board)]
    {:board board
     :tetrimino (rand-nth (vals tetriminos))
     :position [(quot (count (first board)) 2) 1]}))

(defn render-game
  [{:keys [board tetrimino position]}]
  (let [[pos-x pos-y] position
        tetrimino-positions (map (fn [[off-x off-y]]
                                   [(+ off-x pos-x)
                                    (+ off-y pos-y)])
                                 tetrimino)
        board+tetrimino (reduce (fn [b [x y]]
                                  (assoc-in b [y x] 1))
                                board
                                tetrimino-positions)]
    (println (board->str board+tetrimino))))

(defn game-loop
  "Runs the game."
  []
  (loop [{:keys [board position] :as game} (new-game)
         input nil]
    (let [new-game
          (cond 
            (= 'w input) game
            (= 's input) (update-in game [:position 1] inc)
            (= 'a input) (update-in game [:position 0] dec)
            (= 'd input) (update-in game [:position 0] inc)
            :else game)]
      (render-game new-game)
      (prn game)
      (prn new-game)
      (recur new-game (read)))))


(defn -main
  [& args]
  (game-loop)
  )

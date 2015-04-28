(ns tetris.core
  (:gen-class))

(def board-width 10)
(def board-height 20)

(defn new-board
  "Returns a new tetris game board"
  []
  (vec (repeat board-height (vec (repeat board-width 0)))))

(def tetriminos
  {:I  [ [-1  0]  [ 0  0]  [ 1  0]  [ 2  0] ]
   :T  [ [ 0 -1]  [-1  0]  [ 0  0]  [ 1  0] ]
   :O  [ [ 0 -1]  [ 1 -1]  [ 0  0]  [ 1  0] ]
   :J  [ [-1 -1]  [-1  0]  [ 0  0]  [ 1  0] ]
   :L  [ [ 1 -1]  [-1  0]  [ 0  0]  [ 1  0] ]
   :S  [ [ 0 -1]  [ 1 -1]  [-1  0]  [ 0  0] ]
   :Z  [ [-1 -1]  [ 0 -1]  [ 0  0]  [ 1  0] ]})

(defn new-tetrimino
  "Takes a game state and returns a new game state with a fresh tetrimino added
  to the top of the board."
  [{:keys [board] :as game}]
  (assoc game
         :tetrimino (rand-nth (vals tetriminos))
         :position [(dec (quot (count (first board)) 2)) 1]))

(defn new-game
  "Constructs a new instance of the game state."
  []
  (new-tetrimino {:board (new-board)}))

(defn tetrimino-positions
  "Returns all of the current positions of the tetrimino in the game."
  [{:keys [tetrimino position]}]
  (mapv (fn [offset]
          (mapv + position offset))
        tetrimino))

(defn persist-tetrimino
  "Takes a current game state and returns a new game state where the tetrimino
  is added to the board."
  [{:keys [board] :as game}]
  (assoc (dissoc game :tetrimino :position)
         :board (reduce (fn [board [x y]]
                          (assoc-in board [y x] 1))
                        board
                        (tetrimino-positions game))))
(defn within-bounds?
  "Returns logical true if the given game state has a tetrimino that it out of
  bounds."
  [{:keys [board] :as game}]
  (let [rows (count board)
        cols (count (first board))]
    (every?
      (fn [[x y]]
        (and (< -1 x cols)
             (< -1 y rows)))
      (tetrimino-positions game))))

(defn has-collision?
  "Returns a logical true value if in the given game state the tetrimino has
  collided with the persistent blocks."
  [{:keys [board] :as game}]
  (some
    (fn [[x y]]
      (not (zero? (get-in board [y x] 1))))
    (tetrimino-positions game)))

(defn valid-move?
  "Returns logical true if the given game state represents a valid move."
  [game]
  (and (not (has-collision? game))
       (within-bounds? game)))

(defn move-if-valid
  "Returns the next game state resulting from applying a move to an old game
  state.  If the move is valid, returns the new game state, otherwise returns
  the old state."
  [old-state move]
  (let [new-state (move old-state)]
    (if (valid-move? new-state)
      new-state
      old-state)))

(defn rotate-tetrimino
  "Takes a current game state and returns a new game state where the tetrimino
  has been rotated."
  [{:keys [tetrimino] :as game}]
  (let [new-tetrimino (mapv (fn [[x y]]
                              [(- y) x])
                            tetrimino)
        new-state (assoc game :tetrimino new-tetrimino)]
    (assoc game :tetrimino new-tetrimino)))

(defn move-left
  "Returns the new game state that results from trying to move the tetrimino
  left.  May be the same state when moving left is illegal."
  [game]
  (update-in game [:position 0] dec))

(defn move-right
  "Returns the new game state that results from trying to move the tetrimino
  right.  May be the same state when moving left is illegal."
  [game]
  (update-in game [:position 0] inc))

(defn move-down
  "Returns the new game state that results from moving the tetrimino down.  The
  result may be an illegal game state."
  [game]
  (update-in game [:position 1] inc))

(defn check-for-game-end
  "Checks to see if the game has ended because adding the tetrimino would
  result in an invalid state."
  [game]
  (if (valid-move? game)
    game
    (persist-tetrimino game)))

(defn completed-row?
  "Checks to see if the given row is complete."
  [row]
  (every? (fn [cell] (= 1 cell)) row))

(defn remove-completed-rows
  "Checks to see if there are any completed rows in the game, and removes them."
  [{:keys [board] :as game}]
  (let [rows (count board)
        cols (count (first board))
        temp-board (remove completed-row? board)
        removed-rows (- rows (count temp-board))
        empty-rows (repeat removed-rows (vec (repeat cols 0)))
        new-board (vec (concat empty-rows temp-board))]
    (assoc game
           :board new-board)))

(defn handle-hit-bottom
  "Handles all of the logic neccessary when a tetrimino reaches the bottom."
  [game]
  (-> (persist-tetrimino game)
      (remove-completed-rows)
      (new-tetrimino)
      (check-for-game-end)))

(defn drop-tetrimino
  "Performs the necessary modifications to the game state as a result of
  dropping a tetrimino by one row.  This may result in persisting the tetrimino
  and generating a new one.  Returns a new game state."
  [game]
  (let [moved-down (move-down game)]
    (if (valid-move? moved-down)
      moved-down
      (handle-hit-bottom game))))

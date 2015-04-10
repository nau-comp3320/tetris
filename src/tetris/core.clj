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


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

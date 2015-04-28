(ns tetris.gui
  (:require [seesaw.border :as border]
            [seesaw.core :as seesaw]
            [seesaw.graphics :as graphics]
            [seesaw.keymap :as keymap]
            [tetris.core :as tetris])
  (:gen-class))

(defn ^:private make-ui
  "Creates a new frame for the Tetris game."
  []
  (seesaw/frame
    :title "Tetris"
    :resizable? false
    :on-close :dispose
    :content (seesaw/border-panel
               :border 5
               :hgap 5
               :vgap 5
               :north (seesaw/button
                        :id :new-game
                        :text "New Game"
                        :mnemonic \n)
               :center (seesaw/canvas
                         :id :canvas
                         :background :black
                         :size [200 :by 400]
                         :border (border/line-border
                                   :thickness 2
                                   :color :black)))))

(defn ^:private make-block-renderer
  "Returns a function that will render a block at the given coordinates and
  with the given colour."
  [canvas graphics board]
  (let [rows (count board)
        cols (count (first board))
        cellh (/ (seesaw/height canvas) rows)
        cellw (/ (seesaw/width canvas) cols)
        rx (* 0.25 cellw)
        ry (* 0.25 cellh)]
    (fn [x y colour]
      (graphics/draw graphics
                     (graphics/rounded-rect (inc (* x cellw)) (inc (* y cellh))
                                            (- cellw 2) (- cellh 2)
                                            rx ry)
                     (graphics/style :background colour)))))

(defn ^:private render-board!
  "Draws the given game state to the canvas using the graphics context."
  [render-block board]
  (let [rows (count board)
        cols (count (first board))]
    (doseq [x (range cols)
            y (range rows)]
      (when-not (zero? (get-in board [y x]))
        (render-block x y :grey)))))

(defn ^:private render-tetrimino!
  "Draws the tetrimino at the given position"
  [render-block game]
  (dorun (map (fn [[x y]] (render-block x y (if (tetris/has-collision? game)
                                              :red
                                              :yellow
                                              )))
              (tetris/tetrimino-positions game))))

(defn ^:private render-game!
  "Draws the given game state to the canvas using the graphics context."
  [canvas graphics {:keys [board] :as game}]
  (let [render-block (make-block-renderer canvas graphics board)]
    (render-board! render-block board)
    (render-tetrimino! render-block game)))

(defn ^:private add-behaviours
  "Takes a game user interface and adds the dynamic behaviours for the game.
  Returns the UI."
  [root game-atom]
  (seesaw/listen (seesaw/select root [:#new-game])
                 :action  (fn [_] (reset! game-atom (tetris/new-game))))
  (doto root
    (keymap/map-key "W" (fn [_] (swap! game-atom tetris/move-if-valid tetris/rotate-tetrimino)))
    (keymap/map-key "A" (fn [_] (swap! game-atom tetris/move-if-valid tetris/move-left)))
    (keymap/map-key "S" (fn [_] (swap! game-atom tetris/move-if-valid tetris/move-down)))
    (keymap/map-key "D" (fn [_] (swap! game-atom tetris/move-if-valid tetris/move-right)))
    (keymap/map-key "P" (fn [_] (swap! game-atom tetris/drop-tetrimino)))
    (seesaw/listen :window-closing (fn [_] (remove-watch game-atom :gui))))
  (let [canvas (seesaw/select root [:#canvas])]
    (seesaw/config! canvas
                    :paint (fn [canvas graphics] (render-game! canvas graphics @game-atom)))
    (add-watch game-atom :gui (fn [_ _ _ _] (seesaw/repaint! canvas))))
  root)

(defn create-gui!
  "Creates a new user interface that will respond to the given game reference."
  [game-atom]
  (-> (make-ui)
      (add-behaviours game-atom)
      (seesaw/pack!) 
      (seesaw/show!)))

(comment
  (require ['seesaw.dev :refer :all])
  (show-events (seesaw/frame)))

(defn -main
  "Main entry point for the application."
  [& args]
  (let [game (atom (tetris/new-game))
        gui (create-gui! game)]
    (seesaw/listen gui
      :window-closed (fn [_] (System/exit 0)))))

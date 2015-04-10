(ns tetris.core-test
  (:require [clojure.test :refer :all]
            [tetris.core :refer :all]))

(deftest new-board-test
  (let [board (new-board)]
    (testing "right number of rows"
      (is (= board-height (count board))))
    (testing "each row is the right size"
      (is (every? (fn [row] (= board-width (count row)))
                  board)))
    (testing "the entire board has zeroes"
      (is (every? zero? (flatten board))))))

(deftest row->str-test
  (is (= "   " (row->str [0 0 0])))
  (is (= "XXX" (row->str [1 1 1])))
  (is (= "X X" (row->str [1 0 1]))))

(deftest board->str-test
  (is (= "\n\n" (board->str [[] []])))
  (is (= "   \nXXX\n" (board->str [[0 0 0] [1 1 1]]))))

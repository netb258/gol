;; Conway's Game of Life
(require '[quil.core :as q])
(require '[clojure.string :as s])

(set! *warn-on-reflection* true)

(defmacro safe-let
  "Same as a let form, but will just return nil, if any of it's bindings shit themselves with an Exception."
  [bindings & body]
  `(try
     (let [~@bindings] ~@body)
     (catch Exception _#
       nil)))

(def WINDOW-WIDTH 500)
(def WINDOW-HEIGHT 500)
(def SQUARE-WIDTH 10)
(def SQUARE-HEIGHT 10)
(def FPS 10)

;; NOTE: The world is a 50x50 matrix.
;; The cells are represented as maps, like this {:row int :col int :val str}.
;; If the :val is "s", then the cell is populated.
(defn get-empty-matrix []
  (into
    (vector)
    (for [i (range 50)
          :let [row (into
                      (vector)
                      (map (fn [col] {:row i :col col :val "."}) (range 50)))]]
      row)))

(def WORLD (atom (get-empty-matrix)))

(defn place-cell! [row col]
  (safe-let [target-row (nth @WORLD row)]
            (swap! WORLD (fn [w] (assoc w row (assoc target-row col {:row row :col col :val "s"}))))))

(defn is-populated? [row col]
  (safe-let [target-row (nth @WORLD row)
             target-col (nth target-row col)]
            (= (:val target-col) "s")))

(defn get-neighbours
  "The neighbours are returned as a vector of vectors in this format:
  [[row1 col1] [row2 col2] ...]"
  [row col]
  (let [n1 [(- row 1) (- col 1)]
        n2 [(- row 1) col]
        n3 [(- row 1) (+ col 1)]
        n4 [row (- col 1)]
        n5 [row (+ col 1)]
        n6 [(+ row 1) (- col 1)]
        n7 [(+ row 1) col]
        n8 [(+ row 1) (+ col 1)]]
    [n1 n2 n3 n4 n5 n6 n7 n8]))

(defn get-neighbours-population
  "Returns a list of 8 items (one for each neighbour), they can be either: true, false or nil.
  True indicates the neighbour is populated, false is not populated and nil indicates there is no such neighbour."
  [row col]
  (let [neighbours (get-neighbours row col)]
    (map #(is-populated? (first %) (second %)) neighbours)))

(defn game [row-of-cells]
  (into
    (vector)
    (map (fn [cell]
           (let [neighbours-population (get-neighbours-population (:row cell) (:col cell))
                 npopulation-size (count (filter #{true} neighbours-population))]
             (cond
               (and (= "s" (:val cell)) (< npopulation-size 2)) (assoc cell :val ".")
               (and (= "s" (:val cell)) (or (= npopulation-size 2) (= npopulation-size 3))) (assoc cell :val "s")
               (and (= "s" (:val cell)) (> npopulation-size 3)) (assoc cell :val ".")
               (and (= "." (:val cell)) (= npopulation-size 3)) (assoc cell :val "s")
               :else cell)))
         row-of-cells)))

(defn setup []
  (q/frame-rate FPS)
  (q/stroke 0)
  (q/stroke-weight 0)
  (q/background 255 255 255))

(defn get-color
  [ch]
  (cond
    (= \s ch) (q/fill 0 0 0)
    :else (q/fill 255 255 255)))

(defn print-line!
  [text lnum use-color]
  (doall
    (map-indexed
      (fn [idx ch]
        (get-color ch)
        (q/rect (* idx SQUARE-WIDTH) (* lnum SQUARE-HEIGHT) SQUARE-WIDTH SQUARE-HEIGHT))
      text)))

(defn print-matrix!
  [matrix offset]
  (if (empty? matrix) (recur (get-empty-matrix) offset)
    (let [get-vals (fn [row] (map #(:val %) row))
          lines (map #(s/join "" (get-vals %)) matrix)]
      (doseq [[line i] (map list lines (range (count lines)))]
        (print-line! line (+ i offset) true)))))

(defn show-world! []
  (print-matrix! @WORLD 0)
  (swap! WORLD (fn [w] (into [] (map game w)))))

(defn init-game! []
  ;; Blinker:
  (place-cell! 12 5)
  (place-cell! 12 6)
  (place-cell! 12 7)

  ;; Block:
  (place-cell! 15 10)
  (place-cell! 15 11)
  (place-cell! 16 10)
  (place-cell! 16 11)

  ;; Glider:
  (place-cell! 18 21)
  (place-cell! 19 22)
  (place-cell! 20 20)
  (place-cell! 20 21)
  (place-cell! 20 22)

  ;; Gosper's glider gun:
  (place-cell! 1 25)
  (place-cell! 2 23)
  (place-cell! 2 25)
  (place-cell! 3 13)
  (place-cell! 3 14)
  (place-cell! 3 21)
  (place-cell! 3 22)
  (place-cell! 3 35)
  (place-cell! 3 36)
  (place-cell! 4 12)
  (place-cell! 4 16)
  (place-cell! 4 21)
  (place-cell! 4 22)
  (place-cell! 4 35)
  (place-cell! 4 36)
  (place-cell! 5 1)
  (place-cell! 5 2)
  (place-cell! 5 11)
  (place-cell! 5 17)
  (place-cell! 5 21)
  (place-cell! 5 22)
  (place-cell! 6 1)
  (place-cell! 6 2)
  (place-cell! 6 11)
  (place-cell! 6 15)
  (place-cell! 6 17)
  (place-cell! 6 18)
  (place-cell! 6 23)
  (place-cell! 6 25)
  (place-cell! 7 11)
  (place-cell! 7 17)
  (place-cell! 7 25)
  (place-cell! 8 12)
  (place-cell! 8 16)
  (place-cell! 9 13)
  (place-cell! 9 14))

(defn -main []
  (println "Done!") ;; Signal that we have loaded the program.
  (init-game!)
  (q/defsketch gol
    :title "Game of life"
    :settings #(q/smooth 2)
    :setup setup
    :draw show-world!
    :features [:exit-on-close]
    :size [WINDOW-WIDTH WINDOW-HEIGHT]))

(-main)

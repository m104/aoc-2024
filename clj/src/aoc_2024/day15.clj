(ns aoc-2024.day15
  (:require [aoc-2024.helpers :refer [load-lines lines->grid idx->coord
                                      report-grid get-in-grid set-in-grid
                                      find-grid-locations apply-2d]]
            [clojure.set :refer [difference union intersection]]))

(def move-codes
  {\^ [0 -1]
   \> [1  0]
   \v [0  1]
   \< [-1 0]})

(defn parse-lines
  [lines]
  (let [divider (.indexOf lines "")
        grid-lines (subvec lines 0 divider)
        grid (lines->grid grid-lines)
        move-lines (subvec lines (inc divider))
        coded-moves (apply concat move-lines)
        moves (mapv #(get move-codes %) coded-moves)
        locations (find-grid-locations grid #{\@})
        bot (first (get locations \@))
        grid (set-in-grid grid (first bot) (second bot) \.)]
    {:grid grid
     :bot bot
     :moves moves}))

(defn get-grid-strip
  [grid coord dir usable]
  (loop [coord (apply-2d + coord dir)
         strip []]
    (let [[x y] coord
          value (get-in-grid grid x y)]
      (if (contains? usable value)
        (recur (apply-2d + coord dir) (conj strip [coord value]))
        strip))))

(defn do-bot-move
  [grid bot move]
  (let [strip (get-grid-strip grid bot move #{\. \O})
        values (mapv second strip)
        space-idx (.indexOf values \.)
        new-bot (apply-2d + bot move)]
    (cond (= -1 space-idx) {:grid grid :bot bot}
          (= 0 space-idx) {:grid grid :bot new-bot}
          :else (let [space-place (last (take (inc space-idx) (mapv first strip)))
                      [bot-x bot-y] new-bot
                      [box-x box-y] space-place]
                  {:grid (-> grid
                             (set-in-grid bot-x bot-y \.)
                             (set-in-grid box-x box-y \O))
                   :bot new-bot}))))

(defn do-bot-moves
  [grid bot moves]
  (loop [grid grid
         bot bot
         [move & moves] moves]
    ;(report-grid (set-in-grid grid (first bot) (second bot) \@))
    (if (nil? move)
      {:grid grid :bot bot}
      (let [{:keys [grid bot]} (do-bot-move grid bot move)]
        (recur grid bot moves)))))

(defn part1
  [lines]
  (let [{:keys [grid bot moves]} (->> lines parse-lines)
        {:keys [grid bot]} (do-bot-moves grid bot moves)
        box-coords (get (find-grid-locations grid #{\O}) \O)
        box-gps-idxs (mapv (fn [[x y]] (+ (* y 100) x)) box-coords)
        gps-sum (reduce + box-gps-idxs)]
    ;(println "Final grid")
    ;(report-grid (set-in-grid grid (first bot) (second bot) \@))
    gps-sum))

;(part1 (load-lines "day15-test.txt"))
;(part1 (load-lines "day15.txt"))

(defn widen-coord [[x y]] [(* 2 x) y])

(defn draw-expanded-grid [bot boxes walls]
  (let [width (inc (apply max (mapv first walls)))
        height (inc (apply max (mapv second walls)))]
    (println "Expanded grid:")
    (doseq [y (range 0 height)]
      (doseq [x (range 0 width)]
        (let [coord [x y]
              value (cond (= bot coord) "@"
                          (contains? walls coord) "#"
                          (contains? boxes coord) "["
                          (contains? boxes (apply-2d + coord [-1 0])) "]"
                          :else ".")]
          (print value)))
      (println))
    (println)))

(defn find-mobile-boxes
  [walls boxes box dir]
  (let [other-boxes (difference boxes #{box})
        new-box-l (apply-2d + box dir)
        new-box-r (apply-2d + new-box-l [1 0])
        new-box-set #{new-box-l new-box-r}
        wall-collision? (some? (seq (intersection walls new-box-set)))]
    (if wall-collision?
      #{:wall}
      (let [mobile-boxes (union (intersection other-boxes new-box-set)
                                (intersection other-boxes
                                              (set (mapv #(apply-2d + % [-1 0]) new-box-set))))
            further-boxes (apply union (mapv #(find-mobile-boxes walls boxes % dir) mobile-boxes))]
        (if (contains? further-boxes :wall)
          #{:wall}
          (union #{box} mobile-boxes further-boxes))))))

(defn push-boxes
  [boxes mobile-boxes dir]
  (let [stable-boxes (difference boxes mobile-boxes)
        moved-boxes (into #{} (mapv #(apply-2d + % dir) mobile-boxes))]
    (union stable-boxes moved-boxes)))

(defn do-expanded-bot-move
  [walls bot boxes move]
  (let [new-bot (apply-2d + bot move)
        wall? (contains? walls new-bot)
        box (or (some boxes #{new-bot})
                (some boxes #{(apply-2d + new-bot [-1 0])}))
        mobile-boxes (when box (find-mobile-boxes walls boxes box move))]
    (cond wall? {:bot bot :boxes boxes}
          (nil? box) {:bot new-bot :boxes boxes}
          (and box (contains? mobile-boxes :wall)) {:bot bot :boxes boxes}
          :else {:bot new-bot :boxes (push-boxes boxes mobile-boxes move)})))

(defn do-expanded-bot-moves
  [walls bot boxes moves]
  (loop [bot bot
         boxes boxes
         [move & moves] moves]
    ;(draw-expanded-grid bot boxes walls)
    (if (nil? move)
      {:bot bot :boxes boxes}
      (let [{:keys [bot boxes]} (do-expanded-bot-move walls bot boxes move)]
        (recur bot boxes moves)))))


(defn part2
  [lines]
  (let [{:keys [grid bot moves]} (->> lines parse-lines)
        bot (widen-coord bot)
        locations (find-grid-locations grid #{\O \# \@})
        walls-l (mapv widen-coord (get locations \#))
        walls-r (mapv (fn [[x y]] [(inc x) y]) walls-l)
        walls (into #{} (concat walls-l walls-r))
        boxes (into #{} (mapv widen-coord (get locations \O)))
        {:keys [bot boxes]} (do-expanded-bot-moves walls bot boxes moves)

        box-gps-idxs (mapv (fn [[x y]] (+ (* y 100) x)) boxes)
        gps-sum (reduce + box-gps-idxs)]
    ;(draw-expanded-grid bot boxes walls)
    gps-sum))

;(part2 (load-lines "day15-test.txt"))
;(part2 (load-lines "day15.txt"))

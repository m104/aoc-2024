(ns aoc-2024.core
  (:require [aoc-2024.helpers :refer [load-lines str->int]]
            [criterium.core :refer [quick-benchmark]])
  (:gen-class))

(def problems-by-day
  {1 {:title "Historian Hysteria"
      :description "sorted lists -> frequency list"
      :comments []
      :answers [{:test 11 :submit 1889772}
                {:test 31 :submit 23228917}]}
   2 {:title "Red-Nosed Reports"
      :description "reactor reports -> report redactions"
      :comments ["Struggled with debugging a delta list"
                 "Reverted to brute force"
                 "May resume with a transducer"]
      :answers [{:test 2 :submit 332}
                {:test 4 :submit 398}]}
   3 {:title "Mull It Over"
      :description "parse instructions -> small state machine"
      :comments []
      :answers [{:test 161 :submit 166357705}
                {:test2 48 :submit 88811886}]}
   4 {:title "Ceres Search"
      :description "word search -> combined word search"
      :comments ["Fun intro grid search problem"
                 "Got super lucky by returning the coords and dirs in part1"
                 "Part 2 head scratcher: only need to search 4 directions"
                 "Reworked to avoid searching some fruitless directions"]
      :answers [{:test 18 :submit 2613}
                {:test 9 :submit 1905}]}
   5 {:title "Print Queue"
      :description "page orderings -> page reorderings"
      :comments ["Started with a basic 'did not already see' restriction list"
                 "Straightforward to validate the page lists"
                 "Tried a more complicated fix method for part 2"
                 "Realized this was just sorting after taking a brute force approach"]
      :answers [{:test 143 :submit 5329}
                {:test 123 :submit 5833}]}
   6 {:title "Guard Gallivant"
      :description "grid walk + check -> grid loop testing"
      :comments ["Struggled a lot with part2, specifically the runtime"
                 "Should refactor with a more efficient grid data structure"]
      :answers [{:test 41 :submit 4696}
                {:test 6 :submit 1443}]}
   7 {:title "Bridge Repair"
      :description "reverse calc -> extended reverse calc"
      :comments ["Part 2 kinda hurt"
                 "Should re-approach with a reverse calculation"]
      :answers [{:test 3749 :submit 2299996598890}
                {:test 11387 :submit 362646859298554}]}
   8 {:title "Resonant Collinearity"
      :description "closest antinodes -> all antinodes"
      :comments ["Pretty straightforward: just run out the deltas on each coord pair"
                 "Part 2 forced some code cleanup"
                 "This would be fun to revisit with a clean generator fn"]
      :answers [{:test 14 :submit 289}
                {:test 34 :submit 1030}]}
   9 {:title "Disk Fragmenter"
      :description "defrag blocks + checksum -> defrag files + checksum"
      :comments ["Part 1 not so bad by expanding the blocks"
                 "Got stuck managing the free space lists"
                 "Part 2 may have been better with a tree"]
      :answers [{:test 1928 :submit 6463499258318}
                {:test 2858 :submit 6493634986625}]}
   10 {:title "Hoof It"
       :description "unique path destinations -> unique paths"
       :comments ["Part 1: fun with recursive nagivation"
                  "Part 2: simple tweak to reducing the path info"]
       :answers [{:test 36 :submit 624}
                 {:test 81 :submit 1483}]}
   11 {:title "Plutonian Pebbles"
       :description "in memory -> out of memory"
       :comments ["Fun computation puzzle"
                  "Part 1: simple list concats"
                  "Part 2: test ran at 45 generations"
                  "Glad I didn't need big math here"
                  "Counts are the way!"]
       :answers [{:test 55312 :submit 199982}
                 {:test 65601038650482 :submit 237149922829154}]}
   12 {:title "Garden Groups"
       :description "area + perimeter -> area + faces"
       :comments ["Wow a really good challenge"
                  "Lots of debugging with the region forming fn"
                  "Part 1 very simple afterwards"
                  "Part 2 stuck while looking for an algorithm"
                  "Faces = regions of directed neighbors"
                  "Got very lucky with the region forming fn"]
       :answers [{:test 1930 :submit 1483212}
                 {:test 1206 :test2 368 :submit 897062}]}
   13 {:title "Claw Contraption"
       :description "look for best solution -> derive maths"
       :comments ["Part 1 was fun, particularly writing different solutions"
                  "Part 2 was like, oh, maths"
                  "Nearly all of the time looking for edge cases was unnecessary"
                  "Overall this is just a math problem plus fancy parsing"]
       :answers [{:test 480 :submit 29187}
                 {:test 875318608908 :submit 99968222587852}]}
   ;
   })

(comment
  {:title ""
   :description ""
   :comments []
   :answers [{:test nil :submit nil}
             {:test nil :submit nil}]})

(defn unload-solution-ns
  [day]
  (let [ns-str (format "aoc-2024.day%02d" day)]
    (remove-ns (symbol ns-str))))

(defn load-solution-ns
  [day]
  (let [filename (format "src/aoc_2024/day%02d.clj" day)]
    (load-file filename)))

(defn reload-solutions
  [day]
  (unload-solution-ns day)
  (load-solution-ns day))

(defn input-for-day
  ([day variation]
   (let [filename (str (format "day%02d" day)
                       (when (not= :submit variation)
                         (str "-" (name variation)))
                       ".txt")]
     (load-lines filename))))

(defn run-trial
  [day part variation]
  (let [ns-str (str "aoc-2024.day" (format "%02d" day))
        f (resolve (symbol ns-str (str "part" part)))
        input (input-for-day day variation)
        start-ts (. System (nanoTime))
        output (f input)
        ms (int (/ (double (- (. System (nanoTime)) start-ts)) 1000000.0))
        answers (get-in problems-by-day [day :answers])
        expected (get (nth answers (dec part)) variation)
        ok (= output expected)
        [seconds ms] [(quot ms 1000) (rem ms 1000)]
        [minutes seconds] [(quot seconds 60) (rem seconds 60)]]
    {:day day
     :part part
     :variation variation
     :time [minutes seconds ms]
     :output output
     :expected expected
     :ok ok}))

(let [ms (int 65078.56)
      [seconds ms] [(quot ms 1000) (rem ms 1000)]
      [minutes seconds] [(quot seconds 60) (rem seconds 60)]]
  [minutes seconds ms])

(defn report-trial
  [day part variation]
  (let [{:keys [ok output expected time]} (run-trial day part variation)
        [minutes seconds ms] time
        time-str (format "(%02d:%02d.%03d)" minutes seconds ms)]
    (if ok
      (str "✅ output: " output " " time-str)
      (str "❌ output: " output " expected: " expected " " time-str))))

(defn -main
  [& args]
  (let [days (if args
               [(str->int (first args))]
               (sort (keys problems-by-day)))]
    (doseq [day days] (reload-solutions day))
    (println "Advent of Code, 2024")
    (println)
    (doseq [day days]
      (let [{:keys [title answers]} (get problems-by-day day)]
        (println (str "day" (format "%02d" day) " - " title))
        (doseq [part (mapv inc (range 0 (count answers)))]
          (println (str "  part" part ":"))
          (doseq [[variation _] (nth answers (dec part))]
            (print (format "    %6s: " (name variation)))
            (print (report-trial day part variation))
            (println))))
      (println))))

(comment
  (defn run-repl-trial
    [day part variation]
    (reload-solutions day)
    (run-trial day part variation))

  (run-repl-trial 1 1 :test)
  ;
  )
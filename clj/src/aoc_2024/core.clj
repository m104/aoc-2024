(ns aoc-2024.core
  (:require [aoc-2024.helpers :refer [load-lines]]
            [criterium.core :refer [quick-benchmark]])
  (:gen-class))

(def template
  {:title ""
   :description ""
   :comments []
   :answers [{:test nil :submit nil}
             {:test nil :submit nil}]})

(def days
  {1 {:title "Historian Hysteria"
      :description "sorted lists -> frequency list"
      :comments []
      :answers [{:test 11 :submit 1889772}
                {:test 31 :submit 23228917}]}
   2 {:title "Red-Nosed Reports"
      :description "reactor reports -> reports redactions"
      :comments ["Struggled with debugging a delta list"
                 "Reverted to brute force"
                 "May resume with a transducer"]
      :answers [{:test 2 :submit 332}
                {:test 4 :submit 398}]}
   3 {:title "Mull It Over"
      :description "parse instructions -> small state machine"
      :comments []
      :answers [{:test 161 :submit 166357705}
                {:test2 48 :submit 88811886}]}})

(defn unload-solution-ns
  [day]
  (let [ns-str (format "aoc-2024.day%02d" day)]
    (remove-ns (symbol ns-str))))

(defn load-solution-ns
  [day]
  (let [filename (format "src/aoc_2024/day%02d.clj" day)]
    (load-file filename)))

(defn reload-solutions
  []
  (doseq [day (keys days)]
    (unload-solution-ns day)
    (load-solution-ns day)))

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
        output (f input)
        ;bench (quick-benchmark (f input) {})
        answers (get-in days [day :answers])
        expected (get (nth answers (dec part)) variation)
        ok (= output expected)]
    {:day day
     :part part
     :variation variation
     :output output
     :expected expected
     :ok ok}))

(defn report-trial
  [day part variation]
  (let [{:keys [ok output expected]} (run-trial day part variation)]
    (if ok
      (str "✅ output: " output)
      (str "❌ output: " output " expected: " expected))))

(defn -main
  [& _]
  (reload-solutions)
  (println "Advent of Code, 2024")
  (println)
  (doseq [[day {:keys [title answers]}] days]
    (println (str "day" (format "%02d" day) " - " title))
    (doseq [part (mapv inc (range 0 (count answers)))]
      (println (str "  part" part ":"))
      (doseq [[variation _] (nth answers (dec part))]
        (print (format "    %6s: " (name variation)))
        (print (report-trial day part variation))
        (println)))
    (println)))

(comment
  (defn run-repl-trial
    [day part variation]
    (unload-solution-ns day)
    (load-solution-ns day)
    (run-trial day part variation))

  (run-repl-trial 1 1 :test)

  ;
  )
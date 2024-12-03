(ns aoc-2024.core
  (:require [aoc-2024.helpers :refer [load-lines]])
  (:gen-class))

(def template
  {:title ""
   :description ""
   :comments nil
   :test-answers []
   :answers []})

(def days
  {1 {:title "Historian Hysteria"
      :description "sorted lists -> frequency list"
      :comments nil
      :test-answers [11 31]
      :answers [1889772 23228917]}
   2 {:title "Red-Nosed Reports"
      :description "reactor reports -> reports with up to one redaction"
      :comments nil
      :test-answers [2 4]
      :answers [332 398]}
   3 {:title "Mull It Over"
      :description ""
      :comments nil
      :test-answers [161 48]
      :answers [166357705 88811886]}})

(defn unload-day-ns
  [day]
  (let [ns-str (format "aoc-2024.day%02d" day)]
    (remove-ns (symbol ns-str))))

(defn load-day-ns
  [day]
  (let [filename (format "src/aoc_2024/day%02d.clj" day)]
    (load-file filename)))

(defn reload-days
  []
  (doseq [day (keys days)]
    (unload-day-ns day)
    (load-day-ns day)))

(defn lines-for-day
  ([day] (lines-for-day day false))
  ([day test?]
   (let [filename (str (format "day%02d" day)
                       (when test? "-test")
                       ".txt")]
     (load-lines filename))))

(defn run-trial
  [day part test?]
  (let [ns-str (str "aoc-2024.day" (format "%02d" day))
        f (resolve (symbol ns-str (str "part" part)))
        input (lines-for-day day test?)
        output (f input)
        expected (nth (get-in days [day (if test? :test-answers :answers)])
                      (dec part))
        ok (= output expected)]
    {:day day
     :part part
     :test test?
     :output output
     :expected expected
     :ok ok}))

(defn report-trial
  [day part test?]
  (let [{:keys [ok output expected]} (run-trial day part test?)]
    (if ok "✅ " "❌ ")))

(defn -main
  [& _]
  (reload-days)
  (doseq [[day {:keys [title test-answers answers]}] days]
    (let [test-count (count test-answers)
          answer-count (count answers)]
      (println (str "[" (format "%02d" day) "] " title " (" test-count "/" answer-count ")"))
      (print "  part1: ")
      (if (>= test-count 1)
        (print (report-trial day 1 true))
        (print "-  "))
      (if (>= answer-count 1)
        (print (report-trial day 1 false))
        (print "-  "))
      (println)
      (print "  part2: ")
      (if (>= test-count 2)
        (print (report-trial day 2 true))
        (print "-  "))
      (if (>= answer-count 2)
        (print (report-trial day 2 false))
        (print "-  "))
      (println))))

(defn run-repl-trial
  [day part test?]
  (unload-day-ns day)
  (load-day-ns day)
  (run-trial day part test?))

;; (run-repl-trial 1 1 true)

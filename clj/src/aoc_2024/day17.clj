(ns aoc-2024.day17
  (:require [aoc-2024.helpers :refer [load-lines str->int]]
            [clojure.string :refer [split lower-case join]]
            [clojure.math :as math]))

(defn parse-reg
  [s]
  (let [[reg v] (->> s (re-seq #"Register ([A-C]): (\d+)") first next)]
    {(keyword (str "reg-" (lower-case reg))) (str->int v)}))

(defn parse-program
  [s]
  (let [op-str (->> s (re-seq #"Program: ([0-9,]+)")
                    first next first)]
    (mapv str->int (split op-str #","))))

(defn parse-input
  [lines]
  (let [reg-strs (take 3 lines)
        reg-maps (mapv parse-reg reg-strs)
        regs (reduce merge reg-maps)
        program-str (nth lines 4)
        program (parse-program program-str)]
    (merge {:program program
            :ip 0
            :out []}
           regs)))

(defn combo-operand
  [operand regs]
  (case operand
    4 (:reg-a regs)
    5 (:reg-b regs)
    6 (:reg-c regs)
    7 nil ; invalid
    operand ; literal
    ))

(defn op-div
  [out-reg cmb regs]
  (let [num (:reg-a regs)
        denom (long (math/pow 2 (combo-operand cmb regs)))]
    {out-reg (long (/ num denom))}))

(def ops
  [[:adv (partial op-div :reg-a)]
   [:bxl (fn [lit regs] {:reg-b (bit-xor (:reg-b regs) lit)})]
   [:bst (fn [cmb regs] {:reg-b (mod (combo-operand cmb regs) 8)})]
   [:jnz (fn [lit regs] (when (not= 0 (:reg-a regs))
                          {:ip lit}))]
   [:bxc (fn [_ regs] {:reg-b (bit-xor (:reg-b regs) (:reg-c regs))})]
   [:out (fn [cmb regs] {:out (mod (combo-operand cmb regs) 8)})]
   [:bdv (partial op-div :reg-b)]
   [:cdv (partial op-div :reg-c)]])

(defn run-machine-cycle
  [machine]
  (let [{:keys [ip program out]} machine
        regs (select-keys machine [:reg-a :reg-b :reg-c])
        op-code (nth program ip)
        operand (nth program (inc ip))
        [_ op-fn] (nth ops op-code)
        result (op-fn operand regs)]
    (-> machine
        (merge (select-keys result [:reg-a :reg-b :reg-c]))
        (assoc :ip (get result :ip (+ 2 ip)))
        (assoc :out (if-let [added-out (get result :out)]
                      (conj out added-out)
                      out)))))

(defn run-machine
  [machine]
  (loop [{:keys [ip out program] :as machine} machine]
    (if (and ip (< ip (count program)))
      (recur (run-machine-cycle machine))
      out)))

(defn part1
  [lines]
  (->> lines
       parse-input
       run-machine
       (join ",")))

;(part1 (load-lines "day17-test.txt"))
;(part1 (load-lines "day17.txt"))

(defn run-with-reg-a [machine value]
  (-> (assoc machine :reg-a value)
      (run-machine)))

(defn partial-solution
  [machine power known target]
  (->> (range 0 8)
       (mapv #(long (+ known (* % (math/pow 8 power)))))
       (filterv (fn [reg-a]
                  (let [output (run-with-reg-a machine reg-a)
                        tail-size (- (count target) power)
                        target-tail (into [] (take-last tail-size target))
                        output-tail (into [] (take-last tail-size output))
                        verdict? (and (= (count target) (count output))
                                      (= target-tail
                                         output-tail))]
                    verdict?)))
       (mapv #(vector (dec power) %))))

(defn search-solutions
  [machine target]
  (loop [[[power known] & untested] (list [(dec (count target)) 0])
         matches []]
    (if (nil? power)
      matches
      (let [possibilities (partial-solution machine power known target)]
        (if (= 0 power)
          (recur untested (into [] (concat matches (mapv second possibilities))))
          (recur (concat possibilities untested) matches))))))

(defn part2
  [lines]
  (let [machine (->> lines parse-input)
        expected (:program machine)
        solutions (search-solutions machine expected)]
    (first (sort solutions))))

;(part2 (load-lines "day17-test2.txt"))
;(part2 (load-lines "day17.txt"))

(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not (= (count a-seq) (count b-seq))) false
    (not (= (first a-seq) (first b-seq))) false
    :else (recur (rest a-seq) (rest b-seq))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         new-seq a-seq]
    (cond
      (empty? new-seq) nil
      (pred (first new-seq)) index
      :else (recur (inc index) (rest new-seq)))))

(defn avg [a-seq]
  (loop [n-values 0
         sum 0
         new-seq a-seq]
    (if (empty? new-seq)
      (/ sum n-values)
      (recur (inc n-values) (+ sum (first new-seq)) (rest new-seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
         (if (contains? a-set elem)
           (disj a-set elem)
           (conj a-set elem)))]
    (loop [new-set #{}
           new-seq a-seq]
      (if (empty? new-seq)
        new-set
        (recur (toggle new-set (first new-seq)) (rest new-seq))))))

(defn fast-fibo [n]
  (loop [fn-2 0
         fn-1 1
         k n]
    (if (zero? k)
      fn-2
      (recur fn-1 (+ fn-2 fn-1) (dec k)))))

(defn cut-at-repetition [a-seq]
  (loop [new-seq a-seq
         elems #{}
         cut []]
    (if (or (empty? new-seq) (contains? elems (first new-seq)))
      cut
      (recur (rest new-seq) (conj elems (first new-seq)) (conj cut (first new-seq))))))

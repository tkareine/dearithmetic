(ns dearithmetic.classic)

(defn to-int-list [str]
  (map #(Character/getNumericValue %) str))

(defn left-pad-to-longest [padding a b]
  (let [diff (- (count a) (count b))]
    (if (> diff 0)
      (list a (concat (repeat diff padding) b))
      (list (concat (repeat (Math/abs diff) padding) a) b))))

(defn operate-on-pairs [op pairs]
  (loop [carry 0 result '() remaining pairs]
    (if (seq remaining)
      (let [[a b] (first remaining)
            r (+ (op a b) carry)]
        (recur (quot r 10) (cons (rem r 10) result) (rest remaining)))
      (if (> carry 0)
        (cons carry result)
        result))))

(defn multiply-pairs [pairs]
  (operate-on-pairs * pairs))

(defn sum-pairs [pairs]
  (operate-on-pairs + pairs))

(defn create-direct-pairs [as bs]
  (let [[pas pbs] (left-pad-to-longest 0 as bs)]
    (partition 2 (interleave (reverse pas) (reverse pbs)))))

(defn sum-num-seqs [num-seq & num-seqs]
  (loop [sum num-seq remaining num-seqs]
    (if (seq remaining)
      (let [pairs (create-direct-pairs sum (first remaining))]
        (recur (sum-pairs pairs) (rest remaining)))
      sum)))

(defn create-cross-pairs [as bs]
  (let [[pas pbs] (left-pad-to-longest 0 as bs)]
    (for [b (reverse pas)]
      (for [a (reverse pbs)]
        (list b a)))))

(defn index-pairs [pairs]
  (loop [result [] index 0 remaining pairs]
    (if (seq remaining)
      (recur (conj result (list (first remaining) index)) (inc index) (rest remaining))
      result)))

(defn clean-num-seq [num-seq]
  (let [result (drop-while #(= 0 %) num-seq)]
    (if (empty? result)
      '(0)
      result)))

(defn multiply-num-seqs [num-seq & num-seqs]
  (loop [product num-seq remaining num-seqs]
    (if (seq remaining)
      (let [new-product (reduce
                            (fn [sum [pairs order]]
                              (let [inter-product (concat (multiply-pairs pairs) (repeat order 0))]
                                (sum-num-seqs sum inter-product)))
                            '(0)
                            (index-pairs (create-cross-pairs product (first remaining))))]
        (recur new-product (rest remaining)))
      (clean-num-seq product))))

(defn longhand [as bs]
  (apply str (multiply-num-seqs (to-int-list as) (to-int-list bs))))

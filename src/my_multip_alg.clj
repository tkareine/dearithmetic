(ns my-multip-alg
  (:use clojure.test))

(defn- to-int-list [str]
  (map #(Character/getNumericValue %) str))

(defn- left-pad-to-longest [padding a b]
  (let [diff (- (count a) (count b))]
    (if (> diff 0)
      (list a (concat (repeat diff padding) b))
      (list (concat (repeat (Math/abs diff) padding) a) b))))

(defn- multiply-pairs [pairs]
  (loop [carry 0 result '() remaining pairs]
    (if (seq remaining)
      (let [[a b] (first remaining)
            r (+ (* a b) carry)]
        (recur (quot r 10) (cons (rem r 10) result) (rest remaining)))
      (if (> carry 0)
        (cons carry result)
        result))))

(defn- sum-pairs [pairs]
  (loop [carry 0 result '() remaining pairs]
    (if (seq remaining)
      (let [[a b] (first remaining)
            r (+ (+ a b) carry)]
        (recur (quot r 10) (cons (rem r 10) result) (rest remaining)))
      (if (> carry 0)
        (cons carry result)
        result))))

(defn- create-direct-pairs [as bs]
  (let [[pas pbs] (left-pad-to-longest 0 as bs)]
    (partition 2 (interleave (reverse pas) (reverse pbs)))))

(defn- sum-num-seqs [num-seq & num-seqs]
  (loop [sum num-seq remaining num-seqs]
    (if (seq remaining)
      (let [pairs (create-direct-pairs sum (first remaining))]
        (recur (sum-pairs pairs) (rest remaining)))
      sum)))

(defn- create-cross-pairs [as bs]
  (let [[pas pbs] (left-pad-to-longest 0 as bs)]
    (for [b (reverse pas)]
      (for [a (reverse pbs)]
        (list b a)))))

(defn- index-pairs [pairs]
  (loop [result [] index 0 remaining pairs]
    (if (seq remaining)
      (recur (conj result (list (first remaining) index)) (inc index) (rest remaining))
      result)))

(defn- clean-num-seq [num-seq]
  (let [result (drop-while #(= 0 %) num-seq)]
    (if (empty? result)
      '(0)
      result)))

(defn- multiply-num-seqs [num-seq & num-seqs]
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

(deftest test-left-pad-to-longest
  (is (= '((0 0) (2 3)) (left-pad-to-longest 0 '() '(2 3))))
  (is (= '((0 1) (2 3)) (left-pad-to-longest 0 '(1) '(2 3))))
  (is (= '((0 0 1) (2 3 4)) (left-pad-to-longest 0 '(1) '(2 3 4)))))

(deftest test-sum-pairs
  (is (= '()        (sum-pairs '())))
  (is (= '(5)       (sum-pairs '((3 2)))))
  (is (= '(1 1)     (sum-pairs '((3 8)))))
  (is (= '(1 3 1)   (sum-pairs '((3 8) (3 9)))))
  (is (= '(1 3 1 6) (sum-pairs '((8 8) (6 4) (3 9))))))

(deftest test-create-direct-pairs
  (is (= '((1 3) (0 2)) (create-direct-pairs '(1) '(2 3)))))

(deftest test-sum-num-seqs
  (is (= '(0)     (sum-num-seqs '(0))))
  (is (= '(3)     (sum-num-seqs '(3))))
  (is (= '(3)     (sum-num-seqs '(3) '(0))))
  (is (= '(5)     (sum-num-seqs '(3) '(2))))
  (is (= '(1 1)   (sum-num-seqs '(3) '(8))))
  (is (= '(7 7)   (sum-num-seqs '(3 8) '(3 9))))
  (is (= '(5 4 6) (sum-num-seqs '(8 8) '(6 4) '(3 9 4)))))

(deftest test-multiply-pairs
  (is (= '()        (multiply-pairs '())))
  (is (= '(6)       (multiply-pairs '((3 2)))))
  (is (= '(1 2 6)   (multiply-pairs '((3 2) (3 4)))))
  (is (= '(1 6 2)   (multiply-pairs '((3 4) (3 5)))))
  (is (= '(2 8 3 5) (multiply-pairs '((3 5) (3 4) (3 9))))))

(deftest test-create-cross-pairs
  (is (= '(((1 3) (1 2)) ((0 3) (0 2))) (create-cross-pairs '(1) '(2 3)))))

(deftest test-index-pairs
  (is (= ['("a" 0) '("b" 1)] (index-pairs '("a" "b"))))
  (is (= ['("a" 0)] (index-pairs '("a"))))
  (is (= [] (index-pairs '()))))

(deftest test-multiply-num-seqs
  (is (= '(3)       (multiply-num-seqs '(3))))
  (is (= '(0)       (multiply-num-seqs '(3) '(0))))
  (is (= '(6)       (multiply-num-seqs '(3) '(2))))
  (is (= '(2 4)     (multiply-num-seqs '(3) '(8))))
  (is (= '(5 4)     (multiply-num-seqs '(3) '(1 8))))
  (is (= '(1 4 8 2) (multiply-num-seqs '(3 8) '(3 9))))
  (is (= '(2 9 6 4) (multiply-num-seqs '(3 8) '(3 9) '(2)))))

(deftest test-clean-num-seq
  (is (= '(0) (clean-num-seq '(0))))
  (is (= '(0) (clean-num-seq '(0 0))))
  (is (= '(1) (clean-num-seq '(1))))
  (is (= '(1) (clean-num-seq '(0 1)))))

(deftest test-longhand
  (is (= "0"   (longhand "1"    "0")))
  (is (= "1"   (longhand "1"    "1")))
  (is (= "123" (longhand "1"    "123")))
  (is (= "369" (longhand "123"  "3")))
  (is (= "369" (longhand "3"    "123")))
  (is (= "62923130473259441604535" (longhand "50982345203045" "1234214123")))
  (is (= "62923130473259441604535" (longhand "1234214123" "50982345203045"))))

(run-tests 'my-multip-alg)

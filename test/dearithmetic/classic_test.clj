(ns dearithmetic.classic-test
  (:use clojure.test
        dearithmetic.classic))

(deftest test-left-pad-to-longest
  (is (= '((0 0) (2 3)) (left-pad-to-longest 0 '() '(2 3))))
  (is (= '((0 1) (2 3)) (left-pad-to-longest 0 '(1) '(2 3))))
  (is (= '((0 0 1) (2 3 4)) (left-pad-to-longest 0 '(1) '(2 3 4)))))

(deftest test-add-pairs
  (is (= '()        (add-pairs '())))
  (is (= '(5)       (add-pairs '((3 2)))))
  (is (= '(1 1)     (add-pairs '((3 8)))))
  (is (= '(1 3 1)   (add-pairs '((3 8) (3 9)))))
  (is (= '(1 3 1 6) (add-pairs '((8 8) (6 4) (3 9))))))

(deftest test-create-direct-pairs
  (is (= '((1 3) (0 2)) (create-direct-pairs '(1) '(2 3)))))

(deftest test-add-num-seqs
  (is (= '(0)     (add-num-seqs '(0))))
  (is (= '(3)     (add-num-seqs '(3))))
  (is (= '(3)     (add-num-seqs '(3) '(0))))
  (is (= '(5)     (add-num-seqs '(3) '(2))))
  (is (= '(1 1)   (add-num-seqs '(3) '(8))))
  (is (= '(7 7)   (add-num-seqs '(3 8) '(3 9))))
  (is (= '(5 4 6) (add-num-seqs '(8 8) '(6 4) '(3 9 4)))))

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

(deftest test-multiply-strs
  (is (= "0"   (multiply-strs "1"   "0")))
  (is (= "1"   (multiply-strs "1"   "1")))
  (is (= "123" (multiply-strs "1"   "123")))
  (is (= "369" (multiply-strs "123" "3")))
  (is (= "369" (multiply-strs "3"   "123")))
  (is (= "62923130473259441604535" (multiply-strs "50982345203045" "1234214123")))
  (is (= "62923130473259441604535" (multiply-strs "1234214123" "50982345203045"))))

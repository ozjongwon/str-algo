(defpackage str-algo/tests/main
  (:use :cl :str-algo :rove))

(in-package :str-algo/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :str-algo)' in your Lisp.
(deftest test-target-1
  (TESTING "Single char replacement"
    (OK (equalp (str-algo::diff-operations  "A" "B")'((:DELETE 0 0) (:INSERT 1 0)))))
  (TESTING "Single deletion at end"
    (OK (equalp (str-algo::diff-operations "AB" "A")'((:DELETE 1 1)))))
  (TESTING "Single insertion at end"
    (OK (equalp (str-algo::diff-operations "A" "AB")'((:INSERT 1 1)))))
  (TESTING "Multiple replacements"
    (OK
     (equalp (str-algo::diff-operations "AAAA" "BBBB")
             '((:DELETE 0 0) (:DELETE 1 0) (:DELETE 2 0) (:DELETE 3 0) (:INSERT 4 0)
               (:INSERT 4 1) (:INSERT 4 2) (:INSERT 4 3)))))
  (TESTING "Complex interleaved operations"
    (OK
     (equalp (str-algo::diff-operations "ABCABBA" "CBABAC")
             '((:DELETE 0 0) (:DELETE 1 0) (:KEEP 2 0) (:INSERT 3 1) (:KEEP 3 2)
               (:KEEP 4 3) (:DELETE 5 4) (:KEEP 6 4) (:INSERT 7 5)))))
  (TESTING "Alignment with surrounding context"
    (OK
     (equalp (str-algo::diff-operations "AABAA" "ABBA")
             '((:DELETE 1 1) (:KEEP 2 1) (:INSERT 3 2) (:KEEP 3 3) (:DELETE 4 4)))))
  (TESTING "Shifted pattern"
    (OK
     (equalp (str-algo::diff-operations "ABABAB" "BABABA")
             '((:DELETE 0 0) (:KEEP 1 0) (:KEEP 2 1) (:KEEP 3 2) (:KEEP 4 3)
               (:KEEP 5 4) (:INSERT 6 5)))))
  (TESTING "Change in middle of identical sequences"
    (OK
     (equalp (str-algo::diff-operations "XXXAYYY" "XXXBYYY")
             '((:DELETE 3 3) (:INSERT 4 3) (:KEEP 4 4) (:KEEP 5 5) (:KEEP 6 6)))))
  (TESTING "All insertions"
    (OK
     (equalp (str-algo::diff-operations "" "ABC")'((:INSERT 0 0) (:INSERT 0 1) (:INSERT 0 2)))))
  (TESTING "All deletions"
    (OK
     (equalp (str-algo::diff-operations "ABC" "")'((:DELETE 0 0) (:DELETE 1 0) (:DELETE 2 0)))))
  (TESTING "Sliding window pattern"
    (OK
     (equalp (str-algo::diff-operations "ABCDE" "BCDEX")
             '((:DELETE 0 0) (:KEEP 1 0) (:KEEP 2 1) (:KEEP 3 2) (:KEEP 4 3)
               (:INSERT 5 4)))))
  (TESTING "Change in repetitive sequence"
    (OK
     (equalp (str-algo::diff-operations "AAAA" "AABA")
             '((:INSERT 2 2) (:KEEP 2 3) (:DELETE 3 4)))))
  (TESTING "Multiple separate changes"
    (OK
     (equalp (str-algo::diff-operations "ABCD" "AXCY")
             '((:DELETE 1 1) (:INSERT 2 1) (:KEEP 2 2) (:DELETE 3 3) (:INSERT 4 3)))))
  (TESTING "Single change with long context"
    (OK
     (equalp (str-algo::diff-operations "ABCDEFG" "ABXDEFG")
             '((:DELETE 2 2) (:INSERT 3 2) (:KEEP 3 3) (:KEEP 4 4) (:KEEP 5 5) (:KEEP 6 6)))))
  (TESTING "Delete-then-insert at boundaries"
    (OK
     (equalp (str-algo::diff-operations "ABCDEF" "BCDEFX")
             '((:DELETE 0 0) (:KEEP 1 0) (:KEEP 2 1) (:KEEP 3 2) (:KEEP 4 3)
               (:KEEP 5 4) (:INSERT 6 5))))))

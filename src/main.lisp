(uiop:define-package str-algo
  (:use #:cl)
  (:export #:test-function
           #:test-constant))
(in-package #:str-algo)

;; The goal is to build an array of (k, d) dimension, which has 'x'.
;; Each 'd' round, the array transforms and the final one has the size of string 'x'.
;; At that point, 'd', is the min 'distance'.
;; To get all the required transform operations, need to keep the changes of the arrary.
;;
;; In the transform graph, moving x(right) direction, is 'delete' op,
;; y(down) is 'insert', and x-y(diagonal) is 'keep'. Only compute the new x for
;; 'delete'(RIGHT OP), otherwise use previous value as the new x. So 'insert'
;; preferred.
;;
;; To transform str1 to str2, 'delete' refers the index of str1, and 'insert'
;; refers the index of str2
;;

(defun build-operations (xy-distance-list)
  (labels ((recur (prev-x prev-y xy-list next-xy-distance-list op-list)
             (if (null xy-list)
                 op-list
                 (uiop:if-let (found (find (list prev-x (1- prev-y)) xy-list :test #'equal))
                   (recur (first found) (second found) (first next-xy-distance-list)
                          (rest next-xy-distance-list)
                          (cons (cons :insert found) op-list))
                   (uiop:if-let (found (find (list (1- prev-x) prev-y) xy-list :test #'equal))
                     (recur (first found) (second found) (first next-xy-distance-list)
                            (rest next-xy-distance-list)
                            (cons (list :delete (1- prev-x) prev-y) op-list))
                     (recur (1- prev-x) (1- prev-y) xy-list next-xy-distance-list
                            (cons (list :keep (1- prev-x) (1- prev-y)) op-list)))))))
    (if (null (cdr xy-distance-list))
        :match
        (recur (caaar xy-distance-list) 
               (cadaar xy-distance-list)
               (second xy-distance-list)
               (cddr xy-distance-list) ()))))

(defun myers-distance (str1 str2)
  (let* ((n (length str1))
         (m (length str2))
         (max-d (+ n m))
         (v-size (1+ (* max-d 2)))
         ;; v stores the latest x for each k, k can be -max <= k <= max
         ;; v[1] = 0
         (k-vec (make-array v-size :initial-element -1 :element-type 'integer)))
    (flet ((vget (i)
             (aref k-vec (if (minusp i)
                             (+ v-size i)
                             i)))
           (vset (i val)
             (setf (aref k-vec (if (minusp i)
                                   (+ v-size i)
                                   i))
                   val)))
      (loop initially (vset 1 0) ;; coord (0,0) = 0
            with xy-history
            for d from 0 to max-d
            do (loop with x and y and xy-list
                     for k integer from (- d) to d by 2
                     do (setf x (if (or (zerop (+ d k))
                                        (and (/= d k)
                                             (< (vget (1- k)) (vget (1+ k)))))
                                    (vget (1+ k)) ;; use previous x
                                    ;; new x, insert first
                                    (1+ (vget (1- k))))
                              y (- x k))
                        (loop while (and (< x n)
                                         (< y m)
                                         (char= (aref str1 x) (aref str2 y)))
                              do (incf x)
                                 (incf y))
                        (vset k x)
                        (push (list x y) xy-list)
                     when (and (>= x n) (>= y m))
                       do (push xy-list xy-history)
                          (return-from  myers-distance (build-operations xy-history))
                     finally (push xy-list xy-history))
            finally (return (or xy-history :fail))))))


;; ;; ============= Ukkonen's Algorithm =============

;; (defstruct (dp-cell (:type list))
;;   cost
;;   operation)

;; (defun make-dp-matrix (rows cols)
;;   (let ((matrix (make-array (list rows cols))))
;;     ;; Initialize first row and column
;;     (loop for i from 0 below rows do
;;       (setf (aref matrix i 0)
;;             (make-dp-cell :cost i :operation :delete)))
;;     (loop for j from 0 below cols do
;;       (setf (aref matrix 0 j)
;;             (make-dp-cell :cost j :operation :insert)))
;;     matrix))

;; (defun ukkonen-distance (str1 str2)
;;   "Non-optimized implementation of Ukkonen's algorithm"
;;   (let* ((m (1+ (length str1)))
;;          (n (1+ (length str2)))
;;          (dp (make-dp-matrix m n))
;;          (operations nil))

;;     (loop for i from 1 below m do
;;       (loop for j from 1 below n do
;;         (let* ((char1 (char str1 (1- i)))
;;                (char2 (char str2 (1- j)))
;;                (match-cost (if (char= char1 char2) 0 1))
;;                (costs (list (+ (dp-cell-cost (aref dp (1- i) j)) 1)      ; deletion
;;                             (+ (dp-cell-cost (aref dp i (1- j))) 1)      ; insertion
;;                             (+ (dp-cell-cost (aref dp (1- i) (1- j)))    ; replace/match
;;                                match-cost)))
;;                (min-cost (apply #'min costs))
;;                (operation (case (position min-cost costs)
;;                             (0 :delete)
;;                             (1 :insert)
;;                             (2 (if (zerop match-cost) :keep :replace)))))
;;           (setf (aref dp i j)
;;                 (make-dp-cell :cost min-cost :operation operation)))))

;;     ;; Backtrack to get operations
;;     (let ((i (1- m))
;;           (j (1- n)))
;;       (loop while (and (>= i 0) (>= j 0)) do
;;         (let ((cell (aref dp i j)))
;;           (push (list (dp-cell-operation cell) (1- i) (1- j))
;;                 operations)
;;           (case (dp-cell-operation cell)
;;             (:keep (decf i) (decf j))
;;             (:replace (decf i) (decf j))
;;             (:delete (decf i))
;;             (:insert (decf j))))))

;;     ;; Return edit distance and operations
;;     (values (dp-cell-cost (aref dp (1- m) (1- n)))
;;             (reverse operations))))


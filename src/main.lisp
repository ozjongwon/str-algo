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

(defun myers-distance (str1 str2)
  (let* ((n (length str1))
         (m (length str2))
         (max-d (+ n m))
         (v-size (1+ (* max-d 2)))
         ;; v stores the latest x for each k, k can be -max <= k <= max
         ;; v[1] = 0
         (k-vec (make-array v-size :initial-element -1 :element-type 'integer)))
    (flet ((vget (i)
             (if (minusp i)
                 (aref k-vec (+ v-size i))
                 (aref k-vec i)))
           (vset (i val)
             (if (minusp i)
                 (setf (aref k-vec (+ v-size i)) val)
                 (setf (aref k-vec i) val))))
      (loop initially (vset 1 0) ;; coord (0,0) = 0
            with dkxy-history
            for d from 0 to max-d
            do
               (loop with x and y and dkxy-list
                     for k integer from (- d) to d by 2
                     do (setf x (if (or (zerop (+ d k))
                                        (and (/= k d)
                                             (< (vget (1- k)) (vget (1+ k)))))
                                    (vget (1+ k)) ;; use previous x
                                    ;; new x, insert first
                                    (1+ (vget (1- k))))
                              y (- x k))
                        (loop while (and (< x n)
                                         (< y m)
                                         (char= (aref str1 x)
                                                (aref str2 y)))
                              do (incf x)
                                 (incf y))
                        (vset k x)
                        (format t "~%*** (d k x y) = (~A ~A ~A ~A) k-vec: ~A"
                                d k x y k-vec)
                        (progn
                          (setf dkxy-list (delete-if #'(lambda (dkxy)
                                                         (and (= d (first dkxy))
                                                              (> x (third dkxy))))
                                                     dkxy-list))
                          (push (list d k x y) dkxy-list))
                     when (and (>= x n) (>= y m))
                       do (push dkxy-list dkxy-history)
                          (return-from  myers-distance dkxy-history)
                     finally (push dkxy-list dkxy-history))
            finally (return (or dkxy-history :fail))))))

;; V2
(defun diff-operations (str1 str2)
  (let ((history (myers-distance str1 str2)))
    (destructuring-bind (((prev-d prev-k prev-x prev-y)) &rest more-history)
        history
      (format t "~&111 ~A ~A ~A ~A" prev-d prev-k prev-x prev-y)
      (cons (caar history)
            (loop for dkxy-list in more-history
                  as next-step = (if (null (cdr dkxy-list))
                                     (first dkxy-list)
                                     (find (1- prev-y) dkxy-list :test #'= :key #'fourth))
                  do (format t "~&2222 ~A ~A ~A ~A ~A ~A" dkxy-list next-step prev-d prev-k prev-x prev-y)
                     (setf prev-d (first next-step)
                           prev-k (second next-step)
                           prev-x (third next-step)
                           prev-y (fourth next-step))
                  collect next-step)))))

;; V3
(defun diff-operations (str1 str2)
  (let ((history (myers-distance str1 str2)))
    (destructuring-bind (((prev-d prev-k prev-x prev-y)) &rest more-history)
        history
      (format t "~&111 ~A ~A ~A ~A" prev-d prev-k prev-x prev-y)
      (cons (caar history)
            (loop for dkxy-list in more-history
                  as next-step = (if (null (cdr dkxy-list))
                                     (first dkxy-list)
                                     (find (1- prev-y) dkxy-list :test #'= :key #'fourth))
                  do (format t "~&2222 ~A ~A ~A ~A ~A ~A" dkxy-list next-step prev-d prev-k prev-x prev-y)
                     (setf prev-d (first next-step)
                           prev-k (second next-step)
                           prev-x (third next-step)
                           prev-y (fourth next-step))
                  collect next-step)))))

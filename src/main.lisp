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
            with xy-history
            for d from 0 to max-d
            do
               (loop with x and y and xy-list
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
                        ;; (format t "~%*** (d k x y) = (~A ~A ~A ~A) k-vec: ~A"
                        ;;         d k x y k-vec)
                        (progn
                          (setf xy-list (delete-if #'(lambda (xy)
                                                       (> x (first xy)))
                                                   xy-list))
                          (push (list x y) xy-list))
                     when (and (>= x n) (>= y m))
                       do (push xy-list xy-history)
                          (return-from  myers-distance xy-history)
                     finally (push xy-list xy-history))
            finally (return (or xy-history :fail))))))

;; V2
(defun diff-operations (str1 str2)
  (let ((history (myers-distance str1 str2)))
    (destructuring-bind (((prev-x prev-y)) &rest more-history)
        history
      (format t "~&111 ~A ~A" prev-x prev-y)
      (cons (caar history)
            (loop for xy-list in more-history
                  as next-step = (if (null (cdr xy-list))
                                     (first xy-list)
                                     (find (1- prev-y) xy-list :test #'= :key #'second))
                  do (format t "~&2222 ~A ~A ~A ~A" xy-list next-step prev-x prev-y)
                     (setf prev-x (first next-step)
                           prev-y (second next-step))
                  collect next-step)))))

;; V3
(defun diff-operations (str1 str2)
  (let ((history (myers-distance str1 str2)))
    (destructuring-bind (((prev-x prev-y)) &rest more-history)
        history
      (loop for (xy-list &rest more-xy-list) = more-history then more-xy-list
            do (format t "~%*** ~A ~A" x y)
            if (and (= prev-x x) (= prev-y (1+ y)))
              collect :insert
            else
              if (and (= prev-x (1+ x)) (= prev-y y))
                collect :delete
            else
              append (loop for keep-x = prev-x then (1- keep-x)
                           and keep-y = prev-y then (1- keep-y)
                           collect :keep 
                           until (or (= keep-x x) (= keep-y y)))))))

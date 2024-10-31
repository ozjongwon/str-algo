(uiop:define-package str-algo
  (:use #:cl)
  (:export #:test-function
           #:test-constant))
(in-package #:str-algo)

(defun myers-distance (str1 str2)
  (let* ((n (length str1))
         (m (length str2))
         (max-d (+ n m))
         (v-size (1+ (* max-d 2)))
         ;; v stores the latest x for each k, k can be -max <= k <= max
         ;; v[1] = 0
         (v (make-array v-size :initial-element -1 :element-type 'integer)))
    (flet ((vget (i)
             (if (minusp i)
                 (aref v (+ v-size i))
                 (aref v i)))
           (vset (i val)
             (if (minusp i)
                 (setf (aref v (+ v-size i)) val)
                 (setf (aref v i) val))))
      (vset 1 0) ;; start point
      (loop for d from 0 to max-d do
        (loop with x and y
              for k integer from (- d) to d by 2
              do (setf x (if (or (= k (- d))
                                 (and (/= k d)
                                      (< (vget (1- k)) (vget (1+ k)))))
                             (vget (1+ k))
                             (1+ (vget (1- k))))
                       y (- x k))
                 (loop while (and (< x n)
                                  (< y m)
                                  (char= (aref str1 x)
                                         (aref str2 y)))
                       do (incf x)
                          (incf y))
                 (vset k x)
              when (and (>= x n)
                        (>= y m))
                do (return-from myers-distance d))
            finally (return :fail)))))
;; V2

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
            for d from 0 below max-d
            collect
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
            finally (return :fail)))))
;; V1
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

(defun backtrack (str1 str2)
  (let* ((backtrack-list (myers-distance str1 str2))
         (arr-len (length (first backtrack-list)))
         (x (length str1))
         (y (length str2)))
    backtrack-list
    #+XY
    (dolist (arr backtrack-list)
      (loop with prev-k and prev-x and prev-y and ops
            for d from 0 below arr-len
            and k = (- x y)
            do (if (or (= k (- d))
                       (and (/= k d)
                            (< (aref arr (1- k)) (aref arr (1+ k)))))
                   (setf prev-k (1+ k))
                   (setf prev-k (1- k)))
               (setf prev-x (aref arr prev-k)
                     prev-y (- prev-x prev-k))
               (loop while (and (> x prev-x) (> y prev-y))
                     do (incf x)
                        (incf y)
                        (push (list :keep (aref str1 x) (aref str2 y)) ops))
               (when (> d 0)
                 (cond ((= x prev-x)
                        (push (list :insert (aref str2 prev-y)) ops))
                       ((= y prev-y)
                        (push (list :delete (aref str2 prev-x)) ops)))
                 (setf x prev-x
                       y prev-y))))))


;; ;; Type declarations for optimization
;; (deftype positive-fixnum () '(and fixnum unsigned-byte))
;; (deftype simple-string () 'simple-base-string)
(deftype operation () '(member :keep :insert :delete :replace))
(deftype operation-list () 'list)

;; ============= Myers' Algorithm =============

(declaim (inline snake make-v get-v))
(defun make-v (size)
  (declare (type fixnum size))
  (make-array size :initial-element 0
                   :element-type 'fixnum))

(defun get-v (v k offset)
  (declare (type (simple-array fixnum (*)) v)
           (type fixnum k offset))
  (aref v (+ k offset)))

(defun snake (str1 str2 x y end1 end2)
  (declare (type simple-string str1 str2)
           (type fixnum x y end1 end2)
           (optimize (speed 3) (safety 0)))
  (loop while (and (< x end1)
                   (< y end2)
                   (char= (char str1 x) (char str2 y)))
        do (incf x)
           (incf y)
        finally (return (values x y))))

(defun myers-distance (str1 str2)
  "Efficient implementation of Myers' diff algorithm"
  (declare (type simple-string str1 str2)
           (optimize (speed 3) (safety 1)))
  (let* ((n (length str1))
         (m (length str2))
         (max-d (+ n m))
         (v-size (1+ (* 2 max-d)))
         (v (make-v v-size))
         (offset (1+ n))
         (operations nil))
    (declare (type fixnum n m max-d v-size offset))

    ;; Main loop
    (loop for d from 0 to max-d do
      (loop for k from (- d) to d by 2 do
        (let* ((k-index (+ k offset))
               (x (if (or (= k (- d))
                          (and (not (= k d))
                               (< (get-v v (1- k-index) offset)
                                  (get-v v (1+ k-index) offset))))
                      (get-v v (1+ k-index) offset)
                      (1+ (get-v v (1- k-index) offset))))
               (y (- x k)))
          (declare (type fixnum k-index x y))
          (format t "~%**** ~A ~A ~A" x k y)
          (multiple-value-bind (new-x new-y)
              (snake str1 str2 x y n m)
            (setf (aref v k-index) new-x)

            ;; Record operation
            (when (and (>= new-x 0) (>= new-y 0))
              (push (list :snake new-x new-y) operations))

            ;; Check if we reached the end
            (when (and (>= new-x n) (>= new-y m))
              (return-from myers-distance
                (values d (process-operations operations str1 str2))))))))

    ;; Should not reach here
    (error "No solution found")))

;; ============= Ukkonen's Algorithm =============

#+X(defstruct (dp-cell (:type vector))
     (cost 0 :type fixnum)
     (operation nil :type operation))

(defstruct dp-cell
  (cost 0 :type fixnum)
  (operation :keep :type operation))

;; (declaim (ftype (function (positive-fixnum positive-fixnum)
;;                           (simple-array dp-cell (* *)))
;;                 make-dp-matrix))
(defun make-dp-matrix (rows cols)
  (let ((matrix (make-array (list rows cols)
                            :initial-element (make-dp-cell))))
    ;; Initialize first row and column
    (loop for i from 0 below rows do
      (setf (aref matrix i 0)
            (make-dp-cell :cost i :operation :delete)))
    (loop for j from 0 below cols do
      (setf (aref matrix 0 j)
            (make-dp-cell :cost j :operation :insert)))
    matrix))

(defun ukkonen-distance (str1 str2)
  "Efficient implementation of Ukkonen's algorithm"
  (declare (type simple-string str1 str2)
           (optimize (speed 3) (safety 1)))
  (let* ((m (1+ (length str1)))
         (n (1+ (length str2)))
         (dp (make-dp-matrix m n))
         (operations nil))
    (declare (type fixnum m n))

    ;; Main DP loop
    (loop for i from 1 below m do
      (loop for j from 1 below n do
        (let* ((char1 (char str1 (1- i)))
               (char2 (char str2 (1- j)))
               (match-cost (if (char= char1 char2) 0 1))
               (costs (vector
                       (+ (dp-cell-cost (aref dp (1- i) j)) 1)      ; deletion
                       (+ (dp-cell-cost (aref dp i (1- j))) 1)      ; insertion
                       (+ (dp-cell-cost (aref dp (1- i) (1- j)))    ; replace/match
                          match-cost)))
               (min-cost (reduce #'min costs))
               (operation (case (position min-cost costs)
                            (0 :delete)
                            (1 :insert)
                            (2 (if (zerop match-cost) :keep :replace)))))
          (declare (type fixnum match-cost min-cost))

          (setf (aref dp i j)
                (make-dp-cell :cost min-cost :operation operation)))))

    ;; Backtrack to get operations
    (let ((i (1- m))
          (j (1- n)))
      (loop while (and (>= i 0) (>= j 0)) do
        (let ((cell (aref dp i j)))
          (push (list (dp-cell-operation cell) (1- i) (1- j))
                operations)
          (case (dp-cell-operation cell)
            (:keep (decf i) (decf j))
            (:replace (decf i) (decf j))
            (:delete (decf i))
            (:insert (decf j))))))

    ;; Return edit distance and operations
    (values (dp-cell-cost (aref dp (1- m) (1- n)))
            operations)))

;; ============= Utility Functions =============

(defun process-operations (snake-ops str1 str2)
  "Convert Myers' snake operations to standard edit operations"
  (declare (type list snake-ops)
           (type simple-string str1 str2))
  (let ((operations nil)
        (last-x 0)
        (last-y 0))
    (dolist (op (reverse snake-ops))
      (destructuring-bind (type x y) op
        (declare (ignore type))
        ;; Fill in operations between snakes
        (loop while (or (< last-x x) (< last-y y)) do
          (cond
            ((= last-x x)
             (push (list :insert last-x last-y) operations)
             (incf last-y))
            ((= last-y y)
             (push (list :delete last-x last-y) operations)
             (incf last-x))
            (t
             (if (char= (char str1 last-x) (char str2 last-y))
                 (push (list :keep last-x last-y) operations)
                 (push (list :replace last-x last-y) operations))
             (incf last-x)
             (incf last-y))))))
    operations))

(defun apply-edit-script (source target operations)
  "Apply edit operations to transform source into target"
  (declare (type simple-string source target)
           (type operation-list operations))
  (let ((result (make-array (length source)
                            :element-type 'character
                            :fill-pointer 0
                            :adjustable t))
        (steps nil))

    (loop for (op pos1 pos2) in operations do
      (let ((step-desc
              (case op
                (:keep
                 (vector-push-extend (char source pos1) result)
                 (format nil "Keep '~A' at position ~D"
                         (char source pos1) pos1))
                (:replace
                 (vector-push-extend (char target pos2) result)
                 (format nil "Replace '~A' with '~A' at position ~D"
                         (char source pos1) (char target pos2) pos1))
                (:insert
                 (vector-push-extend (char target pos2) result)
                 (format nil "Insert '~A' at position ~D"
                         (char target pos2) pos1))
                (:delete
                 (format nil "Delete '~A' at position ~D"
                         (char source pos1) pos1)))))
        (push (list (coerce result 'string) step-desc) steps)))

    (nreverse steps)))

;; ============= Example Usage =============

#|
(let* ((str1 "kitten")
(str2 "sitting")
(myers-result (multiple-value-list (myers-distance str1 str2)))
(ukkonen-result (multiple-value-list (ukkonen-distance str1 str2))))
(format t "Myers: Distance=~A~%Operations:~%~{  ~A~%~}~%"
(first myers-result)
(second myers-result))
(format t "Ukkonen: Distance=~A~%Operations:~%~{  ~A~%~}"
(first ukkonen-result)
(second ukkonen-result)))
|#


;;; 000000000000000000000000000000
(defpackage :edit-distance
  (:use :cl)
  (:export #:myers-distance
           #:ukkonen-distance
           #:apply-edit-script))

(in-package :edit-distance)

;; ============= Myers' Algorithm =============

(defun snake (str1 str2 x y end1 end2)
  (loop while (and (< x end1)
                   (< y end2)
                   (char= (char str1 x) (char str2 y)))
        do (incf x)
           (incf y)
        finally (return (values x y))))

(defun myers-distance (str1 str2)
  "Non-optimized implementation of Myers' diff algorithm"
  (let* ((n (length str1))
         (m (length str2))
         (max-d (+ n m))
         (v-forward (make-array (+ (* 2 max-d) 1) :initial-element most-negative-fixnum))
         (v-reverse (make-array (+ (* 2 max-d) 1) :initial-element most-negative-fixnum))
         (offset (1+ n))
         (operations nil))
    (setf (aref v-forward offset) 0)
    (setf (aref v-reverse offset) n)

    (loop for d from 0 to max-d do
      (loop for k from (- d) to d by 2 do
        (let* ((k-index (+ k offset))
               (x (if (or (= k (- d))
                          (and (not (= k d))
                               (< (aref v-forward (1- k-index))
                                  (aref v-forward (1+ k-index)))))
                      (aref v-forward (1+ k-index))
                      (1+ (aref v-forward (1- k-index)))))
               (y (- x k)))

          (multiple-value-bind (new-x new-y)
              (snake str1 str2 x y n m)
            (setf (aref v-forward k-index) new-x)
            (setf (aref v-reverse (+ (- delta k) offset)) new-y)

            (when (and (>= new-x n) (>= new-y m))
              (return-from myers-distance
                (values d (process-operations (reverse operations) str1 str2))))

            (push (list :snake new-x new-y) operations))))))
  ;; Should not reach here
  (error "No solution found"))

;; ============= Ukkonen's Algorithm =============

(defstruct (dp-cell (:type list))
  cost
  operation)

(defun make-dp-matrix (rows cols)
  (let ((matrix (make-array (list rows cols))))
    ;; Initialize first row and column
    (loop for i from 0 below rows do
      (setf (aref matrix i 0)
            (make-dp-cell :cost i :operation :delete)))
    (loop for j from 0 below cols do
      (setf (aref matrix 0 j)
            (make-dp-cell :cost j :operation :insert)))
    matrix))

(defun ukkonen-distance (str1 str2)
  "Non-optimized implementation of Ukkonen's algorithm"
  (let* ((m (1+ (length str1)))
         (n (1+ (length str2)))
         (dp (make-dp-matrix m n))
         (operations nil))

    (loop for i from 1 below m do
      (loop for j from 1 below n do
        (let* ((char1 (char str1 (1- i)))
               (char2 (char str2 (1- j)))
               (match-cost (if (char= char1 char2) 0 1))
               (costs (list (+ (dp-cell-cost (aref dp (1- i) j)) 1)      ; deletion
                            (+ (dp-cell-cost (aref dp i (1- j))) 1)      ; insertion
                            (+ (dp-cell-cost (aref dp (1- i) (1- j)))    ; replace/match
                               match-cost)))
               (min-cost (apply #'min costs))
               (operation (case (position min-cost costs)
                            (0 :delete)
                            (1 :insert)
                            (2 (if (zerop match-cost) :keep :replace)))))
          (setf (aref dp i j)
                (make-dp-cell :cost min-cost :operation operation)))))

    ;; Backtrack to get operations
    (let ((i (1- m))
          (j (1- n)))
      (loop while (and (>= i 0) (>= j 0)) do
        (let ((cell (aref dp i j)))
          (push (list (dp-cell-operation cell) (1- i) (1- j))
                operations)
          (case (dp-cell-operation cell)
            (:keep (decf i) (decf j))
            (:replace (decf i) (decf j))
            (:delete (decf i))
            (:insert (decf j))))))

    ;; Return edit distance and operations
    (values (dp-cell-cost (aref dp (1- m) (1- n)))
            (reverse operations))))

;; ============= Utility Functions =============

(defun process-operations (snake-ops str1 str2)
  (let ((operations nil)
        (last-x 0)
        (last-y 0))
    (dolist (op snake-ops)
      (destructuring-bind (type x y) op
        (loop while (or (< last-x x) (< last-y y)) do
          (cond
            ((= last-x x)
             (push (list :insert last-x last-y) operations)
             (incf last-y))
            ((= last-y y)
             (push (list :delete last-x last-y) operations)
             (incf last-x))
            (t
             (if (char= (char str1 last-x) (char str2 last-y))
                 (push (list :keep last-x last-y) operations)
                 (push (list :replace last-x last-y) operations))
             (incf last-x)
             (incf last-y))))))
    operations))

(defun apply-edit-script (source target operations)
  (let ((result (make-array (length source)
                            :element-type 'character
                            :fill-pointer 0
                            :adjustable t))
        (steps nil))

    (loop for (op pos1 pos2) in operations do
      (let ((step-desc
              (case op
                (:keep
                 (vector-push-extend (char source pos1) result)
                 (format nil "Keep '~A' at position ~D"
                         (char source pos1) pos1))
                (:replace
                 (vector-push-extend (char target pos2) result)
                 (format nil "Replace '~A' with '~A' at position ~D"
                         (char source pos1) (char target pos2) pos1))
                (:insert
                 (vector-push-extend (char target pos2) result)
                 (format nil "Insert '~A' at position ~D"
                         (char target pos2) pos1))
                (:delete
                 (format nil "Delete '~A' at position ~D"
                         (char source pos1) pos1)))))
        (push (list (coerce result 'string) step-desc) steps)))

    (nreverse steps)))

;; ============= Example Usage =============

#|
(let* ((str1 "kitten")
(str2 "sitting")
(myers-result (multiple-value-list (myers-distance str1 str2)))
(ukkonen-result (multiple-value-list (ukkonen-distance str1 str2))))
(format t "Myers: Distance=~A~%Operations:~%~{  ~A~%~}~%"
(first myers-result)
(second myers-result))
(format t "Ukkonen: Distance=~A~%Operations:~%~{  ~A~%~}"
(first ukkonen-result)
(second ukkonen-result)))
|#

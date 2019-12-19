;;;; Advent of code day 19

(in-package #:aoc2019)

(defparameter *input-day-19* #(109 424 203 1 21102 1 11 0 1106 0 282
21101 18 0 0 1106 0 259 2102 1 1 221 203 1 21101 0 31 0 1106 0 282
21102 1 38 0 1106 0 259 21002 23 1 2 21202 1 1 3 21102 1 1 1 21101 57
0 0 1106 0 303 2102 1 1 222 20101 0 221 3 21002 221 1 2 21102 259 1 1
21102 1 80 0 1106 0 225 21102 96 1 2 21101 91 0 0 1105 1 303 2101 0 1
223 21001 222 0 4 21101 259 0 3 21101 225 0 2 21102 1 225 1 21101 118
0 0 1106 0 225 21002 222 1 3 21102 1 43 2 21101 0 133 0 1105 1 303
21202 1 -1 1 22001 223 1 1 21101 148 0 0 1106 0 259 1201 1 0 223 20101
0 221 4 20101 0 222 3 21101 16 0 2 1001 132 -2 224 1002 224 2 224 1001
224 3 224 1002 132 -1 132 1 224 132 224 21001 224 1 1 21101 195 0 0
106 0 109 20207 1 223 2 20101 0 23 1 21102 -1 1 3 21101 0 214 0 1105 1
303 22101 1 1 1 204 1 99 0 0 0 0 109 5 1202 -4 1 249 22102 1 -3 1
22101 0 -2 2 21202 -1 1 3 21102 250 1 0 1106 0 225 21202 1 1 -4 109 -5
2106 0 0 109 3 22107 0 -2 -1 21202 -1 2 -1 21201 -1 -1 -1 22202 -1 -2
-2 109 -3 2105 1 0 109 3 21207 -2 0 -1 1206 -1 294 104 0 99 22102 1 -2
-2 109 -3 2105 1 0 109 5 22207 -3 -4 -1 1206 -1 346 22201 -4 -3 -4
21202 -3 -1 -1 22201 -4 -1 2 21202 2 -1 -1 22201 -4 -1 1 21202 -2 1 3
21101 0 343 0 1105 1 303 1106 0 415 22207 -2 -3 -1 1206 -1 387 22201
-3 -2 -3 21202 -2 -1 -1 22201 -3 -1 3 21202 3 -1 -1 22201 -3 -1 2
21202 -4 1 1 21102 384 1 0 1106 0 303 1105 1 415 21202 -4 -1 -4 22201
-4 -3 -4 22202 -3 -2 -2 22202 -2 -4 -4 22202 -3 -2 -3 21202 -4 -1 -2
22201 -3 -2 1 22102 1 1 -4 109 -5 2105 1 0))

(defun solution-day19-1 ()
  (let ((out ())
	(answer 0))
    (dotimes (y 50)
      (dotimes (x 50)
	(push x out)
	(push y out)
	(intcode11 :memory *input-day-19*
		   :input (list x y)
		   :output-fn (lambda (val) (when (= val 1) (incf answer)))
	)))
    answer))

(defun find-start (start y)
  (let ((found 0))
    (do ((x start (1+ x)))
	((or (= 1 (first (intcode11 :memory *input-day-19*
				    :input (list x y))))
	     (> x (+ start 2)))
	 (setf found (if (> x (+ start 2))
			 0
			 x)))
      )))

(defun find-end (start y)
  (let ((found 0))
    (do ((x start (1+ x)))
	((or (= 0 (first (intcode11 :memory *input-day-19*
				    :input (list x y))))
	     (> x (+ start 4)))
	 (setf found (if (> x (+ start 4))
			 0
			 x)))
      )))

(defun solution-day19-2 (n)
  (declare (optimize debug))
  (let ((hash (make-hash-table))
	(out (make-array (list 10) :adjustable t :fill-pointer 0)))
    (do* ((y 0 (1+ y))
	  (start (find-start 0 y) (find-start start y))
	  (end (find-end 0 y) (find-end (max end start) y)))
	 (())
      (vector-push-extend (list start end (- end start)) out)
      (dotimes (x (- end start))
	(setf (gethash (pos (+ start x) y) hash) 1))

      (when (and (> y n)
		 (>= (- end start) n))
	(destructuring-bind (t-s t-e t-l)
	    (aref out (- y (1- n)))
	  (declare (ignorable t-l t-s))
	  (when (>= t-e (+ start n))
	    (return-from solution-day19-2 (+ (* start 10000) (- y (1- n))))))))))

(defun solve-day19 ()
  (format t "Answer for puzzle 1 of day 19: ~A~&"
	  (solution-day19-1))
  (format t "Answer for puzzle 2 of day 19: ~A~&"
	  (solution-day19-2 100)))

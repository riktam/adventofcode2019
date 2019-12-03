;;;; Advent of code day 2

(in-package #:aoc2019)

(defparameter *input-day-02-o* (vector 1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 1 6
19 1 9 19 23 2 23 10 27 1 27 5 31 1 31 6 35 1 6 35 39 2 39 13 43 1 9
43 47 2 9 47 51 1 51 6 55 2 55 10 59 1 59 5 63 2 10 63 67 2 9 67 71 1
71 5 75 2 10 75 79 1 79 6 83 2 10 83 87 1 5 87 91 2 9 91 95 1 95 5 99
1 99 2 103 1 103 13 0 99 2 14 0 0 ))

(defparameter *input-day-02* (vector 1 12 2 3 1 1 2 3 1 3 4 3 1 5 0 3 2 1 6
19 1 9 19 23 2 23 10 27 1 27 5 31 1 31 6 35 1 6 35 39 2 39 13 43 1 9
43 47 2 9 47 51 1 51 6 55 2 55 10 59 1 59 5 63 2 10 63 67 2 9 67 71 1
71 5 75 2 10 75 79 1 79 6 83 2 10 83 87 1 5 87 91 2 9 91 95 1 95 5 99
1 99 2 103 1 103 13 0 99 2 14 0 0 ))


(defun copy-vec (vec)
  (make-array  (list (length vec)) :initial-contents vec))

(defun opcode (memory fn op1 op2 dst)
  (let* ((val-op1 (aref memory (aref memory op1)))
	 (val-op2 (aref memory (aref memory op2)))
	 (pos-dst (aref memory dst))
	 (out (funcall fn val-op1 val-op2)))
    (setf (aref memory pos-dst)
	  out)
    memory))

(defun intcode (&key (position 0) (memory *input-day-02*))
  (let* ((new-memory (copy-vec memory))
	 (opcode (aref new-memory position))
	 (new-position (+ position 4)))
    (case opcode
      (1 (opcode new-memory #'+ (+ position 1) (+ position 2) (+ position 3))
	 (intcode :position new-position :memory new-memory))
      (2 (opcode  new-memory #'* (+ position 1) (+ position 2) (+ position 3))
	 (intcode :position new-position :memory new-memory))
      (99 (aref new-memory 0)))))

(defun solution-day02-1 ()
  (intcode :memory *input-day-02*))

(defun solution-day02-2 ()
  (dotimes (noun 100)
    (dotimes (verb 100)
      (let* ((memory (copy-vec *input-day-02-o*)))
	(setf (aref memory 1) noun)
	(setf (aref memory 2) verb)
	(let ((result (intcode :memory memory)))
	  (when (= result 19690720)
	    (return-from solution-day02-2
	      (+ (* 100 noun) verb))))))))


(defun solve-day02 ()
  (format t "Answer for puzzle 1 of day 2: ~A~&"
	  (solution-day02-1))
  (format t "Answer for puzzle 2 of day 2: ~A~&"
	  (solution-day02-2)))

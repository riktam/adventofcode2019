;;;; Advent of code day 11

(in-package #:aoc2019)

(defparameter *input-day-11* #(3 8 1005 8 324 1106 0 11 0 0 0 104 1
104 0 3 8 1002 8 -1 10 1001 10 1 10 4 10 1008 8 1 10 4 10 1001 8 0 29
1 1107 14 10 1006 0 63 1006 0 71 3 8 1002 8 -1 10 101 1 10 10 4 10
1008 8 1 10 4 10 1002 8 1 61 1 103 18 10 1006 0 14 1 105 7 10 3 8 1002
8 -1 10 101 1 10 10 4 10 1008 8 1 10 4 10 101 0 8 94 1006 0 37 1006 0
55 2 1101 15 10 3 8 1002 8 -1 10 1001 10 1 10 4 10 1008 8 0 10 4 10
101 0 8 126 2 1006 12 10 3 8 102 -1 8 10 101 1 10 10 4 10 1008 8 1 10
4 10 1001 8 0 152 3 8 102 -1 8 10 1001 10 1 10 4 10 108 0 8 10 4 10
101 0 8 173 1006 0 51 1006 0 26 3 8 102 -1 8 10 101 1 10 10 4 10 1008
8 0 10 4 10 1001 8 0 202 2 8 18 10 1 103 19 10 1 1102 1 10 1006 0 85 3
8 102 -1 8 10 1001 10 1 10 4 10 108 0 8 10 4 10 1001 8 0 238 2 1002 8
10 1006 0 41 3 8 102 -1 8 10 1001 10 1 10 4 10 108 0 8 10 4 10 101 0 8
267 2 1108 17 10 2 105 11 10 1006 0 59 1006 0 90 3 8 1002 8 -1 10 1001
10 1 10 4 10 1008 8 1 10 4 10 1001 8 0 304 101 1 9 9 1007 9 993 10
1005 10 15 99 109 646 104 0 104 1 21102 936735777688 1 1 21101 341 0 0
1105 1 445 21101 0 937264173716 1 21101 352 0 0 1106 0 445 3 10 104 0
104 1 3 10 104 0 104 0 3 10 104 0 104 1 3 10 104 0 104 1 3 10 104 0
104 0 3 10 104 0 104 1 21101 3245513819 0 1 21102 1 399 0 1105 1 445
21102 1 29086470235 1 21102 410 1 0 1105 1 445 3 10 104 0 104 0 3 10
104 0 104 0 21101 825544712960 0 1 21102 1 433 0 1106 0 445 21102
825460826472 1 1 21101 0 444 0 1106 0 445 99 109 2 22102 1 -1 1 21101
0 40 2 21101 0 476 3 21102 466 1 0 1105 1 509 109 -2 2105 1 0 0 1 0 0
1 109 2 3 10 204 -1 1001 471 472 487 4 0 1001 471 1 471 108 4 471 10
1006 10 503 1101 0 0 471 109 -2 2106 0 0 0 109 4 2101 0 -1 508 1207 -3
0 10 1006 10 526 21101 0 0 -3 21202 -3 1 1 21201 -2 0 2 21101 0 1 3
21101 0 545 0 1105 1 550 109 -4 2105 1 0 109 5 1207 -3 1 10 1006 10
573 2207 -4 -2 10 1006 10 573 21202 -4 1 -4 1106 0 641 21202 -4 1 1
21201 -3 -1 2 21202 -2 2 3 21101 0 592 0 1105 1 550 22101 0 1 -4 21101
1 0 -1 2207 -4 -2 10 1006 10 611 21102 1 0 -1 22202 -2 -1 -2 2107 0 -3
10 1006 10 633 22101 0 -1 1 21102 633 1 0 105 1 508 21202 -2 -1 -2
22201 -4 -2 -4 109 -5 2105 1 0))

(defun intcode11 (&key (memory *input-day-11*) input input-fn output-fn)
  (let ((ip 0)
	(rp 0)
	(curr-input input)
	(output)
	(mem (vector-to-hash memory)))
    (labels ((ip (n) (+ ip n))
	     (mem (ip &optional (mod 1))
	       (ecase mod
		 (0 (gethash (gethash ip mem) mem 0))
		 (1 (gethash ip mem 0))
		 (2 (gethash (+ rp (gethash ip mem)) mem 0))))
	     ((setf mem) (val ip &optional (mod 1))
	       (ecase mod
		 (0 (setf (gethash (gethash ip mem ) mem) val))
		 (1 (setf (gethash ip mem) val))
		 (2 (setf (gethash (+ rp (gethash ip mem)) mem) val))))
	     (decode-opcode (int)
	       "ABCDE -> (DE C B A)"
	       (let ((str (format nil "~5,'0d" int)))
		 (list (parse-integer str :start 3)
		       (parse-integer str :start 2 :end 3)
		       (parse-integer str :start 1 :end 2)
		       (parse-integer str :start 0 :end 1))))
	     (gen-opcode (fn mod1 mod2 mod3)
	       (setf (mem (ip 3) mod3)
		     (funcall fn (mem (ip 1) mod1)
			      (mem (ip 2) mod2)))
	       (ip 4))
	     (opcode1 (mod1 mod2 mod3)
	       (gen-opcode #'+ mod1 mod2 mod3))
	     (opcode2 (mod1 mod2 mod3)
	       (gen-opcode #'* mod1 mod2 mod3))
	     (opcode3 (mod1)
	       (setf (mem (ip 1) mod1)
		     (if input-fn (funcall input-fn)
			 (pop curr-input)))
	       (ip 2))
	     (opcode4 (mod1)
	       (ecase mod1
		 ((0 1) (push (mem (ip 1) mod1) output))
		 (2 (push (mem (ip 1) mod1) output)))
	       (when output-fn
		 (funcall output-fn (car output)))
	       (ip 2))
	     (opcode5 (mod1 mod2)
	       (if (zerop (mem (ip 1) mod1))
		   (ip 3)
		   (mem (ip 2) mod2)))
	     (opcode6 (mod1 mod2)
	       (if (zerop (mem (ip 1) mod1)) (mem (ip 2) mod2) (ip 3)))
	     (opcode7 (mod1 mod2 mod3)
	       (gen-opcode #'op< mod1 mod2 mod3))
	     (opcode8 (mod1 mod2 mod3)
	       (gen-opcode #'op= mod1 mod2 mod3))
	     (opcode9 (mod1)
	       (incf rp (mem (ip 1) mod1))
	       (ip 2)))
      (do ()
	  (())
	(destructuring-bind (opcode mod1 mod2 mod3)
	    (decode-opcode (mem ip))
	  (setf ip
		(ecase opcode
		  (1 (opcode1 mod1 mod2 mod3))
		  (2 (opcode2 mod1 mod2 mod3))
		  (3 (opcode3 mod1))
		  (4 (opcode4 mod1))
		  (5 (opcode5 mod1 mod2))
		  (6 (opcode6 mod1 mod2))
		  (7 (opcode7 mod1 mod2 mod3))
		  (8 (opcode8 mod1 mod2 mod3))
		  (9 (opcode9 mod1))
		  (99 (return-from intcode11 (nreverse output))))
		))))))

(defun turn (dir turn)
  "0 means it should turn left 90 degrees,
and 1 means it should turn right 90 degrees."
  (if (zerop turn)
      (case dir
	(#\^ #\<)
	(#\v #\>)
	(#\< #\v)
	(#\> #\^))
      (case dir
	(#\^ #\>)
	(#\v #\<)
	(#\< #\^)
	(#\> #\v))))

(defun move (pos dir)
  (pos (+ (pos-x pos) (case dir
		       (#\< -1)
		       (#\> 1)
		       (t 0)))
       (+ (pos-y pos) (case dir
			(#\v 1)
			(#\^ -1)
			(t 0)))))

(defun always (x)
  (lambda () x))

(defparameter *hash-11* (make-hash-table :test 'equal))
(defparameter *pos-11* ())
(defparameter *orientation-11* ())
(defparameter *robot-q-11* ())

(defun robot-input ()
  (gethash *pos-11* *hash-11* 0))

(defun robot-output (val)
  (push val *robot-q-11*)
  (when (cdr *robot-q-11*)
    (setf (gethash *pos-11* *hash-11*) (second *robot-q-11*))
    (setf *orientation-11* (turn *orientation-11* (first *robot-q-11*)))
    (setf *pos-11* (move *pos-11* *orientation-11*))
    (setf *robot-q-11* nil)
    (return-from robot-output *pos-11*)))

(defun estimate-area (&key (program *input-day-11*))
  (setf *hash-11* (make-hash-table :test 'equal))
  (setf *pos-11* (pos 0 0))
  (setf *robot-q-11* ())
  (setf *orientation-11* #\^)
  (intcode11 :memory program :input-fn #'robot-input :output-fn #'robot-output)
  (hash-table-count *hash-11*))

(defun hash-to-array (hash)
  (let ((pos))
    (maphash (lambda (k v)
	       (push (cons k v) pos))
	     hash)
    (let* ((x (1+ (reduce #'max pos :key (lambda (p) (pos-x (car p))))))
	   (y (1+ (reduce #'max pos :key (lambda (p) (pos-y (car p))))))
	   (out (make-array (list y x) :initial-element 0)))
      (dolist (e pos out)
	(setf (aref out (pos-y (car e)) (pos-x (car e))) (cdr e))))))

(defun registration-plate-11 (&key (program *input-day-11*))
  (setf *hash-11* (make-hash-table :test 'equal))
  (setf *pos-11* (pos 0 0))
  (setf (gethash *pos-11* *hash-11*) 1)
  (setf *robot-q-11* ())
  (setf *orientation-11* #\^)
  (intcode11 :memory program :input-fn #'robot-input :output-fn #'robot-output)
  (print-arr
   (hash-to-array *hash-11*)))


(defun solution-day11-1 ()
  (estimate-area))

(defun solution-day11-2 ()
  (registration-plate-11))

(defun solve-day11 ()
  (format t "Answer for puzzle 1 of day 11: ~A~&"
	  (solution-day11-1))
  (format t "Answer for puzzle 2 of day 11: ~A~&"
	  (solution-day11-2)))

;;;; Advent of code day 7

(in-package #:aoc2019)

(defparameter *input-day-07* #(3 8 1001 8 10 8 105 1 0 0 21 34 51 64
81 102 183 264 345 426 99999 3 9 102 2 9 9 1001 9 4 9 4 9 99 3 9 101 4
9 9 102 5 9 9 1001 9 2 9 4 9 99 3 9 101 3 9 9 1002 9 5 9 4 9 99 3 9
102 3 9 9 101 3 9 9 1002 9 4 9 4 9 99 3 9 1002 9 3 9 1001 9 5 9 1002 9
5 9 101 3 9 9 4 9 99 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 1001 9 1
9 4 9 3 9 101 1 9 9 4 9 3 9 101 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 101 2
9 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 99 3 9
101 2 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9
3 9 1001 9 2 9 4 9 3 9 101 1 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2 9 9
4 9 3 9 1002 9 2 9 4 9 3 9 101 1 9 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 102
2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 101 2 9 9 4 9 3 9
101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001 9 2 9 4 9
3 9 1002 9 2 9 4 9 99 3 9 1001 9 1 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9
2 9 4 9 3 9 101 2 9 9 4 9 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 102
2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001 9 1 9 4 9 99
3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 101 2 9 9
4 9 3 9 102 2 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9
2 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 99))

(defun intcode7 (&key (memory (copy-vec *input-day-05*)) (input '(1)) (ip 0))
  (let ((output)
	(input-q input)
	(mem (copy-vec memory)))
    (labels ((ip (n) (+ ip n))
	     (mem (ip &optional (mod 1))
	       (if (zerop mod)
		   (aref mem (aref mem ip))
		   (aref mem ip)))
	     ((setf mem) (val ip &optional (mod 1))
	       (if (zerop mod)
		   (setf (aref mem (aref mem ip)) val)
		   (setf (aref mem ip) val)))
	     (gen-opcode (fn mod1 mod2)
	       (setf (mem (ip 3) 0)
		     (funcall fn (mem (ip 1) mod1) (mem (ip 2) mod2)))
	       (ip 4))
	     (opcode1 (mod1 mod2)
	       (gen-opcode #'+ mod1 mod2))
	     (opcode2 (mod1 mod2)
	       (gen-opcode #'* mod1 mod2))
	     (opcode3 ()
	       (unless input-q (return-from intcode7 (list mem ip output)))
	       (setf (mem (ip 1) 0) (pop input-q))
	       (ip 2))
	     (opcode4 ()
	       (setf output (mem (ip 1) 0))
	       (ip 2))
	     (opcode5 (mod1 mod2)
	       (if (zerop (mem (ip 1) mod1)) (ip 3) (mem (ip 2) mod2)))
	     (opcode6 (mod1 mod2)
	       (if (zerop (mem (ip 1) mod1)) (mem (ip 2) mod2) (ip 3)))
	     (opcode7 (mod1 mod2)
	       (gen-opcode #'op< mod1 mod2))
	     (opcode8 (mod1 mod2)
	       (gen-opcode #'op= mod1 mod2)))
      (do ()
	  (())
	(destructuring-bind (opcode mod1 mod2 mod3)
	    (decode-opcode (mem ip))
	  (declare (ignorable mod3))
	  (setf ip
		(case opcode
		  (1 (opcode1 mod1 mod2))
		  (2 (opcode2 mod1 mod2))
		  (3 (opcode3))
		  (4 (opcode4))
		  (5 (opcode5 mod1 mod2))
		  (6 (opcode6 mod1 mod2))
		  (7 (opcode7 mod1 mod2))
		  (8 (opcode8 mod1 mod2))
		  (99 (return-from intcode7 output)))
		))))))


(defun combinations (list)
  (mapcan (lambda (l)
	    (let ((sub (combinations (remove l list))))
	      (if sub
		  (mapcar (lambda (l1)
			    (cons l l1))
			  sub)
		  (list (list l)))))
	  list))

(defun score (memory order)
  (let* ((s0 (intcode7 :memory memory :input (list (elt order 0) 0)))
	 (s1 (intcode7 :memory memory :input (list (elt order 1) s0)))
	 (s2 (intcode7 :memory memory :input (list (elt order 2) s1)))
	 (s3 (intcode7 :memory memory :input (list (elt order 3) s2)))
	 (s4 (intcode7 :memory memory :input (list (elt order 4) s3))))
    s4))

(defun score2 (memory order)
  (let (v0 v1 v2 v3 v4)
    (setf v0
	  (intcode7 :memory memory :input (list (elt order 0) 0))
	  v1
	  (intcode7 :memory memory :input (list (elt order 1) (elt v0 2)))
	  v2
	  (intcode7 :memory memory :input (list (elt order 2) (elt v1 2)))
	  v3
	  (intcode7 :memory memory :input (list (elt order 3) (elt v2 2)))
	  v4
	  (intcode7 :memory memory :input (list (elt order 4) (elt v3 2))))
    (unless (listp v4)
      (return-from score2 v4))
    (do ()
	(())
      ;;; mem ip score
    (setf v0
	  (intcode7 :memory (elt v0 0) :ip (elt v0 1)
		    :input (if (numberp v4) (list v4) (list (elt v4 2))))
	  v1
	  (intcode7 :memory (elt v1 0) :ip (elt v1 1)
		    :input (if (numberp v0) (list v0) (list (elt v0 2))))
	  v2
	  (intcode7 :memory (elt v2 0) :ip (elt v2 1)
		    :input (if (numberp v1) (list v1) (list (elt v1 2))))
	  v3
	  (intcode7 :memory (elt v3 0) :ip (elt v3 1)
		    :input (if (numberp v2) (list v2) (list (elt v2 2))))
	  v4
	  (intcode7 :memory (elt v4 0) :ip (elt v4 1)
		    :input (if (numberp v3) (list v3) (list (elt v3 2))))
	  )
    (unless (listp v4)
      (return-from score2 v4))
      )))

(defun search7 (&optional (memory *input-day-07*) )
  (let ((orders (combinations '(0 1 2 3 4)))
	(score 0))
    (dolist (order orders score)
      (setf score
	    (max score (score memory order))))))

(defun search7-2 (&optional (memory *input-day-07*) )
  (let ((orders (combinations '(5 6 7 8 9)))
	(score 0))
    (dolist (order orders score)
      (setf score
	    (max score (score2 memory order))))))


(defun solution-day07-1 ()
  (search7))

(defun solution-day07-2 ()
  (search7-2))

(defun solve-day07 ()
  (format t "Answer for puzzle 1 of day 7: ~A~&"
	  (solution-day07-1))
  (format t "Answer for puzzle 2 of day 7: ~A~&"
	  (solution-day07-2)))

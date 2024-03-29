;;;; Advent of code day 5

(in-package #:aoc2019)

(defparameter *input-day-05* #(3 225 1 225 6 6 1100 1 238 225 104 0
1101 72 36 225 1101 87 26 225 2 144 13 224 101 -1872 224 224 4 224 102
8 223 223 1001 224 2 224 1 223 224 223 1102 66 61 225 1102 25 49 224
101 -1225 224 224 4 224 1002 223 8 223 1001 224 5 224 1 223 224 223
1101 35 77 224 101 -112 224 224 4 224 102 8 223 223 1001 224 2 224 1
223 224 223 1002 195 30 224 1001 224 -2550 224 4 224 1002 223 8 223
1001 224 1 224 1 224 223 223 1102 30 44 225 1102 24 21 225 1 170 117
224 101 -46 224 224 4 224 1002 223 8 223 101 5 224 224 1 224 223 223
1102 63 26 225 102 74 114 224 1001 224 -3256 224 4 224 102 8 223 223
1001 224 3 224 1 224 223 223 1101 58 22 225 101 13 17 224 101 -100 224
224 4 224 1002 223 8 223 101 6 224 224 1 224 223 223 1101 85 18 225
1001 44 7 224 101 -68 224 224 4 224 102 8 223 223 1001 224 5 224 1 223
224 223 4 223 99 0 0 0 677 0 0 0 0 0 0 0 0 0 0 0 1105 0 99999 1105 227
247 1105 1 99999 1005 227 99999 1005 0 256 1105 1 99999 1106 227 99999
1106 0 265 1105 1 99999 1006 0 99999 1006 227 274 1105 1 99999 1105 1
280 1105 1 99999 1 225 225 225 1101 294 0 0 105 1 0 1105 1 99999 1106
0 300 1105 1 99999 1 225 225 225 1101 314 0 0 106 0 0 1105 1 99999 7
677 226 224 102 2 223 223 1005 224 329 101 1 223 223 8 677 226 224
1002 223 2 223 1005 224 344 1001 223 1 223 1107 677 677 224 102 2 223
223 1005 224 359 1001 223 1 223 1107 226 677 224 102 2 223 223 1005
224 374 101 1 223 223 7 226 677 224 102 2 223 223 1005 224 389 101 1
223 223 8 226 677 224 1002 223 2 223 1005 224 404 101 1 223 223 1008
226 677 224 1002 223 2 223 1005 224 419 1001 223 1 223 107 677 677 224
102 2 223 223 1005 224 434 101 1 223 223 1108 677 226 224 1002 223 2
223 1006 224 449 101 1 223 223 1108 677 677 224 102 2 223 223 1006 224
464 101 1 223 223 1007 677 226 224 102 2 223 223 1006 224 479 101 1
223 223 1008 226 226 224 102 2 223 223 1006 224 494 101 1 223 223 108
226 226 224 1002 223 2 223 1006 224 509 101 1 223 223 107 226 226 224
102 2 223 223 1006 224 524 101 1 223 223 1107 677 226 224 102 2 223
223 1005 224 539 1001 223 1 223 108 226 677 224 1002 223 2 223 1005
224 554 101 1 223 223 1007 226 226 224 102 2 223 223 1005 224 569 101
1 223 223 8 226 226 224 102 2 223 223 1006 224 584 101 1 223 223 1008
677 677 224 1002 223 2 223 1005 224 599 1001 223 1 223 107 226 677 224
1002 223 2 223 1005 224 614 1001 223 1 223 1108 226 677 224 102 2 223
223 1006 224 629 101 1 223 223 7 677 677 224 1002 223 2 223 1005 224
644 1001 223 1 223 108 677 677 224 102 2 223 223 1005 224 659 101 1
223 223 1007 677 677 224 102 2 223 223 1006 224 674 101 1 223 223 4
223 99 226))

(defun decode-opcode (int)
  "ABCDE -> (DE C B A)"
  (let ((str (format nil "~5,'0d" int)))
    (list (parse-integer str :start 3)
	  (parse-integer str :start 2 :end 3)
	  (parse-integer str :start 1 :end 2)
	  (parse-integer str :start 0 :end 1))))

(defun op< (p1 p2)
  (if (< p1 p2) 1 0))

(defun op= (p1 p2)
  (if (= p1 p2) 1 0))

(defun intcode5 (&key (memory (copy-vec *input-day-05*)) (input 1))
  (let ((ip 0)
	(output)
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
	       (setf (mem (ip 1) 0) input)
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
		  (99 (return-from intcode5 output)))
		))))))

(defun solution-day05-1 ()
  (intcode5))

(defun solution-day05-2 ()
  (intcode5 :input 5))

(defun solve-day05 ()
  (format t "Answer for puzzle 1 of day 5: ~A~&"
	  (solution-day05-1))
  (format t "Answer for puzzle 2 of day 5: ~A~&"
	  (solution-day05-2)))

;;;; Advent of code day 12

(in-package #:aoc2019)

(defparameter *input-day-12*
  '(12 0 -15
    -8 -5 -10
    7 -17 1
    2 -11 -6))

(defun pos3 (x y z)
  (cons x (cons y z)))

(defun pos3-x (pos)
  (car pos))

(defun pos3-y (pos)
  (cadr pos))

(defun pos3-z (pos)
  (cddr pos))

(defun pos3= (pos1 pos2)
  (and (= (car pos1) (car pos2))
       (= (cadr pos1) (cadr pos2))
       (= (cddr pos1) (cddr pos2))))

(defun pos3+ (pos1 pos2)
  (pos3 (+ (car pos1) (car pos2))
	(+ (cadr pos1) (cadr pos2))
	(+ (cddr pos1) (cddr pos2))))

(defstruct moon
  (pos)
  (vel (pos3 0 0 0)))

(defun moon= (m1 m2)
  (and (pos3= (moon-pos m1) (moon-pos m2))
       (pos3= (moon-vel m1) (moon-vel m2))))

(defun read-moons (&key (input *input-day-12*))
  (do* ((inputs input (cdddr inputs))
	(moons (list (make-moon :pos (pos3 (first inputs)
						  (second inputs)
						  (third inputs))))
	       (push (make-moon :pos (pos3 (first inputs)
						  (second inputs)
						  (third inputs)))
		     moons)))
       ((null (cdddr inputs)) (reverse moons))))

(defun g-comp (v1 v2)
  (cond ((= v1 v2) 0)
	((< v1 v2) 1)
	((> v1 v2) -1)))

(defun g-pull (m1 m2)
  (let ((p1 (moon-pos m1))
	(p2 (moon-pos m2)))
  (pos3 (g-comp (pos3-x p1) (pos3-x p2))
	(g-comp (pos3-y p1) (pos3-y p2))
	(g-comp (pos3-z p1) (pos3-z p2)))))

(defun pot-e (moon)
  (let ((p (moon-pos moon)))
    (+ (abs (pos3-x p))
       (abs (pos3-y p))
       (abs (pos3-z p)))))

(defun kin-e (moon)
  (let ((v (moon-vel moon)))
    (+ (abs (pos3-x v))
       (abs (pos3-y v))
       (abs (pos3-z v)))))

(defun total-energy (moons)
  (reduce #'+ (mapcar (lambda (m) (* (pot-e m) (kin-e m))) moons)))


(defun gravity (moons)
  (let* ((pulls))
    (dolist (moon moons)
      (let* ((o-moons (remove moon moons :test #'eq :count 1)))
	(push (reduce #'pos3+ (mapcar (lambda (m) (g-pull moon m)) o-moons))
	      pulls)))
    (mapc (lambda (moon g)
	      (setf (moon-vel moon)
		    (pos3+ (moon-vel moon) g)))
	    moons
	    (nreverse pulls))))

(defun velocity (moons)
  (mapc (lambda (m)
	    (setf (moon-pos m)
		  (pos3+ (moon-pos m) (moon-vel m))))
	  moons))

(defun simulate (moons &key (steps 1))
  (dotimes (i steps moons)
    (gravity moons)
    (velocity moons)))


(defun g1 (ps vs)
  "Returns new velocities"
  (let* ((pulls))
    (dolist (p ps)
      (let* ((o-ps (remove p ps :test #'eq :count 1)))
	(push (reduce #'+ (mapcar (lambda (m)
				    (g-comp p m))
				  o-ps))
	      pulls)))
    (mapcar #'+ vs (nreverse pulls))))

(defun v1 (ps vs)
  "Return positions after applying velocity"
  (mapcar #'+ ps vs))

(defun simulate-circle-1 (i-ps)
  (let* ((i-vs (mapcar (lambda (x) (declare (ignorable x)) 0) i-ps))
	 (start (append i-ps i-vs)))
    (do* ((vs (g1 i-ps i-vs) (g1 ps vs))
	  (ps (v1 i-ps vs) (v1 ps vs))
	  (state (append ps vs) (append ps vs))
	  (steps 1 (incf steps)))
	 ((equal state start) steps))))

(defun simulate-circle (moons)
  (lcm (simulate-circle-1 (mapcar (lambda (m) (pos3-x (moon-pos m))) moons))
       (simulate-circle-1 (mapcar (lambda (m) (pos3-y (moon-pos m))) moons))
       (simulate-circle-1 (mapcar (lambda (m) (pos3-z (moon-pos m))) moons))))

(defun solution-day12-1 ()
  (total-energy (simulate (read-moons) :steps 1000)))

(defun solution-day12-2 ()
  (simulate-circle (read-moons)))

(defun solve-day12 ()
  (format t "Answer for puzzle 1 of day 9: ~A~&"
	  (solution-day12-1))
  (format t "Answer for puzzle 2 of day 9: ~A~&"
	  (solution-day12-2)))

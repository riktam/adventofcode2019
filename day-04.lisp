;;;; Advent of code day 4

(in-package #:aoc2019)

(defparameter *range* '(246540 787419))

(defun increasing-digits-p (string)
  (char<= (char string 0)
	  (char string 1)
	  (char string 2)
	  (char string 3)
	  (char string 4)
	  (char string 5)))

(defun some-adjacent-p (string)
  (or (char= (char string 0) (char string 1))
      (char= (char string 1) (char string 2))
      (char= (char string 2) (char string 3))
      (char= (char string 3) (char string 4))
      (char= (char string 4) (char string 5))))

(defun group-digits (string)
  (let ((groups)
	(count 1))
    (labels ((group (n)
	       (if (char= (char string n)
			  (char string (1+ n)))
		   (incf count)
		   (progn (push count groups)
			  (setf count 1)))))
      (dotimes (i 5)
	(group i)))
    (push count groups)
    (some (lambda (x) (= x 2)) groups)))

(defun search-password (start end)
  (let ((count 0))
    (dotimes (i (1+ (- end start)) count)
      (let ((string (format nil "~d" (+ start i))))
	(when (and (increasing-digits-p string)
		   (some-adjacent-p string))
	  (incf count))))))

(defun search-password2 (start end)
  (let ((count 0))
    (dotimes (i (1+ (- end start)) count)
      (let ((string (format nil "~d" (+ start i))))
	(when (and (increasing-digits-p string)
		   (some-adjacent-p string)
		   (group-digits string))
	  (incf count))))))

(defun solution-day04-1 ()
  (search-password (first *range*) (second *range*)))

(defun solution-day04-2 ()
  (search-password2 (first *range*) (second *range*)))

(defun solve-day04 ()
  (format t "Answer for puzzle 1 of day 4: ~A~&"
	  (solution-day04-1))
  (format t "Answer for puzzle 2 of day 4: ~A~&"
	  (solution-day04-2)))

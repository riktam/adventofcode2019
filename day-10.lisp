;;;; Advent of code day 10

(in-package #:aoc2019)

(defparameter *input-day-10*
"#.#................#..............#......#......
.......##..#..#....#.#.....##...#.........#.#...
.#...............#....#.##......................
......#..####.........#....#.......#..#.....#...
.....#............#......#................#.#...
....##...#.#.#.#.............#..#.#.......#.....
..#.#.........#....#..#.#.........####..........
....#...#.#...####..#..#..#.....#...............
.............#......#..........#...........#....
......#.#.........#...............#.............
..#......#..#.....##...##.....#....#.#......#...
...#.......##.........#.#..#......#........#.#..
#.............#..........#....#.#.....#.........
#......#.#................#.......#..#.#........
#..#.#.....#.....###..#.................#..#....
...............................#..........#.....
###.#.....#.....#.............#.......#....#....
.#.....#.........#.....#....#...................
........#....................#..#...............
.....#...#.##......#............#......#.....#..
..#..#..............#..#..#.##........#.........
..#.#...#.......#....##...#........#...#.#....#.
.....#.#..####...........#.##....#....#......#..
.....#..#..##...............................#...
.#....#..#......#.#............#........##...#..
.......#.....................#..#....#.....#....
#......#..###...........#.#....#......#.........
..............#..#.#...#.......#..#.#...#......#
.......#...........#.....#...#.............#.#..
..##..##.............#........#........#........
......#.............##..#.........#...#.#.#.....
#........#.........#...#.....#................#.
...#.#...........#.....#.........#......##......
..#..#...........#..........#...................
.........#..#.......................#.#.........
......#.#.#.....#...........#...............#...
......#.##...........#....#............#........
#...........##.#.#........##...........##.......
......#....#..#.......#.....#.#.......#.##......
.#....#......#..............#.......#...........
......##.#..........#..................#........
......##.##...#..#........#............#........
..#.....#.................###...#.....###.#..#..
....##...............#....#..................#..
.....#................#.#.#.......#..........#..
#........................#.##..........#....##..
.#.........#.#.#...#...#....#........#..#.......
...#..#.#......................#...............#")


(defun to-array (str)
  (let ((lines)
	(out))
    (with-input-from-string (stream str)
      (do ((line (read-line stream nil) (read-line stream nil)))
	  ((null line))
	(push line lines)))
    (setf lines (nreverse lines))
    (let ((x (length (first lines)))
	  (y (length lines)))
      (setf out (make-array (list y x)))
      (dotimes (j y out)
	(let ((l (nth j lines)))
	  (dotimes (i x)
	    (setf (aref out j i)
		  (char l i))))))))

(defun angles (arr)
  (let ((lenx (array-dimension arr 1))
	(leny (array-dimension arr 0))
	(hash (make-hash-table :test 'equal))
	(out))
    (dotimes (x lenx)
      (dotimes (y leny)
	(unless (and (zerop x) (zerop y))
	  (let* ((gcd (gcd x y))
		 (gcdx (/ x gcd))
		 (gcdy (/ y gcd)))
	    (setf (gethash (pos gcdx gcdy) hash) t)
	    (setf (gethash (pos gcdx (- gcdy)) hash) t)
	    (setf (gethash (pos (- gcdx) gcdy) hash) t)
	    (setf (gethash (pos (- gcdx) (- gcdy)) hash) t)))))
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (push k out))
	     hash)
    out))

(defun asteroidp (arr x y)
  (char= #\# (aref arr y x)))

(defun vaporize (arr x y)
  (setf (aref arr y x) #\.))

(defun detect (arr pos pos-angle)
  (let* ((posx (pos-x pos))
	 (posy (pos-y pos))
	 (angx (pos-x pos-angle))
	 (angy (pos-y pos-angle))
	 (maxx (array-dimension arr 1))
	 (maxy (array-dimension arr 0))
	 (max-steps (min (floor maxx (max angx 1))
			 (floor maxy (max angy 1)))))
    (dotimes (i max-steps)
      (let ((newx (+ posx (* (1+ i) angx)))
	    (newy (+ posy (* (1+ i) angy))))
	(when (and (>= newx 0) (> maxx newx)
		   (>= newy 0) (> maxy newy)
		   (asteroidp arr newx newy))
	  (return-from detect t))))))

(defun count-detect (arr pos angles)
  (let ((count 0))
    (dolist (a angles count)
      (when (detect arr pos a)
	(incf count)))))

(defun max-detect (arr)
  (let ((angles (angles arr))
	(out)
	(pos-max))
    (dotimes (y (array-dimension arr 0))
      (dotimes (x (array-dimension arr 1))
	(when (asteroidp arr x y)
	  (push (list* (count-detect arr (pos x y) angles) x y) out))))
    (setf pos-max (position (reduce #'max out :key #'first) out :key #'first))
    (values (first (nth pos-max out))
	    (rest (nth pos-max out)))))

(defun try-vaporize (arr pos pos-angle)
  (let* ((posx (pos-x pos))
	 (posy (pos-y pos))
	 (angx (pos-x pos-angle))
	 (angy (pos-y pos-angle))
	 (maxx (array-dimension arr 1))
	 (maxy (array-dimension arr 0))
	 (max-steps (min (floor maxx (max angx 1))
			 (floor maxy (max angy 1)))))
    (dotimes (i max-steps)
      (let ((newx (+ posx (* (1+ i) angx)))
	    (newy (+ posy (* (1+ i) angy))))
	(when (and (>= newx 0) (> maxx newx)
		   (>= newy 0) (> maxy newy)
		   (asteroidp arr newx newy))
	  (vaporize arr newx newy)
	  (return-from try-vaporize (pos newx newy)))))))

(defun sort-angles (angles)
  (let (q1 q2 q3 q4)
    (dolist (a angles)
      (let ((x (pos-x a))
	    (y (pos-y a)))
	(cond ((and (>= x 0) (>= y 0)) (push a q2))
	      ((and (>= x 0) (< y 0)) (push a q3))
	      ((and (< x 0) (< y 0)) (push a q4))
	      ((and (< x 0) (>= y 0)) (push a q1)))))
    (nconc
     (sort q1 #'< :key (lambda (a) (atan (pos-y a) (pos-x a))))
     (sort q2 #'< :key (lambda (a) (atan (pos-y a) (pos-x a))))
     (sort q3 #'< :key (lambda (a) (atan (pos-y a) (pos-x a))))
     (sort q4 #'< :key (lambda (a) (atan (pos-y a) (pos-x a)))))))

(defun vaporize200 (arr &optional (n 200))
  (let ((angles (sort-angles (angles arr)))
	(count 0))
    (multiple-value-bind (maxd station)
	(max-detect arr)
      (declare (ignore maxd))
      (dotimes (i n)
	(dolist (angle angles)
	  (let ((asteroid (try-vaporize arr station angle)))
	    (when asteroid
	      (incf count)
	      (when (= n count)
		(return-from vaporize200
		  (+ (* 100 (pos-x asteroid))
		     (pos-y asteroid)))))))))))


(defun solution-day10-1 ()
  (max-detect (to-array *input-day-10*)))

(defun solution-day10-2 ()
  (vaporize200 (to-array *input-day-10*)))

(defun solve-day10 ()
  (format t "Answer for puzzle 1 of day 9: ~A~&"
	  (solution-day10-1))
  (format t "Answer for puzzle 2 of day 9: ~A~&"
	  (solution-day10-2)))

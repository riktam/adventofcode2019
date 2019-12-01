;;;; Advent of code day 1

(in-package #:aoc2019)

(defparameter *input-day-01* '(53035 84950 100012 75128 96658 80583
  72951 131866 99675 115923 65140 59430 81288 53129 96172 58912 138055
  62920 122353 59176 149310 105061 58808 103111 128538 61366 53480
  94427 121742 143966 63092 92543 67136 81575 131971 71040 57035
  114448 101314 123688 137916 68612 122453 98141 61137 97628 126791
  111826 50630 67829 126285 97331 88587 64552 111221 89639 72177
  132711 51062 98061 57919 57166 134565 58677 62505 85253 147337 84791
  114516 95597 139271 83561 68285 100684 86803 85887 74554 113749
  81899 107220 110959 118220 52743 71651 74775 106517 132133 56919
  129699 137357 75781 59409 134589 131438 101641 105503 104371 145308
  75777 107333))


#|
Fuel required to launch a given module is based on its mass. Specifically,
to find the fuel required for a module, take its mass, divide by three,
round down, and subtract 2.

For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2.
For a mass of 14, dividing by 3 and rounding down still yields 4, so the 
  fuel required is also 2.
For a mass of 1969, the fuel required is 654.
For a mass of 100756, the fuel required is 33583.
|#

(defun fuel (mass)
  "Fuel required to launch a given module is based on its mass. ~
 Specifically, to find the fuel required for a module, take its mass, ~
 divide by three, round down, and subtract 2."
  (max (- (floor mass 3) 2) 0))

#|
Fuel itself requires fuel just like a module - take its mass, divide by three,
 round down, and subtract 2. However, that fuel also requires fuel, and that
fuel requires fuel, and so on. Any mass that would require negative fuel should
 instead be treated as if it requires zero fuel; the remaining mass, if any, is
 instead handled by wishing really hard, which has no mass and is outside the
 scope of this calculation.

So, for each module mass, calculate its fuel and add it to the total.
 Then, treat the fuel amount you just calculated as the input mass and repeat
 the process, continuing until a fuel requirement is zero or negative. 
For example:

    A module of mass 14 requires 2 fuel. This fuel requires no further fuel
 (2 divided by 3 and rounded down is 0, which would call for a negative fuel),
so the total fuel required is still just 2.
    At first, a module of mass 1969 requires 654 fuel. Then, this fuel requires
 216 more fuel (654 / 3 - 2). 216 then requires 70 more fuel, which requires
 21 fuel, which requires 5 fuel, which requires no further fuel. So, the total
 fuel required for a module of mass 1969 is 654 + 216 + 70 + 21 + 5 = 966.
The fuel required by a module of mass 100756 and its fuel is: 33583 + 11192 +
 3728 + 1240 + 411 + 135 + 43 + 12 + 2 = 50346.
|#

(defun rfuel (mass)
  "Fuel itself requires fuel just like a module - take its mass, divide by three,
 round down, and subtract 2. However, that fuel also requires fuel, and that
fuel requires fuel, and so on. Any mass that would require negative fuel should
 instead be treated as if it requires zero fuel; the remaining mass, if any, is
 instead handled by wishing really hard, which has no mass and is outside the
 scope of this calculation.

So, for each module mass, calculate its fuel and add it to the total.
 Then, treat the fuel amount you just calculated as the input mass and repeat
 the process, continuing until a fuel requirement is zero or negative."
  (let ((fuel (fuel mass)))
    (if (zerop fuel)
	0
	(+ fuel (rfuel fuel)))))


(defun solution-day01-1 ()
  (reduce #'+ *input-day-01* :key #'fuel))

(defun solution-day01-2 ()
  (reduce #'+ *input-day-01* :key #'rfuel))


(defun solve-day01 ()
  (format t "Answer for puzzle 1 of day 1: ~A~&"
	  (solution-day01-1))
  (format t "Answer for puzzle 2 of day 1: ~A~&"
	  (solution-day01-2)))

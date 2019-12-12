;;;; aoc2019.asd

(asdf:defsystem #:aoc2019
  :description "Advent of code 2019 submission http://adventofcode.com/2019/"
  :author "Riktam <ruirrs@gmail.com>"
  :license  "What license?"
  :version "2019.12.11"
  :serial t
  :components ((:file "package")
               (:file "aoc2019")
               (:file "day-01")
	       (:file "day-02")
	       (:file "day-03")
	       (:file "day-04")
	       (:file "day-05")
	       (:file "day-06")
	       (:file "day-07")
	       (:file "day-08")
	       (:file "day-09")
	       (:file "day-10")
	       (:file "day-11")
	       ))

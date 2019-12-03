;;;; aoc2019.asd

(asdf:defsystem #:aoc2019
  :description "Advent of code 2019 submission http://adventofcode.com/2019/"
  :author "Riktam <ruirrs@gmail.com>"
  :license  "What license?"
  :version "2019.12.2"
  :serial t
  :components ((:file "package")
               (:file "aoc2019")
               (:file "day-01")
	       (:file "day-02")
	       ))

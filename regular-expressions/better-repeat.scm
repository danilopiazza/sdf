#| -*-Scheme-*-

Copyright (C) 2019 Chris Hanson and Gerald Jay Sussman

This file is part of SDF.  SDF is software supporting the book
"Software Design for Flexibility", by Chris Hanson and Gerald Jay
Sussman.

SDF is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SDF is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with SDF.  If not, see <https://www.gnu.org/licenses/>.

|#

;;; This is part of an answer to exercise 2.4

#|
;;; Earlier code in book looked like:
(define (r:repeat min max expr)
  (apply r:seq
         (append
          (make-list min expr)
          (if (eqv? max min)
              '()
              (if max
                  (make-list (- max min) (r:alt expr ""))
                  (list expr "*")))))
|#

(define (r:repeat min max expr)
  (apply r:seq
         (if max
             (optimized-interval min max expr)
             (append (optimized-interval min min expr)
                     (list expr "*")))))

(define (optimized-interval min max expr)
  (if (eqv? min max)
      (case min
        ((0) '())
        ((1) (list expr))
        (else (simple-interval min max expr)))
      (simple-interval min max expr)))

(define (simple-interval min max expr)
  (list expr "\\{" (number->string min)
        "," (number->string max) "\\}"))

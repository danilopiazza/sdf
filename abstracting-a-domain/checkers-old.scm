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

#|
test sequence:
  (move 1)
  (move 1)
  (move 1)
  (move 5)
  (move 3)
|#

(define checkers
  (make-checkers
   (lambda (board)
     (generate-moves
      (map (lambda (piece)
             (initial-pmove board piece))
           (current-pieces board))))))

(define (generate-moves initial-pmoves)
  (crown-kings
   (mandate-jumps
    (evolve-pmoves initial-pmoves))))

(define (crown-kings pmoves)
  (map (lambda (pmove)
         (let ((piece (current-piece pmove)))
           (if (should-be-crowned? piece)
               (update-piece crown-piece pmove)
               pmove)))
       pmoves))

(define (mandate-jumps pmoves)
  (let ((jumps (filter captures-pieces? pmoves)))
    (if (pair? jumps)
        jumps
        pmoves)))

(define (evolve-pmoves initial-pmoves)
  (append-map (lambda (initial-pmove)
                (evolve-pmove initial-pmove))
              initial-pmoves))

(define (evolve-pmove pmove)
  (let ((pmoves (proposed-checkers-moves pmove)))
    (let ((jumps (filter captures-pieces? pmoves)))
      (if (pair? jumps)
          (evolve-jumps jumps)
          pmoves))))

(define (evolve-jumps pmoves)
  (append-map (lambda (pmove)
                (let ((new-pmoves (proposed-checkers-moves pmove)))
                  (if (pair? new-pmoves)
                      (evolve-jumps new-pmoves)
                      (list (finish-move pmove)))))
              pmoves))

(define (proposed-checkers-moves pmove)
  (filter-map (lambda (direction)
                (proposed-checkers-move pmove direction))
              (possible-directions (current-piece pmove))))

(define (proposed-checkers-move pmove direction)
  (let ((new-position (compute-new-position direction 1 pmove))
        (board (current-board pmove)))
    (and (is-position-on-board? new-position board)
         (case (position-info new-position board)
           ((unoccupied)
            (and (not (captures-pieces? pmove))
                 (finish-move (new-piece-position new-position pmove))))
           ((occupied-by-opponent)
            (let ((landing (compute-new-position direction 2 pmove)))
              (and (is-position-on-board? landing board)
                   (is-position-unoccupied? landing board)
                   (new-piece-position landing
                                       (capture-piece-at new-position pmove)))))
           ((occupied-by-self) #f)
           (else (error "Unknown position info"))))))

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

(define (find-pathnames predicate directory)
  (let loop ((directory (pathname-as-directory directory)))
    (append-map (lambda (pathname)
                  (cond ((member (file-namestring pathname)
                                 '("." ".."))
                         '())
                        ((file-directory? pathname)
                         (loop (pathname-as-directory pathname)))
                        ((predicate pathname) (list pathname))
                        (else '())))
                (directory-read directory))))

(define (top-level-subdirs directory)
  (let ((directory (pathname-as-directory directory)))
    (filter-map (lambda (pn)
                  (and (not (string-prefix? "." (file-namestring pn)))
                       (eq? 'directory (file-type-direct pn))
                       (pathname-as-directory pn)))
                (directory-read directory))))

(define (apropos-matches match names)
  (let ((match (string match)))
    (let ((name
           (find (lambda (name)
                   (string=? match (string name)))
                 names)))
      (if name
          (list name)
          (filter (lambda (name)
                    (substring? match (string name)))
                  names)))))

(define (force-top-level-repl! value expression environment)
  (abort->top-level
   (lambda (cmdl)
     (set-repl/environment! cmdl environment)
     (repl-write value expression cmdl))))
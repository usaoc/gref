#lang racket/base
;; Copyright (C) 2022 Wing Hei Chan

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

(provide citet generate-bibliography
         evo-of-lisp fun-con-pl lisp-mac)

(require scriblib/autobib)

(define-cite _cite citet generate-bibliography)

(define evo-of-lisp
  (make-bib
   #:title "The evolution of Lisp"
   #:author (authors (author-name "Guy L." "Steele" #:suffix "Jr.")
                     (author-name "Richard P." "Gabriel"))
   #:date 1993
   #:location (proceedings-location
               "Conference on History of Programming Languages"
               #:pages '(231 270))
   #:doi "10/dsp6sr"))

(define fun-con-pl
  (make-bib
   #:title "Fundamental concepts in programming languages"
   #:author (author-name "Christopher S." "Strachey")
   #:date 2000
   #:location (journal-location
               "Higher-Order and Symbolic Computation"
               #:pages '(11 49) #:volume "13" #:number "1/2")
   #:doi "10/cpt37d"))

(define lisp-mac
  (make-bib
   #:title "A LISP machine with very compact programs"
   #:author (author-name "L. Peter" "Deutsch")
   #:date 1973
   #:location
   (proceedings-location
    "International Joint Conference on Artificial Intelligence"
    #:pages '(697 703))))

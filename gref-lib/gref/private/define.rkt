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

(provide define-accessor)

(require syntax/parse/define
         (for-syntax gref/private/generic
                     racket/base
                     racket/contract/base
                     syntax/parse
                     syntax/transformer))

(define-for-syntax transformer/c (-> syntax? syntax?))

(begin-for-syntax
  (struct accessor (expr set!)
    #:property prop:procedure (struct-field-index expr)
    #:property prop:set!-transformer (struct-field-index expr)
    #:methods gen:set!-expander
    [(define (set!-expand acc ref) ((accessor-set! acc) ref))]))

(define-syntax-parser define-accessor
  [(_:id name:id expr:id set!)
   #:declare set! (expr/c #'transformer/c
                          #:phase (add1 (syntax-local-phase-level)))
   (syntax/loc this-syntax
     (define-syntax name
       (accessor (make-variable-like-transformer #'expr) set!.c)))])

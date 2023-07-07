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

(provide define-set!-syntax define-set!-syntaxes)

(require syntax/parse/define
         (for-syntax gref/private/space
                     racket/base
                     syntax/datum
                     syntax/parse/lib/function-header))

(begin-for-syntax
  (define-syntax-class header
    #:description "definition header"
    #:commit
    #:attributes (name make-fn)
    (pattern (name:id . formals:formals)
      #:attr make-fn
      (lambda (body-stxs)
        (define/syntax-parse (body ...) body-stxs)
        #'(lambda formals body ...)))
    (pattern (inner:header . formals:formals)
      #:attr make-fn
      (lambda (body-stxs)
        (define/syntax-parse (body ...) body-stxs)
        ((datum inner.make-fn) (list #'(lambda formals body ...))))
      #:with name #'inner.name)))

(define-syntax-parser define-set!-syntax
  [(_:id name:id val:expr)
   (syntax/loc this-syntax (define-set!-syntaxes (name) val))]
  [(_:id header:header body:expr ...+)
   #:with fn ((datum header.make-fn) (datum (body ...)))
   (syntax/loc this-syntax (define-set!-syntaxes (header.name) fn))])

(define-syntax-parser define-set!-syntaxes
  [(_:id (name:id ...) vals:expr)
   #:with (set!-name ...) (map in-set!-space (datum (name ...)))
   (syntax/loc this-syntax (define-syntaxes (set!-name ...) vals))])

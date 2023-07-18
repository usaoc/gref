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

(provide check-expand check-raise check-type check-value check-values)

(require expect
         expect/rackunit
         syntax/parse/define
         (for-syntax racket/base
                     syntax/id-table
                     syntax/parse))

(define-for-syntax (make-get-exp! make-exp)
  (define table (make-free-id-table))
  (lambda (id)
    (define (lift-exp) (syntax-local-lift-expression (make-exp id)))
    (free-id-table-ref! table id lift-exp)))

(define-for-syntax get-raise-exp!
  (make-get-exp! (syntax-parser
                   [type #'(expect-raise (expect-struct type))])))

(define-for-syntax get-expand-exp!
  (make-get-exp! (syntax-parser
                   [_
                    #:with raise-exp (get-raise-exp! this-syntax)
                    #'(expect-expand raise-exp)])))

(define-syntax-parser check-expand
  [(_:id exn:id body:expr ...+)
   #:with expand-exp (get-expand-exp! #'exn)
   (syntax/loc this-syntax
     (check-expect #'(let () body ...) expand-exp))])

(define-syntax-parser check-raise
  [(_:id exn:id body:expr ...+)
   #:with raise-exp (get-raise-exp! #'exn)
   (syntax/loc this-syntax
     (check-expect (lambda () body ...) raise-exp))])

(define-for-syntax get-type-exp!
  (make-get-exp! (syntax-parser [pred #'(expect-pred pred)])))

(define-syntax-parser check-type
  [(_:id pred:id body:expr ...+)
   #:with type-exp (get-type-exp! #'pred)
   (syntax/loc this-syntax
     (check-expect (let () body ...) type-exp))])

(define-syntax-parser check-value
  [(_:id value-exp:expr body:expr ...+)
   (syntax/loc this-syntax
     (check-expect (let () body ...) value-exp))])

(define-syntax-parser check-values
  [(_:id (value-exp:expr ...) body:expr ...+)
   (syntax/loc this-syntax
     (check-expect (lambda () body ...)
                   (expect-return value-exp ...)))])

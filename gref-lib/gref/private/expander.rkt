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

(require (for-syntax racket/base
                     racket/provide-transform
                     syntax/parse))
(define-for-syntax (expand-set!-out stx modes)
  (syntax-parse stx
    [(_:id spec ...)
     (expand-export
      (syntax/loc this-syntax
        (combine-out (~@ spec (for-space gref/set! spec)) ...))
      modes)]))
(define-syntax set!-out (make-provide-transformer expand-set!-out))
(provide (set!-out
          values mcar mcdr hash-ref bytes-ref string-ref
          vector-ref vector*-ref unbox unbox*))

(require gref/private/define
         racket/contract/base
         racket/unsafe/ops
         syntax/parse/define
         (for-syntax gref/private/class
                     gref/private/expand
                     gref/private/property
                     racket/base
                     racket/symbol
                     racket/syntax
                     syntax/datum
                     syntax/parse))

(define-syntax-parser define-set!-expander
  [(_:id name:id proc:expr)
   #'(define-set!-syntax name (make-set!-expander proc))])

(define-syntax-parser define-set!-parser
  [(_:id name:id . tail)
   (syntax/loc this-syntax
     (define-set!-expander name (syntax-parser . tail)))])

(define-set!-parser values
  [(_:id . ref:%gref1s)
   (apply set!-pack
          #'ref.getter #'ref.setter
          (datum (ref.preamble ...))
          #:arity (datum ref.arity))])

(define-for-syntax (make-mcar mcar set-mcar!)
  (syntax-parser
    [(_:id pair-expr)
     #:declare pair-expr (maybe-expr/c #'mpair?)
     #:with mcar mcar
     #:with set-mcar! set-mcar!
     (set!-pack #'(lambda () (mcar pair))
                #'(lambda (val) (set-mcar! pair val))
                #'(define pair pair-expr.c))]))

(define-set!-expander mcar
  (make-mcar #'unsafe-mcar #'unsafe-set-mcar!))

(define-set!-expander mcdr
  (make-mcar #'unsafe-mcdr #'unsafe-set-mcdr!))

(define mutable/c (not/c immutable?))

(define-set!-parser hash-ref
  [(_:id hash-expr key-expr:expr (~optional failure-expr))
   #:declare hash-expr (expr/c #'hash?)
   #:declare failure-expr (expr/c #'failure-result/c)
   #:attr failure (and (datum failure-expr) #'failure)
   #:with mutable-hash (syntax/loc #'hash-expr hash)
   #:declare mutable-hash (expr/c #'mutable/c)
   (apply set!-pack
          #'(lambda () (hash-ref hash key (~? failure)))
          #'(lambda (val) (hash-set! mutable-hash.c key val))
          `(,#'(define hash hash-expr.c)
            ,#'(define key key-expr)
            ,@(if (datum failure-expr)
                  (list #'(define failure failure-expr.c))
                  '())))])

(begin-for-syntax
  (define-splicing-syntax-class obj?
    #:attributes (obj? obj?-str)
    (pattern (~seq #:obj? obj?:id)
      #:with obj?-str (datum->syntax
                       #'here
                       (symbol->immutable-string (syntax-e #'obj?))
                       #'obj?)))
  (define-splicing-syntax-class opt
    #:attributes (type obj? obj?-str vector/c)
    (pattern (~seq (~alt (~optional (~seq #:type type:str))
                         (~optional :obj?)
                         (~optional (~seq #:vector/c vector/c:id)))
                   ...))))

(define-syntax-parser define-vector-ref
  [(_:id vector:id opt:opt)
   #:do [(define (vector-id fmt-str [ctx #'vector])
           (format-id ctx fmt-str #'vector #:source #'vector))]
   #:with name (vector-id "~a-ref")
   #:with vector-expr (vector-id "~a-expr" #'here)
   #:with vector-expr.c (vector-id "~a-expr.c" #'here)
   #:with vector/c (or (datum opt.vector/c) (vector-id "~a?"))
   #:with vector-ref (vector-id "unsafe-~a-ref")
   #:with vector-set! (vector-id "unsafe-~a-set!")
   #:with vector-length (vector-id "unsafe-~a-length")
   #:with type (or (datum opt.type)
                   (datum->syntax #'here
                                  (symbol->immutable-string
                                   (syntax-e #'vector))
                                  #'vector))
   #:with check-vector+pos (vector-id "check-~a+pos" #'here)
   #:with check-vector+pos-def
   (syntax/loc this-syntax
     (define (check-vector+pos vector pos)
       (let ([len (vector-length vector)])
         (unless (and (fixnum? pos) (unsafe-fx< pos len))
           (raise-range-error 'name type ""
                              pos vector 0 (unsafe-fx- len 1))))))
   #:attr check-obj (and (datum opt.obj?) #'check-obj)
   #:attr check-obj-def
   (and (datum check-obj)
        (syntax/loc this-syntax
          (define (check-obj obj)
            (unless (opt.obj? obj)
              (raise-argument-error 'name opt.obj?-str obj)))))
   #:with expander-def
   (syntax/loc this-syntax
     (define-set!-parser name
       [(_:id vector-expr pos-expr)
        #:declare vector-expr (maybe-expr/c #'vector/c)
        #:declare pos-expr (maybe-expr/c #'exact-nonnegative-integer?)
        #:with mutable-vector (syntax/loc #'vector-expr vector)
        #:declare mutable-vector (maybe-expr/c #'mutable/c)
        (set!-pack #'(lambda () (vector-ref vector pos))
                   #'(lambda (val)
                       (let ([mutable-vector mutable-vector.c])
                         (~? (unless (variable-reference-from-unsafe?
                                      (#%variable-reference))
                               (check-obj val)))
                         (vector-set! mutable-vector pos val)))
                   #'(define vector vector-expr.c)
                   #'(define pos pos-expr.c)
                   #'(unless (variable-reference-from-unsafe?
                              (#%variable-reference))
                       (check-vector+pos vector pos)))]))
   #'(begin check-vector+pos-def (~? check-obj-def) expander-def)])

(define-vector-ref bytes #:type "byte string" #:obj? byte?)

(define-vector-ref string #:obj? char?)

(define-vector-ref vector)

(define vector*/c (and/c vector? (not/c impersonator?)))

(define-vector-ref vector* #:type "vector" #:vector/c vector*/c)

(define-for-syntax (make-unbox unbox set-box! box/c)
  (syntax-parser
    [(_:id box-expr)
     #:declare box-expr (maybe-expr/c box/c)
     #:with mutable-box (syntax/loc #'box-expr box)
     #:declare mutable-box (maybe-expr/c #'mutable/c)
     #:with unbox unbox
     #:with set-box! set-box!
     (set!-pack #'(lambda () (unbox box))
                #'(lambda (val) (set-box! mutable-box.c val))
                #'(define box box-expr.c))]))

(define-set!-expander unbox
  (make-unbox #'unsafe-unbox #'unsafe-set-box! #'box?))

(define box*/c (and/c box? (not/c impersonator?)))

(define-set!-expander unbox*
  (make-unbox #'unsafe-unbox* #'unsafe-set-box*! #'box*/c))

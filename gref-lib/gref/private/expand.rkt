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

(require racket/contract/base)
(provide gref
         (rename-out [gref generalized-reference])
         (contract-out
          [get-set!-expansion
           (->* (syntax?)
                ((or/c #f exact-nonnegative-integer?))
                (values (listof syntax?) (listof identifier?)
                        syntax? syntax?))]))

(require gref/private/class
         gref/private/generic
         racket/match
         syntax/datum
         syntax/parse
         (for-syntax racket/base)
         (for-template gref/private/literal
                       racket/base
                       (prefix-in srfi- srfi/17)))

(define gref-desc-table (make-hasheqv))

(define (make-gref-desc num)
  (define (make-desc)
    (define base "generalized reference")
    (match num
      [#f (string-append-immutable "any " base)]
      [(? exact-nonnegative-integer? (app number->string num-str))
       (string-append-immutable num-str "-valued " base)]
      [_ (raise-argument-error 'gref
                               "(or/c #f exact-nonnegative-integer?)"
                               num)]))
  (hash-ref! gref-desc-table num make-desc))

(define (apply-expr-trans trans . args)
  (apply syntax-local-apply-transformer trans #f 'expression #f args))

(define-syntax-class :set!-form
  #:description ":set! form"
  #:commit
  #:attributes ([binding 1] [store 1] reader writer)
  #:literals (:set!)
  (pattern (:set! (binding:binding ...) (store:id ...)
                  reader:expr writer:expr)))

(define-syntax-class (gref [num 1])
  #:description (make-gref-desc num)
  #:commit
  #:attributes ([binding 1] [store 1] reader writer)
  #:literals (%values)
  (pattern (~fail #:when (syntax-transforming?))
    #:cut
    #:post (~fail "\
not within the dynamic extent of a macro transformation")
    #:with ::set!-form (assert-unreachable))
  (pattern id:id
    #:cut
    #:when (or (not num) (= num 1))
    #:with ::set!-form
    (apply-expr-trans
     (lambda ()
       (syntax/loc this-syntax (:set! () (obj) id (set! id obj))))))
  (pattern (%values ref:gref ...)
    #:cut
    #:when (or (not num) (= (length (datum (ref ...))) num))
    #:with ::set!-form
    (apply-expr-trans
     (lambda ()
       (syntax/loc this-syntax
         (:set! (ref.binding ... ...) (ref.store ... ...)
                (values ref.reader ...)
                (let () ref.writer ... (void)))))))
  (pattern (acc . _)
    #:declare acc (static set!-expander? #f)
    #:cut
    #:with ::set!-form
    (apply-expr-trans set!-expand (datum acc.value) this-syntax)
    #:when (or (not num) (= (length (datum (store ...))) num)))
  (pattern (getter-expr:expr arg)
    #:declare arg (args 0)
    #:cut
    #:when (or (not num) (= num 1))
    #:with ::set!-form
    (apply-expr-trans
     (lambda ()
       (syntax/loc this-syntax
         (:set! ([(getter) getter-expr]
                 [(arg.val) arg.expr] ...)
                (obj)
                (getter (~@ (~? arg.kw) arg.val) ...)
                ((srfi-setter getter)
                 (~@ (~? arg.kw) arg.val) ... obj)))))))

(define (get-set!-expansion ref-stx [num 1])
  (unless (syntax-transforming?)
    (raise-arguments-error 'get-set!-expansion
                           "not currently expanding"))
  (syntax-parse ref-stx
    [ref
     #:declare ref (gref num)
     (values (datum (ref.binding ...)) (datum (ref.store ...))
             (datum ref.reader) (datum ref.writer))]))

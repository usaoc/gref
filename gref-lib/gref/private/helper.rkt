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

(provide make-ids make-vals make-namer args maybe-expr/c)

(require racket/keyword
         syntax/datum
         syntax/parse
         (for-template racket/base))

(define (indexed-id pfix idx #:source src)
  (define indexed (string-append-immutable pfix (number->string idx)))
  (datum->syntax #'here (string->symbol indexed) src))

(define (make-ids pfix end)
  (for/list ([idx (in-range end)])
    (indexed-id pfix idx #:source #'here)))

(define vals-table (make-ephemeron-hasheqv))

(define (make-vals end)
  (hash-ref! vals-table end (lambda () (make-ids "val" end))))

(define (make-namer name-id)
  (define name (syntax-e name-id))
  (lambda (stx) (syntax-property stx 'inferred-name name)))

(define args-table (make-ephemeron-hash))

(define (make-args kw+idxs)
  (for/fold ([args '()])
            ([kw/idx (in-list kw+idxs)])
    (define arg
      (cond
        [(keyword? kw/idx)
         (define sym
           (string->symbol (keyword->immutable-string kw/idx)))
         (datum->syntax #'here sym #'here)]
        [else (indexed-id "arg" kw/idx #:source #'here)]))
    (cons arg args)))

(define-syntax-class args
  #:description "#%app arguments"
  #:commit
  #:attributes ([kw 1] [expr 1] [val 1])
  (pattern ((~or* (~describe "keyword argument"
                    (~seq kw:keyword ~! expr:expr))
                  expr)
            ...)
    #:do [(define (find-dup/make-vals)
            (define kw+idxs
              (for/fold ([kw+idxs '()]
                         [idx 0]
                         #:result kw+idxs)
                        ([kw (in-list (datum (kw ...)))])
                (define-values (kw/idx next-idx)
                  (if kw
                      (values (syntax-e kw) idx)
                      (values idx (add1 idx))))
                (values (cons kw/idx kw+idxs) next-idx)))
            (define (find/make)
              (define (find)
                (for/fold ([seen-kws #hasheq()]
                           [dup-kw #f]
                           #:result dup-kw)
                          ([kw (in-list (datum (kw ...)))]
                           #:when kw)
                  #:break dup-kw
                  (define kw-datum (syntax-e kw))
                  (if (hash-ref seen-kws kw-datum #f)
                      (values seen-kws kw)
                      (values (hash-set seen-kws kw-datum #t) #f))))
              (or (find) (make-args kw+idxs)))
            (hash-ref! args-table kw+idxs find/make))
          (define dup/vals (find-dup/make-vals))]
    #:fail-when (and (syntax? dup/vals) dup/vals) "duplicate keyword"
    #:with (val ...) dup/vals))

(define-syntax-class (maybe-expr/c contract-expr)
  #:commit
  #:attributes (c)
  (pattern expr
    #:declare expr (expr/c contract-expr)
    #:with c (syntax/loc this-syntax
               (if (variable-reference-from-unsafe?
                    (#%variable-reference))
                   expr
                   expr.c))))

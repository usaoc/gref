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

(provide format-ids args maybe-expr/c)

(require racket/keyword
         racket/syntax
         syntax/datum
         syntax/parse
         (for-template racket/base))

(define fmt-table (make-hash))

(define (format-ids fmt end)
  (define (make-ids)
    (for/list ([idx (in-range end)])
      (format-id #'here fmt idx #:source #'here)))
  (hash-ref! (hash-ref! fmt-table fmt make-hasheqv) end make-ids))

(define-syntax-class (args idx)
  #:description "#%app arguments"
  #:commit
  #:attributes ([kw 1] [expr 1] [val 1])
  (pattern ((~or* (~describe "keyword argument"
                    (~seq kw:keyword ~! expr:expr))
                  expr)
            ...)
    #:do [(define (find-dup)
            (define table (make-hasheq))
            (for/first ([kw (in-list (datum (kw ...)))]
                        #:when kw
                        #:do [(define kw-datum (syntax-e kw))]
                        #:when (cond
                                 [(hash-ref table kw-datum #f)]
                                 [else
                                  (hash-set! table kw-datum #t)
                                  #f]))
              kw))]
    #:fail-when (find-dup) "duplicate keyword"
    #:do [(define vals
            (for/fold ([vals '()]
                       [idx idx]
                       #:result (reverse vals))
                      ([expr (in-list (datum (expr ...)))]
                       [kw (in-list (datum (kw ...)))])
              (define (next val idx) (values (cons val vals) idx))
              (cond
                [kw
                 (define (keyword->symbol kw)
                   (string->symbol (keyword->immutable-string kw)))
                 (define sym (keyword->symbol (syntax-e kw)))
                 (next (datum->syntax #'here sym expr) idx)]
                [else
                 (define id
                   (format-id #'here "arg~a" idx #:source expr))
                 (next id (add1 idx))])))]
    #:with (val ...) vals))

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

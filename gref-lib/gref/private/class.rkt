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

(provide args binding maybe-expr/c)

(require racket/keyword
         racket/match
         racket/syntax
         syntax/datum
         syntax/parse
         (for-template racket/base))

(define-syntax-class (args [idx #f])
  #:description "#%app arguments"
  #:commit
  #:attributes ([kw 1] [expr 1] [val 1])
  (pattern ((~or* (~describe "keyword argument"
                    (~seq kw:keyword ~!
                          (~bind [kw-datum (syntax-e #'kw)])
                          expr:expr))
                  expr)
            ...)
    #:fail-when
    (let ()
      (define table (make-hasheq))
      (for/first ([kw (in-list (datum (kw ...)))]
                  [kw-datum (in-list (datum (kw-datum ...)))]
                  #:when kw
                  #:unless (and (not (hash-has-key? table kw-datum))
                                (hash-set! table kw-datum #t)))
        kw))
    "duplicate keyword"
    #:attr [val 1]
    (and idx
         (for/fold ([vals '()]
                    [idx idx]
                    #:result (reverse vals))
                   ([expr (in-list (datum (expr ...)))]
                    [kw (in-list (datum (kw-datum ...)))])
           (define (next val idx) (values (cons val vals) idx))
           (match kw
             [#f (next (format-id #'here "arg~a" idx #:source expr)
                       (add1 idx))]
             [_ (next (datum->syntax #'here
                                     (string->symbol
                                      (keyword->immutable-string kw))
                                     expr)
                      idx)])))))

(define-syntax-class binding
  #:description "let-values binding pair"
  #:commit
  #:attributes ()
  (pattern [(_:id ...) _:expr]))

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

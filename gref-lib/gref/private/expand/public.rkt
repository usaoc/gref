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

(provide set!-pack make-set!-functional gref get-set!-expansion)

(require gref/private/helper
         gref/private/property
         syntax/datum
         syntax/parse
         (rename-in gref/private/expand/private
           [gref unsafe-gref])
         (for-template racket/base))

(define (make-set!-functional getter-stx setter-stx #:arity [num 1])
  (define/syntax-parse getter getter-stx)
  (define/syntax-parse setter setter-stx)
  (define/syntax-parse (val ...) (make-vals num))
  (make-set!-expander
   (syntax-parser
     [(who:id . arg)
      #:declare arg (args 0)
      (define namer (make-namer #'who))
      (apply set!-pack
             (namer #'(lambda () (getter arg.val ...)))
             (namer #'(lambda (val ...) (setter arg.val ... val ...)))
             (for/list ([val-id (in-list (datum (arg.val ...)))]
                        [expr-stx (in-list (datum (arg.expr ...)))])
               (define/syntax-parse val val-id)
               (define/syntax-parse expr expr-stx)
               #'(define val expr))
             #:arity num)])))

(define-syntax-class (gref #:arity [num 1]
                           #:desc [desc (make-gref-desc num)])
  #:description desc
  #:commit
  #:attributes (arity getter setter [preamble 1])
  (pattern (~or* (~and _
                       (~fail #:when (syntax-transforming?)) ~!
                       (~fail "\
not within the dynamic extent of a macro transformation"))
                 (~var || (unsafe-gref #:arity num #:desc desc
                                       #:show #f)))))

(define (get-set!-expansion ref-stx #:arity [num 1])
  (syntax-parse ref-stx
    #:context 'get-set!-expansion
    [(~var || (unsafe-gref #:arity num))
     (values (datum arity)
             (datum getter) (datum setter) (datum (preamble ...)))]))

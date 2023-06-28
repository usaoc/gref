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

(provide get-set!-expansion gref
         %gref %gref1s %grefns)

(require gref/private/class
         gref/private/property
         syntax/datum
         syntax/parse
         (for-syntax racket/base)
         (for-template gref/private/literal
                       racket/base))

(define gref-desc-table (make-hasheqv))

(define (make-gref-desc num)
  (define (make-desc)
    (define base "generalized reference")
    (if num
        (string-append-immutable (number->string num) "-valued " base)
        (string-append-immutable "any " base)))
  (hash-ref! gref-desc-table num make-desc))

(define (check-num num given) (or (not num) (= given num)))

(define (make-mismatch desc given)
  (string-append-immutable
   "number of values mismatch
  expected: " desc "\n  given: " (make-gref-desc given)))

(define (make-illegal val)
  (string-append-immutable
   "not bound to a set!-expander? value
  value at phase " (number->string (add1 (syntax-local-phase-level)))
   ": " ((error-value->string-handler) val (error-print-width))))

(define unbound #s(unbound))

(define (get-unbound) unbound)

(define (unbound? val) (eq? val unbound))

(define-syntax-class id-trans
  #:description "identifier with transformer binding"
  #:commit
  #:attributes (val)
  (pattern _:id
    #:do [(define val (syntax-local-value this-syntax get-unbound))]
    #:when (not (unbound? val))
    #:attr val val))

(define-syntax-class :set!-form
  #:description ":set! form"
  #:commit
  #:attributes ([binding 1] [store 1] reader writer)
  #:literals (:set!)
  (pattern (:set! (~describe "lexical context" (binding:binding ...))
                  (~describe "store variables" (store:id ...))
                  (~describe "reader expression" reader:expr)
                  (~describe "writer expression" writer:expr))))

(define-syntax-class (%gref [num 1] [desc (make-gref-desc num)]
                            #:show [show desc])
  #:description show
  #:commit
  #:attributes ([binding 1] [store 1] reader writer)
  (pattern id:id
    #:cut
    #:fail-unless (check-num num 1) (make-mismatch desc 1)
    #:with (binding ...) '()
    #:with obj ((make-syntax-introducer) #'obj 'add)
    #:with (store ...) (datum (obj))
    #:with reader #'id
    #:with writer #'(set! id obj))
  (pattern (acc:id-trans . _)
    #:cut
    #:do [(define val (datum acc.val))
          (define make-proc (set!-expander-ref val get-unbound))]
    #:fail-when (and (unbound? make-proc) #'acc) (make-illegal val)
    #:do [(define proc (make-proc val))]
    #:with ::set!-form (syntax-local-apply-transformer
                        proc #'acc 'expression #f this-syntax)
    #:do [(define given (length (datum (store ...))))]
    #:fail-unless (check-num num given) (make-mismatch desc given)))

(define-splicing-syntax-class %gref1s
  #:description "1-valued generalized references"
  #:attributes ([binding 1] [store 1] reader writer)
  (pattern (~seq (~do (define desc (make-gref-desc 1)))
                 (~var ref (%gref 1 desc)) ...)
    #:cut
    #:with (binding ...) (datum (ref.binding ... ...))
    #:with (store ...) (datum (ref.store ... ...))
    #:with reader #'(values ref.reader ...)
    #:with writer #'(#%expression (begin ref.writer ... (void)))))

(define-splicing-syntax-class %grefns
  #:description "same-valued generalized references"
  #:attributes ([binding 2] [store 2] [reader 1] [writer 1] reader0)
  (pattern (~seq (~var ref0 (%gref #f))
                 (~do (define num (length (datum (ref0.store ...))))
                      (define desc (make-gref-desc num)))
                 (~var ref (%gref num desc)) ...)
    #:with ((binding ...) ...)
    (datum ((ref0.binding ...) (ref.binding ...) ...))
    #:with ((store ...) ...)
    (datum ((ref0.store ...) (ref.store ...) ...))
    #:with (reader ...) (datum (ref.reader ...))
    #:with (writer ...) (datum (ref0.writer ref.writer ...))
    #:with reader0 (datum ref0.reader)))

(define-syntax-class (gref [num 1] [desc (make-gref-desc num)])
  #:description desc
  #:commit
  #:attributes ([binding 1] [store 1] reader writer)
  (pattern (~or* (~and _
                       (~fail #:when (syntax-transforming?)) ~!
                       (~fail "\
not within the dynamic extent of a macro transformation"))
                 (~var || (%gref num desc #:show #f)))))

(define (get-set!-expansion ref-stx [num 1])
  (syntax-parse ref-stx
    [ref
     #:declare ref (%gref num)
     (values (datum (ref.binding ...)) (datum (ref.store ...))
             (datum ref.reader) (datum ref.writer))]))

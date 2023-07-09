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

(provide get-set!-expansion gref set!-pack
         %gref %gref1s %grefns)

(require gref/private/property
         gref/private/space
         racket/match
         racket/syntax
         syntax/datum
         syntax/parse
         (for-template racket/base))

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
   "arity number mismatch
  expected: " desc "\n  given: " (make-gref-desc given)))

(define (make-illegal val)
  (string-append-immutable
   "not bound to a set!-expander? value
  value at phase " (number->string (add1 (syntax-local-phase-level)))
   ": " ((error-value->string-handler) val (error-print-width))))

(define ((make-track orig-stx id-stx) new-stx)
  (syntax-track-origin new-stx orig-stx
                       (syntax-local-introduce id-stx)))

(define unbound #s(unbound))

(define (get-unbound) unbound)

(define (unbound? val) (eq? val unbound))

(define-syntax-class set!-trans
  #:description "\
identifier with transformer binding (possibly in gref/set! space)"
  #:commit
  #:attributes (id val)
  (pattern _:id
    #:do [(define set!-id (in-set!-space this-syntax))
          (define val (syntax-local-value set!-id get-unbound))]
    #:when (not (unbound? val))
    #:with id set!-id
    #:attr val val))

(define-syntax-class binding
  #:description "let-values binding pair"
  #:commit
  #:attributes ()
  (pattern [(_:id ...) _:expr]))

(struct set!-packed (num getter setter preambles))

(define (set!-pack getter setter #:arity [num 1] #:source [src #f]
                   . preambles)
  (datum->syntax #f (set!-packed num getter setter preambles) src))

(define-syntax-class (set!-packed-form val track+intro)
  #:description "set!-packed form"
  #:commit
  #:attributes (arity getter setter [preamble 1])
  (pattern _
    #:do [(match-define (set!-packed num getter setter preambles)
            val)]
    #:with (~describe "getter procedure" _:expr) getter
    #:with (~describe "setter procedure" _:expr) setter
    #:with (~describe "preamble forms" (_:expr ...)) preambles
    #:attr arity num
    #:with getter (track+intro getter)
    #:with setter (track+intro setter)
    #:with (preamble ...) (map track+intro preambles)))

(define-syntax-class (%gref-cont num desc track+intro)
  #:description #f
  #:commit
  #:attributes (arity getter setter [preamble 1])
  (pattern (~or* (~and _
                       (~do (define val (syntax-e this-syntax)))
                       (~fail #:unless (set!-packed? val)) ~!
                       (~var || (set!-packed-form val track+intro)))
                 (~parse (~var || (%gref #:arity num #:desc desc))
                         (track+intro this-syntax)))))

(define-syntax-class (%gref #:arity [num 1]
                            #:desc [desc (make-gref-desc num)]
                            #:show [show desc])
  #:description show
  #:commit
  #:attributes (arity getter setter [preamble 1])
  (pattern (~or* (ref:set!-trans . _) ref:set!-trans)
    #:cut
    #:do [(define id #'ref.id)
          (define val (datum ref.val))
          (define make-proc (set!-expander-ref val get-unbound))]
    #:fail-when (and (unbound? make-proc) id) (make-illegal val)
    #:do [(define proc (make-proc val))
          (define track (make-track this-syntax id))
          (define intro (make-syntax-introducer))
          (define (track+intro stx) (track (intro stx)))
          (define use-intro (make-syntax-introducer #t))
          (define expanded
            (proc (use-intro (intro this-syntax 'add) 'add)))]
    #:with (~var || (%gref-cont num desc track+intro)) expanded
    #:do [(define given (datum arity))]
    #:fail-unless (check-num num given) (make-mismatch desc given))
  (pattern id:id
    #:cut
    #:fail-unless (check-num num 1) (make-mismatch desc 1)
    #:attr arity 1
    #:do [(define track (make-track this-syntax #'id))]
    #:with getter (track #'(lambda () id))
    #:with setter (track #'(lambda (val) (set! id val)))
    #:with (preamble ...) '()))

(define-syntax-class %gref1s
  #:description "1-valued generalized references"
  #:commit
  #:attributes (arity getter setter [preamble 1])
  (pattern ((~do (define desc (make-gref-desc 1)))
            (~var ref (%gref #:arity 1 #:desc desc)) ...)
    #:cut
    #:do [(define given (length (datum (ref ...))))]
    #:attr arity given
    #:with getter #'(lambda () (values (ref.getter) ...))
    #:with (val ...) (for/list ([idx (in-range given)])
                       (format-id #'here "val~a" idx #:source #'here))
    #:with setter #'(lambda (val ...) (ref.setter val) ... (void))
    #:with (preamble ...) (datum (ref.preamble ... ...))))

(define-syntax-class %grefns
  #:description "same-valued generalized references"
  #:commit
  #:attributes (getter0 [getter 1] [setter 1] [preamble 2])
  (pattern ((~var ref0 (%gref #:arity #f))
            (~do (define num (datum ref0.arity))
                 (define desc (make-gref-desc num)))
            (~var ref (%gref #:arity num #:desc desc)) ...)
    #:with (getter ...) (datum (ref.getter ...))
    #:with getter0 (datum ref0.getter)
    #:with (setter ...) (datum (ref0.setter ref.setter ...))
    #:with ((preamble ...) ...)
    (datum ((ref0.preamble ...) (ref.preamble ...) ...))))

(define-syntax-class (gref #:arity [num 1]
                           #:desc [desc (make-gref-desc num)])
  #:description desc
  #:commit
  #:attributes (arity getter setter [preamble 1])
  (pattern (~or* (~and _
                       (~fail #:when (syntax-transforming?)) ~!
                       (~fail "\
not within the dynamic extent of a macro transformation"))
                 (~var || (%gref #:arity num #:desc desc
                                 #:show #f)))))

(define (get-set!-expansion ref-stx #:arity [num 1])
  (syntax-parse ref-stx
    #:context 'get-set!-expansion
    [ref
     #:declare ref (%gref #:arity num)
     (values (datum ref.arity)
             #'ref.getter #'ref.setter (datum (ref.preamble ...)))]))

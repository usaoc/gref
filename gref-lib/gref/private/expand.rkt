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
         syntax/datum
         syntax/parse
         (for-syntax racket/base)
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
   "number of values mismatch
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

(struct set!-packed (bindings stores reader writer))

(define (set!-pack bindings stores reader writer #:source [src #f])
  (datum->syntax #f (set!-packed bindings stores reader writer) src))

(define-syntax-class (set!-packed-form val track intro)
  #:description "set!-packed form"
  #:commit
  #:attributes ([binding 1] [store 1] reader writer)
  (pattern _
    #:do [(match-define (set!-packed bindings stores reader writer)
            val)]
    #:with (~describe "lexical context" (_:binding ...)) bindings
    #:with (~describe "store variables" (_:id ...)) stores
    #:with (~describe "reader expression" _:expr) reader
    #:with (~describe "writer expression" _:expr) writer
    #:with (binding ...) (map track (syntax->list (intro bindings)))
    #:with (store ...) (map track (syntax->list (intro stores)))
    #:with reader (track (intro reader))
    #:with writer (track (intro writer))))

(define-syntax-class (%gref-cont num desc track intro)
  #:description #f
  #:commit
  #:attributes ([binding 1] [store 1] reader writer)
  (pattern (~or* (~and _
                       (~do (define val (syntax-e this-syntax)))
                       (~fail #:unless (set!-packed? val)) ~!
                       (~var || (set!-packed-form val track intro)))
                 (~parse (~var || (%gref num desc))
                         (track (intro this-syntax))))))

(define-syntax-class (%gref [num 1] [desc (make-gref-desc num)]
                            #:show [show desc])
  #:description show
  #:commit
  #:attributes ([binding 1] [store 1] reader writer)
  (pattern (~or* (ref:set!-trans . _) ref:set!-trans)
    #:cut
    #:do [(define id #'ref.id)
          (define val (datum ref.val))
          (define make-proc (set!-expander-ref val get-unbound))]
    #:fail-when (and (unbound? make-proc) id) (make-illegal val)
    #:do [(define proc (make-proc val))
          (define track (make-track this-syntax id))
          (define intro (make-syntax-introducer))
          (define use-intro (make-syntax-introducer #t))
          (define expanded
            (proc (use-intro (intro this-syntax 'add) 'add)))]
    #:with (~var || (%gref-cont num desc track intro)) expanded
    #:do [(define given (length (datum (store ...))))]
    #:fail-unless (check-num num given) (make-mismatch desc given))
  (pattern id:id
    #:cut
    #:fail-unless (check-num num 1) (make-mismatch desc 1)
    #:with (binding ...) '()
    #:do [(define track (make-track this-syntax #'id))]
    #:with obj ((make-syntax-introducer) #'obj 'add)
    #:with (store ...) (list (track #'obj))
    #:with reader (track #'id)
    #:with writer (track #'(set! id obj))))

(define-syntax-class %gref1s
  #:description "1-valued generalized references"
  #:commit
  #:attributes ([binding 1] [store 1] reader writer)
  (pattern ((~do (define desc (make-gref-desc 1)))
            (~var ref (%gref 1 desc)) ...)
    #:cut
    #:with (binding ...) (datum (ref.binding ... ...))
    #:with (store ...) (datum (ref.store ... ...))
    #:with reader #'(values ref.reader ...)
    #:with writer #'(#%expression (begin ref.writer ... (void)))))

(define-syntax-class %grefns
  #:description "same-valued generalized references"
  #:commit
  #:attributes ([binding 2] [store 2] [reader 1] [writer 1] reader0)
  (pattern ((~var ref0 (%gref #f))
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
    #:context 'get-set!-expansion
    [ref
     #:declare ref (%gref num)
     (values (datum (ref.binding ...)) (datum (ref.store ...))
             (datum ref.reader) (datum ref.writer))]))

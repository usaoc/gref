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

(define-syntax-class :set!-form
  #:description ":set! form"
  #:commit
  #:attributes ([binding 1] [store 1] reader writer)
  #:literals (:set!)
  (pattern (:set! (binding:binding ...) (store:id ...)
                  reader:expr writer:expr)))

(define-syntax-class (%gref [num 1] [desc (make-gref-desc num)]
                            #:show [show desc])
  #:description show
  #:commit
  #:attributes ([binding 1] [store 1] reader writer)
  #:literals (%values)
  (pattern id:id
    #:cut
    #:when (or (not num) (= num 1))
    #:with (binding ...) '()
    #:with obj ((make-syntax-introducer) #'obj 'add)
    #:with (store ...) (datum (obj))
    #:with reader #'id
    #:with writer #'(set! id obj))
  (pattern (%values ~! (~var || (%gref1s num))))
  (pattern (acc . _)
    #:declare acc (static set!-expander? #f)
    #:cut
    #:do [(define val (datum acc.value))
          (define proc ((set!-expander-ref val) val))]
    #:with ::set!-form (syntax-local-apply-transformer
                        proc #'acc 'expression #f this-syntax)
    #:when (or (not num) (= (length (datum (store ...))) num))))

(define-splicing-syntax-class (%gref1s [num #f])
  #:description "1-valued generalized references"
  #:attributes ([binding 1] [store 1] reader writer)
  (pattern (~seq (~do (define desc (make-gref-desc 1)))
                 (~var ref (%gref 1 desc)) ...)
    #:cut
    #:when (or (not num) (= (length (datum (ref ...))) num))
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

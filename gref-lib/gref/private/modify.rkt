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

(provide set! set!-values pset! pset!-values shift! rotate!
         call! call2! inc! dec! push! mpush! pop! mpop!)

(require racket/unsafe/ops
         syntax/parse/define
         (for-syntax gref/private/expand/private
                     gref/private/helper
                     racket/base
                     syntax/datum
                     syntax/parse
                     syntax/parse/experimental/template
                     syntax/transformer))

(define-syntax-parser define-modify-syntax
  [(_:id name:id proc:expr)
   (syntax/loc this-syntax
     (define-syntax name (make-expression-transformer proc)))])

(define-syntax-parser define-modify-parser
  [(_:id name:id . tail)
   (syntax/loc this-syntax
     (define-modify-syntax name (syntax-parser . tail)))])

(begin-for-syntax
  (define-template-metafunction bind+calls
    (syntax-parser
      [(_ vals (proc _)) (syntax/loc this-syntax (proc vals))]
      [(_ vals . (~or* (~and ()
                             (~parse (val ...) '())
                             (~parse call0 #'(void)))
                       ((~and (_ val ...) call0))
                       ((~and (_ val) call) ...)))
       (syntax/loc this-syntax
         (let-values ([(val ...) vals]) (~? call0 (~@ call ...))))])))

(begin-for-syntax
  (define-splicing-syntax-class set!-pair
    #:description "set! assignment pair"
    #:attributes ([val 1] vals setter [preamble 1])
    (pattern (~seq (~var || (gref #:arity #f)) vals:expr)
      #:with (val ...) (make-vals (datum arity)))))

(define-modify-parser set!
  [(_:id pair:set!-pair ...+)
   (syntax/loc this-syntax
     (begin
       (let ()
         pair.preamble ...
         (bind+calls pair.vals (pair.setter pair.val ...)))
       ...
       (void)))])

(begin-for-syntax
  (define-splicing-syntax-class set!-values-pair
    #:description "set!-values assignment pair"
    #:attributes ([val 1] vals [setter 1] [preamble 2])
    (pattern (~seq (~and :grefs ref) vals:expr)
      #:do [(define arity (length (syntax->list #'ref)))]
      #:with (val ...) (make-vals arity))))

(define-modify-parser set!-values
  [(_:id pair:set!-values-pair ...+)
   (syntax/loc this-syntax
     (begin
       (let ()
         pair.preamble ... ...
         (bind+calls pair.vals (pair.setter pair.val) ...))
       ...
       (void)))])

(begin-for-syntax
  (define-template-metafunction pset!-fold
    (syntax-parser
      [(_ ((preamble0 ...) . preambless)
          (vals0 . valss)
          ((call0 ...) . (~or* () callss)))
       (syntax/loc this-syntax
         (let ()
           preamble0 ...
           (bind+calls (~? (begin0 vals0
                             (pset!-fold preambless valss callss))
                           vals0)
                       call0 ...)))])))

(define-modify-parser pset!
  [(_:id pair:set!-pair ...+)
   (syntax/loc this-syntax
     (begin
       (pset!-fold ((pair.preamble ...) ...)
                   (pair.vals ...)
                   (((pair.setter pair.val ...)) ...))
       (void)))])

(define-modify-parser pset!-values
  [(_:id pair:set!-values-pair ...+)
   (syntax/loc this-syntax
     (begin
       (pset!-fold ((pair.preamble ... ...) ...)
                   (pair.vals ...)
                   (((pair.setter pair.val) ...) ...))
       (void)))])

(begin-for-syntax
  (define-template-metafunction shift!-fold
    (syntax-parser
      [(_ ((preamble0 ...) . preambless)
          vals0 (vals1 . valss)
          (call0 . (~or* () calls)))
       (syntax/loc this-syntax
         (let ()
           preamble0 ...
           (begin0 vals0
             (bind+calls (~? (shift!-fold preambless
                                          vals1 valss
                                          calls)
                             vals1)
                         call0))))])))

(define-modify-parser shift!
  [(_:id maybe-ref0 maybe-ref ... maybe-vals)
   #:cut
   #:with ref0 #'maybe-ref0
   #:declare ref0 (gref #:arity #f)
   #:do [(define arity (datum ref0.arity))]
   #:with ref (datum (maybe-ref ...))
   #:declare ref (grefs #:arity arity)
   #:with vals:expr #'maybe-vals
   #:with (val ...) (make-vals arity)
   (syntax/loc this-syntax
     (shift!-fold ((ref0.preamble ...) (ref.preamble ...) ...)
                  (ref0.getter) ((ref.getter) ... vals)
                  ((ref0.setter val ...) (ref.setter val ...) ...)))])

(define-modify-parser rotate!
  [(_:id maybe-ref0 maybe-ref ...+)
   #:cut
   #:with ref0 #'maybe-ref0
   #:declare ref0 (gref #:arity #f)
   #:do [(define arity (datum ref0.arity))]
   #:with ref (datum (maybe-ref ...))
   #:declare ref (grefs #:arity arity)
   #:with (val ...) (make-vals arity)
   (syntax/loc this-syntax
     (shift!-fold ((ref0.preamble ...) (ref.preamble ...) ...)
                  (void) ((ref.getter) ... (ref0.getter))
                  ((ref0.setter val ...) (ref.setter val ...) ...)))])

(begin-for-syntax
  (define-syntax-class rest
    #:description "rest argument"
    #:commit
    #:attributes (app expr val)
    (pattern (~or* (~and () (~parse app #'#%app))
                   (~and expr:expr
                         (~parse app #'apply)
                         (~parse val #'rest-arg))))))

(define-syntax-parser define-call!
  [(_:id name:id arity:exact-nonnegative-integer)
   #:do [(define arity-num (syntax-e #'arity))]
   #:with (pre-arg ...) (make-ids "pre-arg" arity-num)
   #:with (pre-arg-expr ...) (make-ids "pre-arg-expr" arity-num)
   (syntax/loc this-syntax
     (define-modify-parser name
       [(_:id proc-expr:expr (~var pre-arg-expr expr) ... ref:gref
              maybe-arg (... ...) . maybe-rest)
        #:cut
        #:with arg:args (syntax/loc this-syntax (maybe-arg (... ...)))
        #:with rest:rest #'maybe-rest
        (syntax/loc this-syntax
          (begin
            (let ()
              (define proc proc-expr)
              (define pre-arg pre-arg-expr) ...
              ref.preamble (... ...)
              (define arg.val arg.expr) (... ...)
              (... (~? (define rest.val rest.expr)))
              (ref.setter
               (rest.app proc pre-arg ... (ref.getter)
                         (... (~@ (~? arg.kw) arg.val)) (... ...)
                         (... (~? rest.val)))))
            (void)))]))])

(define-call! call! 0)

(define-call! call2! 1)

(define-for-syntax (make-inc! +-id)
  (define/syntax-parse + +-id)
  (syntax-parser
    [(_:id ref:gref (~optional delta-expr))
     #:declare delta-expr (maybe-expr/c #'number?)
     #:with val (syntax/loc #'ref (ref.getter))
     #:declare val (maybe-expr/c #'number?)
     (syntax/loc this-syntax
       (begin
         (let ()
           ref.preamble ...
           (define delta (~? delta-expr.c 1))
           (ref.setter (+ val.c delta)))
         (void)))]))

(define-modify-syntax inc! (make-inc! #'+))

(define-modify-syntax dec! (make-inc! #'-))

(define-for-syntax (make-push! cons-id)
  (define/syntax-parse cons cons-id)
  (syntax-parser
    [(_:id val-expr:expr ref:gref)
     (syntax/loc this-syntax
       (begin
         (let ()
           (define val val-expr)
           ref.preamble ...
           (ref.setter (cons val (ref.getter))))
         (void)))]))

(define-modify-syntax push! (make-push! #'cons))

(define-modify-syntax mpush! (make-push! #'mcons))

(define-for-syntax (make-pop! pair?-id car-id cdr-id)
  (define/syntax-parse car car-id)
  (define/syntax-parse cdr cdr-id)
  (syntax-parser
    [(_:id ref:gref)
     #:with val (syntax/loc #'ref (ref.getter))
     #:declare val (maybe-expr/c pair?-id)
     (syntax/loc this-syntax
       (let ()
         ref.preamble ...
         (define pair val.c)
         (begin0 (car pair) (ref.setter (cdr pair)))))]))

(define-modify-syntax pop!
  (make-pop! #'pair? #'unsafe-car #'unsafe-cdr))

(define-modify-syntax mpop!
  (make-pop! #'mpair? #'unsafe-mcar #'unsafe-mcdr))

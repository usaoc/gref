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

(require (for-syntax racket/base
                     racket/provide-transform
                     syntax/datum
                     syntax/parse))
(define-for-syntax modify-intro (make-syntax-introducer #t))
(define-for-syntax (in-modify-space id) (modify-intro id 'add))
(define-for-syntax (expand-modify-out stx modes)
  (syntax-parse stx
    [(_:id name:id ...)
     #:with (out-name ...) (map in-modify-space (datum (name ...)))
     (expand-export
      (syntax/loc this-syntax (combine-out out-name ...))
      modes)]))
(define-syntax modify-out
  (make-provide-transformer expand-modify-out))
(provide (modify-out
          set! set!-values pset! pset!-values shift! rotate!
          call! call2! inc! dec! push! mpush! pop! mpop!))

(require racket/unsafe/ops
         syntax/parse/define
         (for-syntax gref/private/expand
                     gref/private/helper
                     racket/base
                     syntax/parse
                     syntax/parse/experimental/template
                     syntax/transformer))

(define-syntax-parser define-modify-syntax
  [(_:id name:id proc:expr)
   #:with proc-name (syntax-local-introduce #'name)
   #:with proc-def (syntax/loc this-syntax
                     (define-for-syntax proc-name proc))
   #:with def (syntax/loc this-syntax
                (define-syntax name proc-name))
   #:with out-name (in-modify-space #'name)
   #:with out-def (syntax/loc this-syntax
                    (define-syntax out-name
                      (make-expression-transformer proc-name)))
   #'(begin proc-def def out-def)])

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
    (pattern (~seq (~var || (%gref #:arity #f)) vals:expr)
      #:with (val ...) (format-ids "val~a" (datum arity)))))

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
    (pattern (~seq (~and :%grefs ref) vals:expr)
      #:do [(define arity (length (syntax->list #'ref)))]
      #:with (val ...) (format-ids "val~a" arity))))

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
   #:declare ref0 (%gref #:arity #f)
   #:do [(define arity (datum ref0.arity))]
   #:with ref (datum (maybe-ref ...))
   #:declare ref (%grefs #:arity arity)
   #:with vals:expr #'maybe-vals
   #:with (val ...) (format-ids "val~a" arity)
   (syntax/loc this-syntax
     (shift!-fold ((ref0.preamble ...) (ref.preamble ...) ...)
                  (ref0.getter) ((ref.getter) ... vals)
                  ((ref0.setter val ...) (ref.setter val ...) ...)))])

(define-modify-parser rotate!
  [(_:id maybe-ref0 maybe-ref ...+)
   #:cut
   #:with ref0 #'maybe-ref0
   #:declare ref0 (%gref #:arity #f)
   #:do [(define arity (datum ref0.arity))]
   #:with ref (datum (maybe-ref ...))
   #:declare ref (%grefs #:arity arity)
   #:with (val ...) (format-ids "val~a" arity)
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
   #:with (arg0 ...) (format-ids "arg~a" arity-num)
   #:with (arg0-expr ...) (format-ids "arg~a-expr" arity-num)
   #:with more-idx (datum->syntax #'here (add1 arity-num) #'arity)
   (syntax/loc this-syntax
     (define-modify-parser name
       [(_:id proc-expr:expr arg0-expr ... ref:%gref
              maybe-arg (... ...) . maybe-rest)
        (~@ #:declare arg0-expr expr) ...
        #:cut
        #:with arg (syntax/loc this-syntax (maybe-arg (... ...)))
        #:declare arg (args more-idx)
        #:with rest:rest #'maybe-rest
        (syntax/loc this-syntax
          (begin
            (let ()
              (define proc proc-expr)
              (define arg0 arg0-expr) ...
              ref.preamble (... ...)
              (define arg.val arg.expr) (... ...)
              (... (~? (define rest.val rest.expr)))
              (ref.setter
               (rest.app proc arg0 ... (ref.getter)
                         (... (~@ (~? arg.kw) arg.val)) (... ...)
                         (... (~? rest.val)))))
            (void)))]))])

(define-call! call! 0)

(define-call! call2! 1)

(define-for-syntax (make-inc! inc)
  (syntax-parser
    [(_:id ref:%gref (~optional delta-expr))
     #:declare delta-expr (maybe-expr/c #'number?)
     #:with val (syntax/loc #'ref (ref.getter))
     #:declare val (maybe-expr/c #'number?)
     #:with inc inc
     (syntax/loc this-syntax
       (begin
         (let ()
           ref.preamble ...
           (define delta (~? delta-expr.c 1))
           (ref.setter (inc val.c delta)))
         (void)))]))

(define-modify-syntax inc! (make-inc! #'+))

(define-modify-syntax dec! (make-inc! #'-))

(define-for-syntax (make-push! cons)
  (syntax-parser
    [(_:id val-expr:expr ref:%gref)
     #:with cons cons
     (syntax/loc this-syntax
       (begin
         (let ()
           (define val val-expr)
           ref.preamble ...
           (ref.setter (cons val (ref.getter))))
         (void)))]))

(define-modify-syntax push! (make-push! #'cons))

(define-modify-syntax mpush! (make-push! #'mcons))

(define-for-syntax (make-pop! pair? car cdr)
  (syntax-parser
    [(_:id ref:%gref)
     #:with val (syntax/loc #'ref (ref.getter))
     #:declare val (maybe-expr/c pair?)
     #:with car car
     #:with cdr cdr
     (syntax/loc this-syntax
       (let ()
         ref.preamble ...
         (define pair val.c)
         (begin0 (car pair) (ref.setter (cdr pair)))))]))

(define-modify-syntax pop!
  (make-pop! #'pair? #'unsafe-car #'unsafe-cdr))

(define-modify-syntax mpop!
  (make-pop! #'mpair? #'unsafe-mcar #'unsafe-mcdr))

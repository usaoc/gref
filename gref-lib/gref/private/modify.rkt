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
  (define-splicing-syntax-class set!-pair
    #:description "set! assignment pair"
    #:attributes (vals getter setter [preamble 1])
    (pattern (~seq (~var || (%gref #:arity #f)) vals:expr))))

(define-modify-parser set!
  [(_:id) (syntax/loc this-syntax (void))]
  [(_:id pair:set!-pair ...)
   (syntax/loc this-syntax
     (begin
       (let ()
         pair.preamble ...
         (call-with-values (lambda () (#%expression pair.vals))
           pair.setter))
       ...
       (void)))])

(begin-for-syntax
  (define-splicing-syntax-class set!-values-pair
    #:description "set!-values assignment pair"
    #:attributes (vals getter setter [preamble 1])
    (pattern (~seq (~var || (%gref1s #:tail #f)) vals:expr))))

(define-modify-parser set!-values
  [(_:id) (syntax/loc this-syntax (void))]
  [(_:id pair:set!-values-pair ...)
   (syntax/loc this-syntax
     (begin
       (let ()
         pair.preamble ...
         (call-with-values (lambda () (#%expression pair.vals))
           pair.setter))
       ...
       (void)))])

(define-syntax-parser pset!-fold
  [(_ () () ()) (syntax/loc this-syntax (void))]
  [(_ (setter0 setter ...)
      ((preamble0 ...) preambles ...)
      (vals0 vals ...))
   (syntax/loc this-syntax
     (let ()
       preamble0 ...
       (call-with-values (lambda ()
                           (begin0 vals0
                             (pset!-fold (setter ...)
                                         (preambles ...)
                                         (vals ...))))
         setter0)))])

(define-modify-parser pset!
  [(_:id pair:set!-pair ...)
   (syntax/loc this-syntax
     (begin
       (pset!-fold (pair.setter ...)
                   ((pair.preamble ...) ...)
                   (pair.vals ...))
       (void)))])

(define-modify-parser pset!-values
  [(_:id pair:set!-values-pair ...)
   (syntax/loc this-syntax
     (begin
       (pset!-fold (pair.setter ...)
                   ((pair.preamble ...) ...)
                   (pair.vals ...))
       (void)))])

(define-syntax-parser shift!-fold
  [(_ () () () getter) (syntax/loc this-syntax (getter))]
  [(_ (getter1 getter ...)
      (setter0 setter ...)
      ((preamble0 ...) preambles ...)
      (~optional getter0))
   (syntax/loc this-syntax
     (let ()
       preamble0 ...
       (begin0 (~? (getter0) (void))
         (call-with-values (lambda ()
                             (shift!-fold (getter ...) (setter ...)
                                          (preambles ...) getter1))
           setter0))))])

(define-modify-parser shift!
  [(_:id maybe-ref ... maybe-vals)
   #:cut
   #:with ref:%grefns (syntax/loc this-syntax (maybe-ref ...))
   #:with vals:expr #'maybe-vals
   (syntax/loc this-syntax
     (shift!-fold (ref.getter ... (lambda () (#%expression vals)))
                  (ref.setter0 ref.setter ...)
                  ref.preambless
                  ref.getter0))])

(define-modify-parser rotate!
  [(_:id . ref:%grefns)
   (syntax/loc this-syntax
     (shift!-fold (ref.getter ... ref.getter0)
                  (ref.setter0 ref.setter ...)
                  ref.preambless))])

(begin-for-syntax
  (define-syntax-class rest
    #:description "rest argument"
    #:commit
    #:attributes (app expr val)
    (pattern () #:with app #'#%app #:attr val #f #:attr expr #f)
    (pattern _
      #:cut
      #:with expr:expr this-syntax
      #:with app #'apply
      #:with val #'rest-arg)))

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
     #:declare delta-expr (expr/c #'number?)
     #:with val (syntax/loc #'ref (ref.getter))
     #:declare val (expr/c #'number?)
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

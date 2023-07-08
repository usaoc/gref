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
         (for-syntax gref/private/class
                     gref/private/expand
                     racket/base
                     racket/keyword
                     racket/syntax
                     syntax/datum
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
    #:attributes (val getter setter [preamble 1])
    (pattern (~seq (~var || (%gref #:arity #f)) val:expr))))

(define-modify-parser set!
  [(_:id) (syntax/loc this-syntax (void))]
  [(_:id pair:set!-pair ...)
   (syntax/loc this-syntax
     (begin
       (let ()
         pair.preamble ...
         (call-with-values (lambda () (#%expression pair.val))
           pair.setter))
       ...
       (void)))])

(begin-for-syntax
  (define-splicing-syntax-class set!-values-pair
    #:description "set!-values assignment pair"
    #:attributes (val getter setter [preamble 1])
    (pattern (~seq :%gref1s val:expr))))

(define-modify-parser set!-values
  [(_:id) (syntax/loc this-syntax (void))]
  [(_:id pair:set!-values-pair ...)
   (syntax/loc this-syntax
     (begin
       (let ()
         pair.preamble ...
         (call-with-values (lambda () (#%expression pair.val))
           pair.setter))
       ...))])

(define-syntax-parser pset!-fold
  [(_ () () () ()) (syntax/loc this-syntax (void))]
  [(_ (getter0 getter ...) (setter0 setter ...)
      ((preamble0 ...) preambles ...) (val0 val ...))
   (syntax/loc this-syntax
     (let ()
       preamble0 ...
       (call-with-values (lambda ()
                           (begin0 val0
                             (pset!-fold (getter ...) (setter ...)
                                         (preambles ...) (val ...))))
         setter0)))])

(define-modify-parser pset!
  [(_:id pair:set!-pair ...)
   (syntax/loc this-syntax
     (begin
       (pset!-fold (pair.getter ...) (pair.setter ...)
                   ((pair.preamble ...) ...) (pair.val ...))
       (void)))])

(define-modify-parser pset!-values
  [(_:id pair:set!-values-pair ...)
   (syntax/loc this-syntax
     (pset!-fold (pair.getter ...) (pair.setter ...)
                 ((pair.preamble ...) ...) (pair.val ...)))])

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
  [(_:id maybe-ref ... maybe-val)
   #:cut
   #:with ref:%grefns (syntax/loc this-syntax (maybe-ref ...))
   #:with val:expr #'maybe-val
   (syntax/loc this-syntax
     (shift!-fold (ref.getter ... (lambda () (#%expression val)))
                  (ref.setter ...)
                  ((ref.preamble ...) ...)
                  ref.getter0))])

(define-modify-parser rotate!
  [(_:id . ref:%grefns)
   (syntax/loc this-syntax
     (shift!-fold (ref.getter ... ref.getter0)
                  (ref.setter ...)
                  ((ref.preamble ...) ...)))])

(define-for-syntax (find-duplicate kws)
  (define table (make-hasheq))
  (for/first ([kw (in-list kws)]
              #:when kw
              #:do [(define kw-datum (syntax-e kw))]
              #:unless (and (not (hash-ref table kw-datum #f))
                            (hash-set! table kw-datum #t)))
    kw))

(define-for-syntax (generate-vals exprs kws idx)
  (for/fold ([vals '()]
             [idx idx]
             #:result (reverse vals))
            ([expr (in-list exprs)]
             [kw (in-list kws)])
    (define (next val idx) (values (cons val vals) idx))
    (cond
      [kw
       (define sym
         (string->symbol (keyword->immutable-string (syntax-e kw))))
       (next (datum->syntax #'here sym expr) idx)]
      [else
       (define id (format-id #'here "arg~a" idx #:source expr))
       (next id (add1 idx))])))

(begin-for-syntax
  (define-syntax-class (args idx)
    #:description "#%app arguments"
    #:commit
    #:attributes ([kw 1] [expr 1] [val 1])
    (pattern ((~or* (~describe "keyword argument"
                      (~seq kw:keyword ~! expr:expr))
                    expr)
              ...)
      #:fail-when (find-duplicate (datum (kw ...)))
      "duplicate keyword"
      #:with (val ...)
      (generate-vals (datum (expr ...)) (datum (kw ...)) idx)))
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
   #:do [(define arity-num (syntax-e #'arity))
         (define (format-args fmt)
           (for/list ([idx (in-range arity-num)])
             (format-id #'here fmt idx #:source #'arity)))]
   #:with (arg0 ...) (format-args "arg~a")
   #:with (arg0-expr ...) (format-args "arg~a-expr")
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
    [(_:id ref:%gref (~optional val))
     #:declare val (expr/c #'number?)
     #:with inc inc
     (syntax/loc this-syntax (call! inc ref (~? val.c 1)))]))

(define-modify-syntax inc! (make-inc! #'+))

(define-modify-syntax dec! (make-inc! #'-))

(define-for-syntax (make-push! cons)
  (syntax-parser
    [(_:id val:expr ref:%gref)
     #:with cons cons
     (syntax/loc this-syntax (call2! cons val ref))]))

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

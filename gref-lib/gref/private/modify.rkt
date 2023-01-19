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
                     racket/syntax
                     syntax/datum
                     syntax/parse
                     syntax/transformer))
(begin-for-syntax
  (define-syntax-class (expr-trans out)
    #:commit
    #:attributes (wrapped)
    (pattern name:id
      #:with wrapped (format-id #'here "wrapped-~a" this-syntax
                                #:source this-syntax)
      #:do [(syntax-local-lift-module-end-declaration
             (syntax/loc out
               (define-syntax wrapped
                 (make-expression-transformer
                  (syntax-local-value #'name)))))])))
(define-for-syntax (expand-expr-trans stx modes)
  (syntax-parse stx
    [(_:id name ...)
     #:declare name (expr-trans this-syntax)
     (pre-expand-export
      (syntax/loc this-syntax (rename-out [name.wrapped name] ...))
      modes)]))
(define-syntax expression-transformer-out
  (make-provide-pre-transformer expand-expr-trans))
(provide (expression-transformer-out
          set! set!-values pset! pset!-values shift! rotate!
          call! call2! inc! dec! push! mpush! pop! mpop!))

(require racket/unsafe/ops
         syntax/parse/define
         (for-syntax gref/private/class
                     gref/private/expand
                     racket/base
                     racket/syntax
                     syntax/datum
                     syntax/parse))

(begin-for-syntax
  (define-splicing-syntax-class set!-pair
    #:description "set! assignment pair"
    #:attributes (val [binding 1] [store 1] writer)
    (pattern (~seq (~var || (gref #f)) val:expr))))

(define-syntax-parser set!
  #:track-literals
  [(_:id pair:set!-pair ...)
   (syntax/loc this-syntax
     (let ()
       (let-values (pair.binding ...)
         (let-values ([(pair.store ...) pair.val])
           (#%expression pair.writer)))
       ...
       (void)))])

(begin-for-syntax
  (define-splicing-syntax-class set!-values-pair
    #:description "set!-values assignment pair"
    #:attributes (val [binding 2] [store 2] [writer 1])
    (pattern (~seq () val:expr)
      #:with ((binding ...) ...) '()
      #:with ((store ...) ...) '()
      #:with (writer ...) #'(void))
    (pattern (~seq (:gref ...) val:expr))))

(define-syntax-parser set!-values
  #:track-literals
  [(_:id pair:set!-values-pair ...)
   (syntax/loc this-syntax
     (let ()
       (let-values (pair.binding ... ...)
         (let-values ([(pair.store ... ...) pair.val])
           (#%expression pair.writer) ...))
       ...
       (void)))])

(define-syntax-parser pset!-fold
  [(_:id () () () ()) (syntax/loc this-syntax #f)]
  [(_:id ((binding0:binding ...) (binding:binding ...) ...)
         ((store0:id ...) (store:id ...) ...)
         ((writer0:expr ...) (writer:expr ...) ...)
         (val0:expr val:expr ...))
   (syntax/loc this-syntax
     (let-values (binding0 ...)
       (let-values
           ([(store0 ...)
             (begin0 val0
               (pset!-fold ((binding ...) ...) ((store ...) ...)
                           ((writer ...) ...) (val ...)))])
         (#%expression writer0) ...)))])

(define-syntax-parser pset!
  #:track-literals
  [(_:id pair:set!-pair ...)
   (syntax/loc this-syntax
     (let ()
       (pset!-fold ((pair.binding ...) ...) ((pair.store ...) ...)
                   ((pair.writer) ...) (pair.val ...))
       (void)))])

(define-syntax-parser pset!-values
  #:track-literals
  [(_:id pair:set!-values-pair ...)
   (syntax/loc this-syntax
     (let ()
       (pset!-fold ((pair.binding ... ...) ...)
                   ((pair.store ... ...) ...)
                   ((pair.writer ...) ...)
                   (pair.val ...))
       (void)))])

(define-syntax-parser shift!-fold
  [(_:id () () () () reader:expr) (syntax/loc this-syntax reader)]
  [(_:id ((binding0:binding ...) (binding:binding ...) ...)
         ((store0:id ...) (store:id ...) ...)
         (reader1:expr reader:expr ...)
         (writer0:expr writer:expr ...)
         (~optional reader0:expr))
   (syntax/loc this-syntax
     (let-values (binding0 ...)
       (let-values
           ([(store0 ...)
             (shift!-fold ((binding ...) ...) ((store ...) ...)
                          (reader ...) (writer ...) reader1)])
         (~? (begin0 reader0 writer0) (#%expression writer0)))))])

(begin-for-syntax
  (define-splicing-syntax-class grefs
    #:description "generalized references"
    #:attributes ([binding 2] [store 2] [reader 1] [writer 1] reader0)
    (pattern (~seq (~var ref0 (gref #f))
                   (~do (define number
                          (length (datum (ref0.store ...)))))
                   (~var ref (gref number))
                   ...)
      #:with ((binding ...) ...)
      (datum ((ref0.binding ...) (ref.binding ...) ...))
      #:with ((store ...) ...)
      (datum ((ref0.store ...) (ref.store ...) ...))
      #:with (reader ...) (datum (ref.reader ...))
      #:with (writer ...) (datum (ref0.writer ref.writer ...))
      #:with reader0 (datum ref0.reader))))

(define-syntax-parser shift!
  #:track-literals
  [(_:id ref:grefs val:expr)
   (syntax/loc this-syntax
     (shift!-fold ((ref.binding ...) ...) ((ref.store ...) ...)
                  (ref.reader ... val) (ref.writer ...)
                  ref.reader0))])

(define-syntax-parser rotate!
  #:track-literals
  [(_:id ref:grefs)
   (syntax/loc this-syntax
     (let ()
       (shift!-fold ((ref.binding ...) ...) ((ref.store ...) ...)
                    (ref.reader ... ref.reader0) (ref.writer ...))
       (void)))])

(begin-for-syntax
  (define-syntax-class np-expr
    #:description "non-parenthesized expression"
    #:commit
    #:attributes ()
    (pattern (~or* (_ ...) (_ ...+ . _)) #:cut #:post (~fail))
    (pattern _:expr))
  (define-syntax-class rest
    #:description "rest argument"
    #:commit
    #:attributes (app expr val)
    (pattern () #:with app #'#%app #:attr val #f #:attr expr #f)
    (pattern expr:np-expr #:with app #'apply #:with val #'rest-arg)))

(define-syntax-parser define-call!
  [(_:id name:id arity:exact-nonnegative-integer)
   #:do [(define arity-num (syntax-e #'arity))
         (define (format-args fmt)
           (for/list ([idx (in-range arity-num)])
             (format-id #'here fmt idx #:source #'arity)))]
   #:with (arg0 ...) (format-args "arg~a")
   #:with (arg0-expr ...) (format-args "arg~a-expr")
   #:with more-idx (datum->syntax #'here (add1 arity-num) #'arity)
   #:with ?@ #'(... ~@)
   #:with ?? #'(... ~?)
   #:with (~var :::) #'(... ...)
   (syntax/loc this-syntax
     (define-syntax-parser name
       #:track-literals
       [(_:id proc-expr:expr arg0-expr ... ref:gref arg . rest:rest)
        (~@ #:declare arg0-expr expr) ...
        #:declare arg (args more-idx)
        (syntax/loc this-syntax
          (let ()
            (let-values ([(proc) proc-expr]
                         [(arg0) arg0-expr] ...
                         ref.binding :::
                         [(arg.val) arg.expr] :::
                         (?? [(rest.val) rest.expr]))
              (let-values ([(ref.store :::)
                            (rest.app proc arg0 ... ref.reader
                                      (?@ (?? arg.kw) arg.val) :::
                                      (?? rest.val))])
                (#%expression ref.writer)))
            (void)))]))])

(define-call! call! 0)

(define-call! call2! 1)

(define-for-syntax (make-inc! inc)
  (syntax-parser
    #:track-literals
    [(_:id ref:gref (~optional val))
     #:declare val (expr/c #'number?)
     #:with inc inc
     (syntax/loc this-syntax (call! inc ref (~? val.c 1)))]))

(define-syntax inc! (make-inc! #'+))

(define-syntax dec! (make-inc! #'-))

(define-for-syntax (make-push! cons)
  (syntax-parser
    #:track-literals
    [(_:id val:expr ref:gref)
     #:with cons cons
     (syntax/loc this-syntax (call2! cons val ref))]))

(define-syntax push! (make-push! #'cons))

(define-syntax mpush! (make-push! #'mcons))

(define-for-syntax (make-pop! pair? car cdr)
  (syntax-parser
    #:track-literals
    [(_:id ref:gref)
     #:with reader (datum->syntax #'ref.reader
                                  (syntax-e #'ref.reader)
                                  #'ref)
     #:declare reader (maybe-expr/c pair?)
     #:with car car
     #:with cdr cdr
     (syntax/loc this-syntax
       (let-values (ref.binding ...)
         (let ([pair reader.c])
           (begin0 (car pair)
             (let-values ([(ref.store ...) (cdr pair)])
               (#%expression ref.writer))))))]))

(define-syntax pop! (make-pop! #'pair? #'unsafe-car #'unsafe-cdr))

(define-syntax mpop! (make-pop! #'mpair? #'unsafe-mcar #'unsafe-mcdr))

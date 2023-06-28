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
    #:attributes (val [binding 1] [store 1] writer)
    (pattern (~seq (~var || (%gref #f)) val:expr))))

(define-modify-parser set!
  #:track-literals
  [(_:id) (syntax/loc this-syntax (void))]
  [(_:id pair:set!-pair ...)
   (syntax/loc this-syntax
     (begin
       (let-values (pair.binding ...)
         (let-values ([(pair.store ...) pair.val])
           (#%expression pair.writer)))
       ...
       (void)))])

(begin-for-syntax
  (define-splicing-syntax-class set!-values-pair
    #:description "set!-values assignment pair"
    #:attributes (val [binding 1] [store 1] writer)
    (pattern (~seq :%gref1s val:expr))))

(define-modify-parser set!-values
  #:track-literals
  [(_:id) (syntax/loc this-syntax (void))]
  [(_:id pair:set!-values-pair ...)
   (syntax/loc this-syntax
     (begin
       (let-values (pair.binding ...)
         (let-values ([(pair.store ...) pair.val])
           pair.writer))
       ...))])

(define-syntax-parser pset!-fold
  [(_ () () () ()) (syntax/loc this-syntax (void))]
  [(_ ((binding0 ...) bindings ...) ((store0 ...) stores ...)
      (writer0 writers ...) (val0 val ...))
   (syntax/loc this-syntax
     (let-values (binding0 ...)
       (let-values
           ([(store0 ...)
             (begin0 val0
               (pset!-fold (bindings ...) (stores ...)
                           (writers ...) (val ...)))])
         writer0)))])

(define-modify-parser pset!
  #:track-literals
  [(_:id pair:set!-pair ...)
   (syntax/loc this-syntax
     (begin
       (pset!-fold ((pair.binding ...) ...) ((pair.store ...) ...)
                   ((#%expression pair.writer) ...) (pair.val ...))
       (void)))])

(define-modify-parser pset!-values
  #:track-literals
  [(_:id pair:set!-values-pair ...)
   (syntax/loc this-syntax
     (pset!-fold ((pair.binding ...) ...) ((pair.store ...) ...)
                 (pair.writer ...) (pair.val ...)))])

(define-syntax-parser shift!-fold
  [(_ () () () () reader) (syntax/loc this-syntax reader)]
  [(_ ((binding0 ...) bindings ...) ((store0 ...) stores ...)
      (reader1 reader ...) (writer0 writer ...)
      (~optional reader0))
   (syntax/loc this-syntax
     (let-values (binding0 ...)
       (let-values
           ([(store0 ...)
             (shift!-fold (bindings ...) (stores ...)
                          (reader ...) (writer ...) reader1)])
         (begin0 (~? reader0 (void)) writer0))))])

(define-modify-parser shift!
  #:track-literals
  [(_:id maybe-ref ... maybe-val)
   #:cut
   #:with ref:%grefns (syntax/loc this-syntax (maybe-ref ...))
   #:with val:expr #'maybe-val
   (syntax/loc this-syntax
     (shift!-fold ((ref.binding ...) ...) ((ref.store ...) ...)
                  (ref.reader ... val) (ref.writer ...)
                  ref.reader0))])

(define-modify-parser rotate!
  #:track-literals
  [(_:id . ref:%grefns)
   (syntax/loc this-syntax
     (shift!-fold ((ref.binding ...) ...) ((ref.store ...) ...)
                  (ref.reader ... ref.reader0) (ref.writer ...)))])

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
     (define-modify-parser name
       #:track-literals
       [(_:id proc-expr:expr arg0-expr ... ref:%gref maybe-arg :::
              . maybe-rest)
        (~@ #:declare arg0-expr expr) ...
        #:cut
        #:with (~var arg (args more-idx))
        (syntax/loc this-syntax (maybe-arg :::))
        #:with rest:rest #'maybe-rest
        (syntax/loc this-syntax
          (begin
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
    [(_:id ref:%gref (~optional val))
     #:declare val (expr/c #'number?)
     #:with inc inc
     (syntax/loc this-syntax (call! inc ref (~? val.c 1)))]))

(define-modify-syntax inc! (make-inc! #'+))

(define-modify-syntax dec! (make-inc! #'-))

(define-for-syntax (make-push! cons)
  (syntax-parser
    #:track-literals
    [(_:id val:expr ref:%gref)
     #:with cons cons
     (syntax/loc this-syntax (call2! cons val ref))]))

(define-modify-syntax push! (make-push! #'cons))

(define-modify-syntax mpush! (make-push! #'mcons))

(define-for-syntax (make-pop! pair? car cdr)
  (syntax-parser
    #:track-literals
    [(_:id ref:%gref)
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

(define-modify-syntax pop!
  (make-pop! #'pair? #'unsafe-car #'unsafe-cdr))

(define-modify-syntax mpop!
  (make-pop! #'mpair? #'unsafe-mcar #'unsafe-mcdr))

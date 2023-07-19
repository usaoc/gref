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

(require gref/base
         gref/syntax
         rackunit/spec
         syntax/parse
         syntax/parse/define
         tests/gref/helper
         (for-syntax racket/base
                     syntax/parse)
         (for-template gref/syntax))

(describe "define-set!-syntax"
  (context "given value"
    (it "defines it in gref/set! space"
      (define-syntax trans 'default)
      (define-set!-syntax trans 'gref/set!)
      (define-syntax-parser get-space
        [(_:id)
         #:with space (syntax-local-value #'trans)
         (syntax/loc this-syntax 'space)])
      (check-value 'default (get-space))))
  (context "given procedure"
    (it "defines it in gref/set! space"
      (define-syntax ((trans _arg #:kw-arg _kw-arg)
                      [_opt-arg 'ignored]
                      #:opt-kw-arg [_opt-kw-arg 'ignored])
        'default)
      (define-set!-syntax ((trans _arg #:kw-arg _kw-arg)
                           [_opt-arg 'ignored]
                           #:opt-kw-arg [_opt-kw-arg 'ignored])
        'set!)
      (define-syntax-parser get-space
        [(_:id)
         #:do [(define proc (syntax-local-value #'trans))]
         #:with space ((proc 'ignored #:kw-arg 'ignored)
                       'ignored #:opt-kw-arg 'ignored)
         (syntax/loc this-syntax 'space)])
      (check-value 'default (get-space)))))

(describe "define-set!-syntaxes"
  (context "given values"
    (it "defines them in gref/set! space"
      (define-syntaxes (trans1 trans2)
        (values 'default 'default))
      (define-set!-syntaxes (trans1 trans2)
        (values 'gref/set! 'gref/set!))
      (define-syntax-parser get-spaces
        [(_:id)
         #:with space1 (syntax-local-value #'trans1)
         #:with space2 (syntax-local-value #'trans2)
         (syntax/loc this-syntax (values 'space1 'space2))])
      (check-values ('default 'default) (get-spaces)))))

(describe "set!-pack"
  (context "given syntaxes"
    (it "packs them"
      (check-type syntax?
        (set!-pack #'getter #'setter #'preamble1 #'preamble2))))
  (context "given non syntax"
    (it "raises a contract violation"
      (check-raise exn:fail:contract
        (set!-pack 'non-stx #'setter #'preamble1 #'preamble2))
      (check-raise exn:fail:contract
        (set!-pack #'getter 'non-stx #'preamble1 #'preamble2))
      (check-raise exn:fail:contract
        (set!-pack #'getter #'setter 'non-stx #'preamble2))
      (check-raise exn:fail:contract
        (set!-pack #'getter #'setter #'preamble1 'non-stx))))
  (context "given non arity"
    (it "raises a contract violation"
      (define (check non-arity)
        (check-raise exn:fail:contract
          (set!-pack #'getter #'setter #'preamble1 #'preamble2
                     #:arity non-arity)))
      (check 'non-arity)
      (check "1")))
  (context "given non source location"
    (it "raises a contract violation"
      (define (check non-srcloc)
        (check-raise exn:fail:contract
          (set!-pack #'getter #'setter #'preamble1 #'preamble2
                     #:source non-srcloc)))
      (check 'non-srcloc)
      (check (srcloc 'non-srcloc 1 #f 1 0)))))

(define-for-syntax (make-dummy-expander val-id)
  (syntax-parser
    [(_:id)
     #:with val val-id
     (set!-pack #'(lambda () val)
                #'(lambda (_val) (set! val 'set)))]
    [who:id (syntax/loc this-syntax (who))]))

(define-for-syntax (make-dummy-struct trans)
  (struct dummy ()
    #:property prop:set!-expander (lambda (_dummy) trans))
  (dummy))

(define-for-syntax (broken-expander _args) 'not-stx)

(describe "prop:set!-expander"
  (context "given a set! expander struct"
    (it "produces a set! expander"
      (define val 'init)
      (call! values val-ref)
      (define-set!-syntax val-ref
        (make-dummy-struct (make-dummy-expander #'val)))
      (check-value 'set val)))
  (context "given a non set! expander struct"
    (it "raises a contract violation"
      (define-syntax-parser check
        [(_:id trans:expr)
         (syntax/loc this-syntax
           (check-expand exn:fail:contract
             (define-set!-syntax non-ref (make-dummy-struct trans))
             (call! values non-ref)))])
      (check 'non-trans)
      (check (procedure-reduce-arity (lambda _args #'stx)
                                     (list 0 (arity-at-least 2))))
      (check broken-expander))))

(describe "make-set!-expander"
  (context "given a transformer"
    (it "produces a set! expander"
      (define val 'init)
      (call! values val-ref)
      (define-set!-syntax val-ref
        (make-set!-expander (make-dummy-expander #'val)))
      (check-value 'set val)))
  (context "given a non transformer"
    (it "raises a contract violation"
      (define (check non-trans)
        (check-raise exn:fail:contract
          (make-set!-expander non-trans)))
      (check 'non-trans)
      (check (procedure-reduce-arity (lambda _args #'stx)
                                     (list 0 (arity-at-least 2))))
      (check-expand exn:fail:contract
        (define-set!-syntax non-ref
          (make-set!-expander broken-expander))
        (call! values non-ref)))))

(describe "make-set!-functional"
  (context "given syntaxes"
    (it "produces a set! expander"
      (define val 'init)
      (call! values (val-ref))
      (define-set!-syntax val-ref
        (make-set!-functional #'(lambda () val)
                              #'(lambda (_val) (set! val 'set))))
      (check-value val 'set)))
  (context "given syntaxes and arity"
    (it "produces a set! expander"
      (define val1 'init)
      (define val2 'init)
      (define (vals-ref) (values val1 val2))
      (shift! (vals-ref) (vals-ref))
      (define-set!-syntax vals-ref
        (make-set!-functional
         #'vals-ref
         #'(lambda (_val1 _val2)
             (set!-values (val1 val2) (values 'set 'set)))
         #:arity 2))
      (check-values ('set 'set) (values val1 val2))))
  (context "given non syntax"
    (it "raises a contract violation"
      (check-raise exn:fail:contract
        (make-set!-functional 'not-stx #'setter))
      (check-raise exn:fail:contract
        (make-set!-functional #'getter 'not-stx))))
  (context "given non arity"
    (it "raises a contract violation"
      (define (check non-arity)
        (check-raise exn:fail:contract
          (make-set!-functional #'getter #'setter
                                #:arity non-arity)))
      (check 'non-arity)
      (check "1"))))

(define-set!-syntax dummy-ref
  (make-set!-expander
   (syntax-parser
     [_ (set!-pack #'getter #'setter #'preamble1 #'preamble2
                   #:arity 0)])))

(describe "gref"
  (context "given an arity"
    (it "matches a reference with expected arity"
      (define-syntax-parser get-ref
        [(_:id ref)
         #:declare ref (gref #:arity 0)
         (syntax/loc this-syntax
           (values 'ref.getter 'ref.setter '(ref.preamble ...)))])
      (check-values ('getter 'setter '(preamble1 preamble2))
        (get-ref dummy-ref)))
    (it "does not match a reference with unexpected arity"
      (check-expand exn:fail:syntax
        (define-syntax-parser get-ref
          [(_:id _:gref) #''ignored])
        (get-ref dummy-ref))))
  (context "given a non arity"
    (it "raises a contract violation"
      (define-syntax-parser check
        [(_:id non-arity:expr)
         (syntax/loc this-syntax
           (check-expand exn:fail:contract
             (define-syntax-parser fail
               [_:id
                #:with ref #'dummy-ref
                #:declare ref (gref #:arity non-arity)
                #''ignored])
             fail))])
      (check 'non-arity)
      (check "1")))
  (context "called when not expanding"
    (it "does not match anything"
      (check-raise exn:fail:syntax
        (syntax-parse #''ignored
          [_
           #:with ref #'dummy-ref
           #:declare ref (gref #:arity #f)
           #''ignored])))))

(describe "generalized-reference"
  (it "is an alias of gref"
    (local-require expect)
    (check-value (expect-compare free-identifier=? #'gref)
      #'generalized-reference)))

(describe "get-set!-expansion"
  (context "given a syntax"
    (it "matches a reference with expected arity"
      (define-syntax-parser get-ref
        [(_:id ref)
         (define-values (_arity getter setter preambles)
           (get-set!-expansion #'dummy-ref #:arity 0))
         (quasisyntax/loc this-syntax
           (values '#,getter '#,setter '#,preambles))])
      (check-values ('getter 'setter '(preamble1 preamble2))
        (get-ref dummy-ref)))
    (it "does not match a reference with unexpected arity"
      (check-expand exn:fail:syntax
        (define-syntax-parser get-ref
          [(_:id ref)
           (get-set!-expansion #'dummy-ref)
           #''ignored])
        (get-ref dummy-ref))))
  (context "given a non syntax"
    (it "raises a contract violation"
      (check-expand exn:fail:contract
        (define-syntax-parser fail
          [_:id
           (get-set!-expansion 'non-stx)
           #''ignored])
        fail)))
  (context "given a non arity"
    (it "raises a contract violation"
      (define-syntax-parser check
        [(_:id non-arity:expr)
         (syntax/loc this-syntax
           (check-expand exn:fail:contract
             (define-syntax-parser fail
               [_:id
                (get-set!-expansion #'dummy-ref #:arity non-arity)
                #''ignored])
             fail))])
      (check 'non-arity)
      (check "1")))
  (context "called when not expanding"
    (it "raises a contract violation"
      (check-raise exn:fail:contract
        (get-set!-expansion #'dummy-ref #:arity #f)))))

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

(require expect/rackunit
         gref/base
         gref/syntax
         syntax/parse
         syntax/parse/define
         (except-in expect attribute)
         (only-in rackunit test-case)
         (for-syntax racket/base
                     syntax/parse)
         (for-template gref/syntax))

(define expect-expand-syntax-exn
  (expect-expand (expect-raise (expect-struct exn:fail:syntax))))

(define expect-expand-contract-exn
  (expect-expand (expect-raise (expect-struct exn:fail:contract))))

(test-case "define-set!-syntax"
  (check-expect (let ()
                  (define-syntax foo 'default)
                  (define-set!-syntax foo 'set!)
                  (define-syntax-parser bar
                    [_:id
                     #:with val (syntax-local-value #'foo)
                     (syntax/loc this-syntax 'val)])
                  bar)
                'default))

(test-case "define-set!-syntaxes"
  (check-expect (let ()
                  (define-syntaxes (foo bar)
                    (values 'default 'default))
                  (define-set!-syntaxes (foo bar)
                    (values 'set! 'set!))
                  (define-syntax-parser baz
                    [_:id
                     #:with val-foo (syntax-local-value #'foo)
                     #:with val-bar (syntax-local-value #'bar)
                     (syntax/loc this-syntax '(val-foo val-bar))])
                  baz)
                '(default default)))

(test-case "set!-pack"
  (check-expect (set!-pack #'getter #'setter #'preamble1 #'preamble2)
                (expect-pred syntax?))
  (check-exn exn:fail:contract?
    (lambda ()
      (set!-pack 'not-stx #'setter #'preamble1 #'preamble2)))
  (check-exn exn:fail:contract?
    (lambda ()
      (set!-pack #'getter 'not-stx #'preamble1 #'preamble2)))
  (check-exn exn:fail:contract?
    (lambda ()
      (set!-pack #'getter #'setter 'not-stx #'preamble2)))
  (check-exn exn:fail:contract?
    (lambda ()
      (set!-pack #'getter #'setter #'preamble1 'not-stx)))
  (check-exn exn:fail:contract?
    (lambda ()
      (set!-pack #'getter #'setter #'preamble1 #'preamble2
                 #:arity 'not-nonneg-int)))
  (check-exn exn:fail:contract?
    (lambda ()
      (set!-pack #'getter #'setter #'preamble1 #'preamble2
                 #:source 'not-srcloc))))

(test-case "prop:set!-expander"
  (check-expect (let ([val 'init])
                  (set! bar 'ignored)
                  (define-set!-syntax bar
                    (let ()
                      (define (expand _foo)
                        (syntax-parser
                          [(_:id) (set!-pack #'(lambda () val)
                                             #'(lambda (_val)
                                                 (set! val 'set)))]
                          [who:id (syntax/loc this-syntax (who))]))
                      (struct foo ()
                        #:property prop:set!-expander expand)
                      (foo)))
                  val)
                'set)
  (check-expect #'(let ()
                    (set! bar 'ignored)
                    (define-set!-syntax bar
                      (let ()
                        (define ((expand _foo) _stx) 'not-stx)
                        (struct foo ()
                          #:property prop:set!-expander expand)
                        (foo)))
                    'unreached)
                expect-expand-contract-exn))

(test-case "make-set!-expander"
  (check-expect (let ([val 'init])
                  (set! foo 'ignored)
                  (define-set!-syntax foo
                    (make-set!-expander
                     (syntax-parser
                       [(_:id) (set!-pack #'(lambda () val)
                                          #'(lambda (_val)
                                              (set! val 'set)))]
                       [who:id (syntax/loc this-syntax (who))])))
                  val)
                'set)
  (check-expect #'(let ()
                    (set! foo 'ignored)
                    (define-set!-syntax foo
                      (make-set!-expander (lambda (_stx) 'not-stx)))
                    'unreached)
                expect-expand-contract-exn))

(test-case "gref"
  (check-expect (let ()
                  (define-set!-syntax foo
                    (make-set!-expander
                     (syntax-parser
                       [_:id (set!-pack #'getter #'setter
                                        #'preamble1 #'preamble2
                                        #:arity 0)])))
                  (define-syntax-parser bar
                    [(_:id (~var ref (gref #:arity 0)))
                     (syntax/loc this-syntax
                       '(ref.getter ref.setter ref.preamble ...))])
                  (bar foo))
                '(getter setter preamble1 preamble2))
  (check-expect #'(let ()
                    (bar foo)
                    (define-set!-syntax foo
                      (make-set!-expander
                       (syntax-parser
                         [_:id (set!-pack #'getter #'setter
                                          #'preamble1 #'preamble2
                                          #:arity 0)])))
                    (define-syntax-parser bar
                      [(_:id _:gref) #''no-exn])
                    'unreached)
                expect-expand-syntax-exn))

(test-case "generalized-reference"
  (check-expect #'generalized-reference
                (expect-compare free-identifier=? #'gref)))

(test-case "get-set!-expansion"
  (check-expect (let ()
                  (define-set!-syntax foo
                    (make-set!-expander
                     (syntax-parser
                       [_:id (set!-pack #'getter #'setter
                                        #'preamble1 #'preamble2
                                        #:arity 0)])))
                  (define-syntax-parser bar
                    [(_:id ref)
                     (define-values (_arity getter setter preambles)
                       (get-set!-expansion #'ref #:arity 0))
                     (quasisyntax/loc this-syntax
                       '(#,getter #,setter #,@preambles))])
                  (bar foo))
                '(getter setter preamble1 preamble2))
  (check-expect #'(let ()
                    (define-syntax (foo _stx)
                      (get-set!-expansion 'not-stx)
                      #''unreached)
                    (foo))
                expect-expand-contract-exn)
  (check-expect #'(let ()
                    (define-syntax (foo _stx)
                      (get-set!-expansion #'foo -1)
                      #''unreached)
                    (foo))
                expect-expand-contract-exn))

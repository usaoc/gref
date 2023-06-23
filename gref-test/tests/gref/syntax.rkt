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

(test-case ":set!"
  (check-expect #':set! expect-expand-syntax-exn)
  (check-expect #'(:set! ([(id) expr]) (store) reader writer)
                expect-expand-syntax-exn))

(define expect-expand-contract-exn
  (expect-expand (expect-raise (expect-struct exn:fail:contract))))

(test-case "define-accessor"
  (check-expect (let ([val 'init])
                  (define (val-ref) val)
                  (define pre (shift! (foo) 'ignored))
                  (define post (foo))
                  (define-accessor foo val-ref
                    (syntax-parser
                      [(_:id) (syntax/loc this-syntax
                                (:set! () (_obj)
                                       val (set! val 'set)))]))
                  (list pre post))
                '(init set))
  (check-expect #'(let ()
                    (define-accessor foo bar 'not-id)
                    'unreached)
                expect-expand-contract-exn))

(test-case "prop:set!-expander"
  (check-expect (let ([val 'init])
                  (set! (bar) 'ignored)
                  (define-syntax bar
                    (let ()
                      (define (expand _foo)
                        (syntax-parser
                          [(_:id)
                           (syntax/loc this-syntax
                             (:set! () (_obj) val (set! val 'set)))]))
                      (struct foo ()
                        #:property prop:set!-expander expand)
                      (foo)))
                  val)
                'set)
  (check-expect #'(let-syntax
                      ([bar (let ()
                              (define ((expand _foo) _stx) 'not-stx)
                              (struct foo ()
                                #:property prop:set!-expander expand)
                              (foo))])
                    (set! (bar) 'ignored))
                expect-expand-contract-exn))

(test-case "gref"
  (check-expect (let ()
                  (define-accessor foo bar
                    (syntax-parser
                      [(_:id)
                       (syntax/loc this-syntax
                         (:set! ([(id) expr]) () reader writer))]))
                  (define-syntax-parser baz
                    [(_:id (~var ref (gref 0)))
                     (syntax/loc this-syntax
                       '((ref.binding ...)
                         (ref.store ...)
                         ref.reader
                         ref.writer))])
                  (baz (foo)))
                '(([(id) expr]) () reader writer))
  (check-expect #'(let ()
                    (define-accessor foo bar
                      (syntax-parser
                        [(_:id)
                         (syntax/loc this-syntax
                           (:set! ([(id) expr]) () reader writer))]))
                    (define-syntax-parser baz
                      [(_:id _:gref)
                       (syntax/loc this-syntax unreached)])
                    (baz (foo)))
                expect-expand-syntax-exn))

(test-case "generalized-reference"
  (check-expect #'generalized-reference
                (expect-compare free-identifier=? #'gref)))

(test-case "get-set!-expansion"
  (check-expect (let ()
                  (define-accessor foo bar
                    (syntax-parser
                      [(_:id)
                       (syntax/loc this-syntax
                         (:set! ([(id) expr]) () reader writer))]))
                  (define-syntax-parser baz
                    [(_:id ref)
                     (define-values (bindings stores reader writer)
                       (get-set!-expansion #'ref 0))
                     (quasisyntax/loc this-syntax
                       '(#,bindings #,stores #,reader #,writer))])
                  (baz (foo)))
                '(([(id) expr]) () reader writer))
  (check-expect #'(let-syntax ([foo (lambda (stx)
                                      (get-set!-expansion 'not-stx 0)
                                      (syntax/loc stx no-exn))])
                    (foo))
                expect-expand-contract-exn)
  (check-expect #'(let-syntax ([foo (lambda (stx)
                                      (get-set!-expansion #'foo -1)
                                      (syntax/loc stx no-exn))])
                    (foo))
                expect-expand-contract-exn))

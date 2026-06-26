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

(provide deftech/rep tech/rep)

(require racket/string
         scribble/core
         scribble/decode
         scribble/manual
         syntax/parse/define
         (for-syntax racket/base
                     racket/string
                     racket/syntax))

(define-syntax-parser make-table
  [(_:id (~seq left+rights:expr fn:expr) ...)
   #:do [(define left+rights/fn
           (syntax-local-eval #'(hasheq (~@ left+rights fn) ...)))
         (define table
           (for*/hash ([(left+rights fn) (in-hash left+rights/fn)]
                       [(lefts rights) (in-hash left+rights)]
                       [left (in-list lefts)]
                       [right (in-list rights)])
             (fn left right)))]
   (datum->syntax #'here table this-syntax)])

(define table
  (make-table
   #hasheq((("expand") . ("ded" "sion")))
   (lambda (base sfix)
     (define root (string-trim base "d" #:left? #f))
     (values (string-append-immutable root sfix) base))
   #hasheq((("access") . ("es"))
           (("store") . ("ed" "ing"))
           (("value" "visit") . ("ed")))
   (lambda (base sfix)
     (define root (string-trim base "e" #:left? #f))
     (values (string-append-immutable root sfix) base))
   #hasheq((("functional") . ("optic"))
           (("generalized") . ("place")))
   (lambda (pfix abbr)
     (values abbr (string-append-immutable pfix " reference")))
   #hasheq((("evaluation") . ("order"))
           (("generalized") . ("reference")))
   (lambda (pfix abbr)
     (values abbr (string-append-immutable pfix " " abbr)))
   #hasheq((("preamble") . ("form"))
           (("arity") . ("number"))
           (("getter" "setter") . ("procedure")))
   (lambda (abbr sfix)
     (values abbr (string-append-immutable abbr " " sfix)))))

(define ((make-tech/rep tech) . pres)
  (define cont (decode-content pres))
  (define str (content->string cont))
  (tech #:key (cond
                [(hash-ref table str #f)]
                [else
                 (define str* (string-trim str "s" #:left? #f))
                 (and (not (eq? str str*)) (hash-ref table str* #f))])
        cont))

(define deftech/rep (make-tech/rep deftech))

(define tech/rep (make-tech/rep tech))

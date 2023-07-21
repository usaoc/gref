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
                     racket/syntax
                     racket/string))

(define-syntax-parser make-table
  [(_:id (~seq fn:expr hash:expr) ...)
   #:do [(define fn+table
           (syntax-local-eval #'(hasheq (~@ fn hash) ...)))
         (define table
           (for*/hash ([(fn table) (in-hash fn+table)]
                       [(left rights) (in-hash table)]
                       [(right) (in-list rights)])
             (fn left right)))]
   (datum->syntax #'here table this-syntax)])

(define table
  (make-table
   (lambda (base suffix)
     (define root (string-trim base "d" #:left? #f))
     (values (string-append-immutable root suffix) base))
   #hasheq(("expand" . ("ded" "sion")))
   (lambda (base suffix)
     (define root (string-trim base "e" #:left? #f))
     (values (string-append-immutable root suffix) base))
   #hasheq(("access" . ("es"))
           ("store" . ("ed" "ing"))
           ("value" . ("ed"))
           ("visit" . ("ed")))
   (lambda (abbrev prefix)
     (values abbrev (string-append-immutable prefix "reference")))
   #hasheq(("optic" . ("functional "))
           ("place" . ("generalized ")))
   (lambda (prefix abbrev)
     (values abbrev (string-append-immutable prefix abbrev)))
   #hasheq(("evaluation " . ("order"))
           ("generalized " . ("reference")))
   (lambda (suffix abbrev)
     (values abbrev (string-append-immutable abbrev suffix)))
   #hasheq((" form" . ("preamble"))
           (" number" . ("arity"))
           (" procedure" . ("getter" "setter")))))

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

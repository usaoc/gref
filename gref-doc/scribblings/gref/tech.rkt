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

(require racket/contract/base
         scribble/core
         scribble/decode)
(provide (contract-out [deftech/rep (-> pre-content? ... element?)]
                       [tech/rep (-> pre-content? ... element?)]))

(require racket/string
         scribble/core
         scribble/decode
         scribble/manual)

(define table
  (for*/hash ([(fn table)
               (in-hash
                (hasheq
                 (lambda (base suffix)
                   (define root (string-trim base "e" #:left? #f))
                   (values (string-append root suffix) base))
                 #hasheq(("access" . ("es"))
                         ("store" . ("ed" "ing"))
                         ("value" . ("ed"))
                         ("visit" . ("ed")))
                 (lambda (abbrev prefix)
                   (values abbrev (string-append prefix "reference")))
                 #hasheq(("optic" . ("functional "))
                         ("place" . ("generalized ")))
                 (lambda (prefix abbrev)
                   (values abbrev (string-append prefix abbrev)))
                 #hasheq(("evaluation " . ("order"))
                         ("generalized " . ("reference"))
                         ("lexical " . ("context"))
                         ("store " . ("variable")))
                 (lambda (suffix abbrev)
                   (values abbrev (string-append abbrev suffix)))
                 #hasheq((" procedure" . ("getter" "setter"))
                         (" expression" . ("reader" "writer")))))]
              [(left rights) (in-hash table)]
              [(right) (in-list rights)])
    (fn left right)))

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

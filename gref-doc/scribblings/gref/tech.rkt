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
         scribble/manual
         (for-syntax racket/base
                     racket/string
                     syntax/transformer))

(define-syntax table
  (make-variable-like-transformer
   (datum->syntax
    #'here
    (make-immutable-hash
     `(,@(for/list ([(base suffixes)
                     #hasheq(("access" . ("es"))
                             ("store" . ("ed" "ing"))
                             ("value" . ("ed"))
                             ("visit" . ("ed")))]
                    #:do [(define root
                            (string-trim base "e" #:left? #f))]
                    [suffix (in-list suffixes)])
           `(,(string-append root suffix) . ,base))
       ,@(for/list ([(abbrev prefix)
                     #hasheq(("optic" . "functional ")
                             ("place" . "generalized "))])
           `(,abbrev . ,(string-append prefix "reference")))
       ,@(for*/list ([(prefix abbrevs)
                      #hasheq(("evaluation " . ("order"))
                              ("generalized " . ("reference"))
                              ("lexical " . ("context"))
                              ("store " . ("variable")))]
                     [abbrev (in-list abbrevs)])
           `(,abbrev . ,(string-append prefix abbrev)))
       ,@(for*/list ([(abbrevs suffix)
                      #hasheq((("getter" "setter") . " procedure")
                              (("reader" "writer") . " expression"))]
                     [abbrev (in-list abbrevs)])
           `(,abbrev . ,(string-append abbrev suffix)))))
    #'here)))

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

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

(provide defacc defattr defcls/alias defsubtogether)

(require scribble/manual
         scribblings/gref/lib
         syntax/parse/define
         (for-label gref/base)
         (for-syntax racket/base))

(begin-for-syntax
  (define-syntax-class set!-form
    #:commit
    #:attributes (name)
    #:literals (set!)
    (pattern (set! name:id _:id))
    (pattern (set! (name:id . _) _:id)))
  (define-syntax-class contract
    #:commit
    #:attributes ()
    (pattern [_:id _:expr]))
  (define-syntax-class grammar
    #:commit
    #:attributes ()
    (pattern (_:id _:expr ...+)))
  (define-splicing-syntax-class link-target-opt
    #:attributes (link-target)
    (pattern (~optional (~seq #:link-target? link-target:expr))))
  (define-splicing-syntax-class grammar-opt
    #:attributes ([grammar 1])
    (pattern (~optional (~seq #:grammar [grammar:grammar ...])))))

(define-syntax-parser defacc
  [(_:id link-target:link-target-opt
         set!-form:set!-form
         grammar:grammar-opt
         #:contracts (contract:contract ...)
         pre:expr ...)
   (syntax/loc this-syntax
     (defform
       #:kind "accessor"
       (~? (~@ #:link-target? link-target.link-target))
       #:id set!-form.name
       #:literals (set!)
       set!-form
       (~? (~@ #:grammar [grammar.grammar ...]))
       #:contracts (contract ...)
       pre ...))])

(define-syntax-parser defattr
  [(_:id name:id cont:expr)
   (syntax/loc this-syntax
     (defthing #:kind "attribute" #:link-target? #f name cont))])

(define-syntax-parser defcls
  [(_:id proto:expr pre:expr ...)
   (syntax/loc this-syntax
     (defproc
       #:kind "syntax class"
       proto #,(tech #:doc stx-parse "syntax class")
       pre ...))])

(define-syntax-parser defcls/alias
  #:literals ([= _])
  [(_:id name:id alias:id (= . subform) pre:expr ...)
   #:with name-def
   (syntax/loc this-syntax
     (defcls (name . subform) pre ...))
   #:with alias-def
   (syntax/loc this-syntax
     (defcls (alias . subform) "An alias for " (racket name) "."))
   #'(begin name-def alias-def)])

(define-syntax-parser defsubtogether
  [(_:id [def:expr ...+] pre:expr ...)
   (syntax/loc this-syntax
     (nested #:style "leftindent" (deftogether [def ...] pre ...)))])

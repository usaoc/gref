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

(provide defset! defattr defcls/alias defsubtogether
         provided-for-syntax racket/set!)

(require scribble/manual
         scribblings/gref/lib
         syntax/parse/define
         (for-label gref/base
                    (only-in racket/base for-syntax))
         (for-syntax racket/base))

(define-for-syntax set!-intro
  (make-interned-syntax-introducer 'gref/set!))

(begin-for-syntax
  (define-syntax-class set!-form
    #:commit
    #:attributes (set!-name)
    #:literals (set!)
    (pattern (set! name:id _:id)
      #:with set!-name (set!-intro #'name 'add))
    (pattern (set! (name:id . _) _:id)
      #:with set!-name (set!-intro #'name 'add)))
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

(define-syntax-parser defset!
  [(_:id link-target:link-target-opt
         set!-form:set!-form
         grammar:grammar-opt
         #:contracts (contract:contract ...)
         pre:expr ...)
   (syntax/loc this-syntax
     (defform
       (~? (~@ #:link-target? link-target.link-target))
       #:id set!-form.set!-name
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

(begin-for-syntax
  (define-splicing-syntax-class both
    #:attributes (pre)
    (pattern (~seq #:both pre:expr))))

(define-syntax-parser defcls/alias
  #:literals ([= _])
  [(_:id name:id alias:id (= . subform)
         (~optional both:both) pre:expr ...)
   #:with name-def
   (syntax/loc this-syntax
     (defcls (name . subform) both.pre pre ...))
   #:with alias-def
   (syntax/loc this-syntax
     (defcls (alias . subform) both.pre
       "An alias for " (racket name) "."))
   #'(begin name-def alias-def)])

(define-syntax-parser defsubtogether
  [(_:id [def:expr ...+] pre:expr ...)
   (syntax/loc this-syntax
     (nested #:style "leftindent" (deftogether [def ...] pre ...)))])

(define provided-for-syntax
  (list (smaller "Exported " (racket for-syntax) ".") "\n" "\n"))

(define-syntax-parser racket/set!
  [(_:id form)
   #:with set!-form (set!-intro #'form 'add)
   (syntax/loc this-syntax (racket set!-form))])

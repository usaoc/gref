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

(provide examples/gref)

(require scribble/example
         scribblings/gref/def
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse))

(define make-eval (make-eval-factory '(gref)))

(define-for-syntax lifted-eval #f)

(define-for-syntax (lift-eval!)
  (define eval-id (syntax-local-lift-expression #'(make-eval)))
  (define/syntax-parse eval eval-id)
  (syntax-local-lift-module-end-declaration #'(close-eval eval))
  (set! lifted-eval eval-id)
  eval-id)

(begin-for-syntax
  (define-splicing-syntax-class label
    #:attributes (expr)
    (pattern (~optional (~seq #:label expr:expr))))
  (define-syntax-class subform/gref
    #:datum-literals (unsyntax)
    #:commit
    #:attributes (show eval)
    (pattern #,form
      #:with show (syntax/loc this-syntax #,(racket/set! form))
      #:with eval (syntax/loc this-syntax form))
    (pattern (subform:subform/gref ...)
      #:with show (syntax/loc this-syntax (subform.show ...))
      #:with eval (syntax/loc this-syntax (subform.eval ...)))
    (pattern (~and show eval)))
  (define-syntax-class form/gref
    #:datum-literals (code:gref)
    #:commit
    #:attributes (form)
    (pattern (code:gref ~! subform:subform/gref)
      #:with form (syntax/loc this-syntax
                    (eval:alts subform.show subform.eval)))
    (pattern form)))

(define-syntax-parser examples/gref
  [(_:id label:label form:form/gref ...)
   #:with eval (or lifted-eval (lift-eval!))
   (syntax/loc this-syntax
     (examples #:eval eval
               (~? (~@ #:label label.expr))
               form.form ...))])

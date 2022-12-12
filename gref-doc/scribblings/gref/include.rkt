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

(provide include/gref)

(require scribble/base
         syntax/parse/define
         (for-syntax racket/base
                     racket/symbol
                     syntax/parse))

(begin-for-syntax
  (define-syntax-class mod
    #:commit
    #:attributes (mod)
    (pattern _:id
      #:with path (datum->syntax
                   #f
                   (string-append "scribblings/gref/"
                                  (symbol->immutable-string
                                   (syntax-e this-syntax))
                                  ".scrbl")
                   #'here)
      #:with mod (syntax/loc this-syntax (lib path)))))

(define-syntax-parser include/gref
  [(_:id mod:mod ...+)
   (syntax/loc this-syntax (begin (include-section mod.mod) ...))])

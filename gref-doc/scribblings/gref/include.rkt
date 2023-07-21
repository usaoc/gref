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
  (define-syntax-class (mod out)
    #:commit
    #:attributes (inc)
    (pattern _:id
      #:do [(define sub-path
              (symbol->immutable-string (syntax-e this-syntax)))
            (define full-path
              (string-append-immutable "scribblings/gref/"
                                       sub-path ".scrbl"))]
      #:with path (datum->syntax #f full-path #'here)
      #:with inc (syntax/loc out (include-section (lib path))))))

(define-syntax-parser include/gref
  [(_:id mod ...+)
   #:declare mod (mod this-syntax)
   #'(begin mod.inc ...)])

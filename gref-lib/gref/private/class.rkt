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

(provide maybe-expr/c)

(require syntax/parse
         (for-template racket/base))

(define-syntax-class (maybe-expr/c contract-expr)
  #:commit
  #:attributes (c)
  (pattern expr
    #:declare expr (expr/c contract-expr)
    #:with c (syntax/loc this-syntax
               (if (variable-reference-from-unsafe?
                    (#%variable-reference))
                   expr
                   expr.c))))

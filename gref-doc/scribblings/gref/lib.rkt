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

(provide lib-path rkt-guide rkt-ref srfi stx-parse)

(require syntax/parse/define
         (for-syntax racket/base
                     racket/string))

(define-syntax-parser lib-path
  [(_:id path:str)
   #:do [(define ext ".scrbl")
         (define path-str (syntax-e #'path))
         (define full-path-str
           (if (string-contains? path-str "/")
               (string-append-immutable path-str ext)
               (string-append-immutable path-str "/main" ext)))]
   #:with full-path (datum->syntax #f full-path-str #'here)
   (syntax/loc this-syntax '(lib full-path))])

(define rkt-guide (lib-path "scribblings/guide/guide"))

(define rkt-ref (lib-path "scribblings/reference/reference"))

(define srfi (lib-path "srfi/scribblings/srfi"))

(define stx-parse (lib-path "syntax/scribblings/syntax"))

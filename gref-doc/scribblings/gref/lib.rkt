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

(provide lib-path rkt-guide rkt-ref stx-parse)

(require racket/string)

(define (lib-path path)
  `(lib ,(string-append
          path
          (if (string-contains? path "/") ".scrbl" "/main.scrbl"))))

(define rkt-guide (lib-path "scribblings/guide/guide"))

(define rkt-ref (lib-path "scribblings/reference/reference"))

(define stx-parse (lib-path "syntax/scribblings/syntax"))

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

(require scribble/core
         racket/contract/base)
(provide (contract-out [rnrs (-> exact-nonnegative-integer?
                                 exact-nonnegative-integer?
                                 element?)]))

(require scribble/base)

(define (rnrs rev sec)
  (define rev-str (number->string rev))
  (define sec-str (number->string sec))
  (define rnrs-str (string-append "r" rev-str "rs"))
  (hyperlink (collection-file-path
              (string-append rnrs-str "-Z-H-" sec-str ".html")
              rnrs-str "scribblings" (string-append rnrs-str "-std"))
             "R" (superscript rev-str) "RS"))

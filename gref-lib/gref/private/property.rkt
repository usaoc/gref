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

(require racket/contract)
(provide set!-expander? set!-expander-ref
         (contract-out
          [prop:set!-expander
           (struct-type-property/c
            (-> set!-expander? (-> syntax? syntax?)))]))

(define-values (prop:set!-expander set!-expander? set!-expander-ref)
  (make-struct-type-property 'set!-expander))


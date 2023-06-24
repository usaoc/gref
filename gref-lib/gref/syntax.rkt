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

(module for-syntax racket/base
  (require gref/private/expand
           gref/private/property
           racket/contract
           syntax/parse/experimental/provide)
  (define num/c (or/c #f exact-nonnegative-integer?))
  (provide (contract-out
             [get-set!-expansion
              (->* (syntax?) (num/c)
                   #:pre/desc (or (syntax-transforming?)
                                  "not currently expanding")
                   (values (listof syntax?) (listof identifier?)
                           syntax? syntax?))]
             [prop:set!-expander
              (struct-type-property/c
               (-> set!-expander? (-> syntax? syntax?)))]))
  (provide-syntax-class/contract [gref (syntax-class/c (num/c))]))

(require gref/private/define
         (only-in gref/private/literal :set!)
         (for-syntax (only-in gref/private/property set!-expander?)
                     (submod "." for-syntax)
                     (rename-in (submod "." for-syntax)
                                [gref generalized-reference])))
(provide (all-from-out gref/private/define)
         (all-from-out gref/private/literal)
         (for-syntax (all-from-out gref/private/property)
                     (all-from-out (submod "." for-syntax))))

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
  (require gref/private/expand/public
           gref/private/property
           racket/contract
           syntax/parse/experimental/provide
           syntax/srcloc)
  (define maybe-arity/c (or/c #f exact-nonnegative-integer?))
  (provide (contract-out
             [set!-pack
              (->* (syntax? syntax?)
                   (#:arity exact-nonnegative-integer?
                    #:source source-location?)
                   #:rest (listof syntax?)
                   syntax?)]
             [prop:set!-expander
              (struct-type-property/c
               (-> set!-expander? (-> syntax? syntax?)))])
           set!-expander?
           (contract-out
             [make-set!-expander
              (-> (-> syntax? syntax?) set!-expander?)]
             [make-set!-functional
              (->* (syntax? syntax?)
                   (#:arity exact-nonnegative-integer?)
                   set!-expander?)])
           maybe-arity/c)
  (provide-syntax-class/contract
    [gref (syntax-class/c () (#:arity maybe-arity/c))])
  (provide (contract-out
             [get-set!-expansion
              (->* (syntax?) (#:arity maybe-arity/c)
                   #:pre/desc (or (syntax-transforming?)
                                  "not currently expanding")
                   (values exact-nonnegative-integer?
                           syntax? syntax? (listof syntax?)))])))

(require gref/private/define
         (for-syntax (submod "." for-syntax)))
(provide (all-from-out gref/private/define)
         (for-syntax (all-from-out (submod "." for-syntax))
                     (rename-out [gref generalized-reference])))

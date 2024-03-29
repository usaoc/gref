#lang scribble/manual
@;{
 Copyright (C) 2022 Wing Hei Chan

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or (at
 your option) any later version.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see
 <https://www.gnu.org/licenses/>.}

@(require scribblings/gref/def
          scribblings/gref/lib
          scribblings/gref/tech
          (for-label gref/syntax
                     racket/contract
                     syntax/parse
                     syntax/srcloc
                     syntax/transformer
                     (except-in racket/base ...)))

@title[#:tag "syntax"]{The Syntax Module}

@defmodule[gref/syntax #:no-declare]
@declare-exporting[gref/syntax gref]
The syntax module @racket[provide]s various bindings useful for user
extensions.

@(define dot (racketparenfont "."))
@defform*[[(define-set!-syntax name val)
           (define-set!-syntax header body ...+)]
          #:grammar [(name @#,racket[id])
                     (header (header args) (name args))
                     (args (code:line arg ...)
                           (code:line arg ... @#,dot @#,racket[id]))
                     (arg (code:line @#,racket[keyword] id-or-id+expr)
                          id-or-id+expr)
                     (id-or-id+expr @#,racket[id]
                                    [@#,racket[id] default-expr])]
          #:contracts ([val any/c]
                       [default-expr any/c])]{
 Like @racket[define-syntax], but the created binding is in the
 @racket['gref/set!] @tech[#:doc rkt-ref]{binding space}.}

@defform[(define-set!-syntaxes (name ...) vals)
         #:grammar [(name @#,racket[id])]
         #:contracts ([vals any])]{
 Like @racket[define-syntaxes], but the created bindings are in the
 @racket['gref/set!] @tech[#:doc rkt-ref]{binding space}.}

@defproc[(set!-pack [getter syntax?] [setter syntax?]
                    [preamble syntax?] ...
                    [#:arity number exact-nonnegative-integer? 1]
                    [#:source src source-location? #f])
         syntax?
         provided-for-syntax]{
 Returns a @deftech{fully-expanded} @racket[number]-@tech/rep{valued}
 @tech/rep{reference}.  The first two by-position arguments are
 the @tech{getter procedure} and @tech{setter procedure}, and the
 remaining by-position arguments are the @tech{preamble forms}.  The
 resulting @tech[#:doc rkt-ref]{syntax object} is given the
 @tech[#:doc rkt-ref]{source-location} information of @racket[src].}

@defthing[prop:set!-expander
          (struct-type-property/c
           (-> set!-expander? (-> syntax? syntax?)))
          provided-for-syntax]{
 A @tech[#:doc rkt-ref]{structure type property} to identify
 @tech[#:doc rkt-ref]{structure types} that act as
 @deftech{@racket[set!] expanders} used by @racket[gref].  The
 property value takes the structure instance that implements
 @racket[prop:set!-expander] and results in a
 @tech{@racket[set!] expander}, which in turn takes the
 @tech[#:doc rkt-ref]{syntax object} of a
 @var[number]-@tech/rep{valued} @tech/rep{reference} and results in
 another @var[number]-@tech/rep{valued} @tech/rep{reference}.}

@defproc[(set!-expander? [val any/c]) boolean?
         provided-for-syntax]{
 Returns @racket[#t] if @racket[val] implements
 @racket[prop:set!-expander], @racket[#f] otherwise.}

@defproc[(make-set!-expander [proc (-> syntax? syntax?)])
         set!-expander?
         provided-for-syntax]{
 Returns an implementation of @racket[prop:set!-expander] that uses
 @racket[proc] as the @tech{@racket[set!] expander}.}

@defproc[(make-set!-functional
          [getter syntax?] [setter syntax?]
          [#:arity number exact-nonnegative-integer? 1])
         syntax?
         provided-for-syntax]{
 Returns an implementation of @racket[prop:set!-expander] that
 @tech/rep{expands} @deftech{functional forms}.  A
 @tech{functional form} with the shape @racket[(_who _arg ...)] where
 @var[who]'s @tech[#:doc rkt-guide]{transformer binding} is the
 resulting implementation will be @tech/rep{expanded} such that:

 @itemlist[
 @item{Each expression in @var[arg] is evaluated in the apparent
   @tech/rep{order} and bound to @var[arg-val];}
 @item{A sequence of @racket[number] @tech[#:doc rkt-ref]{identifiers}
   @racket[_val ...] is generated;}
 @item{The expressions @racket[(lambda () (getter _arg-val ...))] and
   @racket[(lambda (_val ...) (setter _arg-val ... _val ...))] are
   used as the @tech/rep{getter} and @tech/rep{setter}.}]

 In other words, it works as a static alternative to @tech{SRFI 17},
 generalized to multiple @tech/rep{values}.}

@defthing[maybe-arity/c flat-contract?
          provided-for-syntax]{
 A @tech[#:doc rkt-ref]{flat contract} that accepts an expected
 @tech/rep{arity}, where @racket[#f] means any @tech/rep{arity}.

 Equivalent to @racket[(or/c #f exact-nonnegative-integer?)].}

@defcls/alias[gref generalized-reference
              (_ [#:arity number maybe-arity/c 1])
              #:both provided-for-syntax]{
 Matches a @racket[number]-@tech/rep{valued} @tech/rep{reference}.  If
 @racket[number] is @racket[#f], matches any @tech/rep{reference}.
 The @tech/rep{reference} is recursively @deftech/rep{expanded} until
 @tech{fully expanded} as follows:

 @itemlist[
 @item{If it is a valid @racket[set!-pack]ed form, the
   @tech/rep{expansion} is complete;}
 @item{If it is an @tech[#:doc rkt-ref]{identifier} with a
   @tech[#:doc rkt-guide]{transformer binding} or a
   @tech[#:doc rkt-ref]{syntax-object} pair whose first element is
   such an @tech[#:doc rkt-ref]{identifier}, the
   @tech[#:doc rkt-guide]{transformer binding} is used to continue;}
 @item{Otherwise, if it is an @tech[#:doc rkt-ref]{identifier}, a
   @tech{@racket[set!] expander} for
   @tech[#:doc rkt-ref]{variables} is used to continue.}]

 Each @tech[#:doc rkt-guide]{transformer binding} is resolved in the
 @indexed-racket['gref/set!] @tech[#:doc rkt-ref]{binding space}.  Due
 to the way @tech[#:doc rkt-ref]{scope sets} works, a
 @tech[#:doc rkt-guide]{transformer binding} in the
 @tech[#:doc rkt-ref]{default binding space} will be used
 unless another @tech[#:doc rkt-guide]{transformer binding} in the
 @racket['gref/set!] @tech[#:doc rkt-ref]{binding space}
 @tech[#:doc rkt-ref]{shadows} it.

 If the @tech[#:doc rkt-guide]{transformer binding} refers to a
 @racket[set!-expander?] value, it is used to produce a
 @tech{@racket[set!] expander}, which in turn receives the
 @tech[#:doc rkt-ref]{syntax object} of the @tech/rep{reference}
 @tech/rep{expanded} so far.  Otherwise, the matching fails and no
 further @tech/rep{expansion} is done.  During each
 @tech/rep{expansion} step, @tech[#:doc rkt-ref]{scopes} and
 @tech[#:doc rkt-ref]{syntax properties} are accoridingly manipulated.

 From the resulting @racket[set!-pack]ed form, the following
 @tech[#:doc stx-parse]{attributes} are bound:

 @defsubtogether[[@defattr[arity exact-nonnegative-integer?]
                  @defattr[getter syntax?]
                  @defattr[setter syntax?]
                  @defattr[preamble (listof syntax?)]]]{
  The @tech{arity number}, @tech{getter procedure},
  @tech{setter procedure}, and @tech{preamble forms}.}

 If @racket[syntax-transforming?] returns @racket[#f], the matching
 fails and no @tech/rep{expansion} is done.}

@defproc[(get-set!-expansion [ref-stx syntax?]
                             [#:arity number maybe-arity/c 1])
         (values exact-nonnegative-integer?
                 syntax? syntax? (listof syntax?))
         provided-for-syntax]{
 The procedural interface for @racket[gref].  @tech/rep{Expands}
 @racket[ref-stx] as a @racket[(gref #:arity number)] form and returns
 the bound @tech[#:doc stx-parse]{attributes} in the documented order.

 If @racket[syntax-transforming?] returns @racket[#f], the
 @racket[exn:fail:contract] exception is @racket[raise]d and no
 @tech/rep{expansion} is done.}

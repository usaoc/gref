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
                     racket/base
                     racket/contract
                     syntax/parse
                     syntax/srcloc
                     syntax/transformer))

@title[#:tag "syntax"]{The Syntax Module}

@defmodule[gref/syntax #:no-declare]
@declare-exporting[gref/syntax gref]
The syntax module @racket[provide]s various bindings useful for user
extensions.

@defform*[[(define-set!-syntax name val)
           (define-set!-syntax (head args) body ...+)]
         #:grammar [(name @#,racket[id])]
         #:contracts ([val any/c])]{
 Like @racket[define-syntax], but the created binding is in the
 @racket['gref/set!] @tech[#:doc rkt-ref]{binding space}.}

@defform[(define-set!-syntaxes (name ...) vals)
         #:grammar [(name @#,racket[id])]
         #:contracts ([vals any])]{
 Like @racket[define-syntaxes], but the created bindings are in the
 @racket['gref/set!] @tech[#:doc rkt-ref]{binding space}.}

@defproc[(set!-pack [bindings syntax?] [stores syntax?]
                    [reader syntax?] [writer syntax?]
                    [#:source src source-location? #f])
         syntax?
         provided-for-syntax]{
 Returns a @deftech{fully-expanded} @racket[_number]-@tech/rep{valued}
 @tech/rep{reference}, where @racket[_number] is decided by the number
 of @racket[id]s in @racket[stores].  The resulting
 @tech[#:doc rkt-ref]{syntax object} is given the
 @tech[#:doc rkt-ref]{source-location} information of @racket[src].
 The arguments are respectively the @tech{lexical context},
 @tech{store variables}, @tech{reader expression}, and
 @tech{writer expression}.}

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
 @racket[_number]-@tech/rep{valued} @tech/rep{reference} and results
 in another @racket[_number]-@tech/rep{valued} @tech/rep{reference}.}

@defproc[(set!-expander? [val any/c]) boolean?
         provided-for-syntax]{
 Returns @racket[#t] if @racket[val] implements
 @racket[prop:set!-expander], @racket[#f] otherwise.}

@defproc[(make-set!-expander [proc (-> syntax? syntax?)])
         set!-expander?
         provided-for-syntax]{
 Returns an implementation of @racket[prop:set!-expander] that uses
 @racket[proc] as the @tech{@racket[set!] expander}.}

@defthing[maybe-arity/c flat-contract?
          provided-for-syntax]{
 A @tech[#:doc rkt-ref]{flat contract} that accepts an expected
 number of @tech{values}, where @racket[#f] means any number of
 @tech{values}.

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
 @racket['gref/set!] @tech[#:doc rkt-ref]{binding space}.  Due to the
 way @tech[#:doc rkt-ref]{scope sets} works, a
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
 @tech[#:doc stx-parse]{syntax-valued attributes} are bound:

 @defsubtogether[[@defattr[binding (listof syntax?)]
                  @defattr[store (listof identifier?)]
                  @defattr[reader syntax?]
                  @defattr[writer syntax?]]]{
  The @tech{lexical context}, @tech{store variables},
  @tech{reader expression}, and @tech{writer expression}
  respectively.}

 If @racket[syntax-transforming?] returns @racket[#f], the matching
 fails and no @tech/rep{expansion} is done.}

@defproc[(get-set!-expansion [ref-stx syntax?]
                             [#:arity number maybe-arity/c 1])
         (values (listof syntax?) (listof identifier?)
                 syntax? syntax?)
         provided-for-syntax]{
 The procedural interface for @racket[gref].  @tech/rep{Expands}
 @racket[ref-stx] as a @racket[(gref #:arity number)] form and returns
 the bound @tech[#:doc stx-parse]{syntax-valued attributes} in the
 documented order.

 If @racket[syntax-transforming?] returns @racket[#f], the
 @racket[exn:fail:contract] exception is @racket[raise]d and no
 @tech/rep{expansion} is done.}

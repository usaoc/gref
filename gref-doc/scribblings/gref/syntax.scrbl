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

@(require racket/require)
@(require scribblings/gref/def
          scribblings/gref/lib
          scribblings/gref/tech
          (for-label gref/syntax
                     racket/contract/base
                     syntax/parse
                     syntax/transformer
                     (prefix-in base- racket/base)
                     (subtract-in (except-in racket/base ...) gref)))

@title[#:tag "syntax"]{The Syntax Module}

@defmodule[gref/syntax #:no-declare]
@declare-exporting[gref/syntax gref]
The syntax module @racket[provide]s various bindings useful for user
extensions.  In particular, @racket[gen:set!-expander], @racket[gref],
@racket[generalized-reference], and @racket[get-set!-expansion] are
@racket[provide]d @racket[for-syntax].

@defform[(define-accessor name expr-id set!-expr)
         #:grammar [(name @#,racket[id]) (expr-id @#,racket[id])]
         #:contracts ([set!-expr (-> syntax? syntax?)])]{
 Defines the @tech{accessor} @racket[name].  The result of
 @racket[(make-variable-like-transformer #'expr-id)] is used as both
 the @tech[#:doc rkt-ref]{transformer} and
 @tech[#:doc rkt-ref]{assignment transformer}, whereas
 @racket[set!-expr] is used for the @tech{@racket[set!] expander}.}

@defform[(:set! ([(var ...) val] ...) (store ...) reader writer)
         #:grammar [(var @#,racket[id]) (store @#,racket[id])]
         #:contracts ([val any] [reader any] [writer any])]{
 The result of a @tech{@racket[set!] expander}.  The sub-forms are
 respectively the @tech{lexical context}, @tech{store variables},
 @tech{reader expression}, and @tech{writer expression}.}

@defthing[gen:set!-expander any/c]{
 A @tech[#:doc rkt-ref]{generic interface} that supplies a
 @deftech{@racket[set!] expander} method @racket[set!-expand] used by
 @racket[gref].  A @racket[set!-expand] method takes two arguments:
 the structure instance that implements the method and the syntax
 object of the @tech/rep{reference}, and results in a valid
 @racket[:set!] form.}

@defcls/alias[gref generalized-reference
              (_ [number (or/c #f exact-nonnegative-integer?) 1])]{
 Matches a @racket[number]-@tech/rep{valued} @tech/rep{reference}.  If
 @racket[number] is @racket[#f], matches any @tech/rep{reference}.  A
 @tech/rep{reference} is one of the following:

 @specsubform[var #:grammar [(var @#,racket[id])]]{
  A variable as a @tech/rep{reference}.}

 @specsubform[(acc . subforms) #:grammar [(acc @#,racket[id])]]{
  An arbitrary @tech/rep{reference} whose exact form is specified by
  the @tech{@racket[set!] expander} of @racket[acc].  Its
  @tech[#:doc rkt-guide]{transformer binding} must implement
  @racket[gen:set!-expander].}

 @specsubform[(getter-expr arg ...)
              #:grammar [(arg (code:line @#,racket[keyword] arg-expr)
                              arg-expr)]
              #:contracts ([getter-expr procedure?]
                           [arg-expr any/c])]{
  An SRFI 17--like @tech/rep{reference}.  The result of
  @racket[getter-expr] must be associated with a @tech/rep{setter}.}

 The following @tech[#:doc stx-parse]{syntax-valued attributes} are
 bound:

 @defsubtogether[[@defattr[binding (listof syntax?)]
                  @defattr[store (listof identifier?)]
                  @defattr[reader syntax?]
                  @defattr[writer syntax?]]]{
  The @tech{lexical context}, @tech{store variables},
  @tech{reader expression}, and @tech{writer expression}
  respectively.}

 As in @racket[static], matching fails unless
 @racket[(syntax-transforming?)].}

@defproc[(get-set!-expansion
          [ref-stx syntax?]
          [number (or/c #f exact-nonnegative-integer?)])
         (values (listof syntax?) (listof identifier?)
                 syntax? syntax?)]{
 The procedural interface for @racket[gref].  Expands @racket[ref-stx]
 as a @racket[(gref number)] form and returns the bound
 @tech[#:doc stx-parse]{syntax-valued attributes} in the documented
 order.  As in @racket[syntax-local-apply-transformer], the
 @racket[exn:fail:contract] exception is @racket[raise]d unless
 @racket[(syntax-transforming?)].}

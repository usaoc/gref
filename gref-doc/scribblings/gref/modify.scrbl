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

@(require scribblings/gref/example
          scribblings/gref/lib
          scribblings/gref/tech
          (for-label gref/base
                     gref/syntax
                     racket/class
                     racket/contract/base
                     syntax/parse
                     (except-in racket/base ... set! set!-values)))

@title[#:tag "modify"]{Modify Macros}

@deftech{Modify macros} are @tech[#:doc rkt-ref]{macros} that operate
on @tech/rep{references}.  They are defined as usual
@tech[#:doc rkt-ref]{macros}, that is, @racket[(-> syntax? syntax?)]
procedures.  Unless otherwise stated, the result of a modify macro
is always @|void-const|.

@defform[(set! pair ...+)
         #:grammar [(pair (code:line ref vals))
                    (ref @#,racket[(gref #:arity #f)])]
         #:contracts ([vals any])]{
 @tech{Stores} the results of @racket[vals]s to @racket[ref]s
 @deftech{sequentially} in the apparent @tech/rep{order}.}

@defform[(set!-values pair ...+)
         #:grammar [(pair (code:line (ref ...) vals))
                    (ref @#,racket[gref])]
         #:contracts ([vals any])]{
 Like @racket[set!], but constrained to multiple @racket[gref]s.}

@defform[(pset! pair ...+)
         #:grammar [(pair (code:line ref vals))
                    (ref @#,racket[(gref #:arity #f)])]
         #:contracts ([vals any])]{
 Like @racket[set!], but evaluates all @racket[vals]s
 @deftech{parallelly} before @tech/rep{storing} to @racket[ref]s in an
 unspecified @tech/rep{order}.

 @examples/gref[(define mpair (mcons 1 2))
                (code:gref
                 (set! (#,mcar mpair) (mcdr mpair)
                       (#,mcdr mpair) (mcar mpair)))
                mpair]

 @examples/gref[#:label "vs."
                (define mpair (mcons 1 2))
                (code:gref
                 (pset! (#,mcar mpair) (mcdr mpair)
                        (#,mcdr mpair) (mcar mpair)))
                mpair]}

@defform[(pset!-values pair ...+)
         #:grammar [(pair (code:line (ref ...) vals))
                    (ref @#,racket[gref])]
         #:contracts ([vals any])]{
 Like @racket[pset!], but constrained to multiple @racket[gref]s.}

@defform[(shift! ref0 ref ... vals)
         #:grammar [(ref0 @#,racket[(gref #:arity #f)])
                    (ref @#,racket[(gref #:arity _number)])]
         #:contracts ([vals any])]{
 Shifts from right to left, that is, @tech{stores} the @tech{values}
 from the @math{n+1}th @tech/rep{reference} (including @racket[vals]
 as if it were one) to the @math{n}th @tech/rep{reference}.  The
 @tech/rep{arity} of @racket[ref0] is used as @var[number].

 @examples/gref[(define mpair (mcons 1 2))
                (code:gref
                 (shift! (#,mcar mpair) (#,mcdr mpair) 3))
                mpair]}

@defform[(rotate! ref0 ref ...+)
         #:grammar [(ref0 @#,racket[(gref #:arity #f)])
                    (ref @#,racket[(gref #:arity _number)])]]{
 Rotates from right to left (wrapping around), that is, @tech{stores}
 the @tech{values} from the @math{n+1}th (modulo @math{n})
 @tech/rep{reference} to the @math{n}th @tech/rep{reference}.  The
 @tech/rep{arity} of @racket[ref0] is used as @var[number].

 @examples/gref[(define mpair (mcons 1 2))
                (define val 3)
                (code:gref
                 (rotate! (#,mcar mpair) (#,mcdr mpair) val))
                val
                mpair]}

@defform*[[(call! proc ref arg ...)
           (call! proc ref arg ... . arg-list-expr)]
          #:grammar [(ref @#,racket[gref])
                     (arg (code:line @#,racket[keyword] arg-expr)
                          arg-expr)]
          #:contracts ([proc (unconstrained-domain-> any/c)]
                       [arg-expr any/c]
                       [arg-list-expr list?])]{
 Applies (see
 @secref[#:doc rkt-guide "Function_Calls__Procedure_Applications_"])
 @racket[proc] to the @tech/rep{value} @tech/rep{stored} in
 @racket[ref] and @racket[arg]s, then @tech{stores} the result to
 @racket[ref].  The form of @racket[arg]s is as in @racket[#%app].
 As in @racket[send], @racket[apply] is used when a non-parenthesized
 expression @racket[arg-list-expr] is present, otherwise
 @racket[#%app] is used.}

@defform*[[(call2! proc arg0-expr ref arg ...)
           (call2! proc arg0-expr ref arg ... . arg-list-expr)]
          #:grammar [(ref @#,racket[gref])
                     (arg (code:line @#,racket[keyword] arg-expr)
                          arg-expr)]
          #:contracts ([proc (unconstrained-domain-> any/c)]
                       [arg0-expr any/c]
                       [arg-expr any/c]
                       [arg-list-expr list?])]{
 Like @racket[call!], but with @racket[arg0-expr] as the first
 non-keyword argument.}

@defform[(inc! ref maybe-delta)
         #:grammar [(ref @#,racket[gref])
                    (maybe-delta (code:line) delta)]
         #:contracts ([delta number?])]{
 Increments the @tech/rep{value} @tech/rep{stored} in @racket[ref] by
 @racket[delta], which defaults to @racket[1].  That is, it is
 equivalent to @racket[(call! + ref delta)].}

@defform[(dec! ref maybe-delta)
         #:grammar [(ref @#,racket[gref])
                    (maybe-delta (code:line) delta)]
         #:contracts ([delta number?])]{
 Like @racket[inc!], but uses @racket[-] to decrement.}

@defform[(push! val ref)
         #:grammar [(ref @#,racket[gref])]
         #:contracts ([val any/c])]{
 Prepends @racket[val] to the @tech/rep{value} @tech/rep{stored} in
 @racket[ref].  That is, it is equivalent to
 @racket[(call2! cons val ref)].}

@defform[(mpush! val ref)
         #:grammar [(ref @#,racket[gref])]
         #:contracts ([val any/c])]{
 Like @racket[push!], but uses @racket[mcons].}

@defform[(pop! ref) #:grammar [(ref @#,racket[gref])]]{
 Takes the @racket[cdr] of the @tech/rep{value} @tech/rep{stored} in
 @racket[ref] and @tech{stores} the result back to @racket[ref].
 Returns the @racket[car] of the @tech{value} originally
 @tech/rep{stored} in @racket[ref].}

@defform[(mpop! ref) #:grammar [(ref @#,racket[gref])]]{
 Like @racket[pop!], but uses @racket[mcar] and @racket[mcdr].}

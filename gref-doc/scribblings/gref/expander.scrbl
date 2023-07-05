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
                     racket/contract/base
                     (except-in racket/base ... set!)
                     (only-in gref/base set!)
                     (only-space-in gref/set! gref/base)))

@title[#:tag "expander"]{@racket[set!] Expanders}

The provided @tech{@racket[set!] expanders} are defined through
@racket[define-set!-syntax] and @racket[make-set!-expander].  All
documented @tech{@racket[set!] expanders} preserve the apparent
@tech/rep{order} of the sub-expressions and accordingly validate the
results.  When an inapproapriate result is detected, the
@racket[exn:fail:contract] exception is @racket[raise]d.

@nested[#:style 'inset
        @defset![#:link-target? #f
                 (set! form val)
                 #:contracts ([val any])]]

indicates that @racket[form] is in a @deftech{@racket[set!] context},
where the @tech{@racket[set!] expander} is invoked and produces a
further @tech/rep{expanded} @tech/rep{reference}.  A
@tech{@racket[set!] context} is available in certain sub-form
positions of @tech{@racket[set!] expanders} and @tech{modify macros}
where a @tech/rep{reference} is explicitly required.  All documented
@tech{@racket[set!] expanders} extend the base Racket procedures, and
thus @tech[#:doc rkt-ref]{shadow} the corresponding bindings in the
@tech[#:doc rkt-ref]{default binding space} as provided by
@racketmodname[racket/base].

@defset![(set! (values ref ...) val)
         #:grammar [(ref @#,racket[gref])]
         #:contracts ([val any])]{
 Combines @racket[ref]s into a @tech/rep{reference} that @tech{stores}
 as many @tech{values} as there are @racket[ref]s.  Correspondingly,
 the @tech/rep{context} combines all @tech/rep{contexts}, the
 @tech/rep{reader} produces all @tech/rep{stored} @tech{values}, and
 the @tech/rep{writer} @tech{stores} to all @tech{locations}.}

@defset![(set! (mcar mpair) val)
         #:contracts ([mpair mpair?] [val any/c])]{
 @tech{Represents} the @racket[mcar] of @racket[mpair].}

@defset![(set! (mcdr mpair) val)
         #:contracts ([mpair mpair?] [val any/c])]{
 @tech{Represents} the @racket[mcdr] of @racket[mpair].}

@defset![(set! (hash-ref hash key failure-result) val)
         #:contracts ([hash hash?]
                      [key any/c]
                      [failure-result failure-result/c]
                      [val any/c])]{
 @tech{Represents} the association for @racket[key] in @racket[hash].
 Note that @racket[failure-result] is ignored by the
 @tech/rep{writer}.  The @tech/rep{writer} further requires
 @racket[hash] to be @racket[(not/c immutable?)].}

@defset![(set! (bytes-ref bytes pos) val)
         #:contracts ([bytes bytes?]
                      [pos exact-nonnegative-integer?]
                      [val byte?])]{
 @tech{Represents} the @racket[pos]th position of @racket[bytes].  The
 @tech/rep{writer} further requires @racket[bytes] to be
 @racket[(not/c immutable?)].}

@defset![(set! (string-ref string pos) val)
         #:contracts ([string string?]
                      [pos exact-nonnegative-integer?]
                      [val char?])]{
 @tech{Represents} the @racket[pos]th position of @racket[string].
 The @tech/rep{writer} further requires @racket[string] to be
 @racket[(not/c immutable?)].}

@defset![(set! (vector-ref vector pos) val)
         #:contracts ([vector vector?]
                      [pos exact-nonnegative-integer?]
                      [val any/c])]{
 @tech{Represents} the @racket[pos]th position of @racket[vector].
 The @tech/rep{writer} further requires @racket[vector] to be
 @racket[(not/c immutable?)].}

@defset![(set! (vector*-ref vector pos) val)
         #:contracts ([vector (and/c vector? (not/c impersonator?))]
                      [pos exact-nonnegative-integer?]
                      [val any/c])]{
 Like @racket/set![vector-ref], but constrained to
 @racket[(not/c impersonator?)] vectors.}

@defset![(set! (unbox box) val)
         #:contracts ([box box?] [val any/c])]{
 @tech{Represents} the content of @racket[box].  The
 @tech/rep{writer} further requires @racket[box] to be
 @racket[(not/c immutable?)].}

@defset![(set! (unbox* box) val)
         #:contracts ([box (and/c box? (not/c impersonator?))]
                      [val any/c])]{
 Like @racket/set![unbox], but constrained to
 @racket[(not/c impersonator?)] boxes.}

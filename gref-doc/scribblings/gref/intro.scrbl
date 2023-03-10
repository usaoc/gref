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
@(require scribblings/gref/bib
          scribblings/gref/example
          scribblings/gref/lib
          scribblings/gref/tech
          (for-label gref/base
                     racket/contract/base
                     (except-in syntax/parse integer)
                     (prefix-in srfi- srfi/17)
                     (subtract-in (except-in racket/base ...) gref)))

@title[#:tag "introduction"]{Introduction}

What does it mean for something to be @tech/rep{stored}?  To account
for this, imperative languages have long adopted the concept of
@deftech{l-values} since @citet[fun-con-pl].  For example, consider
the following @other-doc[@lib-path["algol60/algol60"]] program:

@codeblock[#:keep-lang-line? #f]{
 #lang algol60
 begin
   integer array foo[0:0];
   foo[0] := 1;
   printnln(foo[0])
 end}

Above, @tt{foo[0]} is an @tech{l-value} that @tech{represents} a
@tech{location}, and thus can be both read and write.  The concept of
@deftech{locations} is already defined in Racket (see
@secref[#:doc rkt-ref "vars-and-locs"]), so it is not difficult to
imagine a concept similar to @tech{l-values}.  Indeed,
@tech{generalized references}, also known as @tech/rep{places}, are
provided by Lisp Machine Lisp--inspired Lisps.

The concept of @tech{generalized references} originates from
@citet[lisp-mac], and has since been implemented for
@hyperlink["https://hanshuebner.github.io/lmman/fd-eva.xml"]{Lisp Machine Lisp},
@hyperlink["http://www.maclisp.info/pitmanual/setf.html"]{MacLisp},
@hyperlink["http://www.lispworks.com/documentation/HyperSpec/Body/05_a.htm"]{Common Lisp},
and
@hyperlink["https://www.gnu.org/software/emacs/manual/html_node/elisp/Generalized-Variables.html"]{Emacs Lisp}.
For a detailed discussion on the history of
@tech{generalized references}, see @citet[evo-of-lisp].  This section
focuses on the technical aspects of @tech{generalized references}.

The simplest implementation of @tech{generalized references} is as in
@hyperlink[@collection-file-path["srfi-17.html" "srfi" "scribblings" "srfi-std"]]{SRFI 17},
most faithfully resembling the original proposal by @citet[lisp-mac].
That is, a @deftech{getter procedure} can be associated with a
@deftech{setter procedure}, where

@racketblock[(srfi-set! (_proc _arg ...) _val)]

is defined as

@racketblock[((_setter _proc) _arg ... _val)]

such that @var[setter] maps the @tech/rep{getter} to
@tech/rep{setter}.  This is a simple yet elegant design.
Unfortunately, more complicated @tech/rep{accesses} require a more
expressive design.  For this reason, Gref has instead adopted the
full-blown @tech[#:doc rkt-ref]{macro} approach of Lisp Machine Lisp,
which is backward-compatible with the SRFI-17 approach.  In Gref, a
@deftech{generalized reference} corresponding to some @tech{locations}
where some @tech{values} are @deftech/rep{stored} consists of four
items:

@itemlist[
 #:style 'ordered
 @item{
  A sequence of bindings in the form of
  @racket[([(id ...) expr] ...)] as in @racket[let-values] that set up
  the @deftech{lexical context};}
 @item{
  A sequence of @deftech{store variables}, that is, @racket[id]s that
  hold the updated @tech{values};}
 @item{
  A @deftech{reader expression} that evaluates to the @tech{values}
  @tech/rep{stored} in the @tech{locations} within the
  @tech/rep{context};}
 @item{
  A @deftech{writer expression} that @tech{stores} the update
  @tech{values} held by the @tech/rep{variables} to the
  @tech{locations} within the @tech/rep{context}.}]

The purpose of singling out the @tech/rep{context} is such that the
two modes of @deftech/rep{accesses}, that is, reads and writes can be
separated from the evaluation and validation of sub-expressions.  This
way, multiple @tech/rep{accesses} can be performed at once while
preserving the apparent left-to-right @deftech{evaluation order} (see
@secref[#:doc rkt-guide "Evaluation_Order_and_Arity"]).

Another technical advantage is that a @tech/rep{reference} is allowed
to @deftech{represent} multiple @tech{locations}, including none.  The
number of @tech/rep{variables} determines the number of
@deftech{values} @tech/rep{stored} in the @tech{locations}.  A
@deftech{well-behaved} @tech/rep{reference} must arrange the
@tech/rep{reader} and @tech/rep{writer} such that the former evaluates
to as many @tech{values} as @tech/rep{stored} in the @tech{locations},
and the latter @tech{stores} as many to the @tech{locations}.  The
results of the @tech/rep{writer} are unspecified, but they should
generally be @void-const following Racket's convention (see
@secref[#:doc rkt-guide "void+undefined"]).

@examples/gref[#:label @elem{
                As an example, consider the @tech{modify macro}
                @racket[call!]:}
               (define box-of-box
                 (box (box #hasheq((foo . "bar")))))
               (call! hash-update (unbox* (unbox* box-of-box))
                      'foo (compose1 string->symbol string-upcase))
               box-of-box]

Before the @tech/rep{accesses} are performed, the inner
@racket[unbox*] expression is evaluated exactly once and validated to
be @racket[(and/c box? (not/c impersonator?))].  Then, the
@tech/rep{accesses} are performed without unnecessary repeated
evaluation.  This capability further enables @tech{modify macros}
like @racket[shift!] and @racket[rotate!].

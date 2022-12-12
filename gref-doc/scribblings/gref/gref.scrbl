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

@(require scribblings/gref/bib
          scribblings/gref/include
          scribblings/gref/lib
          scribblings/gref/tech)

@title{Gref: Generalized References for Racket}
@author{Wing Hei Chan}

Gref is a proof-of-concept implementation of
@tech{generalized references} for Racket.  It is intended as a
showcase for Racket's language extension capabilities.  For practical
purposes, one is reminded of Racket's general discouragement of
assignments (see @secref[#:doc rkt-guide "using-set!"]).  For the
alternative approach of @deftech{functional references}, also
known as @tech/rep{optics}, see @other-doc[@lib-path["lens"]] and
@other-doc[@lib-path["glass"]].

@defmodule[gref]
@margin-note{
 The @racketmodname[gref] library combines @racketmodname[gref/base]
 and @racketmodname[gref/syntax].}

@table-of-contents[]

@include/gref[intro base syntax]

@generate-bibliography[]

@index-section[]

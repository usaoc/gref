#lang info
;; Copyright (C) 2022 Wing Hei Chan

;; Copying and distribution of this file, with or without
;; modification, are permitted in any medium without royalty provided
;; the copyright notice and this notice are preserved.  This file is
;; offered as-is, without any warranty.

(define collection 'multi)
(define deps '("base"))
(define build-deps
  '("scribble-lib"
    "gref-lib" "srfi-lib"
    "algol60" "glass" "lens-doc" "racket-doc" "srfi-doc"))
(define pkg-authors '("whmunkchan@outlook.com"))
(define pkg-desc "Documentation part of \"gref\"")
(define license 'GPL-3.0-or-later)

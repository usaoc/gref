#lang info
;; Copyright (C) 2022 Wing Hei Chan

;; Copying and distribution of this file, with or without
;; modification, are permitted in any medium without royalty provided
;; the copyright notice and this notice are preserved.  This file is
;; offered as-is, without any warranty.

(define collection 'multi)
(define build-deps
  '("base"
    "doc-coverage" "expect" "rackunit-lib" "rackunit-spec"
    "gref-doc" "gref-lib"))
(define pkg-authors '("whmunkchan@outlook.com"))
(define pkg-desc "Tests for \"gref\"")
(define license 'GPL-3.0-or-later)

#lang racket/base
;; Copyright (C) 2022 Wing Hei Chan

;; Copying and distribution of this file, with or without
;; modification, are permitted in any medium without royalty provided
;; the copyright notice and this notice are preserved.  This file is
;; offered as-is, without any warranty.

(require doc-coverage
         rackunit/spec)

(describe "gref/base"
  (it "is fully documented"
    (local-require gref/base)
    (check-all-documented 'gref/base)))

(describe "gref/syntax"
  (it "is fully documented"
    (local-require gref/syntax)
    (check-all-documented 'gref/syntax)))

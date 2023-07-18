#lang racket/base
;; Copyright (C) 2022 Wing Hei Chan

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

(require gref/base
         racket/fixnum
         racket/string
         rackunit/spec
         tests/gref/helper)

(describe "variable"
  (it "acts as a reference"
    (define-values (a b c d) (values 1 2 3 4))
    (rotate! a b c d)
    (check-values (2 3 4 1) (values a b c d))))

(describe "values"
  (context "given subreferences"
    (it "combines single subreference trivially"
      (define-values (a b) (values 1 2))
      (rotate! (values a) (values b))
      (check-values (2 1) (values a b)))
    (it "combines multiple subreferences"
      (define-values (a b c d) (values 1 2 3 4))
      (rotate! (values a b) (values c d))
      (check-values (3 4 1 2) (values a b c d)))))

(describe "mcar"
  (context "given a mpair"
    (it "acts as a reference"
      (define mpair (mcons 1 'ignored))
      (call! add1 (mcar mpair))
      (check-value (mcons 2 'ignored) mpair)))
  (context "given a non mpair"
    (it "raises a contract violation"
      (define (check non-mpair)
        (check-raise exn:fail:contract
          (call! values (mcar non-mpair))))
      (check 'non-mpair)
      (check (cons 1 2)))))

(describe "mcdr"
  (context "given a mpair"
    (it "acts as a reference"
      (define mpair (mcons 'ignored 1))
      (call! add1 (mcdr mpair))
      (check-value (mcons 'ignored 2) mpair)))
  (context "given a non mpair"
    (it "raises a contract violation"
      (define (check non-mpair)
        (check-raise exn:fail:contract
          (call! values (mcdr non-mpair))))
      (check 'non-mpair)
      (check '(1 . 2)))))

(describe "hash-ref"
  (context "given a mutable hash"
    (it "acts as a reference"
      (define table (make-hasheq '((a . a))))
      (rotate! (hash-ref table 'a) (hash-ref table 'b 'b))
      (check-value #hasheq((a . b) (b . a)) table)))
  (context "given a non hash"
    (it "raises a contract violation"
      (define (check non-hash)
        (check-raise exn:fail:contract
          (call! values (hash-ref non-hash 'ignored 'ignored))))
      (check 'non-hash)
      (check '((a . a)))))
  (context "given a non failure result"
    (it "raises a contract violation"
      (define table (make-hasheq))
      (define (check non-fail-res)
        (check-raise exn:fail:contract
          (call! values (hash-ref table 'ignored non-fail-res))))
      (check (lambda _args (values)))
      (check (procedure-reduce-arity (lambda _args 'ignored)
                                     (arity-at-least 1)))))
  (context "given an immutable hash"
    (it "raises a contract violation"
      (define table #hasheq())
      (check-raise exn:fail:contract
        (call! values (hash-ref table 'ignored 'ignored))))))

(describe "bytes-ref"
  (context "given a mutable bytes"
    (it "acts as a reference"
      (define bstr (bytes 1 2))
      (rotate! (bytes-ref bstr 0) (bytes-ref bstr 1))
      (check-value #"\2\1" bstr)))
  (context "given a non bytes"
    (it "raises a contract violation"
      (define (check non-bytes)
        (check-raise exn:fail:contract
          (call! values (bytes-ref non-bytes 0))))
      (check 'non-bytes)
      (check (string #\a #\b))))
  (context "given an empty bytes"
    (it "raises a contract violation"
      (define bstr (bytes))
      (check-raise exn:fail:contract
        (call! values (bytes-ref bstr 0)))))
  (context "given a non index"
    (it "raises a contract violation"
      (define bstr (bytes 1 2))
      (define (check non-idx)
        (check-raise exn:fail:contract
          (call! values (bytes-ref bstr non-idx))))
      (check 'non-idx)
      (check -1)))
  (context "given an out-of-buond index"
    (it "raises a contract violation"
      (define bstr (bytes 1 2))
      (define (check out-of-bound-idx)
        (check-raise exn:fail:contract
          (call! values (bytes-ref bstr out-of-bound-idx))))
      (check (bytes-length bstr))
      (check (add1 (most-positive-fixnum)))))
  (context "given a non byte"
    (it "raises a contract violation"
      (define bstr (bytes 1 2))
      (define (check non-byte)
        (check-raise exn:fail:contract
          (set! (bytes-ref bstr 0) non-byte)))
      (check 'non-byte)
      (check #\a)))
  (context "given an immutable bytes"
    (it "raises a contract violation"
      (define bstr #"\1\2")
      (check-raise exn:fail:contract
        (call! values (bytes-ref bstr 0))))))

(describe "string-ref"
  (context "given a mutable string"
    (it "acts as a reference"
      (define str (string #\a #\b))
      (rotate! (string-ref str 0) (string-ref str 1))
      (check-value "ba" str)))
  (context "given a non string"
    (it "raises a contract violation"
      (define (check non-string)
        (check-raise exn:fail:contract
          (call! values (string-ref non-string 0))))
      (check 'non-string)
      (check (bytes 1 2))))
  (context "given an empty string"
    (it "raises a contract violation"
      (define str (string))
      (check-raise exn:fail:contract
        (call! values (string-ref str 0)))))
  (context "given a non index"
    (it "raises a contract violation"
      (define str (string #\a #\b))
      (define (check non-idx)
        (check-raise exn:fail:contract
          (call! values (string-ref str non-idx))))
      (check 'non-idx)
      (check -1)))
  (context "given an out-of-bound index"
    (it "raises a contract violation"
      (define str (string #\a #\b))
      (define (check out-of-bound-idx)
        (check-raise exn:fail:contract
          (call! values (string-ref str out-of-bound-idx))))
      (check (string-length str))
      (check (add1 (most-positive-fixnum)))))
  (context "given a non character"
    (it "raises a contract violation"
      (define str (string #\a #\b))
      (define (check non-char)
        (check-raise exn:fail:contract
          (set! (string-ref str 0) non-char)))
      (check 'non-char)
      (check 1)))
  (context "given an immutable string"
    (it "raises a contract violation"
      (define str "ab")
      (check-raise exn:fail:contract
        (call! values (string-ref str 0))))))

(define (impersonator-vector . vals)
  (define (ref+set! _vec _pos val) val)
  (impersonate-vector (apply vector vals) ref+set! ref+set!))

(describe "vector-ref"
  (context "given a mutable vector"
    (it "acts as a reference"
      (define vec (impersonator-vector 1 2))
      (rotate! (vector-ref vec 0) (vector-ref vec 1))
      (check-value #(2 1) vec)))
  (context "given a non vector"
    (it "raises a contract violation"
      (define (check non-vec)
        (check-raise exn:fail:contract
          (call! values (vector-ref non-vec 0))))
      (check 'non-vec)
      (check (list 1 2))))
  (context "given an empty vector"
    (it "raises a contract violation"
      (define vec (impersonator-vector))
      (check-raise exn:fail:contract
        (call! values (vector-ref vec 0)))))
  (context "given a non index"
    (it "raises a contract violation"
      (define vec (impersonator-vector 1 2))
      (define (check non-idx)
        (check-raise exn:fail:contract
          (call! values (vector-ref vec non-idx))))
      (check 'non-idx)
      (check -1)))
  (context "given an out-of-bound index"
    (it "raises a contract violation"
      (define vec (impersonator-vector 1 2))
      (define (check out-of-bound-idx)
        (check-raise exn:fail:contract
          (call! values (vector-ref vec out-of-bound-idx))))
      (check (vector-length vec))
      (check (add1 (most-positive-fixnum)))))
  (context "given an immutable vector"
    (it "raises a contract violation"
      (define vec #(1 2))
      (check-raise exn:fail:contract
        (call! values (vector-ref vec 0))))))

(describe "vector*-ref"
  (context "given a mutable vector"
    (it "acts as a reference"
      (define vec* (vector 1 2))
      (rotate! (vector*-ref vec* 0) (vector*-ref vec* 1))
      (check-value #(2 1) vec*)))
  (context "given a non vector*"
    (it "raises a contract violation"
      (define (check non-vec*)
        (check-raise exn:fail:contract
          (call! values (vector*-ref non-vec* 0))))
      (check 'non-vec*)
      (check (list 1 2))
      (check (impersonator-vector 1 2))))
  (context "given an empty vector*"
    (it "raises a contract violation"
      (define vec* (vector))
      (check-raise exn:fail:contract
        (call! values (vector*-ref vec* 0)))))
  (context "given a non index"
    (it "raises a contract violation"
      (define vec* (vector 1 2))
      (define (check non-idx)
        (check-raise exn:fail:contract
          (call! values (vector*-ref vec* non-idx))))
      (check 'non-idx)
      (check -1)))
  (context "given an out-of-bound index"
    (it "raises a contract violation"
      (define vec* (vector 1 2))
      (define (check out-of-bound-idx)
        (check-raise exn:fail:contract
          (call! values (vector*-ref vec* out-of-bound-idx))))
      (check (vector*-length vec*))
      (check (add1 (most-positive-fixnum)))))
  (context "given an immutable vector*"
    (it "raises a contract violation"
      (define vec* #(1 2))
      (check-raise exn:fail:contract
        (call! values (vector*-ref vec* 0))))))

(define (impersonator-box val)
  (define (ref+set! _bx val) val)
  (impersonate-box (box val) ref+set! ref+set!))

(describe "unbox"
  (context "given a mutable box"
    (it "acts as a reference"
      (define bx1 (impersonator-box 1))
      (define bx2 (impersonator-box 2))
      (rotate! (unbox bx1) (unbox bx2))
      (check-values (#&2 #&1) (values bx1 bx2))))
  (context "given a non box"
    (it "raises a contract violation"
      (define (check non-bx)
        (check-raise exn:fail:contract
          (call! values (unbox non-bx))))
      (check 'non-bx)
      (check (list 1))))
  (context "given an immutable box"
    (it "raises a contract violation"
      (define bx #&1)
      (check-raise exn:fail:contract
        (call! values (unbox bx))))))

(describe "unbox*"
  (context "given a mutable box"
    (it "acts as a reference"
      (define bx*1 (box 1))
      (define bx*2 (box 2))
      (rotate! (unbox* bx*1) (unbox* bx*2))
      (check-values (#&2 #&1) (values bx*1 bx*2))))
  (context "given a non box*"
    (it "raises a contract violation"
      (define (check non-bx*)
        (check-raise exn:fail:contract
          (call! values (unbox* non-bx*))))
      (check 'non-bx*)
      (check (list 1))
      (check (impersonator-box 1))))
  (context "given an immutable box*"
    (it "raises a contract violation"
      (define bx* #&1)
      (check-raise exn:fail:contract
        (call! values (unbox* bx*))))))

(describe "set!"
  (context "given a reference"
    (define bx1 (box 1))
    (define bx2 (box 2))
    (it "returns void"
      (check-type void?
        (set! (values (unbox* bx1) (unbox* bx2))
              (values (unbox* bx2) (unbox* bx1)))))
    (it "sets it"
      (check-values (#&2 #&1) (values bx1 bx2))))
  (context "given multiple references"
    (define-values (a b c d) (values 1 2 3 4))
    (it "returns void"
      (check-type void?
        (set! (values a b) (values c d)
              (values c d) (values a b))))
    (it "sets them sequentially"
      (check-values (3 4 3 4) (values a b c d))))
  (context "given no reference"
    (it "raises a syntax error"
      (check-expand exn:fail:syntax (set!))))
  (context "given unmatched arity"
    (it "raises an arity mismatch"
      (check-raise exn:fail:contract:arity
        (set! (values) 'ignored)))))

(describe "set!-values"
  (context "given a group of references"
    (define bx1 (box 1))
    (define bx2 (box 2))
    (it "returns void"
      (check-type void?
        (set!-values ((unbox* bx1) (unbox* bx2))
                     (values (unbox* bx2) (unbox* bx1)))))
    (it "sets them"
      (check-values (#&2 #&1) (values bx1 bx2))))
  (context "given multiple groups of references"
    (define-values (a b c d) (values 1 2 3 4))
    (it "returns void"
      (check-type void?
        (set!-values (a b) (values c d)
                     (c d) (values a b))))
    (it "sets them sequentially"
      (check-values (3 4 3 4) (values a b c d))))
  (context "given no groups of references"
    (it "raises a syntax error"
      (check-expand exn:fail:syntax (set!-values))))
  (context "given unmatched arity"
    (it "raises an arity mismatch"
      (check-raise exn:fail:contract:arity
        (set!-values () 'ignored))))
  (context "given unexpected arity"
    (it "raises a syntax error"
      (check-expand exn:fail:syntax
        (set!-values ((values)) 'ignored)))))

(describe "pset!"
  (context "given a reference"
    (define bx1 (box 1))
    (define bx2 (box 2))
    (it "returns void"
      (check-type void?
        (pset! (values (unbox* bx1) (unbox* bx2))
               (values (unbox* bx2) (unbox* bx1)))))
    (it "sets it"
      (check-values (#&2 #&1) (values bx1 bx2))))
  (context "given multiple references"
    (define-values (a b c d) (values 1 2 3 4))
    (it "returns void"
      (check-type void?
        (pset! (values a b) (values c d)
               (values c d) (values a b))))
    (it "sets them parallelly"
      (check-values (3 4 1 2) (values a b c d))))
  (context "given no reference"
    (it "raises a syntax error"
      (check-expand exn:fail:syntax (pset!))))
  (context "given unmatched arity"
    (it "raises an arity mismatch"
      (check-raise exn:fail:contract:arity
        (pset! (values) 'ignored)))))

(describe "pset!-values"
  (context "given a group of references"
    (define bx1 (box 1))
    (define bx2 (box 2))
    (it "returns void"
      (check-type void?
        (pset!-values ((unbox* bx1) (unbox* bx2))
                      (values (unbox* bx2) (unbox* bx1)))))
    (it "sets them"
      (check-values (#&2 #&1) (values bx1 bx2))))
  (context "given multiple groups of references"
    (define-values (a b c d) (values 1 2 3 4))
    (it "returns void"
      (check-type void?
        (pset!-values (a b) (values c d)
                      (c d) (values a b))))
    (it "sets them parallelly"
      (check-values (3 4 1 2) (values a b c d))))
  (context "given no groups of references"
    (it "raises a syntax error"
      (check-expand exn:fail:syntax (pset!-values))))
  (context "given unmatched arity"
    (it "raises an arity mismatch"
      (check-raise exn:fail:contract:arity
        (pset!-values () 'ignored))))
  (context "given unexpected arity"
    (it "raises a syntax error"
      (check-expand exn:fail:syntax
        (pset!-values ((values)) 'ignored)))))

(describe "shift!"
  (context "given a reference"
    (define bx (box 1))
    (it "returns the shifted value"
      (check-value 1 (shift! (unbox* bx) 2)))
    (it "shifts it"
      (check-value #&2 bx)))
  (context "given multiple references"
    (define bx (box 1))
    (define mpair (mcons 2 3))
    (it "returns the shifted value"
      (check-value 1
        (shift! (unbox* bx) (mcar mpair) (mcdr mpair) 4)))
    (it "shifts from right to left"
      (check-values (#&2 (mcons 3 4)) (values bx mpair))))
  (context "given unexpected arity"
    (it "raises a syntax error"
      (define val 'ignored)
      (check-expand exn:fail:syntax
        (shift! (values) val 'ignored))))
  (context "given unmatched arity"
    (it "raises an arity mismatch"
      (check-raise exn:fail:contract:arity
        (shift! (values) 'ignored)))))

(describe "rotate!"
  (context "given a pair of references"
    (define bx (box 1))
    (define val 2)
    (it "returns void"
      (check-type void? (rotate! (unbox* bx) val)))
    (it "rotates them"
      (check-values (#&2 1) (values bx val))))
  (context "given multiple references"
    (define bx (box 1))
    (define mpair (mcons 2 3))
    (define val 4)
    (it "returns void"
      (check-type void?
        (rotate! (unbox* bx) (mcar mpair) (mcdr mpair) val)))
    (it "rotates from right to left (wrapping around)"
      (check-values (#&2 (mcons 3 4) 1) (values bx mpair val))))
  (context "given unexpected arity"
    (it "raises a syntax error"
      (define val 'ignored)
      (check-expand exn:fail:syntax
        (rotate! (values) val)))))

(describe "call!"
  (context "given a plain application"
    (define bx (box 1))
    (it "returns void"
      (check-type void? (call! add1 (unbox* bx))))
    (it "stores the result"
      (check-value #&2 bx)))
  (context "given an application with keyword arguments"
    (define bx (box "...foo..."))
    (it "returns void"
      (check-type void?
        (call! string-trim (unbox* bx) "." #:left? #f #:repeat? #t)))
    (it "stores the result"
      (check-value #&"...foo" bx)))
  (context "given an application with rest argument"
    (define bx (box "...foo..."))
    (define rest-arg (list "..." ":::"))
    (it "returns void"
      (check-type void?
        (call! string-replace (unbox* bx) #:all? #f . rest-arg)))
    (it "stores the result"
      (check-value bx #&":::foo...")))
  (context "given unexpected arity"
    (it "raises a syntax error"
      (check-expand exn:fail:syntax
        (call! (lambda _args 'ignored) (values)))))
  (context "given unmatched arity"
    (it "raises a contract violation"
      (define val 'ignored)
      (define (check proc)
        (check-raise exn:fail:contract:arity (call! proc val)))
      (check (lambda () 'ignored))
      (check (lambda _args (values)))))
  (context "given a non procedure"
    (it "raises a contract violation"
      (define val 'ignored)
      (define (check non-proc)
        (check-raise exn:fail:contract (call! non-proc val)))
      (check 'non-proc)
      (check '(lambda _args 'ignored))))
  (context "given a non list rest argument"
    (it "raises a contract violation"
      (define val 'ignored)
      (define (check non-list)
        (check-raise exn:fail:contract
          (call! (lambda _args 'ignored) val . non-list)))
      (check 'non-list)
      (check #(1 2 3)))))

(describe "call2!"
  (context "given a plain application"
    (define bx (box 1))
    (it "returns void"
      (check-type void? (call2! + 1 (unbox* bx))))
    (it "stores the result"
      (check-value #&2 bx)))
  (context "given an application with keyword arguments"
    (define bx (box "."))
    (it "returns void"
      (check-type void?
        (call2! string-trim "...foo..." (unbox* bx)
                #:left? #f #:repeat? #t)))
    (it "stores the result"
      (check-value #&"...foo" bx)))
  (context "given an application with rest argument"
    (define bx (box "..."))
    (define rest-arg (list ":::"))
    (it "returns void"
      (check-type void?
        (call2! string-replace "...foo..." (unbox* bx)
                #:all? #f . rest-arg)))
    (it "stores the result"
      (check-value bx #&":::foo...")))
  (context "given unexpected arity"
    (it "raises a syntax error"
      (check-expand exn:fail:syntax
        (call2! (lambda _args 'ignored) 'ignored (values)))))
  (context "given unmatched arity"
    (it "raises a contract violation"
      (define val 'ignored)
      (define (check proc)
        (check-raise exn:fail:contract:arity
          (call2! proc 'ignored val)))
      (check (lambda () 'ignored))
      (check (lambda _args (values)))))
  (context "given a non procedure"
    (it "raises a contract violation"
      (define val 'ignored)
      (define (check non-proc)
        (check-raise exn:fail:contract
          (call2! non-proc 'ignored val)))
      (check 'non-proc)
      (check '(lambda _args 'ignored))))
  (context "given a non list rest argument"
    (it "raises a contract violation"
      (define val 'ignored)
      (define (check non-list)
        (check-raise exn:fail:contract
          (call2! (lambda _args 'ignored) 'ignored val . non-list)))
      (check 'non-list)
      (check #(1 2 3)))))

(describe "inc!"
  (context "given no number"
    (define bx (box 1))
    (it "returns void"
      (check-type void? (inc! (unbox* bx))))
    (it "increments by 1"
      (check-value #&2 bx)))
  (context "given a number"
    (define bx (box 1))
    (it "returns void"
      (check-type void? (inc! (unbox* bx) -1/2+1/2i)))
    (it "increments by it"
      (check-value #&1/2+1/2i bx)))
  (context "given unexpected arity"
    (it "raises a syntax error"
      (check-expand exn:fail:syntax (inc! (values)))))
  (context "given unmatched arity"
    (it "raises an arity mismatch"
      (define num 1)
      (check-raise exn:fail:contract:arity (inc! num (values)))))
  (context "given a non number reference"
    (it "raises a contract violation"
      (define non-num 'non-num)
      (check-raise exn:fail:contract (inc! non-num))))
  (context "given a non number delta"
    (it "raises a contract violation"
      (define num 1)
      (define (check non-num)
        (check-raise exn:fail:contract (inc! num non-num)))
      (check 'non-num)
      (check "1"))))

(describe "dec!"
  (context "given no number"
    (define bx (box -1))
    (it "returns void"
      (check-type void? (dec! (unbox* bx))))
    (it "decrements by 1"
      (check-value #&-2 bx)))
  (context "given a number"
    (define bx (box -1))
    (it "returns void"
      (check-type void? (dec! (unbox* bx) -1/2+1/2i)))
    (it "decrements by it"
      (check-value #&-1/2-1/2i bx)))
  (context "given unexpected arity"
    (it "raises a syntax error"
      (check-expand exn:fail:syntax (dec! (values)))))
  (context "given unmatched arity"
    (it "raises an arity mismatch"
      (define num -1)
      (check-raise exn:fail:contract:arity (dec! num (values)))))
  (context "given a non number reference"
    (it "raises a contract violation"
      (define non-num 'non-num)
      (check-raise exn:fail:contract (dec! non-num))))
  (context "given a non number delta"
    (it "raises a contract violation"
      (define num -1)
      (define (check non-num)
        (check-raise exn:fail:contract (dec! num non-num)))
      (check 'non-num)
      (check "-1"))))

(describe "push!"
  (context "given a list reference"
    (define bx (box '()))
    (it "returns void"
      (check-type void? (push! 1 (unbox* bx))))
    (it "pushes to it"
      (check-value #&(1) bx)))
  (context "given a non list reference"
    (define bx (box 2))
    (it "returns void"
      (check-type void? (push! 1 (unbox* bx))))
    (it "pushes to it"
      (check-value #&(1 . 2) bx)))
  (context "given unexpected arity"
    (it "raises a syntax error"
      (check-expand exn:fail:syntax (push! 'ignored (values)))))
  (context "given unmatched arity"
    (it "raises an arity mismatch"
      (define val 'ignored)
      (check-raise exn:fail:contract:arity (push! (values) val)))))

(define (mlist . vals)
  (for/foldr ([mlst '()])
             ([val (in-list vals)])
    (mcons val mlst)))

(describe "mpush!"
  (context "given a mlist reference"
    (define bx (box '()))
    (it "returns void"
      (check-type void? (mpush! 1 (unbox* bx))))
    (it "pushes to it"
      (check-value (box (mlist 1)) bx)))
  (context "given a non mlist reference"
    (define bx (box 2))
    (it "returns void"
      (check-type void? (mpush! 1 (unbox* bx))))
    (it "pushes to it"
      (check-value (box (mcons 1 2)) bx)))
  (context "given unexpected arity"
    (it "raises a syntax error"
      (check-expand exn:fail:syntax
        (mpush! 'ignored (values)))))
  (context "given unmatched arity"
    (it "raises an arity mismatch"
      (define val 'ignored)
      (check-raise exn:fail:contract:arity
        (mpush! (values) val)))))

(describe "pop!"
  (context "given a list reference"
    (define bx (box (list 1 2)))
    (it "returns the popped value"
      (check-value 1 (pop! (unbox* bx))))
    (it "pops it"
      (check-value #&(2) bx)))
  (context "given a pair reference"
    (define bx (box (cons 1 2)))
    (it "returns the popped value"
      (check-value 1 (pop! (unbox* bx))))
    (it "pops it"
      (check-value #&2 bx)))
  (context "given unexpected arity"
    (it "raises a syntax error"
      (check-expand exn:fail:syntax (pop! (values)))))
  (context "given a non pair reference"
    (it "raises a contract violation"
      (define non-pair (mcons 1 2))
      (check-raise exn:fail:contract (pop! non-pair)))))

(describe "mpop!"
  (context "given a mlist reference"
    (define bx (box (mlist 1 2)))
    (it "returns the popped value"
      (check-value 1 (mpop! (unbox* bx))))
    (it "pops it"
      (check-value (box (mlist 2)) bx)))
  (context "given a mpair reference"
    (define bx (box (mcons 1 2)))
    (it "returns the popped value"
      (check-value 1 (mpop! (unbox* bx))))
    (it "pops it"
      (check-value #&2 bx)))
  (context "given unexpected arity"
    (it "raises a syntax error"
      (check-expand exn:fail:syntax (mpop! (values)))))
  (context "given a non mpair reference"
    (it "raises a contract violation"
      (define non-mpair (cons 1 2))
      (check-raise exn:fail:contract (mpop! non-mpair)))))

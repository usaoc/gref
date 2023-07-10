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

(require expect
         expect/rackunit
         gref/base
         racket/string
         (only-in rackunit test-case))

(test-case "variable"
  (check-expect (lambda ()
                  (let ([a 1] [b 2] [c 3] [d 4])
                    (rotate! a b c d)
                    (values a b c d)))
                (expect-return 2 3 4 1)))

(test-case "values"
  (check-expect (lambda ()
                  (let ([a 1] [b 2])
                    (rotate! (values a) (values b))
                    (values a b)))
                (expect-return 2 1))
  (check-expect (lambda ()
                  (let ([a 1] [b 2] [c 3] [d 4])
                    (rotate! (values a b) (values c d))
                    (values a b c d)))
                (expect-return 3 4 1 2)))

(test-case "mcons"
  (check-expect (let ([mpair (mcons 1 2)])
                  (rotate! (mcar mpair) (mcdr mpair))
                  mpair)
                (mcons 2 1))
  (check-exn exn:fail:contract?
    (lambda () (let ([num 1]) (set! (mcar num) 2))))
  (check-exn exn:fail:contract?
    (lambda () (let ([num 1]) (set! (mcdr num) 2)))))

(test-case "hash"
  (check-expect (let ([table (make-hasheq '((a . a)))])
                  (rotate! (hash-ref table 'a)
                           (hash-ref table 'b 'b))
                  table)
                #hasheq((a . b) (b . a)))
  (check-exn exn:fail:contract?
    (lambda () (let ([num 1]) (set! (hash-ref num 'a) 'b))))
  (check-exn exn:fail:contract?
    (lambda ()
      (let ([table (make-hasheq '((a . a)))])
        (rotate! (hash-ref table 'a) (hash-ref table 'b values)))))
  (check-exn exn:fail:contract?
    (lambda ()
      (let ([table #hasheq((a . a))])
        (set! (hash-ref table 'a) 'b)))))

(test-case "bytes"
  (check-expect (let ([bstr (bytes 1 2)])
                  (rotate! (bytes-ref bstr 0) (bytes-ref bstr 1))
                  bstr)
                #"\2\1")
  (check-exn exn:fail:contract?
    (lambda () (let ([num 1]) (set! (bytes-ref num 0) 3))))
  (check-exn exn:fail:contract?
    (lambda () (let ([bstr (bytes)]) (set! (bytes-ref bstr 0) 1))))
  (check-exn exn:fail:contract?
    (lambda ()
      (let ([bstr (bytes 1 2)]) (set! (bytes-ref bstr 'not-idx) 3))))
  (check-exn exn:fail:contract?
    (lambda ()
      (let ([bstr (bytes 1 2)]) (set! (bytes-ref bstr 2) 3))))
  (check-exn exn:fail:contract?
    (lambda ()
      (let ([bstr (bytes 1 2)])
        (set! (bytes-ref bstr 0) 'not-byte))))
  (check-exn exn:fail:contract?
    (lambda () (let ([bstr #"\1\2"]) (set! (bytes-ref bstr 0) 3)))))

(test-case "string"
  (check-expect (let ([str (string #\a #\b)])
                  (rotate! (string-ref str 0) (string-ref str 1))
                  str)
                "ba")
  (check-exn exn:fail:contract?
    (lambda () (let ([num 1]) (set! (string-ref num 0) 3))))
  (check-exn exn:fail:contract?
    (lambda () (let ([str (string)]) (set! (string-ref str 0) #\a))))
  (check-exn exn:fail:contract?
    (lambda ()
      (let ([str (string #\a #\b)])
        (set! (string-ref str 'not-idx) #\c))))
  (check-exn exn:fail:contract?
    (lambda ()
      (let ([str (string #\a #\b)]) (set! (string-ref str 2) #\c))))
  (check-exn exn:fail:contract?
    (lambda ()
      (let ([str (string #\a #\b)])
        (set! (string-ref str 0) 'not-char))))
  (check-exn exn:fail:contract?
    (lambda () (let ([str "ab"]) (set! (string-ref str 0) #\a)))))

(define (vector->impersonator-vector vec)
  (define (ref+set! _vec _pos val) val)
  (impersonate-vector vec ref+set! ref+set!))

(define (impersonator-vector . vals)
  (vector->impersonator-vector (apply vector vals)))

(test-case "vector"
  (check-expect (let ([vec (impersonator-vector 1 2)])
                  (rotate! (vector-ref vec 0) (vector-ref vec 1))
                  vec)
                #(2 1))
  (check-exn exn:fail:contract?
    (lambda () (let ([num 1]) (set! (vector-ref num 0) 3))))
  (check-exn exn:fail:contract?
    (lambda ()
      (let ([vec (impersonator-vector)])
        (set! (vector-ref vec 0) 1))))
  (check-exn exn:fail:contract?
    (lambda ()
      (let ([vec (impersonator-vector 1 2)])
        (set! (vector-ref vec 'not-idx) 3))))
  (check-exn exn:fail:contract?
    (lambda ()
      (let ([vec (impersonator-vector 1 2)])
        (set! (vector-ref vec 2) 3))))
  (check-exn exn:fail:contract?
    (lambda ()
      (let ([vec (vector->impersonator-vector #(1 2))])
        (set! (vector-ref vec 0) 3)))))

(test-case "vector*"
  (check-expect (let ([vec (vector 1 2)])
                  (rotate! (vector*-ref vec 0) (vector*-ref vec 1))
                  vec)
                #(2 1))
  (check-exn exn:fail:contract?
    (lambda () (let ([num 1]) (set! (vector*-ref num 0) 3))))
  (check-exn exn:fail:contract?
    (lambda () (let ([vec (vector)]) (set! (vector*-ref vec 0) 1))))
  (check-exn exn:fail:contract?
    (lambda ()
      (let ([vec (vector 1 2)]) (set! (vector*-ref vec 'not-idx) 3))))
  (check-exn exn:fail:contract?
    (lambda ()
      (let ([vec (vector 1 2)]) (set! (vector*-ref vec 2) 3))))
  (check-exn exn:fail:contract?
    (lambda ()
      (let ([vec (impersonator-vector 1 2)])
        (set! (vector*-ref vec 0) 3))))
  (check-exn exn:fail:contract?
    (lambda () (let ([vec #(1 2)]) (set! (vector*-ref vec 0) 3)))))

(define (box->impersonator-box bx)
  (define (ref+set! _bx val) val)
  (impersonate-box bx ref+set! ref+set!))

(define (impersonator-box obj)
  (box->impersonator-box (box obj)))

(test-case "box"
  (check-expect (lambda ()
                  (let ([bx1 (impersonator-box 1)]
                        [bx2 (impersonator-box 2)])
                    (rotate! (unbox bx1) (unbox bx2))
                    (values bx1 bx2)))
                (expect-return #&2 #&1))
  (check-exn exn:fail:contract?
    (lambda ()
      (let ([bx (box->impersonator-box #&1)]) (set! (unbox bx) 2)))))

(test-case "box*"
  (check-expect (lambda ()
                  (let ([bx1 (box 1)] [bx2 (box 2)])
                    (rotate! (unbox* bx1) (unbox* bx2))
                    (values bx1 bx2)))
                (expect-return #&2 #&1))
  (check-exn exn:fail:contract?
    (lambda ()
      (let ([bx (impersonator-box 1)]) (set! (unbox* bx) 2))))
  (check-exn exn:fail:contract?
    (lambda () (let ([bx #&1]) (set! (unbox* bx) 2)))))

(define expect-void (expect-pred void?))

(test-case "set!"
  (check-expect (set!) expect-void)
  (check-expect (set! (values) (values)) expect-void)
  (check-expect (lambda ()
                  (let ([bx1 (box 1)] [bx2 (box 2)])
                    (set! (values (unbox* bx1) (unbox* bx2))
                          (values (unbox* bx2) (unbox* bx1)))
                    (values bx1 bx2)))
                (expect-return #&2 #&1))
  (check-expect (lambda ()
                  (let ([a 1] [b 2] [c 3] [d 4])
                    (set! (values a b) (values c d)
                          (values c d) (values a b))
                    (values a b c d)))
                (expect-return 3 4 3 4)))

(test-case "set!-values"
  (check-expect (set!-values) expect-void)
  (check-expect (set!-values () (values)) expect-void)
  (check-expect (lambda ()
                  (let ([bx1 (box 1)] [bx2 (box 2)])
                    (set!-values ((unbox* bx1) (unbox* bx2))
                                 (values (unbox* bx2) (unbox* bx1)))
                    (values bx1 bx2)))
                (expect-return #&2 #&1))
  (check-expect (lambda ()
                  (let ([a 1] [b 2] [c 3] [d 4])
                    (set!-values (a b) (values c d)
                                 (c d) (values a b))
                    (values a b c d)))
                (expect-return 3 4 3 4)))

(test-case "pset!"
  (check-expect (pset!) expect-void)
  (check-expect (pset! (values) (values)) expect-void)
  (check-expect (lambda ()
                  (let ([bx1 (box 1)] [bx2 (box 2)])
                    (pset! (values (unbox* bx1) (unbox* bx2))
                           (values (unbox* bx2) (unbox* bx1)))
                    (values bx1 bx2)))
                (expect-return #&2 #&1))
  (check-expect (lambda ()
                  (let ([a 1] [b 2] [c 3] [d 4])
                    (pset! (values a b) (values c d)
                           (values c d) (values a b))
                    (values a b c d)))
                (expect-return 3 4 1 2)))

(test-case "pset!-values"
  (check-expect (pset!-values) expect-void)
  (check-expect (pset!-values () (values)) expect-void)
  (check-expect (lambda ()
                  (let ([bx1 (box 1)] [bx2 (box 2)])
                    (pset!-values ((unbox* bx1) (unbox* bx2))
                                  (values (unbox* bx2) (unbox* bx1)))
                    (values bx1 bx2)))
                (expect-return #&2 #&1))
  (check-expect (lambda ()
                  (let ([a 1] [b 2] [c 3] [d 4])
                    (pset!-values (a b) (values c d)
                                  (c d) (values a b))
                    (values a b c d)))
                (expect-return 3 4 1 2)))

(test-case "shift!"
  (check-expect (lambda ()
                  (let ([bx (box 1)])
                    (values (shift! (unbox* bx) 2) bx)))
                (expect-return 1 #&2))
  (check-expect (lambda ()
                  (let ([bx (box 1)] [mpair (mcons 2 '())] [val 3])
                    (values (shift! (unbox* bx) (mcar mpair) val 4)
                            bx mpair val)))
                (expect-return 1 #&2 (mcons 3 '()) 4)))

(test-case "rotate!"
  (check-expect (lambda ()
                  (let ([bx (box 1)] [val 2])
                    (rotate! (unbox* bx) val)
                    (values bx val)))
                (expect-return #&2 1))
  (check-expect (lambda ()
                  (let ([bx (box 1)] [mpair (mcons 2 3)] [val 4])
                    (rotate! (unbox* bx) (mcar mpair)
                             (mcdr mpair) val)
                    (values bx mpair val)))
                (expect-return #&2 (mcons 3 4) 1)))

(test-case "call!"
  (check-expect (let ([bx (box 1)]) (call! add1 (unbox* bx)) bx) #&2)
  (check-expect (let ([bx (box "...foo...")])
                  (call! string-trim (unbox* bx) "."
                         #:left? #f #:repeat? #t)
                  bx)
                #&"...foo")
  (check-expect (let ([bx (box "...foo...")]
                      [rest-arg (list "..." ":::")])
                  (call! string-replace (unbox* bx)
                         #:all? #f . rest-arg)
                  bx)
                #&":::foo...")
  (check-exn exn:fail:contract?
    (lambda () (let ([bx (box 1)]) (call! 'not-proc (unbox* bx))))))

(test-case "call2!"
  (check-expect (let ([bx (box 1)]) (call2! + 1 (unbox* bx)) bx) #&2)
  (check-expect (let ([bx (box ".")])
                  (call2! string-trim "...foo..." (unbox* bx)
                          #:left? #f #:repeat? #t)
                  bx)
                #&"...foo")
  (check-expect (let ([bx (box "...")]
                      [rest-arg (list ":::")])
                  (call2! string-replace "...foo..." (unbox* bx)
                          #:all? #f . rest-arg)
                  bx)
                #&":::foo...")
  (check-exn exn:fail:contract?
    (lambda ()
      (let ([bx (box 1)]) (call2! 'not-proc 'arg (unbox* bx))))))

(test-case "inc!"
  (check-expect (let ([bx (box 1)]) (inc! (unbox* bx)) bx) (box 2))
  (check-expect (let ([bx (box 1)]) (inc! (unbox* bx) -1) bx) (box 0))
  (check-exn exn:fail:contract?
    (lambda () (let ([bx (box 'not-num)]) (inc! (unbox* bx)))))
  (check-exn exn:fail:contract?
    (lambda () (let ([bx (box 1)]) (inc! (unbox* bx) 'not-num)))))

(test-case "dec!"
  (check-expect (let ([bx (box 1)]) (dec! (unbox* bx)) bx) (box 0))
  (check-expect (let ([bx (box 1)]) (dec! (unbox* bx) -1) bx) (box 2))
  (check-exn exn:fail:contract?
    (lambda () (let ([bx (box 'not-num)]) (dec! (unbox* bx)))))
  (check-exn exn:fail:contract?
    (lambda () (let ([bx (box 1)]) (dec! (unbox* bx) 'not-num)))))

(test-case "push!"
  (check-expect (let ([bx (box '())]) (push! 1 (unbox* bx)) bx)
                #&(1))
  (check-expect (let ([bx (box 2)]) (push! 1 (unbox* bx)) bx)
                #&(1 . 2)))

(test-case "mpush!"
  (check-expect (let ([bx (box '())]) (mpush! 1 (unbox* bx)) bx)
                (box (mcons 1 '())))
  (check-expect (let ([bx (box 2)]) (mpush! 1 (unbox* bx)) bx)
                (box (mcons 1 2))))

(test-case "pop!"
  (check-expect (lambda ()
                  (let ([bx (box (list 1 2))])
                    (values (pop! (unbox* bx)) bx)))
                (expect-return 1 #&(2)))
  (check-expect (lambda ()
                  (let ([bx (box (cons 1 2))])
                    (values (pop! (unbox* bx)) bx)))
                (expect-return 1 #&2))
  (check-exn exn:fail:contract?
    (lambda () (let ([val 'not-pair]) (pop! val)))))

(test-case "mpop!"
  (check-expect (lambda ()
                  (let ([bx (box (mcons 1 (mcons 2 '())))])
                    (values (mpop! (unbox* bx)) bx)))
                (expect-return 1 (box (mcons 2 '()))))
  (check-expect (lambda ()
                  (let ([bx (box (mcons 1 2))])
                    (values (mpop! (unbox* bx)) bx)))
                (expect-return 1 (box 2)))
  (check-exn exn:fail:contract?
    (lambda () (let ([val 'not-mpair]) (mpop! val)))))

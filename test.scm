;; SPDX-FileCopyrightText: 2024 Artyom Bologov
;; SPDX-License-Identifier: MIT

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

(import (srfi 64))
(import (srfi 253))

(test-begin "check-arg")
;; Sanity checks
(test-assert (check-arg exact-integer? 3))
(test-assert (check-arg integer? 3))
(test-assert (check-arg boolean? #f))
(test-assert (check-arg char? #\d))
(test-assert (check-arg complex? 3+2i))
(test-assert (check-arg inexact? 3.8))
(test-assert (check-arg real? 3))
(test-assert (check-arg real? 3/2))
(test-assert (check-arg real? 3.8))
(test-assert (check-arg list? '()))
(test-assert (check-arg list? '(1 2 3)))
(test-assert (check-arg null? '()))
(test-assert (check-arg number? 3))
(test-assert (check-arg number? 3+2i))
(test-assert (check-arg number? 3.8))
(test-assert (check-arg pair? '(1 2 3)))
(test-assert (check-arg input-port? (current-input-port)))
(test-assert (check-arg output-port? (current-output-port)))
(test-assert (check-arg procedure? procedure?))
(test-assert (check-arg rational? 3))
(test-assert (check-arg rational? 3/2))
(test-assert (check-arg string? ""))
(test-assert (check-arg string? "hello"))
(test-assert (check-arg symbol? 'hello))
;; Only enable on implementations supporting symbol->keyword
;; (test-assert (check-arg keyword? (symbol->keyword 'hello)))
(test-assert (check-arg vector? #(1 2 3)))
;; Predicate checks
(test-assert (check-arg (lambda (x) (positive? (string-length x)))
                        "hello"))
(test-assert (check-arg positive? 9))
(test-assert (check-arg string-length "hello")) ;; If it works it works.
(test-assert (check-arg (lambda (x)
                          (and (integer? x) (positive? x)))
                        8))
(test-assert (check-arg ((lambda (x y)
                           (lambda (a) (and (x a) (y a))))
                         integer? positive?)
                        8))
;; Erroring checks
(test-error (check-arg string? 3))
(test-error (check-arg real? 3+2i))
(test-error (check-arg symbol? "hello"))
(test-error (check-arg procedure? 3))
;; It is an error when predicate doesn't pass, but it doesn't have to
;; throw errors. Disable depending on implementation.
(test-error (check-arg (lambda (a) (> a 3)) 0))
;; Syntax checks
(test-assert (check-arg integer? 3 'testing 'extra 'args))
;; (test-error (check-arg))
(test-end "check-arg")

(test-begin "values-checked")
(test-equal 3 (values-checked (integer?) 3))
(test-equal 3 (values-checked ((lambda (x) (= 3 x))) 3))
(test-approximate 3.0 (values-checked (real?) 3.0) 0.00001)
;; Implementation-specific, might be 3.0
(test-equal 3 (values-checked (real?) 3))
(test-assert (values-checked (integer? string?) 3 "hello"))
(test-approximate 3.0 (values-checked (inexact?) 3.0) 0.00001)
(test-error (values-checked (integer?) "hello"))
(test-error (values-checked (integer? string?) 3 3))
;; Syntax checks
;; (test-error (values-checked real? 3))
;; (test-error (values-checked (real?) 3 8))
;; (test-error (values-checked (real? string?) 3))
(test-end "values-checked")

;; TODO: let-checked
(test-begin "let-checked")
(define a 3)
(define b 4)
(test-equal 3 (let-checked ((a integer?)) a))
(test-equal 3 (let-checked ((a integer? 3)) a))
(test-equal 6 (let-checked ((a integer? 2) (b integer?)) (+ a b)))
(test-equal 3 (let-checked ((a integer? 2) (b integer? 1)) (+ a b)))
(test-equal 3 (let-checked (((a b) (integer? integer?) (values 2 1))) (+ a b)))
(test-error (let-checked ((a string? 3)) a))
;; Syntax checks
;; (test-error (let-checked))
;; (test-error (let-checked (a) #t))
;; (test-error (let-checked (a b) #t))
;; (test-error (let-checked (a (b 3)) #t))
;; (test-error (let-checked ((a 3 4 5)) #t))
;; (test-error (let-checked (((a) 3 ())) #t))
;; (test-error (let-checked (((a) 3 (a b c))) #t))
(test-end "let-checked")

(test-begin "lambda-checked")
(test-assert (lambda-checked () #t))
(test-assert (lambda-checked (a) #t))
(test-assert (lambda-checked (a b) #t))
(test-assert (lambda-checked ((a integer?)) #t))
(test-assert (lambda-checked (a (b integer?)) #t))
(test-assert (lambda-checked ((a string?) (b integer?)) #t))
(test-assert ((lambda-checked (a) #t) 3))
(test-assert ((lambda-checked (a) #t) "hello"))
(test-assert ((lambda-checked ((a integer?)) #t) 3))
(test-assert ((lambda-checked (a (b integer?)) #t) 3 3))
(test-assert ((lambda-checked (a (b integer?)) #t) "hello" 3))
(test-error ((lambda-checked ((a integer?)) #t) "hello"))
(test-error ((lambda-checked (a (b integer?)) #t) "hello" "hi"))
;; Rest args. Sample implementation doesn't reliably pass this.
;; (test-assert (lambda-checked (a . c) #t))
;; (test-assert (lambda-checked ((a integer?) . c) #t))
;; (test-assert (lambda-checked (a b . c) #t))
;; (test-assert (lambda-checked (a (b integer?) . c) #t))
;; Syntax checks
;; (test-error (lambda-checked))
;; (test-error (lambda-checked ()))
(test-end "lambda-checked")

(test-begin "define-checked")
(define-checked (c) #t)
(test-assert (c))
(define-checked (c (a integer?)) #t)
(test-assert (c 3))
(test-error (c "hello"))
(define-checked (c b) #t)
(test-assert (c "anything"))
(test-error (c 1 2 3))
(define-checked (c (b string?)) #t)
(test-assert (c "hello"))
(test-error (c 3))
;; Rest args. Sample implementation doesn't reliably pass this.
;; (test-assert (define-checked (c b . d) #t))
;; (test-error (c))
;; (test-assert (c 1))
;; (test-assert (c 1 2))
;; (test-assert (c 1 2 3))
(define-checked c string? "hello")
(test-assert c)
(set! c "whatever")
(test-assert c)
;; Optional, only if implementation checks all modifications.
;; (test-error (set! c 3))
;; Syntax checks
;; (define-error (define-checked))
;; (define-error (define-checked a))
;; (define-error (define-checked a string?))
;; (define-error (define-checked a string? "hello" 'aux))
(test-end "define-checked")

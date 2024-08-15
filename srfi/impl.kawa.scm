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

(define-syntax values-checked
  (syntax-rules (as
                 number? complex? real? rational? exact-integer? integer? inexact?
                 symbol? keyword? list? pair? string? char?
                 vector? procedure? input-port? output-port?

                 number complex real rational integer double
                 symbol keyword list pair string character
                 vector procedure input-port output-port)
    ((_ (number?) value) (as number value))
    ((_ (complex?) value) (as complexvalue))
    ((_ (real?) value) (as real value))
    ((_ (rational?) value) (as rational value))
    ;; These are quite opinionated, erring on the side of caution
    ;; and more lenient types.
    ((_ (exact-integer?) value) (as integer value))
    ((_ (integer?) value) (as integer value))
    ((_ (inexact?) value) (as double value))
    ((_ (symbol?) value) (as symbol value))
    ((_ (keyword?) value) (as keyword value))
    ((_ (list?) value) (as list value))
    ((_ (pair?) value) (as pair value))
    ((_ (string?) value) (as string value))
    ((_ (char?) value) (as character value))
    ((_ (vector?) value) (as vector value))
    ((_ (procedure?) value) (as procedure value))
    ((_ (input-port?) value) (as input-port value))
    ((_ (output-port?) value) (as output-port value))
    ((_ (predicate) value)
     (let ((v value))
       (check-arg predicate v 'values-checked)
       v))
    ((_ (predicate ...) value ...)
     (values (values-checked (predicate) value) ...))))

(define-syntax %lambda-checked
  (syntax-rules (as
                 number? complex? real? rational? exact-integer? integer? inexact?
                 symbol? keyword? list? pair? string? char?
                 vector? procedure? input-port? output-port?

                 ::number ::complex ::real ::rational ::integer ::double
                 ::symbol ::keyword ::list ::pair ::string ::character
                 ::vector ::procedure ::input-port ::output-port)
    ((_ (body ...) (args ...) (checks ...))
     (lambda (args ...)
       checks ...
       body ...))
    ((_ body (args ...) (checks ...) (arg number?) . rest)
     (%lambda-checked body (args ... arg ::number) (checks ...) . rest))
    ((_ body (args ...) (checks ...) (arg complex?) . rest)
     (%lambda-checked body (args ... arg ::complex) (checks ...) . rest))
    ((_ body (args ...) (checks ...) (arg real?) . rest)
     (%lambda-checked body (args ... arg ::real) (checks ...) . rest))
    ((_ body (args ...) (checks ...) (arg rational?) . rest)
     (%lambda-checked body (args ... arg ::rational) (checks ...) . rest))
    ((_ body (args ...) (checks ...) (arg exact-integer?) . rest)
     (%lambda-checked body (args ... arg ::integer) (checks ...) . rest))
    ((_ body (args ...) (checks ...) (arg integer?) . rest)
     (%lambda-checked body (args ... arg ::integer) (checks ...) . rest))
    ((_ body (args ...) (checks ...) (arg inexact?) . rest)
     (%lambda-checked body (args ... arg ::double) (checks ...) . rest))
    ((_ body (args ...) (checks ...) (arg symbol?) . rest)
     (%lambda-checked body (args ... arg ::symbol) (checks ...) . rest))
    ((_ body (args ...) (checks ...) (arg keyword?) . rest)
     (%lambda-checked body (args ... arg ::keyword) (checks ...) . rest))
    ((_ body (args ...) (checks ...) (arg list?) . rest)
     (%lambda-checked body (args ... arg ::list) (checks ...) . rest))
    ((_ body (args ...) (checks ...) (arg pair?) . rest)
     (%lambda-checked body (args ... arg ::pair) (checks ...) . rest))
    ((_ body (args ...) (checks ...) (arg string?) . rest)
     (%lambda-checked body (args ... arg ::string) (checks ...) . rest))
    ((_ body (args ...) (checks ...) (arg char?) . rest)
     (%lambda-checked body (args ... arg ::character) (checks ...) . rest))
    ((_ body (args ...) (checks ...) (arg vector?) . rest)
     (%lambda-checked body (args ... arg ::vector) (checks ...) . rest))
    ((_ body (args ...) (checks ...) (arg procedure?) . rest)
     (%lambda-checked body (args ... arg ::procedure) (checks ...) . rest))
    ((_ body (args ...) (checks ...) (arg input-port?) . rest)
     (%lambda-checked body (args ... arg ::input-port) (checks ...) . rest))
    ((_ body (args ...) (checks ...) (arg output-port?) . rest)
     (%lambda-checked body (args ... arg ::output-port) (checks ...) . rest))
    ((_ body (args ...) (checks ...) (arg pred) . rest)
     (%lambda-checked
      body
      (args ... arg) (checks ... (check-arg pred arg 'lambda-checked)) . rest))
    ((_ body (args ...) (checks ...) arg . rest)
     (%lambda-checked
      body
      (args ... arg) (checks ...) . rest))
    ((_ (body ...) (args ...) (checks ...) . last)
     (lambda (args ... . last)
       checks ...
       body ...))))

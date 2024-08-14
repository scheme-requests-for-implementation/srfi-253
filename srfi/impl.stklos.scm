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

(define-syntax check-arg
  (syntax-rules (integer?
                 exact-integer? boolean? char? complex? real? inexact?
                 pair? number? null?
                 procedure? rational? string? symbol? keyword? vector?

                 <boolean> <char> <complex> <real> <pair> <number> <null> <procedure> <rational> <string> <symbol> <keyword> <vector>)
    ((_ integer? val . rest)
     (assume (is-a? val <integer>) "type mismatch" . rest))
    ((_ exact-integer? val . rest)
     (assume (is-a? val <integer>) "type mismatch" . rest))
    ((_ boolean? val . rest)
     (assume (is-a? val <boolean>) "type mismatch" . rest))
    ((_ char? val . rest)
     (assume (is-a? val <char>) "type mismatch" . rest))
    ((_ complex? val . rest)
     (assume (is-a? val <complex>) "type mismatch" . rest))
    ((_ real? val . rest)
     (assume (is-a? val <real>) "type mismatch" . rest))
    ((_ inexact? val . rest)
     (assume (is-a? val <real>) "type mismatch" . rest))
    ((_ pair? val . rest)
     (assume (is-a? val <pair>) "type mismatch" . rest))
    ((_ null? val . rest)
     (assume (is-a? val <null>) "type mismatch" . rest))
    ((_ number? val . rest)
     (assume (is-a? val <number>) "type mismatch" . rest))
    ((_ procedure? val . rest)
     (assume (is-a? val <procedure>) "type mismatch" . rest))
    ((_ rational? val . rest)
     (assume (is-a? val <rational>) "type mismatch" . rest))
    ((_ string? val . rest)
     (assume (is-a? val <string>) "type mismatch" . rest))
    ((_ symbol? val . rest)
     (assume (is-a? val <symbol>) "type mismatch" . rest))
    ((_ keyword? val . rest)
     (assume (is-a? val <keyword>) "type mismatch" . rest))
    ((_ vector? val . rest)
     (assume (is-a? val <vector>) "type mismatch" . rest))
    ((_ pred val . rest)
     (assume (pred val) "argument should match the specification"  '(pred val) . rest))))

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

(import-for-syntax (chicken type))

(define-syntax values-checked
  (syntax-rules (the
                 exact-integer? integer? boolean? char? complex?
                 fixnum? flonum?
                 eof? inexact? real?
                 list? null? number? pair?
                 input-port? output-port?
                 procedure? rational?
                 string? symbol? keyword? vector?

                 fixnum float boolean char cplxnum eof
                 number list null number pair input-port output-port
                 procedure ratnum string symbol keyword vector *)
    ((_ (fixnum?) value)      (let ((v value)) (check-arg fixnum? v) (the fixnum v)))
    ((_ (flonum?) value)      (let ((v value)) (check-arg flonum? v) (the float v)))
    ((_ (exact-integer?) value) (let ((v value)) (check-arg exact-integer? v) (the integer v)))
    ((_ (integer?) value)     (let ((v value)) (check-arg integer? v) (the integer v)))
    ((_ (boolean?) value)     (let ((v value)) (check-arg boolean? v) (the boolean v)))
    ((_ (char?) value)        (let ((v value)) (check-arg char? v) (the char v)))
    ((_ (complex?) value)     (let ((v value)) (check-arg complex? v) (the cplxnum v)))
    ((_ (eof?) value)         (let ((v value)) (check-arg eof? v) (the eof v)))
    ((_ (inexact?) value)     (let ((v value)) (check-arg inexact? v) (the float v)))
    ((_ (real?) value)        (let ((v value)) (check-arg number? v) (the number v)))
    ((_ (list?) value)        (let ((v value)) (check-arg list? v) (the list v)))
    ((_ (null?) value)        (let ((v value)) (check-arg null? v) (the null v)))
    ((_ (number?) value)      (let ((v value)) (check-arg number? v) (the number v)))
    ((_ (pair?) value)        (let ((v value)) (check-arg pair? v) (the pair v)))
    ((_ (input-port?) value)  (let ((v value)) (check-arg input-port? v) (the input-port v)))
    ((_ (output-port?) value) (let ((v value)) (check-arg output-port? v) (the output-port v)))
    ((_ (procedure?) value)   (let ((v value)) (check-arg procedure? v) (the procedure v)))
    ((_ (rational?) value)    (let ((v value)) (check-arg rational? v) (the ratnum v)))
    ((_ (string?) value)      (let ((v value)) (check-arg string? v) (the string v)))
    ((_ (symbol?) value)      (let ((v value)) (check-arg symbol? v) (the symbol v)))
    ((_ (keyword?) value)     (let ((v value)) (check-arg keyword? v) (the keyword v)))
    ((_ (vector?) value)      (let ((v value)) (check-arg vector? v) (the vector v)))
    ((_ (predicate) value)
     (let ((v value))
       (check-arg predicate v)
       v))
    ((_ (predicate ...) value ...)
     (values (values-checked (predicate) value) ...))))

(define-syntax %declare-checked-var
  (syntax-rules (: ->
                   any? integer? exact-integer? boolean? char? complex?
                   fixnum? flonum?
                   eof? inexact? real?
                   list? null? number? pair?
                   input-port? output-port?
                   procedure? rational?
                   string? symbol? keyword? vector?

                   integer boolean char cplxnum eof fixnum float
                   number list null number pair input-port output-port
                   procedure ratnum string symbol keyword vector *)
    ((_ (predicate ...) value ...)
     (when #f #f))
    ((_ name any?)         (: name *))
    ((_ name fixnum?)      (: name fixnum))
    ((_ name flonum?)      (: name float))
    ((_ name integer?)     (: name integer))
    ((_ name exact-integer?) (: name integer))
    ((_ name boolean?)     (: name boolean))
    ((_ name char?)        (: name char))
    ((_ name complex?)     (: name cplxnum))
    ((_ name eof?)         (: name eof))
    ((_ name inexact?)     (: name float))
    ((_ name real?)        (: name number))
    ((_ name list?)        (: name list))
    ((_ name null?)        (: name null))
    ((_ name number?)      (: name number))
    ((_ name pair?)        (: name pair))
    ((_ name input-port?)  (: name input-port))
    ((_ name output-port?) (: name output-port))
    ((_ name procedure?)   (: name procedure))
    ((_ name rational?)    (: name ratnum))
    ((_ name string?)      (: name string))
    ((_ name symbol?)      (: name symbol))
    ((_ name keyword?)     (: name keyword))
    ((_ name vector?)      (: name vector))
    ((_ name predicate)
     (when #f #f))))

(define-syntax %declare-checked-fn
  (syntax-rules (: ->
                 any? integer? boolean? char? complex?
                 fixnum? flonum?
                 eof? inexact? real?
                 list? null? number? pair?
                 input-port? output-port?
                 procedure? rational?
                 string? symbol? keyword? vector?

                 integer boolean char cplxnum eof fixnum float
                 number list null number pair input-port output-port
                 procedure ratnum string symbol keyword vector *)
    ((_ name () (type ...))
     (: name (type ... -> *)))
    ((_ name ((arg fixnum?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... fixnum)))
    ((_ name ((arg flonum?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... float)))
    ((_ name ((arg integer?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... integer)))
    ((_ name ((arg boolean?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... boolean)))
    ((_ name ((arg char?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... char)))
    ((_ name ((arg complex?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... cplxnum)))
    ((_ name ((arg eof?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... eof)))
    ((_ name ((arg inexact?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... float)))
    ((_ name ((arg real?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... number)))
    ((_ name ((arg list?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... list)))
    ((_ name ((arg null?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... null)))
    ((_ name ((arg number?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... number)))
    ((_ name ((arg pair?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... pair)))
    ((_ name ((arg input-port?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... input-port)))
    ((_ name ((arg output-port?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... output-port)))
    ((_ name ((arg procedure?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... procedure)))
    ((_ name ((arg rational?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... ratnum)))
    ((_ name ((arg string?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... string)))
    ((_ name ((arg symbol?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... symbol)))
    ((_ name ((arg keyword?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... keyword)))
    ((_ name ((arg vector?) check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... vector)))
    ((_ name (arg check ...) (type ...))
     (%declare-checked-fn name (check ...) (type ... *)))))

(define-syntax define-checked
  (syntax-rules ()
    ;; Function
    ((_ (name arg ...) first-body body ...)
     (begin
       (%declare-checked-fn name (arg ...) ())
       (define name (lambda-checked (arg ...) first-body body ...))))
    ;; Variable
    ((_ name pred value)
     (begin
       (%declare-checked-var name pred)
       (define name (values-checked (pred) value))))))

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

(cond-expand
  (guile
   (use-modules (rnrs base) (srfi srfi-16)))
  (chicken
   (import-for-syntax (chicken type)))
  (else))

(cond-expand
 (srfi-145)
  ((or r6rs chicken loko)
   (define-syntax assume
     (syntax-rules ()
       ((_ expr rest ...)
        (assert expr)))))
  (debug
   (define-syntax assume
     (syntax-rules ()
       ((_ expr rest ...)
        (or expr
            (error "assumption violated" 'expr rest ...))))))
  (else (define-syntax assume
          (syntax-rules ()
            ((_ rest ...)
             (begin))))))

(cond-expand
  (gauche
   (define-syntax check-arg
     (syntax-rules (integer?
                    exact-integer? boolean? char? complex? real? inexact?
                    pair? number? null?
                    procedure? rational? string? symbol? keyword? vector?
                    fixnum? flonum?

                    <integer> <boolean> <char> <complex> <real> <pair> <number>
                    <null> <procedure> <rational> <string> <symbol> <keyword>
                    <vector> <fixnum> <double>)
       ((_ integer? val rest ...)
        (assume (is-a? val <integer>) "type mismatch" caller <integer> val rest ...))
       ((_ exact-integer? val rest ...)
        (assume (is-a? val <integer>) "type mismatch" caller <integer> val rest ...))
       ((_ boolean? val caller)
        (assume (is-a? val <boolean>) "type mismatch" caller <boolean> val rest ...))
       ((_ char? val caller)
        (assume (is-a? val <char>) "type mismatch" <char> val rest ...))
       ((_ complex? val rest ...)
        (assume (is-a? val <complex>) "type mismatch" <complex> val rest ...))
       ((_ real? val rest ...)
        (assume (is-a? val <real>) "type mismatch" <real> val rest ...))
       ((_ inexact? val rest ...)
        (assume (is-a? val <real>) "type mismatch" <real> val rest ...))
       ((_ pair? val rest ...)
        (assume (is-a? val <pair>) "type mismatch" <pair> val rest ...))
       ((_ null? val rest ...)
        (assume (is-a? val <null>) "type mismatch" <null> val rest ...))
       ((_ number? val rest ...)
        (assume (is-a? val <number>) "type mismatch" <number> val rest ...))
       ((_ procedure? val rest ...)
        (assume (is-a? val <procedure>) "type mismatch" <procedure> val rest ...))
       ((_ rational? val rest ...)
        (assume (is-a? val <rational>) "type mismatch" <rational> val rest ...))
       ((_ string? val rest ...)
        (assume (is-a? val <string>) "type mismatch" <string> val rest ...))
       ((_ symbol? val rest ...)
        (assume (is-a? val <symbol>) "type mismatch" <symbol> val rest ...))
       ((_ keyword? val rest ...)
        (assume (is-a? val <keyword>) "type mismatch" <keyword> val rest ...))
       ((_ vector? val rest ...)
        (assume (is-a? val <vector>) "type mismatch" <vector> val rest ...))
       ((_ fixnum? val rest ...)
        (assume (is-a? val <fixnum>) "type mismatch" <fixnum> val rest ...))
       ((_ flonum? val rest ...)
        (assume (is-a? val <double>) "type mismatch" <double> val rest ...))
       ((_ pred val rest ...)
        (assume (pred val) "argument should match the specification" '(pred val) rest ...)))))
  (stklos
   (define-syntax check-arg
     (syntax-rules (integer?
                    exact-integer? boolean? char? complex? real? inexact?
                    pair? number? null?
                    procedure? rational? string? symbol? keyword? vector? fixnum?

                    <boolean> <char> <complex> <real> <pair> <number> <null>
                    <procedure> <rational> <string> <symbol> <keyword> <vector>
                    <fixnum>)
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
       ((_ fixnum? val . rest)
        (assume (is-a? val <fixnum>) "type mismatch" . rest))
       ((_ pred val . rest)
        (assume (pred val) "argument should match the specification"  '(pred val) . rest)))))
  (else
   (define-syntax check-arg
     (syntax-rules ()
       ((_ pred val . rest)
        (assume (pred val) "argument should match the specification"  '(pred val) val . rest))))))

(cond-expand
  (kawa
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
        (values (values-checked (predicate) value) ...)))))
  (chicken
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
                    list null number pair input-port output-port
                    procedure ratnum string symbol keyword vector *)
       ((_ (fixnum?) value)      (let ((v value)) (check-arg fixnum? v) (the fixnum v)))
       ((_ (flonum?) value)      (let ((v value)) (check-arg flonum? v) (the float v)))
       ((_ (exact-integer?) value) (let ((v value)) (check-arg exact-integer? v) (the integer v)))
       ((_ (integer?) value)     (let ((v value)) (check-arg integer? v) (the number v)))
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
          (check-arg predicate v 'values-checked)
          v))
       ((_ (predicate ...) value ...)
        (values (values-checked (predicate) value) ...)))))
  (else
   (define-syntax values-checked
     (syntax-rules ()
       ((_ (predicate) value)
        (let ((v value))
          (check-arg predicate v 'values-checked)
          v))
       ((_ (predicate ...) value ...)
        (values (values-checked (predicate) value) ...))))))

(define-syntax let-checked
  (syntax-rules ()
    ((_ () body ...)
     (begin body ...))
    ((_ ((name pred) bindings ...) body ...)
     (let ((name (values-checked (pred) name)))
       (let-checked
        (bindings ...)
        body ...)))
    ((_ (((name ...) (pred ...) val) bindings ...) body ...)
     (call-with-values
         (lambda () val)
       (lambda (name ...)
         (let ((name (values-checked (pred) name))
               ...)
           (let-checked
            (bindings ...)
            body ...)))))
    ((_ ((name pred val) bindings ...) body ...)
     (let ((name (values-checked (pred) val)))
       (let-checked
        (bindings ...)
        body ...)))))

(cond-expand
  (kawa
   (define-syntax %lambda-checked
     (syntax-rules (as
                    number? complex? real? rational? exact-integer? integer? inexact?
                    symbol? keyword? list? pair? string? char?
                    vector? procedure? input-port? output-port?

                    ::number ::complex ::real ::rational ::integer ::double
                    ::symbol ::keyword ::list ::pair ::string ::character
                    ::vector ::procedure ::input-port ::output-port)
       ((_ name (body ...) (args ...) (checks ...))
        (lambda (args ...)
          checks ...
          body ...))
       ((_ name body (args ...) (checks ...) (arg number?) . rest)
        (%lambda-checked name body (args ... arg ::number) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg complex?) . rest)
        (%lambda-checked name body (args ... arg ::complex) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg real?) . rest)
        (%lambda-checked name body (args ... arg ::real) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg rational?) . rest)
        (%lambda-checked name body (args ... arg ::rational) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg exact-integer?) . rest)
        (%lambda-checked name body (args ... arg ::integer) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg integer?) . rest)
        (%lambda-checked name body (args ... arg ::integer) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg inexact?) . rest)
        (%lambda-checked name body (args ... arg ::double) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg symbol?) . rest)
        (%lambda-checked name body (args ... arg ::symbol) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg keyword?) . rest)
        (%lambda-checked name body (args ... arg ::keyword) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg list?) . rest)
        (%lambda-checked name body (args ... arg ::list) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg pair?) . rest)
        (%lambda-checked name body (args ... arg ::pair) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg string?) . rest)
        (%lambda-checked name body (args ... arg ::string) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg char?) . rest)
        (%lambda-checked name body (args ... arg ::character) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg vector?) . rest)
        (%lambda-checked name body (args ... arg ::vector) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg procedure?) . rest)
        (%lambda-checked name body (args ... arg ::procedure) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg input-port?) . rest)
        (%lambda-checked name body (args ... arg ::input-port) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg output-port?) . rest)
        (%lambda-checked name body (args ... arg ::output-port) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg pred) . rest)
        (%lambda-checked
         name body
         (args ... arg) (checks ... (check-arg pred arg name)) . rest))
       ((_ name body (args ...) (checks ...) arg . rest)
        (%lambda-checked
         name body
         (args ... arg) (checks ...) . rest))
       ((_ (body ...) (args ...) (checks ...) . last)
        (lambda (args ... . last)
          checks ...
          body ...)))))
  (gauche
   (define-syntax %lambda-checked
     (syntax-rules ()
       ((_ name (body ...) args (checks ...))
        (lambda args
          checks ...
          body ...))
       ((_ name body (args ...) (checks ...) (arg pred) rest ...)
        (%lambda-checked
         name body
         (args ... arg) (checks ... (check-arg pred arg 'name)) rest ...))
       ((_ name body (args ...) (checks ...) arg rest ...)
        (%lambda-checked
         name body
         (args ... arg) (checks ...) rest ...)))))
  (else
   (define-syntax %lambda-checked
     (syntax-rules ()
       ((_ name (body ...) args (checks ...))
        (lambda args
          checks ...
          body ...))
       ((_ name body (args ...) (checks ...) (arg pred) . rest)
        (%lambda-checked
         name body
         (args ... arg) (checks ... (check-arg pred arg 'name)) . rest))
       ((_ name body (args ...) (checks ...) arg . rest)
        (%lambda-checked
         name body
         (args ... arg) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) . last)
        (%lambda-checked
         name body
         (args ... . last) (checks ...)))))))

(cond-expand
 (gauche
  (define-syntax lambda-checked
    (syntax-rules ()
      ((_ (args ...) body ...)
       (%lambda-checked lambda-checked (body ...) () () args ...))
      ;; Case of arg->list lambda, no-op.
      ((_ arg body ...)
       (lambda arg body ...)))))
 (else
  (define-syntax lambda-checked
    (syntax-rules ()
      ((_ () body ...)
       (lambda () body ...))
      ((_ (arg . args) body ...)
       (%lambda-checked lambda-checked (body ...) () () arg . args))
      ;; Case of arg->list lambda, no-op.
      ((_ arg body ...)
       (lambda arg body ...))))))

(cond-expand
 (gauche) ;; Too hard to implement
 ((or srfi-16 r7rs)
  (define-syntax %case-lambda-checked
    (syntax-rules ()
      ((_ (clauses-so-far ...)
          ()
          args-so-far (checks-so-far ...) (body ...))
       (case-lambda
         clauses-so-far ...
         (args-so-far
          checks-so-far ...
          body ...)))
      ((_ (clauses-so-far ...)
          ((() body-to-process ...) clauses-to-process ...)
          args-so-far (checks-so-far ...) (body ...))
       (%case-lambda-checked
        (clauses-so-far ... (args-so-far checks-so-far ... body ...))
        (clauses-to-process ...)
        () () (body-to-process ...)))
      ((_ (clauses-so-far ...)
          (((arg . args-to-process) body-to-process ...) clauses-to-process ...)
          args-so-far (checks-so-far ...) (body ...))
       (%case-lambda-checked
        (clauses-so-far ... (args-so-far checks-so-far ... body ...))
        (clauses-to-process ...)
        () () (body-to-process ...) arg . args-to-process))
      ((_ (clauses-so-far ...)
          ((arg-to-process body-to-process ...) clauses-to-process ...)
          args-so-far (checks-so-far ...) (body ...))
       (%case-lambda-checked
        (clauses-so-far ... (args-so-far checks-so-far ... body ...))
        (clauses-to-process ...)
        arg-to-process () (body-to-process ...)))
      ((_ (clauses-so-far ...) (clauses-to-process ...)
          (args-so-far ...) (checks-so-far ...) (body ...) (arg pred) . args)
       (%case-lambda-checked
        (clauses-so-far ...) (clauses-to-process ...)
        (args-so-far ... arg)
        (checks-so-far ... (check-arg pred arg 'case-lambda-checked))
        (body ...) . args))
      ((_ (clauses-so-far ...) (clauses-to-process ...)
          (args-so-far ...) (checks-so-far ...) (body ...) arg . args)
       (%case-lambda-checked
        (clauses-so-far ...) (clauses-to-process ...)
        (args-so-far ... arg) (checks-so-far ...) (body ...) . args))
      ((_ (clauses-so-far ...) (clauses-to-process ...)
          (args-so-far ...) (checks-so-far ...) (body ...) . arg)
       (%case-lambda-checked
        (clauses-so-far ...) (clauses-to-process ...)
        (args-so-far ... . arg) (checks-so-far ...) (body ...)))))
  (define-syntax case-lambda-checked
    (syntax-rules ()
      ((_ (() first-body ...) rest-clauses ...)
       (%case-lambda-checked () (rest-clauses ...) () () (first-body ...)))
      ((_ ((first-arg . first-args) first-body ...) rest-clauses ...)
       (%case-lambda-checked () (rest-clauses ...) () () (first-body ...) first-arg . first-args))
      ((_ (args-var first-body ...) rest-clauses ...)
       (%case-lambda-checked () (rest-clauses ...) args-var () (first-body ...))))))
  (else
   (define-syntax case-lambda-checked
     (syntax-rules ()
       ((_ rest ...)
        (begin))))))

(cond-expand
 (gauche) ;; Too hard to implement
 (srfi-227
  (define-syntax %opt-lambda-checked
    (syntax-rules ()
      ((_ (body ...) args (checks ...))
       (opt-lambda*
        args
        checks ...
        body ...))
      ((_ body (args ...) (checks ...) (arg val) . rest)
       (%opt-lambda-checked
        body
        (args ... (arg val)) (checks ...) . rest))
      ((_ body (args ...) (checks ...) (arg val pred) . rest)
       (%opt-lambda-checked
        body
        (args ... (arg val))
        (checks ... (check-arg pred arg 'lambda-checked))
        . rest))
      ((_ body (args ...) (checks ...) arg . rest)
       (%opt-lambda-checked
        body
        (args ... arg) (checks ...) . rest))
      ((_ body (args ...) (checks ...) . last)
       (%opt-lambda-checked
        body
        (args ... . last) (checks ...)))))
  (define-syntax opt-lambda-checked
    (syntax-rules ()
      ((_ () body ...)
       (lambda () body ...))
      ((_ (arg . args) body ...)
       (%opt-lambda-checked (body ...) () () arg . args))
      ;; Case of arg->list lambda, no-op.
      ((_ arg body ...)
       (opt-lambda* arg body ...))))
  (define-syntax define-optionals-checked
    (syntax-rules ()
      ;; Function
      ((_ (name arg ...) body ...)
       (define name (opt-lambda-checked (arg ...) body ...)))
      ;; Variable
      ((_ name pred value)
       (define name (values-checked (pred) value))))))
  (else
   (define-syntax opt-lambda-checked
     (syntax-rules ()
       ((_ rest ...)
        (begin))))
   (define-syntax define-optionals-checked
     (syntax-rules ()
       ((_ rest ...)
        (begin))))))

(cond-expand
  (chicken
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
       ((_ name integer?)     (: name number))
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
        (%declare-checked-fn name (check ...) (type ... number)))
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
       ((_ (name arg ...) body ...)
        (begin
          (%declare-checked-fn name (arg ...) ())
          (define name (%lambda-checked name (body ...) () () . args))))
       ;; Variable
       ((_ name pred value)
        (begin
          (%declare-checked-var name pred)
          (define name (values-checked (pred) value)))))))
  (gauche
   (define-syntax define-checked
     (syntax-rules ()
       ;; Procedure
       ((_ (name args ...) body ...)
        (define name (%lambda-checked name (body ...) () () args ...)))
       ;; Variable
       ((_ name pred value)
        (define name (values-checked (pred) value))))))
  (else
   (define-syntax define-checked
     (syntax-rules ()
       ;; Procedure
       ((_ (name . args) body ...)
        (define name (%lambda-checked name (body ...) () () . args)))
       ;; Variable
       ((_ name pred value)
        (define name (values-checked (pred) value)))))))

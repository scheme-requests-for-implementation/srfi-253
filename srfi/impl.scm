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
   (use-modules (rnrs base) (srfi srfi-16) (srfi srfi-9)))
  (chicken
   (import-for-syntax (chicken type)))
  (else))

(cond-expand
 (srfi-145)
 ((or r6rs chicken loko)
  (define-syntax assume
    (syntax-rules ()
      ((_ expr rest ...)
       (begin (assert expr) #t)))))
 (debug
  (define-syntax assume
    (syntax-rules ()
      ((_ expr rest ...)
       (or (and expr #t)
           (error "assumption violated" 'expr rest ...))))))
 (else (define-syntax assume
         (syntax-rules ()
           ((_ rest ...)
            (begin #t))))))

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
        (assume (is-a? val <integer>) "type mismatch" <integer> val rest ...))
       ((_ exact-integer? val rest ...)
        (assume (is-a? val <integer>) "type mismatch" <integer> val rest ...))
       ((_ boolean? val rest ...)
        (assume (is-a? val <boolean>) "type mismatch" <boolean> val rest ...))
       ((_ char? val rest ...)
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
  (kawa
   (define-syntax check-arg
     (syntax-rules (:: !
                    number? complex? real? rational? exact-integer? integer? inexact?
                    symbol? keyword? list? pair? string? char? vector? procedure? input-port? output-port?

                    number complex real rational integer double
                    symbol keyword list pair string character vector procedure input-port output-port)
       ((_ number? val . rest)      (begin (! var :: number val) #t))
       ((_ complex? val . rest)     (begin (! var :: complex val) #t))
       ((_ real? val . rest)        (begin (! var :: real val) #t))
       ((_ rational? val . rest)    (begin (! var :: rational val) #t))
       ((_ exact-integer? val . rest)    (begin (! var :: integer val) #t))
       ((_ integer? val . rest)     (begin (! var :: number val) #t))
       ((_ inexact? val . rest)     (begin (! var :: double val) #t))
       ((_ symbol? val . rest)      (begin (! var :: symbol val) #t))
       ((_ keyword? val . rest)     (begin (! var :: keyword val) #t))
       ((_ list? val . rest)        (begin (! var :: list val) #t))
       ((_ pair? val . rest)        (begin (! var :: pair val) #t))
       ((_ string? val . rest)      (begin (! var :: string val) #t))
       ((_ char? val . rest)        (begin (! var :: character val) #t))
       ((_ vector? val . rest)      (begin (! var :: vector val) #t))
       ((_ procedure? val . rest)   (begin (! var :: procedure val) #t))
       ((_ input-port? val . rest)  (begin (! var :: input-port val) #t))
       ((_ output-port? val . rest) (begin (! var :: output-port val) #t))
       ((_ pred val . rest)
        (assume (pred val) "argument should match the specification"  '(pred val) val . rest)))))
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
       ((_ (fixnum?) value)      (the fixnum value))
       ((_ (flonum?) value)      (the float value))
       ((_ (exact-integer?) value) (the integer value))
       ((_ (integer?) value)     (the number value))
       ((_ (boolean?) value)     (the boolean value))
       ((_ (char?) value)        (the char value))
       ((_ (complex?) value)     (the cplxnum value))
       ((_ (eof?) value)         (the eof value))
       ((_ (inexact?) value)     (the float value))
       ((_ (real?) value)        (the number value))
       ((_ (list?) value)        (the list value))
       ((_ (null?) value)        (the null value))
       ((_ (number?) value)      (the number value))
       ((_ (pair?) value)        (the pair value))
       ((_ (input-port?) value)  (the input-port value))
       ((_ (output-port?) value) (the output-port value))
       ((_ (procedure?) value)   (the procedure value))
       ((_ (rational?) value)    (the ratnum value))
       ((_ (string?) value)      (the string value))
       ((_ (symbol?) value)      (the symbol value))
       ((_ (keyword?) value)     (the keyword value))
       ((_ (vector?) value)      (the vector value))
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

(cond-expand
  (kawa
   (define-syntax %lambda-checked
     (syntax-rules (::
                    number? complex? real? rational? exact-integer? integer? inexact?
                    symbol? keyword? list? pair? string? char?
                    vector? procedure? input-port? output-port?

                    number complex real rational integer double
                    symbol keyword list pair string character
                    vector procedure input-port output-port)
       ((_ name (body ...) (args ...) (checks ...))
        (lambda (args ...)
          checks ...
          body ...))
       ((_ name body (args ...) (checks ...) (arg number?) . rest)
        (%lambda-checked name body (args ... arg :: number) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg complex?) . rest)
        (%lambda-checked name body (args ... arg :: complex) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg real?) . rest)
        (%lambda-checked name body (args ... arg :: real) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg rational?) . rest)
        (%lambda-checked name body (args ... arg :: rational) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg exact-integer?) . rest)
        (%lambda-checked name body (args ... arg :: integer) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg integer?) . rest)
        (%lambda-checked name body (args ... arg :: integer) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg inexact?) . rest)
        (%lambda-checked name body (args ... arg :: double) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg symbol?) . rest)
        (%lambda-checked name body (args ... arg :: symbol) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg keyword?) . rest)
        (%lambda-checked name body (args ... arg :: keyword) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg list?) . rest)
        (%lambda-checked name body (args ... arg :: list) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg pair?) . rest)
        (%lambda-checked name body (args ... arg :: pair) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg string?) . rest)
        (%lambda-checked name body (args ... arg :: string) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg char?) . rest)
        (%lambda-checked name body (args ... arg :: character) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg vector?) . rest)
        (%lambda-checked name body (args ... arg :: vector) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg procedure?) . rest)
        (%lambda-checked name body (args ... arg :: procedure) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg input-port?) . rest)
        (%lambda-checked name body (args ... arg :: input-port) (checks ...) . rest))
       ((_ name body (args ...) (checks ...) (arg output-port?) . rest)
        (%lambda-checked name body (args ... arg :: output-port) (checks ...) . rest))
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
  ((or gauche chicken)
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
 ((or gauche chicken)
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
 ((or chicken gauche)
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
          (((args-to-process ...) body-to-process ...) clauses-to-process ...)
          args-so-far (checks-so-far ...) (body ...))
       (%case-lambda-checked
        (clauses-so-far ... (args-so-far checks-so-far ... body ...))
        (clauses-to-process ...)
        () () (body-to-process ...) args-to-process ...))
      ((_ (clauses-so-far ...)
          ((arg-to-process body-to-process ...) clauses-to-process ...)
          args-so-far (checks-so-far ...) (body ...))
       (%case-lambda-checked
        (clauses-so-far ... (args-so-far checks-so-far ... body ...))
        (clauses-to-process ...)
        arg-to-process () (body-to-process ...)))
      ((_ (clauses-so-far ...) (clauses-to-process ...)
          (args-so-far ...) (checks-so-far ...) (body ...) (arg pred) args ...)
       (%case-lambda-checked
        (clauses-so-far ...) (clauses-to-process ...)
        (args-so-far ... arg)
        (checks-so-far ... (check-arg pred arg 'case-lambda-checked))
        (body ...) args ...))
      ((_ (clauses-so-far ...) (clauses-to-process ...)
          (args-so-far ...) (checks-so-far ...) (body ...) arg args ...)
       (%case-lambda-checked
        (clauses-so-far ...) (clauses-to-process ...)
        (args-so-far ... arg) (checks-so-far ...) (body ...) args ...))))
  (define-syntax case-lambda-checked
    (syntax-rules ()
      ((_ (() first-body ...) rest-clauses ...)
       (%case-lambda-checked () (rest-clauses ...) () () (first-body ...)))
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
          (define name (%lambda-checked name (body ...) () () arg ...))))
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

(define-syntax %define-record-type-checked
  (syntax-rules ()
    ((_ type-name constructor predicate
        (fields ...) (field-wrappers ...))
     (begin
       (define-record-type
           type-name constructor predicate
           fields ...)
       field-wrappers ...))
    ((_ type-name constructor predicate
        (fields ...) (field-wrappers ...) (field pred accessor modifier)
        fields-to-process ...)
     (%define-record-type-checked
      type-name constructor predicate
      (fields ... (field internal-accessor internal-modifier))
      (field-wrappers
       ...
       (define-checked (accessor (record predicate))
         (internal-accessor record))
       (define-checked (modifier (record predicate) (val pred))
         (internal-modifier record val)))
      fields-to-process ...))
    ((_ type-name constructor predicate
        (fields ...) (field-wrappers ...) (field pred accessor)
        fields-to-process ...)
     (%define-record-type-checked
      type-name constructor predicate
      (fields ... (field internal-accessor))
      (field-wrappers
       ...
       (define-checked (accessor (record predicate))
         (internal-accessor record)))
      fields-to-process ...))))
(define-syntax %wrap-constructor
  (syntax-rules ()
    ((_ constructor internal-constructor (arg-names ...) (args ...))
     (define-checked (constructor args ...)
       (internal-constructor arg-names ...)))
    ((_ constructor internal-constructor (arg-names ...) (args ...)
        (name pred rest ...) fields-to-process ...)
     (%wrap-constructor constructor internal-constructor
                        (arg-names ... name) (args ... (name pred))
                        fields-to-process ...))))

(define-syntax define-record-type-checked
  (syntax-rules ()
    ((_ type-name (constructor constructor-args ...) predicate field ...)
     (begin
       (%define-record-type-checked
        type-name
        (internal-constructor constructor-args ...)
        predicate
        () () field ...)
       (%wrap-constructor constructor internal-constructor () () field ...)))))

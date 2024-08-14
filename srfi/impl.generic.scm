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
 (srfi-145 #t)
 ((or r6rs chicken loko)
  (define-syntax assume
    (syntax-rules ()
      ((_ expr . rest)
       (assert expr)))))
 (else (define-syntax assume
         (syntax-rules ()
           ((_ expr . rest)
            (or expr
                (error "assumption violated" 'expr . rest)))))))

(cond-expand
 (guile
  (use-modules (rnrs base) (srfi srfi-16)))
 (else))

(define-syntax check-arg
  (syntax-rules ()
    ((_ pred val . rest)
     (assume (pred val) "argument should match the specification"  '(pred val) val . rest))))

(define-syntax values-checked
  (syntax-rules ()
    ((_ (predicate) value)
     (let ((v value))
       (check-arg predicate v)
       v))
    ((_ (predicate ...) value ...)
     (values (values-checked (predicate) value) ...))))

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

(define-syntax %lambda-checked
  (syntax-rules ()
    ((_ (body ...) args (checks ...))
     (lambda args
       checks ...
       body ...))
    ((_ body (args ...) (checks ...) (arg pred) . rest)
     (%lambda-checked
      body
      (args ... arg) (checks ... (check-arg pred arg 'lambda-checked)) . rest))
    ((_ body (args ...) (checks ...) arg . rest)
     (%lambda-checked
      body
      (args ... arg) (checks ...) . rest))
    ((_ body (args ...) (checks ...) . last)
     (%lambda-checked
      body
      (args ... . last) (checks ...)))))

(define-syntax lambda-checked
  (syntax-rules ()
    ((_ () body ...)
     (lambda () body ...))
    ((_ (arg . args) body ...)
     (%lambda-checked (body ...) () () arg . args))
    ;; Case of arg->list lambda, no-op.
    ((_ arg body ...)
     (lambda arg body ...))))

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
         (args-so-far ... arg) (checks-so-far ... (check-arg pred arg)) (body ...) . args))
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
  (else))

(cond-expand
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
  (else))

(define-syntax define-checked
  (syntax-rules ()
    ;; Procedure
    ((_ (name . args) body ...)
     (define name (lambda-checked args body ...)))
    ;; Variable
    ((_ name pred value)
     (define name (values-checked (pred) value)))))

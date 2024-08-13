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


;; Common Lisp: typecase, the, check-type, assert, declare type/ftype

;; Standard Scheme: R6RS assert and assertion-violation

;; Gambit
(declare
 (gambit-scheme)
 (separate)
 (inline)
 (inline-primitives)
 (inlining-limit 370)
 (allocation-limit #t)
 (constant-fold)
 (lambda-lift)
 (not standard-bindings)
 (not extended-bindings)
 (run-time-bindings)
 (safe)
 (interrupts-enabled)
 (not poll-on-return)
 (not debug)          ;; depends on debugging command line options
 (debug-location)     ;; depends on debugging command line options
 (debug-source)       ;; depends on debugging command line options
 (debug-environments) ;; depends on debugging command line options
 (proper-tail-calls)
 (not generative-lambda)
 (optimize-dead-local-variables)
 (not optimize-dead-definitions)
 (generic)
 (mostly-fixnum-flonum))

;; Chicken
(: sf (fixnum string -> string))

;; SRFI-1, used in e.g. SRFI-13
(define (check-arg pred val caller)
  (let lp ((val val))
    (if (pred val) val (lp (error "Bad argument" val pred caller)))))

;; COND-EXPAND is doing checks for features, not predicates. But
;; still, worth referring to.

;; SRFI-6
(define (restrict f pred?)
  (lambda (x . rest)
    (if (pred? x)
        (apply f x rest)
        (error))))

;; SRFI-43
(define (check-type pred? value callee)
  (if (pred? value)
      value
      ;; Recur: when (or if) the user gets a debugger prompt, he can
      ;; proceed where the call to ERROR was with the correct value.
      (check-type pred?
                  (error "erroneous value"
                         (list pred? value)
                         `(while calling ,callee))
                  callee)))

;; SRFI-44
(define (check-arg pred? val caller)
  (if (pred? val)
      #t
      (check-arg pred?
                 (error "Bad argument"
                        val
                        pred?
                        caller)
                 caller)))

;; SRFI-50
;; void SCHEME_CHECK_SYMBOL(scheme_value, int)
;; void SCHEME_CHECK_PAIR(scheme_value, int)
;; void SCHEME_CHECK_VECTOR(scheme_value, int)
;; void SCHEME_CHECK_STRING(scheme_value, int)
;; void SCHEME_CHECK_CHAR(scheme_value, int)
;; void SCHEME_CHECK_INTEGER(scheme_value, int)
;; void SCHEME_CHECK_RATIONAL(scheme_value, int)
;; void SCHEME_CHECK_REAL(scheme_value, int)
;; void SCHEME_CHECK_COMPLEX(scheme_value, int)
;; void SCHEME_CHECK_NUMBER(scheme_value, int)
;; void SCHEME_CHECK_RECORD(scheme_value, int)
;; void SCHEME_CHECK_SHARED_BINDING(scheme_value, int)

;; SRFI-51
(define-syntax err-and
  (syntax-rules ()
    ((err-and err expression ...)
     (and (or expression
	      (if (string? err)
		  (error err 'expression)
		  (error "false expression" 'expression err)))
	  ...))))

;; SRFI-57
(define-syntax define-method
  (syntax-rules ()
    ((define-method (generic (arg pred?) ...) . body)
     (define-method generic (pred? ...) (arg ...) (lambda (arg ...) . body)))
    ((define-method generic (pred? ...) (arg ...) procedure)
     (let ((next ((generic) 'get-proc))
           (proc procedure))
       (((generic) 'set-proc)
        (lambda (arg ...)
          (if (and (pred? arg) ...)
              (proc arg ...)
              (next arg ...))))))))

;; SRFI-66
(define (ensure-octet thing)
  (if (not (and (integer? thing)
		(exact? thing)
		(>= thing 0)
		(<= thing 255)))
      (error "not a octet" thing)))

;; See SRFI-67 for custom comparisons?

;; SRFI-74.
(define (ensure-aligned index base)
  (if (not (zero? (remainder index base)))
      (error "non-aligned blob access" index base)))

;; SRFI-76
(define (ensure-has-vector-type type typed-vector)
  (if (not (has-vector-type? type typed-vector))
      (error "invalid argument: not of type" type typed-vector)))

;; SRFI-64 and SRFI-78

;; SRFI-79
(define (ensure-output-stream-open output-stream)
  (if  (not (eq? (output-stream-status open)
   (output-stream-current-status output-stream)))
       (raise
 (condition
  (&message
   (message "output stream closed"))
  (&i/o-stream-error
   (stream output-stream))
  (&i/o-closed-error)))))
(define (ensure-open)
  (if (not b)
      (raise (condition
       (&message
        (message "blob reader closed"))
       (&i/o-closed-error)
       (&i/o-reader/writer-error
        (reader/writer reader))))))

;; SRFI-92 and SRFI-187 as type-checking

;; SRFI-94
(define (type-error procedure . args)
  (apply error procedure args))
(define (must-be-exact-integer2 name proc)
  (lambda (n1 n2)
    (if (and (integer? n1) (integer? n2) (exact? n1) (exact? n2))
 (proc n1 n2)
 (type-error name n1 n2))))

(define (must-be-real name proc)
  (lambda (x1)
    (if (real? x1) (proc x1) (type-error name x1))))
(define (must-be-real+ name proc)
  (lambda (x1)
    (if (and (real? x1) (>= x1 0))
 (proc x1)
 (type-error name x1))))
(define (must-be-real-1+1 name proc)
  (lambda (x1)
    (if (and (real? x1) (<= -1 x1 1))
 (proc x1)
 (type-error name x1))))

;; SRFI-113
(define (check-set obj) (if (not (set? obj)) (error "not a set" obj)))
(define (check-bag obj) (if (not (bag? obj)) (error "not a bag" obj)))

;; SRFI-117
(define (list-queue-remove-back! list-queue)
  (if (list-queue-empty? list-queue)
    (error "Empty list-queue"))
  ...)

;; SRFI-133
(define (check-type pred? value callee)
  (if (pred? value)
      value
      ;; Recur: when (or if) the user gets a debugger prompt, he can
      ;; proceed where the call to ERROR was with the correct value.
      (check-type pred?
                  (error "erroneous value"
                         (list pred? value)
                         `(while calling ,callee))
                  callee)))
;; + check-index, check-indices

;; SRFI-134
(define (%check-ideque x)
  (unless (ideque? x)
    (error "ideque expected, but got:" x)))

;; SRFI-135
(define (%illegal-utf16 bv i cp . rest)
  (if (null? rest)
      (error "illegal UTF-16: " bv i cp)
      (error "illegal UTF-16: " bv i cp (car rest))))

;; SRFI-144
(define (check-flonum! name x)
  (if (not (flonum? x))
      (error (string-append "non-flonum argument passed to "
                            (symbol->string name))
             x)))
;;; Given a symbol naming a flonum procedure and a generic operation,
;;; returns a flonum procedure that restricts the generic operation
;;; to flonum arguments and result.
(define (flop1 name op)
  (lambda (x)
    (check-flonum! name x)
    (let ((result (op x)))
      (if (not (flonum? result))
          (error (string-append "non-flonum result from "
                                (symbol->string name))
                 result))
      result)))
;; et cetera for other arities...

;; SRFI-145: assume

;; Guile flonum ops
(define (assert-flonum . args)
  (or (for-all flonum? args) (raise (make-assertion-violation))))
(define (assert-iflonum . args)
  (or (for-all (lambda (i) (and (flonum? i) (integer? i))) args)
      (raise (make-assertion-violation))))

;; SRFI-152
(define check-arg
   (lambda (pred val proc)
     (if (pred val) val (error "Bad arg" val pred proc))))
(define (check-substring-spec proc s start end)
  (if (not (substring-spec-ok? s start end))
      (error "Illegal substring spec." proc s start end)))

;; SRFI-167 uses assume
(define (okvs-open home . args)
  (assume (null? args))
  (assume (not home))
  (make-okvs (mapping (make-lexicographic-comparator))
             (make-hook 1)
             (make-hook 1)))

;; SRFI-170
(define (create-hard-link oldname newname)
  (if (not (string? oldname))
        (sanity-check-error "first argument must be a string" 'create-hard-link oldname))
  (if (not (string? newname))
        (sanity-check-error "second argument must be a string" 'create-hard-link newname))
    (if (not (%link oldname newname))
        (errno-error (errno) 'create-hard-link 'link oldname newname)))

;; Guix has a lot of check-* functions everywhere.

;; Dave Thompson's Chickadee
(define (make-sound-system)
  ;; It's possible that there are *no* audio devices available, in
  ;; which case open-device throws an exception.  In that case, return
  ;; #f.
  (let ((device (false-if-exception (openal:open-device))))
    (and device
         (%make-sound-system (openal:make-context device)
                             (make-array-list)
                             (make-hash-table)
                             (make-weak-key-hash-table)
                             (make-hash-table)
                             (make-array-list)
                             (make-hash-table)
                             (make-guardian)))))

;; SRFI-171:
;; helper function which ensures x is reduced.
(define (ensure-reduced x)
  (if (reduced? x)
      x
      (reduced x)))

;; SRFI-173 uses assume
(define (hook-run hook . args)
  (assume (= (length args) (hook-arity hook)))
  (for-each (lambda (proc) (apply proc args)) (hook-procs hook)))

;; SRFI-175
(define (ensure-int x) (if (char? x) (char->integer x) x))

;; SRFI-179
(define (interval-dimension interval)
  (cond ((not (interval? interval))
         (error "interval-dimension: The argument is not an interval: " interval))
        (else
         (%%interval-dimension interval))))

;; SRFI-180
(define (expect value other)
  (when (eof-object? value)
    (raise (make-json-error "Unexpected end-of-file.")))
  (assume (and (char? value) (char? other)) "invalid argument" '%json-tokens expect value other)
  (unless (char=? value other)
    (raise (make-json-error "Unexpected character."))))

;; SRFI-189 uses assume
(define (maybe= equal . maybes)
  (assume (procedure? equal))
  (assume (pair? maybes))
  (let ((maybe1 (car maybes)))
    (every (lambda (maybe2) (%maybe=2 equal maybe1 maybe2))
           (cdr maybes))))

;; Typed Racket (has U-nion types, Optional, Any)
(: x Number)
(define x 7)
(let ()
  (: x Number)
  (define x 7)
  (add1 x))
(define x : Number 7)
(define (id [z : Number]) : Number z)
(let ([x : Number 7])
  (add1 x))
(lambda ([x : String] . [y : Number *])
  (apply + y))
(ann (+ 7 1) Number)
(define p1 : Point (assert (cons 7 0) point?))

;; Guile (and many others) match macro
(match 4
  ((? integer? x) x))

;; SRFI-192 uses assume
(define (port-position port)
  (assume (port? port))
  (port-tell port))

;; SRFI-194
(define (make-random-integer-generator low-bound up-bound)
  (unless (and (integer? low-bound)
               (exact? low-bound))
    (error "expected exact integer for lower bound"))
  (unless (and (integer? up-bound)
               (exact? up-bound))
    (error "expected exact integer for upper bound"))
  (unless (< low-bound up-bound)
    (error "upper bound should be greater than lower bound"))
  (let ((rand-int-proc (random-source-make-integers (current-random-source)))
        (range (- up-bound low-bound)))
    (lambda ()
      (+ low-bound (rand-int-proc range)))))

;; SRFI-196 uses assume
;; The primary range constructor does some extra consistency checking.
(define (range length indexer)
  (assume (exact-natural? length))
  (assume (procedure? indexer))
  (raw-range 0 length indexer 0))

;; SRFI-203
(define (jpeg-file->painter filename)
  (unless (file-exists? filename)
    (error "File does not exist: " filename))
  (let ((payload (file->bytevector filename)))
    (lambda (frame) (idol payload frame))))

;; SRFI-205
(define (terminal-flow-control the-port the-action)
  (if (not (port? the-port))
      (sanity-check-error "first argument must be a port" 'terminal-flow-control the-port the-action))
  (let ((the-fd (port-fileno the-port)))
    (if (not (exact-integer? the-fd))
        (sanity-check-error "first argument must be a port associated with a file descriptor" 'terminal-flow-control the-port the-action))
    (if (not (exact-integer? the-action))
        (sanity-check-error "second argument must be an action that is an exact integer" 'terminal-flow-control the-port the-action))
    (if (not (%tcflow the-fd the-action))
             (errno-error (errno) 'terminal-flow-control 'tcflow the-port the-action))))

;; SRFI-209 uses assert
(define (enum-type-contains? type enum)
  (assert (enum-type? type))
  (assert (enum? enum))
  ((comparator-type-test-predicate (enum-type-comparator type)) enum))

;; SRFI-214 uses assume
(define (flexvector-ref fv index)
  (assume (flexvector? fv))
  (assume (integer? index))
  (assume (< -1 index (flexvector-length fv)))
  (vector-ref (vec fv) index))

;; SRFI-217 uses assume
(define (iset-unfold stop? mapper successor seed)
  (assume (procedure? stop?))
  (assume (procedure? mapper))
  (assume (procedure? successor))
  (let lp ((trie #f) (seed seed))
    (if (stop? seed)
        (raw-iset trie)
        (let ((n (mapper seed)))
          (assume (valid-integer? n))
          (lp (trie-insert trie n) (successor seed))))))

;; SRFI-224 uses assume
(define (alist->fxmapping/combinator comb as)
  (assume (procedure? comb))
  (assume (pair-or-null? as))
  (raw-fxmapping
   (fold (lambda (p trie)
           (assume (pair? p) "alist->fxmapping/combinator: not a pair")
           (trie-insert/combine trie (car p) (cdr p) comb))
         the-empty-trie
         as)))

;; SRFI-226 uses assert
(define unlock!
  (lambda ()
    (assert (locked?))
    (set! *lock* #f)))

;; SRFI-231
(define (index-rotate n k)
  (cond ((not (and (fixnum? n)
                   (fx<= 0 n)))
         (error "index-rotate: The first argument is not a nonnegative fixnum: " n k))
        ((not (and (fixnum? k)
                   (fx<= 0 k n)))
         (error "index-rotate: The second argument is not a fixnum between 0 and the first argument (inclusive): " n k))
        (else
         (%%index-rotate n k))))

;; SRFI-238
(define (typecheck-codeset object)
  (unless (symbol? object)
    (error "Not a codeset" object)))

;; SRFI-242 uses assert
(define make-constraint
  (lambda (box updater)
    (assert (box? box))
    (assert (procedure? updater))
    (lambda ()
      (define old (unbox box))
      (define new (updater old))
      (cond
       [(eqv? old new) #f]
       [else
	(set-box! box new)
	#t]))))

;; SRFI-249
(define-syntax assert-type
  (syntax-rules ()
    ((assert-type loc test . args)
     (unless test
       (error loc "type check failed" 'expr . args)))))
;; ...
;; Exported constructor.
(define (make-restarter tag description invoker)
  (assert-type 'make-restarter-tag (symbol? tag))
  (assert-type 'make-restarter-tag
               (and (pair? description)
                    (every string? description)))
  (assert-type 'make-restarter-tag (procedure? invoker))
  (make-raw-restarter tag description invoker))

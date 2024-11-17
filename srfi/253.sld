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

(define-library (srfi 253)
  (export check-arg values-checked
          check-case
          lambda-checked define-checked
          case-lambda-checked
          define-record-type-checked)
  ;; NOTE: Out of these implementations (ironically!) none are able to
  ;; load this file. Only Guile can. If one wants to integrate this
  ;; SRFI into an implementation or load it portably, they'd
  ;; (ironically) have to use implementation-specific hacks. --aartaka
  (cond-expand
    (chicken
     (import (chicken base)))
    (gambit
     (import (gambit)))
    (gauche
     (import (except (gauche base) check-arg)))
    (kawa
     (import (kawa base)))
    (sagittarius
     (import (sagittarius)))
    (else
     (import (scheme base)
             (scheme case-lambda))))
  (cond-expand
   (chicken)
   ((library (srfi 227))
    (import (srfi 227)))
   (else))
  (cond-expand
   ((or gauche gambit) (include "impl.scm"))
   (else (include-library-declarations "impl.scm"))))

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

(define-library (srfi xxx)
  (export check-arg values-checked
          let-checked lambda-checked define-checked
          case-lambda-checked)
  (cond-expand
   (chicken
    (import (chicken base)))
   (gambit
    (import (gambit)))
   (gauche
    (import (except (gauche base) chec-arg)))
   (kawa
    (import (kawa base)))
   (sagittarius
    (import (sagittarius)))
   (r7rs
    (import (scheme base)
            (scheme case-lambda))))
  (cond-expand
   ((and (not chicken)
         (library (srfi 227)))
    (import (srfi 227))
    (export opt-lambda-checked define-optionals-checked))
   (else))
  (include "impl.generic.scm")
  (cond-expand
   (chicken
    (include "impl.chicken.scm"))
   (gauche
    (include "impl.gauche.scm"))
   (kawa
    (include "impl.kawa.scm"))
   (stklos
    (include "impl.stklos.scm"))
   (else)))

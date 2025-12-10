(cond-expand
 (guile
  (import (ice-9 rdelim))
  (define-checked (read-line (port port?))
    (car (%read-line port))))
 (csi
  (import (chicken io)))
 (csc
  (import (chicken io)))
 (else))

(import (srfi 13))
(import (srfi 1))

;; True when either of PREDICATES is true
;; Poor person’s union type
(define (disjoin . predicates)
  (lambda (x)
    (let rec ((predicates predicates))
      (cond
       ((null? predicates)   #f)
       (((car predicates) x) #t)
       (else                 (rec (cdr predicates)))))))

;; True when every element in the provided list
;; Satisfies at least one of the PREDICATES
(define (list-of? . predicates)
  (lambda (x)
    (check-arg list? x 'list-of?)
    (let rec ((pred (apply disjoin predicates))
              (args x))
      (cond
       ((null? args)      #t)
       ((pred (car args)) (rec pred (cdr args)))
       (else              #f)))))

;; Read all the lines in FILE, return a list of string
(define-checked (read-file-lines (file string?))
  (let ((in (open-input-file file)))
    (let lines ((acc (list))
                (new-line (read-line in)))
      (if (eof-object? new-line)
          (values-checked ((list-of? string?))
                          (begin
                            (close-input-port in)
                            acc))
          (lines (append acc (list new-line))
                 (read-line in))))))

;; Split the STR in two based on the SEPARATOR
;; Used to split key-value pairs in .INI file
(define-checked (split-on-separator (str string?) (separator char?))
  (let rec ((idx 0))
    (values-checked
     ((disjoin boolean?
               (list-of? string?)))
     (cond
      ((= idx (string-length str))
       #f)
      ((char=? separator (string-ref str idx))
       (list (string-trim-right (string-copy str 0 idx) char-whitespace?)
             (string-trim (string-copy str (+ 1 idx)) char-whitespace?)))
      (else (rec (+ 1 idx)))))))

;; Alias for the parsed .INI structure—list of lists of strings
(define ini? (list-of? (list-of? string?)))

;; Parse the .INI FILE to a list of key-value pairs
(define read-ini
  (case-lambda-checked
   (((file string?))
    (read-ini file #\=))
   (((file string?) (separator char?))
    (values-checked (ini?)
                    (filter identity
                            (map (lambda (line)
                                   (split-on-separator line separator))
                                 (read-file-lines file)))))))

;; Get a string value from INI residing under a string KEY
(define-checked (get-val (ini ini?) (key string?))
  (let ((found-pairs (filter (lambda (kv) (string=? key (car kv)))
                             ini)))
    (if (null? found-pairs)
        #f
        (cadar found-pairs))))

;; Get a string value from INI residing under a string KEY
;; Essentially a type-checked GET-VAL
(define-checked (get-string-val (ini ini?) (key string?))
  (values-checked (string?) (get-val ini key)))

;; Get a number value from INI residing under a string KEY
(define-checked (get-number-val (ini ini?) (key string?))
  (values-checked (number?) (string->number (get-val ini key))))

;; Checked record type allowing strings and numbers as entry
;; values. Providing e.g. a symbol is an error:
;; (make-ini-entry "key" 'value) => !!!
(define-record-type-checked <ini-entry>
  (make-ini-entry key value)
  ini-entry?
  (key string? ini-entry-key)
  (value (disjoin number? string?) ini-entry-value set-ini-entry-value!))

;; Not a particularly useful function converting
;; list-of-lists-of-strings (ini?) into a list of records with number
;; conversion in the process.
(define-checked (recordify-ini (ini ini?))
  (values-checked (list-of? ini-entry?)
                  (map (lambda (key-value)
                         (let* ((value (cadr key-value))
                                (number-value (string->number value)))
                           (make-ini-entry (car key-value) (or number-value value))))
                       ini)))

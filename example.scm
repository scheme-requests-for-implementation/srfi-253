(cond-expand
 (guile
  (import (ice-9 rdelim))
  (define-checked (read-line (port port?))
    (car (%read-line port)))))

(define (disjoin . predicates)
  (lambda (x)
    (let rec ((predicates predicates))
      (cond
       ((null? predicates)   #f)
       (((car predicates) x) #t)
       (else                 (rec (cdr predicates)))))))

(define (list-of? . predicates)
  (lambda (x)
    (check-arg list? x 'list-of?)
    (let rec ((pred (apply disjoin predicates))
              (args x))
      (cond
       ((null? args)      #t)
       ((pred (car args)) (rec pred (cdr args)))
       (else              #f)))))

(define-checked (read-file-lines (file-name string?))
  (let ((in (open-input-file file-name)))
    (let lines ((acc (list))
                (new-line (read-line in)))
      (display new-line)
      (newline)
      (if (eof-object? new-line)
          (values-checked ((list-of? string?))
                          (begin
                            (close-input-port in)
                            acc))
          (lines (append acc (list new-line))
                 (read-line in))))))

(define-checked (split-on-eq (str string?))
  (let rec ((idx 0))
    (values-checked
     ((disjoin boolean?
               (list-of? string?)))
     (cond
      ((= idx (string-length str))
       #f)
      ((char=? #\= (string-ref str idx))
       (list (string-copy str 0 idx)
             (string-copy str (+ 1 idx))))
      (else (rec (+ 1 idx)))))))

(define-checked (read-ini (file-name string?))
  (filter identity
          (map split-on-eq (read-file-lines file-name))))

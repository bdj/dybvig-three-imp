#lang scheme
(provide (all-defined-out))

(define-syntax-rule (record val (var ...) exp ...)
  (apply (lambda (var ...) exp ...) val))

(define-syntax record-case
  (syntax-rules (else)
    [(record-case exp1
       [key vars exp2 ...]
       ...
       [else exp3 ...])   
     (let ([r exp1])
       (cond
         [(eq? (car r) 'key)
          (record (cdr r) vars exp2 ...)]
         ...
         [else exp3 ...]))]
    [(record-case exp1
       [key vars exp2 ...]
       ...)
     (let ([r exp1])
       (cond
         [(eq? (car r) 'key)
          (record (cdr r) vars exp2 ...)]
         ...))]))
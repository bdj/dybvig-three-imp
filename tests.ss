#lang scheme
(require tests/eli-tester)
(require (prefix-in 3-4: "dybvig-3-4.ss"))
(require (prefix-in 3-5: "dybvig-3-5.ss"))
(require (prefix-in 4-1: "dybvig-4-1.ss"))
(require (prefix-in 4-2: "dybvig-4-2.ss"))
(require (prefix-in 4-3: "dybvig-4-3.ss"))
(require (prefix-in 4-4: "dybvig-4-4.ss"))
(require (prefix-in 4-5: "dybvig-4-5.ss"))
(require (prefix-in 4-6: "dybvig-4-6.ss"))

(define (apply-test eval)
  (test (eval '((lambda (x y) y) 3 4)) =>  4))

(define (if-test eval)
  (test (eval '(if #t 3 4)) => 3
        (eval '(if #f 3 4)) => 4))

(define (tester* f f-name ts)
  (parameterize ([failure-message (lambda (expr fmt . args)
                                    (format "~a > ~a: ~a" f-name expr 
                                            (apply format fmt args)))])
    (test
     (for ([t ts])
       (t f)))))

(define-syntax-rule (tester f (t ...)) (tester* f (symbol->string 'f) (list t ...)))

(test
 (tester 3-4:evaluate (apply-test if-test))
 (tester 3-5:evaluate (apply-test if-test))
 (tester 4-1:evaluate (apply-test if-test))
 (tester 4-2:evaluate (apply-test if-test))
 (tester 4-3:evaluate (apply-test if-test))
 (tester 4-4:evaluate (apply-test if-test))
 (tester 4-5:evaluate (apply-test if-test))
 (tester 4-6:evaluate (apply-test if-test)))
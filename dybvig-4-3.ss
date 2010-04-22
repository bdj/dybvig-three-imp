#lang scheme
(require "dybvig-macros.ss")
(provide evaluate)

(define stack (make-vector 1000))

(define push
  (lambda (x s)
    (vector-set! stack s x)
    (+ s 1)))

(define index
  (lambda (s i)
    (vector-ref stack (- (- s i) 1))))

(define compile 
  (lambda (x e next)
    (cond
      [(symbol? x)
       (compile-lookup x e 
         (lambda (n m)
           (list 'refer n m next)))]
      [(pair? x)
       (record-case x
         [quote (obj)
          (list 'constant obj next)]
         [lambda (vars body)
          (list 'close 
                (compile body 
                         (extend e vars) 
                         (list 'return (+ (length vars) 1))) 
                next)]
         [if (test then else)
          (let ([thenc (compile then e next)]
                [elsec (compile else e next)])
            (compile test e (list 'test thenc elsec)))]
         [call/cc (x)
          (list 'frame
                next
                (list 'conti
                      (list 'argument
                            (compile x e '(apply)))))]
         [else
          (let loop ([args (cdr x)]
                     [c (compile (car x) e '(apply))])
            (if (null? args)
                (list 'frame next c)
                (loop (cdr args)
                      (compile (car args) 
                               e
                               (list 'argument c)))))])]
      [else
       (list 'constant x next)])))

(define VM 
  (lambda (a x e s)
    (record-case x
      [halt () a]
      [refer (n m x)
       (VM (index (find-link n e) m) x e s)]
      [constant (obj x)
       (VM obj x e s)]
      [close (body x)
       (VM (closure body e) x e s)]
      [test (then else)
       (VM a (if a then else) e s)]
      [conti (x)
       (VM (continuation s) x e s)]
      [nuate (stack x)
       (VM a x e (restore-stack stack))]
      [frame (ret x)
       (VM a x e (push ret (push e s)))]
      [argument (x)
       (VM a x e (push a s))]
      [apply ()
       (record a (body link)
        (VM a body s (push link s)))]
      [return (n)
       (let ([s (- s n)])
         (VM a (index s 0) (index s 1) (- s 2)))])))

(define compile-lookup
  (lambda (var e return)
    (let nxtrib ([e e] [rib 0])
      (let nxtelt ([vars (car e)] [elt 0])
        (cond
          [(null? vars) (nxtrib (cdr e) (+ rib 1))]
          [(eq? (car vars) var) (return rib elt)]
          [else (nxtelt (cdr vars) (+ elt 1))])))))

(define closure 
  (lambda (body e)
    (list body e)))

(define continuation 
  (lambda (s)
    (closure (list 'refer 0 0 (list 'nuate (save-stack s) '(return 0))) '())))

(define save-stack
  (lambda (s)
    (let ([v (make-vector s)])
      (let copy ([i 0])
        (unless (= i s)
          (vector-set! v i (vector-ref stack i))
          (copy (+ i 1)))))))

(define restore-stack
  (lambda (v)
    (let ([s (vector-length v)])
      (let copy ([i 0])
        (unless (= i s)
          (vector-set! stack i (vector-ref v i))
          (copy (+ i 1))))
      s)))

(define find-link
  (lambda (n e)
    (if (= n 0)
        e
        (find-link (- n 1) (index e -1)))))

(define extend 
  (lambda (e r)
    (cons r e)))

(define evaluate 
  (lambda (x)
    (VM '() (compile x '() '(halt)) '() 0)))


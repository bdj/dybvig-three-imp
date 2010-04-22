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
       (list 'refer (compile-lookup x e) next)]
      [(pair? x)
       (record-case x
         [quote (obj)
          (list 'constant obj next)]
         [lambda (vars body)
          (list 'close (compile body (extend e vars) '(return)) next)]
         [if (test then else)
          (let ([thenc (compile then e next)]
                [elsec (compile else e next)])
            (compile test e (list 'test thenc elsec)))]
         [set! (var x)
          (let ([access (compile-lookup var e)])
            (compile x e (list 'assign access next)))]
         [call/cc (x)
          (let ([c (list 'conti
                         (list 'argument
                               (compile x e '(apply))))])
            (if (tail? next)
                c
                (list 'frame next c)))]
         [else
          (let loop ([args (cdr x)]
                     [c (compile (car x) e '(apply))])
            (if (null? args)
                (if (tail? next)
                    c
                    (list 'frame next c))
                (loop (cdr args)
                      (compile (car args) e
                               (list 'argument c)))))])]
      [else
       (list 'constant x next)])))

(define VM 
  (lambda (a x e r s)
    (record-case x
      [halt () a]
      [refer (var x)
       (VM (mcar (lookup var e)) x e r s)]
      [constant (obj x)
       (VM obj x e r s)]
      [close (body x)
       (VM (closure body e) x e r s)]
      [test (then else)
       (VM a (if a then else) e r s)]
      [assign (var x)
       (set-mcar! (lookup var e) a)
       (VM a x e r s)]
      [conti (x)
       (VM (continuation s) x e r s)]
      [nuate (stack var)
       (VM a x e r (restore-stack stack))]
      [frame (ret x)
       (VM a x e '() (push ret (push e (push r s))))]
      [argument (x)
       (VM a x e (mcons a r) s)]
      [apply ()
       (record a (body e)
        (VM a body (extend e r) '() s))]
      [return ()
       (VM a (index s 0) (index s 1) (index s 2) (- s 3))])))

(define compile-lookup 
  (lambda (var e)
    (let nxtrib ([e e] [rib 0])
      (let nxtelt ([vars (car e)] [elt 0])
        (cond
          [(null? vars) (nxtrib (cdr e) (+ rib 1))]
          [(eq? (car vars) var) (cons rib elt)]
          [else (nxtelt (cdr vars) (+ elt 1))])))))

(define lookup 
  (lambda (access e)
    (let nxtrib ([e e] [rib (car access)])
      (if (= rib 0)
          (let nxtelt ([r (car e)] [elt (cdr access)])
            (if (= elt 0)
                r
                (nxtelt (mcdr r) (- elt 1))))
          (nxtrib (cdr e) (- rib 1))))))

(define closure 
  (lambda (body e)
    (list body e)))

(define continuation 
  (lambda (s)
    (closure (list 'refer 0 0 (list 'nuate (save-stack s) '(return))) '())))

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

(define extend 
  (lambda (e r)
    (cons r e)))

(define evaluate 
  (lambda (x)
    (VM '() (compile x '() '(halt)) '() '() 0)))

(define tail? 
  (lambda (next)
    (eq? (car next) 'return)))


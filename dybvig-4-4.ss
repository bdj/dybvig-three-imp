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

(define find-free
  (lambda (x b)
    (cond
      [(symbol? x) (if (set-member? x b) '() (list x))]
      [(pair? x)
       (record-case x
         [quote (obj) '()]
         [lambda (vars body)
          (find-free body (set-union vars b))]
         [if (test then else)
          (set-union (find-free test b)
                     (set-union (find-free then b)
                                (find-free else b)))]
         [call/cc (exp) (find-free exp b)]
         [else
          (let next ([x x])
            (if (null? x)
                '()
                (set-union (find-free (car x) b)
                           (next (cdr x)))))])]
      [else '()])))

(define set-member?
  (lambda (x s)
    (cond
      [(null? s) '()]
      [(eq? x (car s)) 't]
      [else (set-member? x (cdr s))])))

(define set-cons
  (lambda (x s)
    (if (set-member? x s)
        s
        (cons x s))))

(define set-union
  (lambda (s1 s2)
    (if (null? s1)
        s2
        (set-union (cdr s1) (set-cons (car s1) s2)))))

(define set-minus
  (lambda (s1 s2)
    (if (null? s1)
        '()
        (if (set-member? (car s1) s2)
            (set-minus (cdr s1) s2)
            (cons (car s1) (set-minus (cdr s1) s2))))))

(define set-intersect
  (lambda (s1 s2)
    (if (null? s1)
        '()
        (if (set-member? (car s1) s2)
            (cons (car s1) (set-intersect (cdr s1) s2))
            (set-intersect (cdr s1) s2)))))

(define compile
  (lambda (x e next)
    (cond
      [(symbol? x) (compile-refer x e next)]
      [(pair? x)
       (record-case x
         [quote (obj) (list 'constant obj next)]
         [lambda (vars body)
           (let ([free (find-free body vars)])
             (collect-free free e
               (list 'close
                     (length free)
                     (compile body
                              (cons vars free)
                              (list 'return
                                    (length vars)))
                     next)))]
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
      [else (list 'constant x next)])))

(define collect-free
  (lambda (vars e next)
    (if (null? vars)
        next
        (collect-free (cdr vars) e
          (compile-refer (car vars) e
            (list 'argument next))))))

(define compile-refer
  (lambda (x e next)
    (compile-lookup x e
      (lambda (n) (list 'refer-local n next))
      (lambda (n) (list 'refer-free n next)))))

(define compile-lookup
  (lambda (x e return-local return-free)
    (let nxtlocal ([locals (car e)] [n 0])
      (if (null? locals)
          (let nxtfree ([free (cdr e)] [n 0])
            (if (eq? (car free) x)
                (return-free n)
                (nxtfree (cdr free) (+ n 1))))
          (if (eq? (car locals) x)
              (return-local n)
              (nxtlocal (cdr locals) (+ n 1)))))))

(define VM
  (lambda (a x f c s)
    (record-case x
      [halt () a]
      [refer-local (n x)
       (VM (index f n) x f c s)]
      [refer-free (n x)
       (VM (index-closure c n) x f c s)]
      [constant (obj x)
       (VM obj x f c s)]
      [close (n body x)
       (VM (closure body n s) x f c (- s n))]
      [test (then else)
       (VM a (if a then else) f c s)]
      [conti (x)
       (VM (continuation s) x f c s)]
      [nuate (stack x)
       (VM a x f c (restore-stack stack))]
      [frame (ret x)
       (VM a x f c (push ret (push f (push c s))))]
      [argument (x)
       (VM a x f c (push a s))]
      [apply ()
       (VM a (closure-body a) s a s)]
      [return (n)
       (let ([s (- s n)])
         (VM a (index s 0) (index s 1) (index s 2) (- s 3)))])))

(define closure
  (lambda (body n s)
    (let ([v (make-vector (+ n 1))])
      (vector-set! v 0 body)
      (let f ([i 0])
        (unless (= i n)
          (vector-set! v (+ i 1) (index s i))
          (f (+ i 1))))
      v)))

(define closure-body
  (lambda (c)
    (vector-ref c 0)))

(define index-closure
  (lambda (c n)
    (vector-ref c (+ n 1))))

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

(define evaluate
  (lambda (x)
    (VM '() (compile x '() '(halt)) 0 '() 0)))


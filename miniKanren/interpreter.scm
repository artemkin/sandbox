
(load "pmatch.scm")

(define lookup
  (lambda (x env)
    (cond
      [(null? env) (error 'lookup "unbound variable")]
      [(eq? x (caar env)) (cdar env)]
      [else (lookup x (cdr env))])))


(define eval-exp
  (lambda (expr env)
    (pmatch expr
      [,x (guard (symbol? x))
        (lookup x env)]
      [,n (guard (number? n))
        n]
      [(lambda (,x) ,body)
        `(closure ,x ,body ,env)]
      [(,e1 ,e2)
        (let [(proc (eval-exp e1 env))
              (val (eval-exp e2 env))]
          (pmatch proc
            [(closure ,x ,body ,env)
              (eval-exp body `((,x . ,val) . ,env))]
            [,else (error 'eval-exp "wrong procedure")]))])))


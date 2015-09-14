
(load "mk.scm")
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


(define lookupo
  (lambda (x env out)
    (fresh (y v env^)
      (== `((,y . ,v) . ,env^) env)
      (conde
        [(== x y) (== v out)]
        [(=/= x y) (lookupo x env^ out)]))))

(define eval-expo
  (lambda (expr env out)
    (conde
      [(symbolo expr) (lookupo expr env out)]
      [(numbero expr) (== expr out)]
      [(fresh (x body)
        (== `(lambda (,x) ,body) expr)
        (== `(closure ,x ,body ,env) out))]
      [(fresh (e1 e2 val x body env^)
        (== `(,e1 ,e2) expr)
        (eval-expo e1 env `(closure ,x ,body ,env^))
        (eval-expo e2 env val)
        (eval-expo body `((,x . ,val) . ,env^) out))])))


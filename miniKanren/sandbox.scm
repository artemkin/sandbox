
(load "mk.scm")

(define appendo
  (lambda (l s out)
    (conde
      ((== '() l) (== s out))
      ((fresh (a d res)
        (== (cons a d) l)
        (== (cons a res) out)
        (appendo d s res))))))

(define m  ; scheme member function
  (lambda (x l)
    (cond
      ((null? l) #f)
      ((equal? x (car l)) l)
      (else
        (m x (cdr l))))))

(define membero
  (lambda (x l out)
    (fresh (a d)
      (== (cons a d) l)
      (conde
        ((== x a) (== out l))
        ((=/= x a) (membero x d out))))))

(define memberoo ; no =/= constraint
  (lambda (x l out)
    (fresh (a d)
      (== (cons a d) l)
      (conde
        ((== x a) (== out l))
        ((memberoo x d out))))))

(define peano
  (lambda (n)
    (conde
      ((== 'z n))
      ((fresh (n-)
        (== (cons 's n-) n)
        (peano n-))))))


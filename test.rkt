#lang super 

;; TODO
;;   [ ] Use #lang super racket 

(require (for-syntax racket/base racket/syntax syntax/parse))

(define-syntax (#%ref stx)
  (syntax-parse stx 
    [(#%ref x:id index:expr)
     (syntax/loc stx
       (let ([i index])
         (cond
           [(vector? x) (vector-ref x i)]
           [(list?   x) (list-ref   x i)]
           [(string? x) (string-ref x i)]
           [(bytes?  x) (bytes-ref  x i)]
           [else
            (error '#%ref (~a "expected a vector, list, string or byte string, got: " x))])))]))

(define v (vector 0  11  22  33  44))
(define l (vector 0 111 222 333 444))
(define s "foo")
(define b #"bar")

v[1]
l[1]
s[1]
b[1]



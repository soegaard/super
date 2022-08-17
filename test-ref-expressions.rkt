#lang super racket

(define v (vector 0  11  22  33  44))
(define l (vector 0 111 222 333 444))
(define s "foo")
(define b #"bar")

v[1]
l[1]
s[1]
b[1]

(define Array (class object%
                (init-field elements)
                (super-new)
                #;(define/public (subscript-get i)
                  (vector-ref elements i))))

(define an-array (new Array [elements #(a b c d e)]))

an-array[2]

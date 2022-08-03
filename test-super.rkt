#lang super

(define point%
  (class object%
    (super-new)
    (init-field [x 0] [y 0])
    (define/public (move-x dx)
      (new this% [x (+ x dx)]))
    (define/public (move-y dy)
      (new this% [y (+ y dy)]))))

(define circle%
  (class object%
    (super-new)
    (init-field [center (new point%)] [radius 1])))

;; 1. o.f  field access
(define p (new point% [x 11] [y 22]))
p
p.x

;; 2. o.f1.f2  repeated field access

(define c (new circle% [center p] [radius 33]))
c.radius
c.center.x
c.center.y


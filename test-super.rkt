#lang super racket

(define point% 
  (class* object% (printable<%>)
    (super-new)
    (init-field [x 0] [y 0])

    (define/public (custom-print     port qq-depth) (do-print this print   port))
    (define/public (custom-display   port)          (do-print this display port))
    (define/public (custom-write     port)          (do-print this write   port))
    (define (do-print object out port)
      (display (~a "(object:point% x=" x " y=" y ")") port))
    
    (define/public (move-x dx)
      (new this% [x (+ x dx)] [y y]))
    (define/public (move-y dy)
      (new this% [y (+ y dy)] [x x]))
    (define/public (get-x)
      x)))

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


;; 3. (o .m a ...)              invoke method m on object o wth arguments a...

(define p2 (p .move-x 1))
p2.x

;; 4. (o .m1 a1 ... .m2 a2 ...) same as ((o .m1 a1 ...) .m2 a2 ...)
(p .move-x 1 .move-y 2 .get-x)

;; 5. (o.m a ...)               invoke method m on object o wth arguments a... 

(p.move-x 20) 
(p.move-x 20 .move-y 4)

; same as 
(p .move-x 20 .move-y 4)



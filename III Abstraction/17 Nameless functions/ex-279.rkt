#lang htdp/isl+

(lambda (x y) (x y y))
; legal - a function of two arguments that calls first one
; on the two values of the second

(lambda () 10)
; illegal - functions of zero arguments are not allowed

(lambda (x) x)
; legal - identity function

(lambda (x y) x)
; legal - like identity function but with an extra argument, which is ignored

(lambda x 10)
; illegal - wrong syntax

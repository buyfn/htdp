;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname balls) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define BALL-SIZE 10)
(define SCENE-HEIGHT 300)
(define SCENE-WIDTH 500)
(define BACKG (empty-scene SCENE-WIDTH SCENE-HEIGHT))
(define GRAVITY 1)
(define BOUNCE-HARDNESS 0.95)

; Vel is a structure
; (make-vel Number Number)
; interpretation (make-vel dx dy) means a velocity of
; dx pixels [per tick] along the horizontal and
; dy pixels [per tick] along the vertical direction
(define-struct vel [dx dy])

; Ball is a structure
; (make-ball Position Velocity String)
; interpretation a ball object with position, velocity and color
(define-struct ball [pos vel col])

; A WorldState is a list of Balls

; Ball -> Ball
(define (update-ball-pos b)
  (local [(define new-dy (+ (vel-dy (ball-vel b)) GRAVITY))
          (define nx (+ (posn-x (ball-pos b)) (vel-dx (ball-vel b))))
          (define ny (+ (posn-y (ball-pos b)) (vel-dy (ball-vel b))))]
    (if (and (> SCENE-HEIGHT (+ ny BALL-SIZE))
             (> (- ny BALL-SIZE) 0))
        (make-ball (make-posn nx ny)
                   (make-vel (vel-dx (ball-vel b)) new-dy)
                   (ball-col b))
        (make-ball (ball-pos b)
                   (make-vel (vel-dx (ball-vel b))
                             (* (* -1 BOUNCE-HARDNESS) (vel-dy (ball-vel b))))
                   (ball-col b)))))

; WorldState -> WorldState
(define (update-balls bs)
  (map update-ball-pos bs))

; WorldState -> WorldState
(define (add-ball b bs)
  (cons b bs))

; WorldState Number Number String -> WorldState
(define (handle-mouse bs x-mouse y-mouse me)
  (cond
    [(string=? me "button-down")
     (add-ball (make-ball (make-posn x-mouse y-mouse)
                          (make-vel 0 2)
                          "red")
               bs)]
    [else bs]))
  
; Ball -> Image
(define (show-ball b scene)
  (place-image (circle BALL-SIZE "solid" (ball-col b))
               (posn-x (ball-pos b))
               (posn-y (ball-pos b))
               scene))

; WorldState -> Image
(define (render s)
  (foldr show-ball BACKG s))

; Ball -> Ball
(define (ball-simulation initial-state)
  (big-bang initial-state
    [on-tick update-balls]
    [on-mouse handle-mouse]
    [to-draw render]))

(ball-simulation (list (make-ball (make-posn 30 BALL-SIZE)
                                  (make-vel 0 3)
                                  "red")))

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-215) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define SEGMENT-SIZE 5)

(define FIELD-SIZE 20)
(define CANVAS (empty-scene (* SEGMENT-SIZE FIELD-SIZE)
                            (* SEGMENT-SIZE FIELD-SIZE)))

(define-struct worm [pos dir])
; A Worm is a structure (make-worm Posn Direction)
; interpretation: (make-worm pos dir) represents a worm with
; position on canvas `pos` and
; direction `dir`

; A Direction is one of:
; - "up"
; - "right"
; - "down"
; - "left"

; A WorldState is a Worm

(define worm1 (make-worm (make-posn (- (quotient FIELD-SIZE 2) 1)
                                    (- (quotient FIELD-SIZE 2) 1)) "right"))

; Number -> WorldState
(define (main frame-rate)
  (big-bang worm1
    [to-draw render]
    [on-tick tock frame-rate]
    [on-key handle-key]
    [stop-when hit-wall? render/game-over]))

; WorldState -> Image
; renders a worm to the canvas
(define (render w)
  (place-image/align (circle SEGMENT-SIZE "solid" "red")
                     (* (posn-x (worm-pos w)) SEGMENT-SIZE)
                     (* (posn-y (worm-pos w)) SEGMENT-SIZE)
                     "left"
                     "top"
                     CANVAS))

; WorldState -> Image
; renders "game over" screen
(define (render/game-over w)
  (place-image/align (text "Worm hit border" 12 "black")
               2 (* SEGMENT-SIZE FIELD-SIZE) "left" "bottom"
               (render w)))

; WorldState KeyEvent -> WorldState
; handles worm direction control
(define (handle-key w ke)
  (cond
    [(key=? ke "up") (change-direction "up" w)]
    [(key=? ke "right") (change-direction "right" w)]
    [(key=? ke "down") (change-direction "down" w)]
    [(key=? ke "left") (change-direction "left" w)]
    [else w]))

; WorldState -> WorldState
; updates the state of the world after tick
(define (tock w)
  (cond
    [(string=? "up" (worm-dir w)) (move-up w)]
    [(string=? "right" (worm-dir w)) (move-right w)]
    [(string=? "down" (worm-dir w)) (move-down w)]
    [(string=? "left" (worm-dir w)) (move-left w)]))

; Worm -> Worm
; moves worm one segment above
(define (move-up w)
  (make-worm (make-posn (posn-x (worm-pos w))
                        (- (posn-y (worm-pos w)) 1))
             (worm-dir w)))

; Worm -> Worm
; moves worm one segment to the right
(define (move-right w)
  (make-worm (make-posn (+ (posn-x (worm-pos w)) 1)
                        (posn-y (worm-pos w)))
             (worm-dir w)))

; Worm -> Worm
; moves worm one segment below
(define (move-down w)
  (make-worm (make-posn (posn-x (worm-pos w))
                        (+ (posn-y (worm-pos w)) 1))
             (worm-dir w)))

; Worm -> Worm
; moves worm one segment to the left
(define (move-left w)
  (make-worm (make-posn (- (posn-x (worm-pos w)) 1)
                        (posn-y (worm-pos w)))
             (worm-dir w)))

; Direction Worm -> Worm
; changes the direction of the worm
(define (change-direction d w)
  (cond
    [(and (string=? (worm-dir w) "up")
          (string=? d "down"))
     w]
    [(and (string=? (worm-dir w) "right")
          (string=? d "left"))
     w]
    [(and (string=? (worm-dir w) "down")
          (string=? d "up"))
     w]
    [(and (string=? (worm-dir w) "left")
          (string=? d "right"))
     w]
    [else
     (make-worm (worm-pos w) d)]))

; WordState -> WordState
; rewind word state to a previos tick
(define (rewind w) w)

; WorldState -> Boolean
; returns true if worm hit a wall
(define (hit-wall? w)
  (not (and (<= 0 (posn-y (worm-pos w)) (- FIELD-SIZE 2))
            (<= 0 (posn-x (worm-pos w)) (- FIELD-SIZE 2)))))

(main 0.1)

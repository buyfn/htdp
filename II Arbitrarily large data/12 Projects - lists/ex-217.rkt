;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-217) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define SEGMENT-SIZE 5)

(define FIELD-SIZE 20)
(define CANVAS (empty-scene (* SEGMENT-SIZE FIELD-SIZE)
                            (* SEGMENT-SIZE FIELD-SIZE)))

(define-struct worm [body dir])
; A Worm is a structure (make-worm WormBody Direction)
; interpretation: (make-worm w dir) represents a worm with
; body segments `w` and direction `dir`

; A WormBody is a list of posn
; constraint: sequential segments are adjasent,
; meaning they differ by 1 in single direction

; A Direction is one of:
; - "up"
; - "right"
; - "down"
; - "left"

; A WorldState is a Worm

(define worm1 (make-worm (list (make-posn (- (quotient FIELD-SIZE 2) 1)
                                          (- (quotient FIELD-SIZE 2) 1))
                               (make-posn (- (quotient FIELD-SIZE 2) 2)
                                          (- (quotient FIELD-SIZE 2) 1)))
                         "right"))

; Number -> WorldState
(define (main frame-rate)
  (big-bang worm1
    [to-draw render]
    [on-tick tock frame-rate]
    [on-key handle-key]
    [stop-when hit-wall? render/game-over]))

; WorldState -> Image
; renders a worm to the canvas
(define (render w) (render/worm w CANVAS))

; Worm Image -> Image
; renders worm to image
(define (render/worm w i)
  (cond
    [(empty? (worm-body w)) i]
    [else (render/segment (first (worm-body w))
                          (render/worm (make-worm (rest (worm-body w))
                                                  (worm-dir w))
                                       i))]))

; Posn Image -> Image
; renders a worm segment to image
(define (render/segment p i)
  (place-image/align (circle SEGMENT-SIZE "solid" "red")
                     (* (posn-x p) SEGMENT-SIZE)
                     (* (posn-y p) SEGMENT-SIZE)
                     "left"
                     "top"
                     i))

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
  (make-worm (remove-last (cons (one-up w) (worm-body w)))
             (worm-dir w)))

; Worm -> Posn
; creates a new segment to the top of worm
(define (one-up w)
  (make-posn (posn-x (first (worm-body w)))
             (- (posn-y (first (worm-body w))) 1)))

; Worm -> Worm
; moves worm one segment to the right
(define (move-right w)
  (make-worm (remove-last (cons (one-right w) (worm-body w)))
             (worm-dir w)))

; Worm -> Posn
; creates a new segment to the right of wrom
(define (one-right w)
  (make-posn (+ (posn-x (first (worm-body w ))) 1)
             (posn-y (first (worm-body w)))))

; Worm -> Worm
; moves worm one segment below
(define (move-down w)
  (make-worm (remove-last (cons (one-down w) (worm-body w)))
             (worm-dir w)))

; Worm -> Posn
; creates a new segment below of wrom
(define (one-down w)
  (make-posn (posn-x (first (worm-body w )))
             (+ (posn-y (first (worm-body w))) 1)))

; Worm -> Worm
; moves worm one segment to the left
(define (move-left w)
  (make-worm (remove-last (cons (one-left w) (worm-body w)))
             (worm-dir w)))

; Worm -> Posn
; creates a new segment to the left of wrom
(define (one-left w)
  (make-posn (- (posn-x (first (worm-body w ))) 1)
             (posn-y (first (worm-body w)))))

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
     (make-worm (worm-body w) d)]))

; WordState -> WordState
; rewind word state to a previos tick
(define (rewind w) w)

; WorldState -> Boolean
; returns true if worm hit a wall
(define (hit-wall? w)
  (not (and (<= 0 (posn-y (first (worm-body w))) (- FIELD-SIZE 2))
            (<= 0 (posn-x (first (worm-body w))) (- FIELD-SIZE 2)))))

; List -> List
; removes last element of a list
(check-expect (remove-last '()) '())
(check-expect (remove-last (list 1)) '())
(check-expect (remove-last (list 1 2)) (list 1))
(define (remove-last l)
  (cond
    [(empty? l) '()]
    [(empty? (rest l)) '()]
    [else (cons (first l) (remove-last (rest l)))]))

(main 0.1)

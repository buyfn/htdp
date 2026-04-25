;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-97) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define WIDTH-OF-WORLD 300)
(define HEIGHT-OF-WORLD 400)
(define GROUND-HEIGHT 100)
(define BACKGROUND (place-image/align
                    (rectangle WIDTH-OF-WORLD
                               GROUND-HEIGHT
                               "solid"
                               "navajo white")
                    0
                    HEIGHT-OF-WORLD
                    "left"
                    "bottom"
                    (empty-scene WIDTH-OF-WORLD
                                 HEIGHT-OF-WORLD
                                 "light sky blue")))

(define MISSILE-WIDTH 4)
(define MISSILE-HEIGHT 15)
(define MISSILE (rectangle MISSILE-WIDTH
                           MISSILE-HEIGHT
                           "solid"
                           "red"))

(define TANK-WIDTH 40)
(define TANK-HEIGHT 15)
(define TANK (overlay/offset (rectangle (/ TANK-WIDTH 2)
                                        (/ TANK-HEIGHT 2)
                                        "solid"
                                        "dark green")
                             0
                             (/ TANK-HEIGHT 1.5)
                             (rectangle TANK-WIDTH
                                        TANK-HEIGHT
                                        "solid"
                                        "dark green")))

(define UFO-WIDTH 60)
(define UFO-HEIGHT 20)
(define UFO (overlay/offset (circle (/ UFO-HEIGHT 2) "solid" "aqua")
                            0
                            (/ UFO-HEIGHT 4)
                            (ellipse UFO-WIDTH
                                     UFO-HEIGHT
                                     "solid"
                                     "steel blue")))

(define PROXIMITY-TRESHOLD 5)
(define JUMP-SIZE 5)
(define DESCEND-SPEED 2)
(define TANK-SPEED 1)
(define MISSILE-SPEED 5)

; A UFO is a Posn
; interpretation (make-posn x y) is the UFO's location
; (using the top-down, left-to-right convention)

(define-struct tank [loc vel])
; A Tank is a structure:
; (make-tank Number Number)
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick

; A Missle is a Posn
; interpretation (make-posn x y) is the missile's place

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])
; A SIGS is one of:
; - (make-aim UFO Tank)
; - (make-fired UFO Tank Missile)
; interpretation represents the complete state of a space invader game

; SIGS -> Image
; renders the given game state on top of BACKGROUND
(define (si-render s)
  (cond
    [(aim? s)
     (tank-render (aim-tank s)
                  (ufo-render (aim-ufo s) BACKGROUND))]
    [(fired? s)
     (tank-render
      (fired-tank s)
      (ufo-render (fired-ufo s)
                  (missile-render (fired-missile s)
                                  BACKGROUND)))]))
; SIGS -> Image
; renders the final screen
(define (si-render-final s)
  (cond
    [(aim? s)
     (place-image/align (above (text "Game over." 24 "black")
                               (text "You lost." 24 "black"))
                        (/ WIDTH-OF-WORLD 2)
                        (/ HEIGHT-OF-WORLD 2)
                        "middle"
                        "center"
                        (aim-render s))]
    [(fired? s)
     (cond
       [(close-enough? (fired-ufo s) (fired-missile s))
        (place-image/align (above (text "Game over." 24 "black")
                                  (text "You win." 24 "black"))
                           (/ WIDTH-OF-WORLD 2)
                           (/ HEIGHT-OF-WORLD 2)
                           "middle"
                           "center"
                           (fired-render s))]
       [else
        (place-image/align (above (text "Game over" 24 "black")
                                  (text "You lost" 24 "black"))
                           (/ WIDTH-OF-WORLD 2)
                           (/ HEIGHT-OF-WORLD 2)
                           "middle"
                           "center"
                           (fired-render s))])]))

; SIGS -> Image
; renders aim scene
(define (aim-render s)
  (tank-render (aim-tank s)
               (ufo-render (aim-ufo s) BACKGROUND)))

; SIGS -> Image
; renders fired scene
(define (fired-render s)
  (tank-render (fired-tank s)
               (ufo-render (fired-ufo s)
                           (missile-render (fired-missile s)
                                           BACKGROUND))))

; Tank Image -> Image
; adds t to the given image im
(define (tank-render t im)
  (place-image TANK (tank-loc t) (- HEIGHT-OF-WORLD TANK-HEIGHT) im))

; UFO Image -> Image
; adds u to the given image im
(define (ufo-render u im)
  (place-image UFO (posn-x u) (posn-y u) im))

; Missile Image -> Image
; adds m to the given image im
(define (missile-render m im)
  (place-image MISSILE (posn-x m) (posn-y m) im))

; Number -> Number
(define (random-vel s)
  (cond
    [(= (random 2) 0) s]
    [else (* -1 s)]))

; SIGS -> SIGS
(define (si-move w)
  (si-move-proper w (random-vel JUMP-SIZE)))

; SIGS Number -> SIGS
; moves the space-invader objects predictably by delta
(define (si-move-proper w delta)
  (cond
    [(aim? w)
     (make-aim (move-ufo (aim-ufo w) delta)
               (move-tank (aim-tank w)))]
    [(fired? w)
     (make-fired (move-ufo (fired-ufo w) delta)
                 (move-tank (fired-tank w))
                 (move-missile (fired-missile w)))]))

; Posn Number -> Posn
; Moves UFO by delta
(define (move-ufo u d)
  (make-posn (+ (posn-x u) d)
             (+ (posn-y u) DESCEND-SPEED)))

; Tank -> Tank
; Moves tank
(define (move-tank t)
  (make-tank (+ (tank-loc t) (tank-vel t))
             (tank-vel t)))

; Posn -> Posn
; Moves missile
(define (move-missile m)
  (make-posn (posn-x m) (- (posn-y m) MISSILE-SPEED)))

; SIGS String -> SIGS
; handle key presses to control the tank
; - "left" key ensures that the tank moves left
; - "right" key ensures that the tank moves right
; - "space" fires the missle if hasn't been launched yet
(define (si-control ws ke)
  (cond
    [(aim? ws)
     (cond
       [(string=? " " ke)
        (make-fired (aim-ufo ws)
                    (aim-tank ws)
                    (make-posn (tank-loc (aim-tank ws))
                               HEIGHT-OF-WORLD))]
       [(string=? "left" ke)
        (make-aim (aim-ufo ws)
                  (make-tank (tank-loc (aim-tank ws)) (* -1 TANK-SPEED)))]
       [(string=? "right" ke)
        (make-aim (aim-ufo ws)
                  (make-tank (tank-loc (aim-tank ws)) TANK-SPEED))]
       [else ws])]
    [(fired? ws) ws]))

; SIGS -> Boolean
(define (si-game-over? ws)
  (cond
    [(fired? ws)
     (cond
       [(close-enough? (fired-ufo ws) (fired-missile ws)) #true]
       [(close-enough? (fired-ufo ws) (- HEIGHT-OF-WORLD GROUND-HEIGHT)) #true]
       [else false])]
    [(aim? ws)
     (cond
       [(close-enough? (aim-ufo ws) (- HEIGHT-OF-WORLD GROUND-HEIGHT)) #true]
       [else false])]))

; Posn, Posn or Number -> Boolean
(define (close-enough? ufo target)
  (cond
    [(posn? target)
     (and (> (/ UFO-WIDTH 2) (abs (- (posn-x ufo) (posn-x target))))
          (> (/ UFO-HEIGHT 2) (abs (- (posn-y ufo) (posn-y target)))))]
    [else (> PROXIMITY-TRESHOLD (abs (- (posn-y ufo) target)))]))

; SIGS -> SIGS
(define (main ws)
  (big-bang ws
    [on-tick si-move]
    [on-key si-control]
    [stop-when si-game-over? si-render-final]
    [to-draw si-render]))

(define initial-state (make-aim (make-posn (/ WIDTH-OF-WORLD 2) 0)
                                (make-tank (/ WIDTH-OF-WORLD 2) TANK-SPEED)))
(main initial-state)

; Wishlist
; - [ ] si-render-final
; - [x] si-control
; - [x] move-ufo
; - [x] move-tank
; - [x] move-missile
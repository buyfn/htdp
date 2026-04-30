;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-101) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
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

(define-struct sigs [ufo tank missile])
; A SIGS.v2 (short for SIGS version 2) is a structure:
; (make-sigs UFO Tank MissleOrNot)
; interpretation represents the complete state of a
; space invader game

; A MissleOrNot is one of:
; - #false
; - Posn
; interpretation #false means the missile is in the tank;
; Posn says the missile is at that location

; MissileOrNot Image -> Image
; adds an image of missile m to scene s
(check-expect (missile-render.v2 #false BACKGROUND) BACKGROUND)
(check-expect (missile-render.v2 (make-posn 30 40) BACKGROUND)
              (place-image MISSILE 30 40 BACKGROUND))
(define (missile-render.v2 m s)
  (cond
    [(boolean? m) s]
    [else (place-image MISSILE (posn-x m) (posn-y m) s)]))

; SIGS.v2 -> Image
; renders game state on top of BAKGROUND
(define (si-render.v2 s)
  (tank-render (sigs-tank s)
               (ufo-render (sigs-ufo s)
                           (missile-render.v2 (sigs-missile s)
                                              BACKGROUND))))

; SIGS.v2 -> Image
; renders the final screen
(define (si-render-final.v2 s)
  (place-image/align (above (text "Game over" 24 "black")
                            (cond
                              [(close-enough?.v2 (sigs-ufo s) (sigs-missile s))
                               (text "You win" 24 "black")]
                              [else
                               (text "You lost" 24 "black")]))
                        (/ WIDTH-OF-WORLD 2)
                        (/ HEIGHT-OF-WORLD 2)
                        "middle"
                        "center"
                        (si-render.v2 s)))

; Tank Image -> Image
; adds t to the given image im
(define (tank-render t im)
  (place-image TANK (tank-loc t) (- HEIGHT-OF-WORLD TANK-HEIGHT) im))

; UFO Image -> Image
; adds u to the given image im
(define (ufo-render u im)
  (place-image UFO (posn-x u) (posn-y u) im))

; Number -> Number
(define (random-vel s)
  (cond
    [(= (random 2) 0) s]
    [else (* -1 s)]))

; SIGS.v2 -> SIGS.v2
(define (si-move.v2 w)
  (si-move-proper.v2 w (random-vel JUMP-SIZE)))

; SIGS.v2 Number -> SIGS.v2
; moves the space invader objects predictably by delta
(define (si-move-proper.v2 w delta)
  (make-sigs (move-ufo (sigs-ufo w) delta)
             (move-tank (sigs-tank w))
             (move-missile.v2 (sigs-missile w))))

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

; MissileOrNot -> MissileOrNot
; Moves missile
(define (move-missile.v2 m)
  (cond
    [(boolean? m) m]
    [else (make-posn (posn-x m) (- (posn-y m) MISSILE-SPEED))]))

; SIGS.v2 String -> SIGS
; handle key presses to control the tank
; - "left" key ensures that the tank moves left
; - "right" key ensures that the tank moves right
; - "space" fires the missle if hasn't been launched yet
(define (si-control.v2 ws ke)
  (cond
    [(string=? " " ke)
     (cond
       [(false? (sigs-missile ws))
        (make-sigs (sigs-ufo ws)
                   (sigs-tank ws)
                   (make-posn (tank-loc (sigs-tank ws))
                              HEIGHT-OF-WORLD))]
       [else ws])]
    [(string=? "left" ke)
     (make-sigs (sigs-ufo ws)
                (make-tank (tank-loc (sigs-tank ws))
                           (* -1 TANK-SPEED))
                (sigs-missile ws))]
    [(string=? "right" ke)
     (make-sigs (sigs-ufo ws)
                (make-tank (tank-loc (sigs-tank ws))
                           TANK-SPEED)
                (sigs-missile ws))]
    [else ws]))

; SIGS.v2 -> Boolean
(check-expect (si-game-over?.v2 (make-sigs (make-posn 185 102)
                                           (make-tank 173 1)
                                           (make-posn 170 385)))
                                #false)
(define (si-game-over?.v2 ws)
  (cond
    [(close-enough?.v2 (sigs-ufo ws) (sigs-missile ws)) #true]
    [(close-enough?.v2 (sigs-ufo ws) GROUND-HEIGHT) #true]
    [else #false]))

; MissileOrNot or Number -> Boolean
(check-expect (close-enough?.v2 (make-posn 185 102)
                                (make-posn 170 385))
              #false)
(check-expect (close-enough?.v2 (make-posn 185 102)
                                GROUND-HEIGHT)
              #false)
(define (close-enough?.v2 ufo target)
  (cond
    [(number? target)
     (< (- HEIGHT-OF-WORLD (posn-y ufo)) target)]
    [(boolean? target) #false]
    [(posn? target)
     (and (> (/ UFO-WIDTH 2) (abs (- (posn-x ufo) (posn-x target))))
          (> (/ UFO-HEIGHT 2) (abs (- (posn-y ufo) (posn-y target)))))]))

; SIGS.v2 -> SIGS.v2
(define (main.v2 ws)
  (big-bang ws
    [on-tick si-move.v2]
    [on-key si-control.v2]
    [stop-when si-game-over?.v2 si-render-final.v2]
    [to-draw si-render.v2]))

(define initial-state (make-sigs (make-posn (/ WIDTH-OF-WORLD 2) 0)
                                 (make-tank (/ WIDTH-OF-WORLD 2) TANK-SPEED)
                                 #false))

(main.v2 initial-state)
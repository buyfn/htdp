;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-224) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))
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

(define-struct sigs [ufo tank missiles charges#])
; A SIGS is a structure:
; (make-sigs UFO Tanks List-of-missiles Number)
; interpretation represents the complete state of a
; space invader game

; Missile Image -> Image
; adds an image of missile m to scene s
(check-expect (missile-render (make-posn 30 40) BACKGROUND)
              (place-image MISSILE 30 40 BACKGROUND))
(define (missile-render m s)
  (place-image MISSILE (posn-x m) (posn-y m) s))

; SIGS -> Image
; renders game state on top of BACKGROUND
(define (si-render s)
  (tank-render (sigs-tank s)
               (ufo-render (sigs-ufo s)
                           (missiles-render (sigs-missiles s) BACKGROUND))))

; SIGS -> Image
; renders the final screen
(define (si-render-final s)
  (place-image/align (above (text "Game over" 24 "black")
                            (cond
                              [(close-enough? (sigs-ufo s) GROUND-HEIGHT)
                               (text "You lost" 24 "black")]
                              [else
                               (text "You win" 24 "black")]))
                        (/ WIDTH-OF-WORLD 2)
                        (/ HEIGHT-OF-WORLD 2)
                        "middle"
                        "center"
                        (si-render s)))

; Tank Image -> Image
; adds t to the given image im
(define (tank-render t im)
  (place-image TANK (tank-loc t) (- HEIGHT-OF-WORLD TANK-HEIGHT) im))

; UFO Image -> Image
; adds u to the given image im
(define (ufo-render u im)
  (place-image UFO (posn-x u) (posn-y u) im))

; List-of-missiles -> Image
; adds missiles to the given image im
(define (missiles-render missiles im)
  (cond
    [(empty? missiles) im]
    [else (missile-render (first missiles)
                          (missiles-render (rest missiles) im))]))

; Number -> Number
(define (random-vel s)
  (cond
    [(= (random 2) 0) s]
    [else (* -1 s)]))

; SIGS -> SIGS
(define (si-move w)
  (si-move-proper w (random-vel JUMP-SIZE)))

; SIGS Number -> SIGS
; moves the space invader objects predictably by delta
(define (si-move-proper w delta)
  (make-sigs (move-ufo (sigs-ufo w) delta)
             (move-tank (sigs-tank w))
             (move-missiles (sigs-missiles w))
             (sigs-charges# w)))

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

; List-of-missiles -> List-of-missiles
; Moves missiles
(define (move-missiles ms)
  (cond
    [(empty? ms) ms]
    [else (cons (move-missile (first ms))
                (move-missiles (rest ms)))]))

; Missile -> Missile
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
    [(string=? " " ke)
     (if (> (sigs-charges# ws) 0)
         (make-sigs (sigs-ufo ws)
                    (sigs-tank ws)
                    (cons (make-posn (tank-loc (sigs-tank ws))
                                     HEIGHT-OF-WORLD)
                          (sigs-missiles ws))
                    (- (sigs-charges# ws) 1))
         ws)]
    [(string=? "left" ke)
     (make-sigs (sigs-ufo ws)
                (make-tank (tank-loc (sigs-tank ws))
                           (* -1 TANK-SPEED))
                (sigs-missiles ws)
                (sigs-charges# ws))]
    [(string=? "right" ke)
     (make-sigs (sigs-ufo ws)
                (make-tank (tank-loc (sigs-tank ws))
                           TANK-SPEED)
                (sigs-missiles ws)
                (sigs-charges# ws))]
    [else ws]))

; SIGS -> Boolean
(check-expect (si-game-over? (make-sigs (make-posn 185 102)
                                        (make-tank 173 1)
                                        (list (make-posn 170 385))
                                        3))
              #false)
(define (si-game-over? ws)
  (cond
    [(missile-hit? (sigs-ufo ws) (sigs-missiles ws)) #true]
    [(close-enough? (sigs-ufo ws) GROUND-HEIGHT) #true]
    [else #false]))

; Determines whether any missile has hit the ufo
; UFO List-of-missiles -> Boolean
(define (missile-hit? ufo missiles)
  (cond
    [(empty? missiles) #false]
    [else (or (close-enough? (first missiles) ufo)
              (missile-hit? ufo (rest missiles)))]))

; MissileOrNot or Number -> Boolean
(check-expect (close-enough? (make-posn 185 102)
                                (make-posn 170 385))
              #false)
(check-expect (close-enough? (make-posn 185 102)
                                GROUND-HEIGHT)
              #false)
(define (close-enough? ufo target)
  (cond
    [(number? target)
     (< (- HEIGHT-OF-WORLD (posn-y ufo)) target)]
    [(boolean? target) #false]
    [(posn? target)
     (and (> (/ UFO-WIDTH 2) (abs (- (posn-x ufo) (posn-x target))))
          (> (/ UFO-HEIGHT 2) (abs (- (posn-y ufo) (posn-y target)))))]))

; SIGS -> SIGS
(define (main ws)
  (big-bang ws
    [on-tick si-move]
    [on-key si-control]
    [stop-when si-game-over? si-render-final]
    [to-draw si-render]
    [close-on-stop #false]))

(define initial-state (make-sigs (make-posn (/ WIDTH-OF-WORLD 2) 0)
                                 (make-tank (/ WIDTH-OF-WORLD 2) TANK-SPEED)
                                 '()
                                 3))

(main initial-state)
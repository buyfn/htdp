#lang htdp/isl+
(require 2htdp/image)
(require 2htdp/universe)

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

(define CHARGES# 15)
(define UFOS# 10)
(define JUMP-SIZE 5)
(define DESCEND-SPEED 2)
(define TANK-SPEED 1)
(define MISSILE-SPEED 5)
(define UFO-GENERATE-RATE 50)

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

(define-struct sigs [ufos tank missiles ufos# charges# counter])
; A SIGS is a structure:
; (make-sigs List-of-UFO Tank List-of-missiles Number Number Number)
; interpretation represents the complete state of a
; space invader game


;;; RENDER ;;;
; SIGS -> Image
; renders game state on top of BACKGROUND
(define (si-render s)
  (local (; [SIGS Image -> Image] Image -> Image
          ; calls current render function with s
          (define (render-layer render-func image)
            (render-func s image)))
    (foldr render-layer BACKGROUND
           (list render-charges#
                 tank-render
                 ufos-render
                 missiles-render))))

; SIGS -> Image
; renders the final screen
(define (si-render-final s)
  (place-image/align (above (text "Game over" 24 "black")
                            (cond
                              [(ufo-landed? (sigs-ufos s))
                               (text "You lost" 24 "black")]
                              [else
                               (text "You win" 24 "black")]))
                        (/ WIDTH-OF-WORLD 2)
                        (/ HEIGHT-OF-WORLD 2)
                        "middle"
                        "center"
                        (si-render s)))

; SIGS Image -> Image
; adds t to the given image im
(define (tank-render s im)
  (place-image TANK (tank-loc (sigs-tank s)) (- HEIGHT-OF-WORLD TANK-HEIGHT) im))

; SIGS Image -> Image
; adds UFOs to the given image im
(define (ufos-render s im)
  (foldr ufo-render im (sigs-ufos s)))

; UFO Image -> Image
; adds u to the given image im
(define (ufo-render u im)
  (place-image UFO (posn-x u) (posn-y u) im))

; SIGS Image -> Image
; adds missiles to the given image im
(define (missiles-render s im)
  (foldr missile-render im (sigs-missiles s)))

; Missile Image -> Image
; adds an image of missile m to scene s
(check-expect (missile-render (make-posn 30 40) BACKGROUND)
              (place-image MISSILE 30 40 BACKGROUND))
(define (missile-render m s)
  (place-image MISSILE (posn-x m) (posn-y m) s))

; SIGS Image -> Image
; adds charges counter to the image
(define (render-charges# s i)
  (local ((define n (sigs-charges# s)))
    (place-image/align (beside (text "Charges: " 12 "black")
                               (text (number->string n) 12 "black"))
                       0 HEIGHT-OF-WORLD "left" "bottom" i)))

;;; END OF RENDER ;;;


; Number -> Number
(define (random-vel s)
  (cond
    [(= (random 2) 0) s]
    [else (* -1 s)]))

; SIGS -> SIGS
(define (si-move w)
  (add-ufo
   (remove-collisions
    (make-sigs (move-ufos (sigs-ufos w))
               (move-tank (sigs-tank w))
               (move-missiles (sigs-missiles w))
               (sigs-ufos# w)
               (sigs-charges# w)
               (+ (sigs-counter w) 1)))))

; SIGS -> SIGS
; Adds a UFO
(define (add-ufo ws)
  (cond
    [(= 0 (sigs-ufos# ws)) ws]
    [(not (= 0 (modulo (sigs-counter ws) UFO-GENERATE-RATE))) ws]
    [else (make-sigs (cons (make-posn (/ WIDTH-OF-WORLD 2) 0)
                           (sigs-ufos ws))
                     (sigs-tank ws)
                     (sigs-missiles ws)
                     (- (sigs-ufos# ws) 1)
                     (sigs-charges# ws)
                     (sigs-counter ws))]))

; SIGS -> SIGS
; Removes UFOs and missiles that has collided
(define (remove-collisions ws)
  (make-sigs (remove-hit-ufos (sigs-ufos ws) (sigs-missiles ws))
             (sigs-tank ws)
             (remove-collided-missiles (sigs-missiles ws) (sigs-ufos ws))
             (sigs-ufos# ws)
             (sigs-charges# ws)
             (sigs-counter ws)))

; [X Y -> Boolean] [List-of X] [List-of Y] -> [List-of X]
; removes targets that were hit by a projectile
(define (remove-hit-targets proximity-test targets projectiles)
  (local (; X -> Boolean
          ; determines whether the target is not hit by a projectile
          (define (target-not-hit? target)
            (local (; Y -> Boolean
                    ; determines whether projectile is not close enough to the target
                    (define (projectile-not-close-enough? projectile)
                      (not (proximity-test target projectile))))
              (andmap projectile-not-close-enough? projectiles))))
    (filter target-not-hit? targets)))

; [List-of UFO] [List-of Missile] -> [List-of UFO]
; Removes UFOs from the list that are hit by a missile from the list
(define (remove-hit-ufos ufos missiles)
  (remove-hit-targets close-enough? ufos missiles))

; [List-of Missile] [List-of UFO] -> [List-of Missile]
; Removes missiles from the list that hit a UFO from the list
(define (remove-collided-missiles missiles ufos)
  (remove-hit-targets close-enough? missiles ufos))

; Posn, Number, Number -> Posn
; updates a position by adding dx and dy
(define (next-pos p dx dy)
  (make-posn (+ (posn-x p) dx)
             (+ (posn-y p) dy)))

; [List-of Posn] [Posn -> Number] Number -> [List-of Posn]
; updates posn by adding (get-dx p) and dy to p
(define (move-poss ps get-dx dy)
  (local (; Posn -> Posn
          (define (move-pos p)
            (next-pos p (get-dx p) dy)))
    (map move-pos ps)))

; [List-of UFO] -> [List-of UFO]
; Moves all UFOs by random delta
(define (move-ufos ufos)
  (local (; UFO -> Number
          ; creates a random dx for UFO to move by
          (define (get-dx p) (random-vel JUMP-SIZE)))
    (move-poss ufos get-dx DESCEND-SPEED)))

; [List-of Missile] -> [List-of Missile]
; Moves missiles
(define (move-missiles ms)
  (local (; Missile -> Number
          ; returns 0 as dx to move missile by
          (define (get-dx m) 0))
    (move-poss ms get-dx (* MISSILE-SPEED -1))))

; Tank -> Tank
; Moves tank
(define (move-tank t)
  (make-tank (+ (tank-loc t) (tank-vel t))
             (tank-vel t)))

; SIGS String -> SIGS
; handle key presses to control the tank
; - "left" key ensures that the tank moves left
; - "right" key ensures that the tank moves right
; - "space" fires the missle if hasn't been launched yet
(define (si-control ws ke)
  (cond
    [(string=? " " ke)
     (if (> (sigs-charges# ws) 0)
         (make-sigs (sigs-ufos ws)
                    (sigs-tank ws)
                    (cons (make-posn (tank-loc (sigs-tank ws))
                                     HEIGHT-OF-WORLD)
                          (sigs-missiles ws))
                    (sigs-ufos# ws)
                    (- (sigs-charges# ws) 1)
                    (sigs-counter ws))
         ws)]
    [(string=? "left" ke)
     (make-sigs (sigs-ufos ws)
                (make-tank (tank-loc (sigs-tank ws))
                           (* -1 TANK-SPEED))
                (sigs-missiles ws)
                (sigs-ufos# ws)
                (sigs-charges# ws)
                (sigs-counter ws))]
    [(string=? "right" ke)
     (make-sigs (sigs-ufos ws)
                (make-tank (tank-loc (sigs-tank ws))
                           TANK-SPEED)
                (sigs-missiles ws)
                (sigs-ufos# ws)
                (sigs-charges# ws)
                (sigs-counter ws))]
    [else ws]))

; SIGS -> Boolean
(check-expect (si-game-over? (make-sigs (list (make-posn 185 102))
                                        (make-tank 173 1)
                                        (list (make-posn 170 385))
                                        2
                                        3
                                        0))
              #false)
(define (si-game-over? ws)
  (cond
    [(all-ufos-hit? ws) #true]
    [(ufo-landed? (sigs-ufos ws)) #true]
    [else #false]))

; SIGS -> Boolean
; Determines whether all of the UFOs has been destroyed
(define (all-ufos-hit? ws)
  (and (= 0 (length (sigs-ufos ws)))
       (= 0 (sigs-ufos# ws))))

; [List-of UFO] -> Boolean
; Determines whether one of the UFOs has landed
(define (ufo-landed? ufos)
  (local (; UFO -> Boolean
          ; determines if the UFO has landed
          (define (ufo-close-to-ground? ufo)
            (close-enough? ufo GROUND-HEIGHT)))
    (ormap ufo-close-to-ground? ufos)))

; Posn Posn-or-Number -> Boolean
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
    [close-on-stop #false]
    [state #false]))

(define initial-state (make-sigs '()
                                 (make-tank (/ WIDTH-OF-WORLD 2) TANK-SPEED)
                                 '()
                                 UFOS#
                                 CHARGES#
                                 0))

(main initial-state)

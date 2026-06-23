;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-225) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))
(define SEGMENT-WIDTH 10)
(define WIDTH-SEGMENTS 60)
(define WIDTH-OF-WORLD (* WIDTH-SEGMENTS SEGMENT-WIDTH))
(define HEIGHT-OF-WORLD 200)
(define ALTITUDE 40)

(define DX 1)
(define FLAME-PROB 10)

(define GROUND-HEIGHT 20)
(define BACKGROUND (place-image/align
                    (rectangle WIDTH-OF-WORLD
                               GROUND-HEIGHT
                               "solid"
                               "dark green")
                    0
                    HEIGHT-OF-WORLD
                    "left"
                    "bottom"
                    (empty-scene WIDTH-OF-WORLD
                                 HEIGHT-OF-WORLD
                                 "light sky blue")))

(define HELI (ellipse 30 20 "solid" "red"))
(define FIRE (triangle SEGMENT-WIDTH "solid" "orange"))

(define-struct ffgs [heli loads fires])
; An FFGS (fire-fighting game state) is a structure:
; (make-ffgs HELI List-of-loads List-of-fires)
; interpretation represents the complete state of a
; fire-fighting game

(define-struct heli [loc vel])
; A Heli is a structure:
; (make-heli Number Number)
; interpretations (make-heli x dx) specifies a Helicopter
; at horizontal position x and speed dx

(define initial-state (make-ffgs (make-heli (/ WIDTH-OF-WORLD 2) 0)
                                 '()
                                 '(0)))

; FFGS -> FFGS
(define (main ws)
  (big-bang ws
    [on-tick ff-move]
    [on-key ff-control]
    [stop-when ff-game-over?]
    [to-draw ff-render]
    [close-on-stop #false]))


;;; RENDER ;;;

; FFGS -> Image
; renders game state
(define (ff-render ws)
  (place-image HELI (heli-loc (ffgs-heli ws)) ALTITUDE
               (fires-render (ffgs-fires ws) BACKGROUND)))

; List-of-fires Image -> Image
; renders fires to image
(define (fires-render fires image)
  (cond
    [(empty? fires) image]
    [else (fire-render (first fires)
                       (fires-render (rest fires) image))]))

; Number Image -> Image
; renders fire at given x coordinate to image
(define (fire-render segment-index image)
  (place-image/align FIRE
                     (* segment-index SEGMENT-WIDTH)
                     HEIGHT-OF-WORLD
                     "left"
                     "bottom"
                     image))

;;; END OF RENDER ;;;

; FFGS -> FFGS
; - moves helicopter
; - moves water loads
; - creates and extinguishes fires
(define (ff-move ws)
  (make-ffgs (make-heli (+ (heli-loc (ffgs-heli ws))
                           (heli-vel (ffgs-heli ws)))
                        (heli-vel (ffgs-heli ws)))
             (ffgs-loads ws)
             (spread (ffgs-fires (fires-extinguish ws)))))

; FFGS -> FFGS
; Removes fires hit by water loads
(define (fires-extinguish ws) ws)

; List-of-fires -> List-of-fires
; Creates new fires adjacent to existing with random probability
(define (spread fires)
  (spread-aux 0 fires '()))

; Number List-of-fires List-of-fires -> List-of-fires
(define (spread-aux segment-index fires next-fires)
  (cond
    [(= segment-index WIDTH-SEGMENTS) next-fires]
    [(create-fire? segment-index fires)
     (spread-aux (+ segment-index 1)
                 fires
                 (cons segment-index next-fires))]
    [else (spread-aux (+ segment-index 1) fires next-fires)]))

; Number List-of-fires -> Boolean
; Determines whether segment at given index should be on fire next tick
(define (create-fire? segment-index fires)
  (cond
    [(= segment-index 0) (member? 0 fires)]
    [(member? segment-index fires) #true]
    [(member? (- segment-index 1) fires)
     (<= (random 100) FLAME-PROB)]
    [else #false]))

; FFGS String -> FFGS
; handles key presses to control the helicoper
; - "left" decreases horizontal velocity
; - "right" increases horizontal velocity
; - "space" drops a water load
(define (ff-control ws ke)
  (cond
    [(string=? "left" ke) (make-ffgs (make-heli (heli-loc (ffgs-heli ws))
                                                (- (heli-vel (ffgs-heli ws)) DX))
                                     (ffgs-loads ws)
                                     (ffgs-fires ws))]
    [(string=? "right" ke) (make-ffgs (make-heli (heli-loc (ffgs-heli ws))
                                                (+ (heli-vel (ffgs-heli ws)) DX))
                                     (ffgs-loads ws)
                                     (ffgs-fires ws))]
    [else ws]))

; FFGS -> Boolean
; Determines whether the game has ended
(define (ff-game-over? ws) #false)

(main initial-state)

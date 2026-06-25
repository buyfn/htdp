;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-225) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))
(define SEGMENT-WIDTH 10)
(define WIDTH-SEGMENTS 60)
(define WIDTH-OF-WORLD (* WIDTH-SEGMENTS SEGMENT-WIDTH))
(define HEIGHT-OF-WORLD 200)
(define ALTITUDE 40)

(define DX 1)
(define FLAME-PROB 3)
(define G-ACCEL 1)
(define PROXIMITY-THRESHOLD 10)
(define FREE-FALL-VELOCITY 5)
(define DRAG 0.95)
(define MAX-HELI-VELOCITY 5)
(define INITIAL-FIRES# 3)

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
(define LOAD (circle (/ SEGMENT-WIDTH 2) "solid" "blue"))

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

(define-struct load [pos dx dy])
; A Load is a structure:
; (make-load Posn Number)
; interpretation: (make-load p dx) represents a position of a water load
; dropped from the heli at position p and with horizontal velocity dx

; A Fire is a Number
; interpretation: index of a segment which is burning

; FFGS -> FFGS
(define (main ws)
  (big-bang ws
    [on-tick ff-move]
    [on-key ff-control]
    [stop-when ff-game-over? ff-render-final]
    [to-draw ff-render]
    [close-on-stop #false]))


;;; RENDER ;;;

; FFGS -> Image
; renders final screen
(define (ff-render-final ws)
  (place-image (text (if (no-fires? ws) "You won" "You lost") 12 "black")
               (/ WIDTH-OF-WORLD 2)
               (/ HEIGHT-OF-WORLD 2)
               (ff-render ws)))
  
; FFGS -> Image
; renders game state
(define (ff-render ws)
  (place-image HELI (heli-loc (ffgs-heli ws)) ALTITUDE
               (loads-render (ffgs-loads ws)
                             (fires-render (ffgs-fires ws) BACKGROUND))))

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

; List-of-loads Image -> Image
(define (loads-render loads image)
  (cond
    [(empty? loads) image]
    [else (load-render (first loads)
                       (loads-render (rest loads) image))]))

; Load Image -> Image
; renders a load to image
(define (load-render load image)
  (place-image/align LOAD
                     (posn-x (load-pos load))
                     (posn-y (load-pos load))
                     "center"
                     "top"
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
             (move-loads (ffgs-loads ws))
             (spread (ffgs-fires (fires-extinguish ws)))))

; List-of-loads -> List-of-loads
; Updates the location of loads
(define (move-loads ls)
  (cond
    [(empty? ls) ls]
    [else (cons (move-load (first ls))
                (move-loads (rest ls)))]))

; Load -> Load
; Updates the location and vertical speed of load
(define (move-load l)
  (make-load (make-posn (+ (posn-x (load-pos l))
                           (load-dx l))
                        (+ (posn-y (load-pos l))
                           (load-dy l)))
             (* (load-dx l) DRAG)
             (min FREE-FALL-VELOCITY (+ (load-dy l) G-ACCEL))))

; FFGS -> FFGS
; Removes fires hit by water loads
(check-expect (fires-extinguish
               (make-ffgs (make-heli 0 0)
                          '()
                          '()))
              (make-ffgs (make-heli 0 0)
                         '()
                         '()))
(check-expect (fires-extinguish
               (make-ffgs (make-heli 0 0)
                          '()
                          (list 2 3 5)))
              (make-ffgs (make-heli 0 0)
                         '()
                         (list 2 3 5)))
(check-expect (fires-extinguish
               (make-ffgs (make-heli 0 0)
                          (list (make-load
                                 (make-posn (* 2 SEGMENT-WIDTH) HEIGHT-OF-WORLD)
                                 0 10))
                          (list 2 3 5)))
              (make-ffgs (make-heli 0 0)
                         (list (make-load
                                (make-posn (* 2 SEGMENT-WIDTH) HEIGHT-OF-WORLD)
                                0 10))
                         (list 3 5)))
(define (fires-extinguish ws)
  (make-ffgs (ffgs-heli ws)
             (loads-remove-off-screen (ffgs-loads ws))
             (fires-remove-under-loads (ffgs-fires ws)
                                       (ffgs-loads ws))))

; List-of-loads -> List-of-loads
; Filters out loads that sunk below the bottom of canvas
(define (loads-remove-off-screen loads)
  (cond
    [(empty? loads) '()]
    [(> (posn-y (load-pos (first loads))) HEIGHT-OF-WORLD)
     (loads-remove-off-screen (rest loads))]
    [else (cons (first loads)
                (loads-remove-off-screen (rest loads)))]))

; List-of-fires List-of-loads -> List-of-fires
; Removes fires that are located under water loads
(define (fires-remove-under-loads fires loads)
  (cond
    [(empty? fires) fires]
    [(under-any-load? (first fires) loads)
     (fires-remove-under-loads (rest fires) loads)]
    [else (cons (first fires)
                (fires-remove-under-loads (rest fires) loads))]))

; Number List-of-loads
; Determines whether given fire is located under any load from the list
(define (under-any-load? fire-segment loads)
  (cond
    [(empty? loads) #false]
    [else (or (under-load? fire-segment (first loads))
              (under-any-load? fire-segment (rest loads)))]))

; Number Load -> Boolean
; Determines whether given fire is located under given load
(check-expect (under-load? 2 (make-load (make-posn 20 HEIGHT-OF-WORLD) 0 10)) #true)
(define (under-load? fire-segment load)
  (close-enough? (make-posn (* fire-segment SEGMENT-WIDTH) HEIGHT-OF-WORLD)
                 (load-pos load)))

; Posn Posn -> Boolean
; Determines whether too posns are close enough to consider overlapping
(check-expect (close-enough? (make-posn 20 200) (make-posn 20 200)) #true)
(define (close-enough? p1 p2)
  (and (< (abs (- (posn-x p1) (posn-x p2))) PROXIMITY-THRESHOLD)
       (< (abs (- (posn-y p1) (posn-y p2))) PROXIMITY-THRESHOLD)))

; List-of-fires -> List-of-fires
; Creates new fires adjacent to existing with random probability
(define (spread fires)
  (spread-aux fires '()))

; List-of-fires List-of-fires -> List-of-fires
(define (spread-aux fires next-fires)
  (cond
    [(empty? fires) next-fires]
    [else (create-set (spread-aux (rest fires)
                                  (append (maybe-ignite-neighbours (first fires))
                                          next-fires)))]))

; Number -> List-of-numbers
; Adds neighbour segments to the list of fires with some probability
(define (maybe-ignite-neighbours segment)
  (maybe-ignite-at (+ segment 1) (maybe-ignite-at (- segment 1) (list segment))))

; Number List-of-fires
(define (maybe-ignite-at i lf)
  (cond
    [(< i 0) lf]
    [(> i WIDTH-SEGMENTS) lf]
    [(<= (random 100) FLAME-PROB) (cons i lf)]
    [else lf]))

; FFGS -> FFGS
; Drops a new load from the heli
(define (drop-load ws)
  (make-ffgs (ffgs-heli ws)
             (cons (make-load (make-posn (heli-loc (ffgs-heli ws))
                                         ALTITUDE)
                              (heli-vel (ffgs-heli ws))
                              0)
                   (ffgs-loads ws))
             (ffgs-fires ws)))

; FFGS String -> FFGS
; handles key presses to control the helicoper
; - "left" decreases horizontal velocity
; - "right" increases horizontal velocity
; - "space" drops a water load
(define (ff-control ws ke)
  (cond
    [(string=? "left" ke) (make-ffgs (make-heli (heli-loc (ffgs-heli ws))
                                                (max (- (heli-vel (ffgs-heli ws)) DX)
                                                     (* MAX-HELI-VELOCITY -1)))
                                     (ffgs-loads ws)
                                     (ffgs-fires ws))]
    [(string=? "right" ke) (make-ffgs (make-heli (heli-loc (ffgs-heli ws))
                                                 (min MAX-HELI-VELOCITY
                                                      (+ (heli-vel (ffgs-heli ws)) DX)))
                                     (ffgs-loads ws)
                                     (ffgs-fires ws))]
    [(string=? " " ke) (drop-load ws)]
    [else ws]))

; FFGS -> Boolean
; Determines whether the game has ended
(define (ff-game-over? ws)
  (or (no-fires? ws)
      (all-on-fire? ws)))

; FFGS -> Boolean
; Determines if there are no fires left
(define (no-fires? ws)
  (= 0 (length (ffgs-fires ws))))

; FFGS -> Boolean
; Determines if all field is on fire
(define (all-on-fire? ws)
  (>= (length (ffgs-fires ws)) WIDTH-SEGMENTS))

; List-of-strings -> List-of-strings
; Constructs a list that contains every string from
; the input list exactly once
; empty list stays empty
(check-expect (create-set '()) '())
; single element is kept
(check-expect (create-set (list "a")) (list "a"))
; no duplicates – nothing to remove
(check-expect (create-set (list "a" "b")) (list "a" "b"))
; simple duplicate
(check-expect (create-set (list "a" "b" "a")) (list "b" "a"))
; all elements the same
(check-expect (create-set (list "x" "x" "x")) (list "x"))
; duplicate at the end
(check-expect (create-set (list "a" "b" "c" "c")) (list "a" "b" "c"))
; non-adjacent duplicates
(check-expect (create-set (list "a" "b" "a" "b")) (list "a" "b"))
; multiple distinct duplicates in a longer list
(check-expect (create-set (list "a" "b" "c" "b" "a" "d"))
              (list "c" "b" "a" "d"))
(define (create-set los)
  (cond
    [(empty? los) '()]
    [(member? (first los) (rest los))
     (create-set (rest los))]
    [else (cons (first los) (create-set (rest los)))]))

; Number -> List-of-fires
; Creates at most n fires at random segments
(define (create-random-fires n)
  (cond
    [(= 0 n) '()]
    [else (cons (random WIDTH-SEGMENTS)
                (create-random-fires (- n 1)))]))

(define initial-state (make-ffgs (make-heli (/ WIDTH-OF-WORLD 2) 0)
                                 '()
                                 (create-random-fires INITIAL-FIRES#)))

(main initial-state)

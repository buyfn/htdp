;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-47) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define WIDTH-OF-WORLD 300)
(define HEIGHT-OF-WORLD 200)
(define BAR-WIDTH 20)
(define HAPPINESS-DECAY 0.1)
(define PET-INCREASE 1/5)
(define FEED-INCREASE 1/3)

(define canvas (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))

; HappinessScore is a Number
; interpretation: happiness level between 0 and 100

; HappinesScore -> Image
; given HappinessScore returns image of a bar
; length of which represent level of happiness
; examples:
(define (render hs)
  (place-image/align (rectangle (/ (* hs WIDTH-OF-WORLD) 100) BAR-WIDTH
                                "solid" "red")
                     0 HEIGHT-OF-WORLD "left" "bottom"
                     canvas))

; HappinessScore -> HappinessScore
; decreases level of happiness with each clock tick
; unless it's already at the minimum level of 0
; examples:
(check-expect (tock 100) 99.9)
(check-expect (tock 0) 0)
(define (tock hs)
  (if (= hs 0) 0 (- hs HAPPINESS-DECAY)))

; HappinessScore -> HappinessScore
; Increases happiness by 1/5
; Resulting happiness shouldn't exceed 100
; examples:
(check-expect (pet 0) 0)
(check-expect (pet 20) 24)
(check-expect (pet 100) 100)
(define (pet hs)
  (min 100 (+ hs (* PET-INCREASE hs))))

; Increases happiness by 1/3, but no more than 100
; examples:
(check-expect (feed 0) 0)
(check-expect (feed 30) 40)
(check-expect (feed 90) 100)
(define (feed hs)
  (min 100 (+ hs (* FEED-INCREASE hs))))

; HappinessScore, String -> HappinessScore
; Hanles keyboard presses by invoking appropriate functions
; examples:
(check-expect (handle-key 20 "down") 24)
(check-expect (handle-key 30 "up") 40)
(check-expect (handle-key 90 "down") 100)
(check-expect (handle-key 50 "left") 50)
(define (handle-key hs key)
  (cond [(string=? key "down") (pet hs)]
        [(string=? key "up") (feed hs)]
        [else hs]))

; HappinessScore -> HappinessScore
; launches the program from some initial state
(define (gauge-prog ws)
  (big-bang ws
    [on-tick tock]
    [on-key handle-key]
    [to-draw render]))

(gauge-prog 50)
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-109) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define (draw-canvas color)
  (empty-scene 100 100 color))

(define EMPTY-CANVAS (draw-canvas "white"))
(define YELLOW-CANVAS (draw-canvas "yellow"))
(define GREEN-CANVAS (draw-canvas "green"))
(define RED-CANVAS (draw-canvas "red"))

; ExpectsToSee is one of:
; - AA
; - BB
; - DD
; - ER
(define AA "start, expect an 'a'")
(define BB "expect 'b', 'c', or 'd'")
(define DD "finished")
(define ER "error, illegal key")

; ExpectsToSee -> ExpectsToSee
(define (main s)
  (big-bang s
    [on-key process-key]
    [to-draw render]))

; ExpectsToSee String -> ExpectsToSee
(define (process-key s ke)
  (cond
    [(string=? AA s)
     (cond
       [(string=? "a" ke) BB]
       [else ER])]
    [(string=? BB s)
     (cond
       [(or (string=? "b" ke)
            (string=? "c" ke))
        BB]
       [(string=? "d" ke) DD]
       [else ER])]
    [(string=? DD s) AA]
    [(string=? ER s) AA]))

; ExpectsToSee -> Image
(define (render s)
  (cond
    [(string=? AA s) EMPTY-CANVAS]
    [(string=? BB s) YELLOW-CANVAS]
    [(string=? DD s) GREEN-CANVAS]
    [(string=? ER s) RED-CANVAS]))

(main AA)

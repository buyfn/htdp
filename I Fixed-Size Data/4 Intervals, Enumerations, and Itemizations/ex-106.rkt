;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-106) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define WIDTH-OF-WORLD 300)
(define HEIGHT-OF-WORLD 200)
(define canvas (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))

; A VCat is a structure
; (make-vcat Number Number String)
; interpretation (make-vcat p h d)
; p - cat's x-coordinate
; h - happiness level
; d - direction of walking: "forward" or "backward"
(define-struct vcat [pos happiness direction])

; A VCham is a structure
; (make-vcham Number Number String)
; interpretation (make-vcham x h c)
; represents a chamelion position at x along horizontal axis
; with happiness level h
; and color c
(define-struct vcham [pos happiness color])

; A VAnimal is either
; - a VCat
; - a VCham

; Number VAnimal -> VAnimal
; Given location and an animal, walks the animal
; across the canvas starting from the location
(define (main loc animal)
  (big-bang
      (cond
        [(vcat? animal) ...]
        [(vcham? animal) ...])
    [to-draw render]
    [on-key handle-key]
    [on-tick walk]))

; VAnimal -> Image
; renders the animal to canvas
(define (render animal) canvas)

; VAnimal -> VAnimal
; updates animal position and happiness
(define (walk animal)
  (cond
    [(vcat? animal) ...]
    [(vcham? animal) ...]))

; VAnimal String -> VAnimal
; handles key presses
(define (handle-key animal)
  (cond
    [(vcat? animal) ...]
    [(vcham? animal) ...]))

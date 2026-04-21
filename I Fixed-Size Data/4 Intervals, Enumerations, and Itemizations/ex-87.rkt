;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-87) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define CURSOR (rectangle 1 20 "solid" "red"))
(define TEXT-SIZE 11)
(define TEXT-COLOR "black")
(define SCENE-WIDTH 200)
(define SCENE-HEIGHT 20)

(define-struct editor [text cursor])
; An Editor is a structure:
; (make-editor String Number)
; interpretation (make-editor s t) describes an editor
; whose visible text is s with
; the cursor displayed after t characters of s (counting from the left) 

; Editor -> Image
; renders Editor text on an empty scene
(check-expect (render (make-editor "hello world" 6))
              (overlay/align "left" "center"
                             (beside/align "center"
                                           (text "hello " 11 "black")
                                           CURSOR
                                           (text "world" 11 "black"))
                             (empty-scene SCENE-WIDTH SCENE-HEIGHT)))
(define (render state)
  (overlay/align "left" "center"
                 (render-text state)
                 (empty-scene SCENE-WIDTH SCENE-HEIGHT)))

(define (render-text state)
  (beside/align "center"
                (text (substring (editor-text state) 0 (editor-cursor state)) TEXT-SIZE TEXT-COLOR)
                CURSOR
                (text (substring (editor-text state) (editor-cursor state)) TEXT-SIZE TEXT-COLOR))) 

; Editor KeyEvent -> Editor
; Produces updated editor based on key pressed;
; - ignores Tab "\t" and Return "\r"
; - adds single-character KeyEvent to the end of (editor-pre ed)
; - for ke = "\b", deletes the last character of (editor-pre ed)
; - for ke = "left" moves cursor one character to the left
; - for ke = "right" moves cursor one character to the right
; - ignores all other KeyEvents
(check-expect (edit (make-editor "ab" 1) "c")
              (make-editor "acb" 2))
(check-expect (edit (make-editor "ab" 1) "\b")
              (make-editor "b" 0))
; deleting a character at cursor position 0
(check-expect (edit (make-editor "ab" 0) "\b")
              (make-editor "ab" 0))
; moving cursor left at cursor position 0
(check-expect (edit (make-editor "ab" 0) "left")
              (make-editor "ab" 0))
(check-expect (edit (make-editor "ab" 1) "left")
              (make-editor "ab" 0))
(check-expect (edit (make-editor "ab" 1) "right")
              (make-editor "ab" 2))
; right at end (post is empty)
(check-expect (edit (make-editor "ab" 2) "right")
              (make-editor "ab" 2))
; tab key ignored
(check-expect (edit (make-editor "ab" 1) "\t")
              (make-editor "ab" 1))
; return key ignored
(check-expect (edit (make-editor "ab" 1) "\r")
              (make-editor "ab" 1))
; other ignored keys
(check-expect (edit (make-editor "ab" 1) "escape")
              (make-editor "ab" 1))
(check-expect (edit (make-editor "ab" 1) "up")
              (make-editor "ab" 1))
(check-expect (edit (make-editor "ab" 1) "down")
              (make-editor "ab" 1))
; empty editor
(check-expect (edit (make-editor "" 0) "c")
              (make-editor "c" 1))
(check-expect (edit (make-editor "" 0) "\b")
              (make-editor "" 0))
(check-expect (edit (make-editor "" 0) "left")
              (make-editor "" 0))
(check-expect (edit (make-editor "" 0) "right")
              (make-editor "" 0))
; backspace with multiple chars in pre
(check-expect (edit (make-editor "abcd" 3) "\b")
              (make-editor "abd" 2))

(define (edit ed ke)
  (cond
    [(string=? "\t" ke) ed]
    [(string=? "\r" ke) ed]
    [(string=? "\b" ke)
     (cond
       [(> (editor-cursor ed) 0)
        (make-editor (string-remove-at (editor-text ed) (editor-cursor ed))
                     (- (editor-cursor ed) 1))]
       [else ed])]
    [(string=? "left" ke)
     (cond
       [(> (editor-cursor ed) 0)
        (make-editor (editor-text ed) (- (editor-cursor ed) 1))]
       [else ed])]
    [(string=? "right" ke)
     (cond
       [(> (string-length (editor-text ed)) (editor-cursor ed))
        (make-editor (editor-text ed) (+ (editor-cursor ed) 1))]
       [else ed])]
    [(= (string-length ke) 1)
     (cond
       [(> (image-width
            (render-text (make-editor (string-append (editor-text ed) ke)
                                      (+ (editor-cursor ed) 1))))
           SCENE-WIDTH)
        ed]
       [else
        (make-editor (string-insert-at (editor-text ed) ke (editor-cursor ed))
                     (+ (editor-cursor ed) 1))])]
    [else ed]))

; Editor -> Editor
(define (run state)
  (big-bang state
      [on-key edit]
      [to-draw render]))

; String -> String
; removes the last character from a string
(check-expect (string-remove-last "abc") "ab")
(define (string-remove-last str)
  (substring str 0 (- (string-length str) 1)))

; String -> String
; removes the first character from a string
(check-expect (string-remove-first "abc") "bc")
(define (string-remove-first str)
  (substring str 1))

; String -> 1String
; returns the last character of a string
(check-expect (string-last "abc") "c")
(define (string-last str)
  (substring str (- (string-length str) 1)))

; String -> 1String
; returns the first character of a string
(check-expect (string-first "abc") "a")
(define (string-first str)
  (substring str 0 1))

; String 1String Number -> String
; returns a string with a new character insrted at given index of initial string
(check-expect (string-insert-at "ab" "c" 1) "acb")
(define (string-insert-at str char i)
  (string-append (substring str 0 i)
                 char
                 (substring str i)))

; String Number -> String
; removes a character from string at index
(check-expect (string-remove-at "abc" 2) "ac")
(define (string-remove-at str i)
  (string-append (string-remove-last (substring str 0 i))
                 (substring str i)))

(run (make-editor "Edit me" 0))
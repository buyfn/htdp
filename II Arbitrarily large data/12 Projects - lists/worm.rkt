;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname worm) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))
(define SEGMENT-SIZE 10)

(define FIELD-SIZE 20)
(define CANVAS (empty-scene (* SEGMENT-SIZE FIELD-SIZE)
                            (* SEGMENT-SIZE FIELD-SIZE)))

(define-struct worm [body dir])
; A Worm is a structure (make-worm WormBody Direction)
; interpretation: (make-worm w dir) represents a worm with
; body segments `w` and current direction `dir`

; A WormBody is a list of Posn
; constraint: sequential segments are adjasent,
; meaning they differ by 1 in single direction

; A Direction is one of:
; - "up"
; - "right"
; - "down"
; - "left"

(define-struct world [worm food dir-queue])
; A WorldState is a structure (make-world Worm Posn)
; interpretation: (make-world worm food) represents a world with
; a worm and a piece of food

(define worm0 (make-worm (list (make-posn 1 0)
                               (make-posn 0 0))
                         "right"))
(define worm1 (make-worm (list (make-posn (- (quotient FIELD-SIZE 2) 1)
                                          (- (quotient FIELD-SIZE 2) 1))
                               (make-posn (- (quotient FIELD-SIZE 2) 2)
                                          (- (quotient FIELD-SIZE 2) 1)))
                         "right"))
(define world0 (make-world worm1 (make-posn 10 10) '()))

; Number Boolean -> WorldState
(define (main frame-rate show-state?)
  (length
   (worm-body
    (world-worm
     (big-bang world0
       [state show-state?]
       [to-draw render]
       [on-tick tock frame-rate]
       [on-key handle-key]
       [stop-when will-collide? render/game-over]
       [close-on-stop #false])))))

;;; RENDER FUNCTIONS :;;

; WorldState -> Image
; renders a worm to the canvas
(define (render w)
  (render/food (world-food w)
               (render/worm (worm-body (world-worm w)) CANVAS)))

; WormBody Image -> Image
; renders worm to image
(define (render/worm w i)
  (cond
    [(empty? w) i]
    [else (render/segment (first w)
                          "red"
                          (render/worm (rest w) i))]))

; Posn Image -> Image
; renders food to image
(define (render/food food i)
  (render/segment food "green" i))

; Posn String Image -> Image
; renders a worm segment to image
(define (render/segment p color i)
  (place-image/align (circle (/ SEGMENT-SIZE 2) "solid" color)
                     (* (posn-x p) SEGMENT-SIZE)
                     (* (posn-y p) SEGMENT-SIZE)
                     "left"
                     "top"
                     i))

; WorldState -> Image
; renders "game over" screen
(define (render/game-over w)
  (place-image/align (text (if (run-on-itself? w)
                               "Worm ran on itself"
                               "Worm hit border")
                           12 "black")
               2 (* SEGMENT-SIZE FIELD-SIZE) "left" "bottom"
               (render w)))

;;; END OF RENDER FUNCTIONS ;;;


;;; INPUT HANDLING

; WorldState KeyEvent -> WorldState
; handles worm direction control
(define (handle-key w ke)
  (cond
    [(member? ke (list "up" "right" "down" "left"))
     (enqueue ke w)]
    [else w]))

; Direction World
; adds given dirction to the queue of directions
(define (enqueue dir world)
  (make-world (world-worm world)
              (world-food world)
              (append (world-dir-queue world) (list dir))))

; Direction Worm -> Worm
; changes the direction of the worm
(define (change-direction d w)
  (cond
    [(and (string=? (worm-dir w) "up")
          (string=? d "down")) w]
    [(and (string=? (worm-dir w) "right")
          (string=? d "left")) w]
    [(and (string=? (worm-dir w) "down")
          (string=? d "up")) w]
    [(and (string=? (worm-dir w) "left")
          (string=? d "right")) w]
    [else
     (make-worm (worm-body w) d)]))

;;; END OF INPUT HANDLING ;;;


;;; FOOD PLACEMENT ;;;

; Posn -> Posn
;
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
(define (food-create p)
  (food-check-create
   p (make-posn (random FIELD-SIZE) (random FIELD-SIZE))))

; Posn Posn -> Posn
; generative recursion
;
(define (food-check-create p candidate)
  (if (equal? p candidate)
      (food-create p)
      candidate))

; Posn -> Boolean
; use for testing only
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))

;;; END OF FOOD PLACEMENT ;;;


; WorldState -> WorldState
; updates the state of the world after tick
(define (tock w)
  (cond
    [(reached-food? w) (make-world (worm-grow (world-worm (dequeue-direction w)))
                                   (food-create (world-food w))
                                   (world-dir-queue (dequeue-direction w)))]
    [else (make-world (worm-move (world-worm (dequeue-direction w)))
                      (world-food w)
                      (world-dir-queue (dequeue-direction w)))]))

; WorldState -> WorldState
; updates direction of the worm by dequeueing the first direction
(define (dequeue-direction w)
  (cond
    [(empty? (world-dir-queue w)) w]
    [else (make-world (change-direction (first (world-dir-queue w))
                                        (world-worm w))
                      (world-food w)
                      (rest (world-dir-queue w)))]))

; Worm -> Worm
; moves worm one segment ahead
(define (worm-move w)
  (make-worm (cons (get-next-head w) (remove-last (worm-body w)))
             (worm-dir w)))

; Worm -> Worm
; grows worm
(define (worm-grow w)
  (make-worm (cons (get-next-head w)
                   (worm-body w))
             (worm-dir w)))

; WorldState -> Boolean
; returns true if worm head is at food
(define (reached-food? w)
  (equal? (first (worm-body (world-worm w)))
          (world-food w)))

; Worm -> Posn
; gets the next position of worm's head
(define (get-next-head w)
  (cond
    [(string=? (worm-dir w) "up")
     (make-posn (posn-x (first (worm-body w)))
                (if (= (posn-y (first (worm-body w))) 0)
                    FIELD-SIZE
                    (- (posn-y (first (worm-body w))) 1)))]
    [(string=? (worm-dir w) "right")
     (make-posn (if (= (posn-x (first (worm-body w))) (- FIELD-SIZE 1))
                    0
                    (+ (posn-x (first (worm-body w))) 1))
                (posn-y (first (worm-body w))))]
    [(string=? (worm-dir w) "down")
     (make-posn (posn-x (first (worm-body w)))
                (if (= (posn-y (first (worm-body w))) FIELD-SIZE)
                    0
                    (+ (posn-y (first (worm-body w))) 1)))]
    [(string=? (worm-dir w) "left")
     (make-posn (if (= (posn-x (first (worm-body w))) 0)
                    (- FIELD-SIZE 1)
                    (- (posn-x (first (worm-body w))) 1))
                (posn-y (first (worm-body w))))]))

; WorldState -> Boolean
; determines if the worm will run into a wall or itself if moved
(define (will-collide? w) (run-on-itself? w))

; WorldState -> Boolean
; determines whether the worm will run onto iteslf if moved
(define (run-on-itself? w)
  (member? (get-next-head (world-worm (dequeue-direction w)))
           (worm-body (world-worm w))))

; Posn -> Boolean
; determines whether given Posn is outside of play field
(define (out-of-bounds? p)
  (or (< (posn-x p) 0)
      (> (posn-x p) (- FIELD-SIZE 1))
      (< (posn-y p) 0)
      (> (posn-y p) (- FIELD-SIZE 1))))

; List -> List
; removes last element of a list
(check-expect (remove-last '()) '())
(check-expect (remove-last (list 1)) '())
(check-expect (remove-last (list 1 2)) (list 1))
(define (remove-last l)
  (cond
    [(empty? l) '()]
    [(empty? (rest l)) '()]
    [else (cons (first l) (remove-last (rest l)))]))

(main 0.1 #false)

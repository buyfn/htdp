;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-29) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define BASE-PRICE 5.0)
(define BASE-ATTENDANCE 120)
(define PRICE-CHANGE 0.10)
(define ATTENDANCE-CHANGE 15)
(define FIXED-COST 0)
(define COST-PER-TICKET 1.50)

(define (attendees ticket-price)
  (- BASE-ATTENDANCE (* (- ticket-price BASE-PRICE)
                        (/ ATTENDANCE-CHANGE PRICE-CHANGE))))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ FIXED-COST (* COST-PER-TICKET (attendees ticket-price))))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))


(define (profit-2 price)
  (- (* (+ 120
           (* (/ 15 0.1)
              (- 5.0 price)))
        price)
     (+ 0
        (* 1.50
           (+ 120
              (* (/ 15 0.1)
                 (- 5.0 price)))))))


(profit 1)
(profit 2)
(profit 3)
(profit 4)
(profit 5)

(profit-2 1)
(profit-2 2)
(profit-2 3)
(profit-2 4)
(profit-2 5)

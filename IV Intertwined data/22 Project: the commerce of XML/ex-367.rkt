#lang htdp/isl+

; Finished function doesn't contain self-reference because
; it returns attributes from the root xexpr only.
; Therefore it doesn't need to recur on its body elements
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (... (first optional-loa+content)
            ... (rest optional-loa+content) ...)])))


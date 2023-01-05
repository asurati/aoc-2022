;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

;; easier way is to represent the space ranges
;; (498 (0 . 999))
;; after (498 4),
;; (498 (0 . 3) (5 . 999))
;; after (498 5)
;; (498 (0 . 3) (6 . 999))
;; after (498 6)
;; (498 (0 . 3) (7 . 999))
;; after (498 9)
;; (498 (0 . 3) (7 . 8) (10 . 999))


;; closed interval
;; alist entry (col (r0 . r1) (r2 . r3) ...)

;; since the ranges represent space, it will only
;; ever decrease

(define is-pos-in-range?
  (lambda (pos r)
	(let ((lt (car r))
		  (rt (cdr r)))
	  (and (<= lt pos)
		   (<= pos rt)))))

(define range-empty?
  (lambda (r)
	(> (car r) (cdr r))))

(define split-range
  (lambda (r pos)
	(let ((lt (car r))
		  (rt (cdr r)))
	  (cond ((= lt rt)
			 '())
			((= pos lt)
			 (list (cons (+ 1 lt) rt)))
			((= pos rt)
			 (list (cons lt (- rt 1))))
			(else
			 (list (cons lt (- pos 1))
				   (cons (+ pos 1) rt)))))))

;; ((a . b) (c . d) ...)
(define insert-in-range-list
  (lambda (r-list pos)
	;;(display "r-list:")(display r-list)(newline)
	(let loop ((r-list r-list)
			   (result '()))
	  (if (null? r-list)
		  (begin ;;(display "ret:")(display (reverse result))(newline)
				 (reverse result))
		  (let ((r (car r-list)))
			(if (is-pos-in-range? pos r)
				(let ((r (split-range r pos)))
				  (if (null? r)
					  (loop (cdr r-list) result)
					  (loop (cdr r-list) (append (reverse r) result))))
				(loop (cdr r-list) (cons r result))))))))

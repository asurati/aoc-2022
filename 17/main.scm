;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

(import (scheme base)
		(scheme file)
		(scheme write)
		(aoc-2022))

;; maintain the tower as ranges of space.
;; but the row# 0 is at the bottom
;; (highest-rock (r0 . r1) (r2. r3) ...)
;; tower =(
;;  (col# highest-rock (r0.r1)(r2.r3)..) for col#0
;;  ...
;;  (col# highest-rock (r0.r1)(r2.r3)..) for col#6)
;; col#0 is the left most when viewing the picture of the towe

;; col#6 is the right most
;; row#0 is the bottom row.
;; position of a rock is identified by
;; (row . col) of the bottom-left edge of the box around the block
;; note again that row# increases as we go above.

;; shapes minus plus ell stick square

(define build-tower
  (lambda ()
	(let loop ((ix 0)
			   (tower '()))
	  (if (= ix 7)
		  (reverse tower)
		  (loop (+ 1 ix) (cons (list ix -1) tower))))))

;; return a list of (a . b) in any order
;; pos is the bototmleft corner
(define build-minus-at-pos
  (lambda (pos)
	(let* ((row (car pos))
		   (col (cdr pos))
		   (res '())

		   (e (cons row col))
		   (res (cons e res))
		   (e (cons row (+ 1 col)))
		   (res (cons e res))
		   (e (cons row (+ 2 col)))
		   (res (cons e res))
		   (e (cons row (+ 3 col)))
		   (res (cons e res)))
	  res)))

;; return a list of (a . b) in any order
;; pos is the bototmleft corner
(define build-plus-at-pos
  (lambda (pos)
	(let* ((row (car pos))
		   (col (cdr pos))
		   (res '())

		   (e (cons row (+ 1 col)))
		   (res (cons e res))

		   (e (cons (+ 1 row) col))
		   (res (cons e res))
		   (e (cons (+ 1 row) (+ 1 col)))
		   (res (cons e res))
		   (e (cons (+ 1 row) (+ 2 col)))
		   (res (cons e res))

		   (e (cons (+ 2 row) (+ 1 col)))
		   (res (cons e res)))
	  res)))

;; return a list of (a . b) in any order
;; pos is the bototmleft corner
(define build-ell-at-pos
  (lambda (pos)
	(let* ((row (car pos))
		   (col (cdr pos))
		   (res '())

		   (e (cons row col))
		   (res (cons e res))
		   (e (cons row (+ 1 col)))
		   (res (cons e res))
		   (e (cons row (+ 2 col)))
		   (res (cons e res))

		   (e (cons (+ 1 row) (+ 2 col)))
		   (res (cons e res))
		   (e (cons (+ 2 row) (+ 2 col)))
		   (res (cons e res)))
	  res)))

;; return a list of (a . b) in any order
;; pos is the bototmleft corner
(define build-stick-at-pos
  (lambda (pos)
	(let* ((row (car pos))
		   (col (cdr pos))
		   (res '())

		   (e (cons row col))
		   (res (cons e res))
		   (e (cons (+ 1 row) col))
		   (res (cons e res))
		   (e (cons (+ 2 row) col))
		   (res (cons e res))
		   (e (cons (+ 3 row) col))
		   (res (cons e res)))
	  res)))

;; return a list of (a . b) in any order
;; pos is the bototmleft corner
(define build-square-at-pos
  (lambda (pos)
	(let* ((row (car pos))
		   (col (cdr pos))
		   (res '())

		   (e (cons row col))
		   (res (cons e res))
		   (e (cons row (+ 1 col)))
		   (res (cons e res))

		   (e (cons (+ 1 row) col))
		   (res (cons e res))
		   (e (cons (+ 1 row) (+ 1 col)))
		   (res (cons e res)))
	  res)))

(define build-shape-at-pos
  (lambda (type pos)
	(cond ((= type 0)
		   (build-minus-at-pos pos))
		  ((= type 1)
		   (build-plus-at-pos pos))
		  ((= type 2)
		   (build-ell-at-pos pos))
		  ((= type 3)
		   (build-stick-at-pos pos))
		  (else
		   (build-square-at-pos pos)))))

;; t-col; (col# highest-rock-row# (r0.r1)(r2.r3)...)
;; ranges of space
(define t-col-get-col-num
  (lambda (t-col) (car t-col)))
(define t-col-get-hrr
  (lambda (t-col) (cadr t-col)))
(define t-col-get-space-list
  (lambda (t-col) (cddr t-col)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; note that the lt and rt are reversed in position.
(define is-row-in-range?
  (lambda (range row)
	(let ((rt (car range))
		  (lt (cdr range)))
	  (and (<= lt row)
		   (<= row rt)))))

;; sample col#4
;; (4 16 (11.10) (1.1))
;;    from 12 to 16 is rocks,
;;    from 2 to 9 is rocks.
;;    form 0 to 0 is rocks.
;; if row is +2 or more above the hrr
;; the new hrr is row, but a new space range is added
;; ((- row 1) (+ 1 hrr))
;; if row is +1 above hrr, that is laready handled
;; also for a column, row != hrr

;; must return a list, in the same descending order
(define split-range
  (lambda (range row)
	(let ((rt (car range))
		  (lt (cdr range)))
	  (cond ((= rt lt)
			 '())
			((= rt row)
			 (list (cons (- rt 1) lt)))
			((= lt row)
			 (list (cons rt (+ 1 lt))))
			(else
			 (list (cons rt (+ 1 row))
				   (cons (- row 1) lt)))))))

(define split-t-col-sl
  (lambda (col row hrr sl)
	(if (> row hrr)
		(let* ((entry (cons (- row 1) (+ 1 hrr)))
			   (sl (cons entry sl)))
		  (append (list col row) sl))
		(let loop ((sl sl)
				   (res '()))
		  (if (null? sl)
			  (append (list col hrr) (reverse res))
			  (let ((r (car sl))
					(rest (cdr sl)))
				(if (not (is-row-in-range? r row))
					(loop rest (cons r res))
					(loop rest (append (reverse (split-range r row)) res)))))))))

(define add-rock
  (lambda (t-col row)
	(let ((col (t-col-get-col-num t-col))
		  (hrr (t-col-get-hrr t-col))
		  (sl (t-col-get-space-list t-col)))
	  ;;(display "\nadd-rock:") (display (list row col))(newline)
	  ;;(display "curr-t-col:") (display t-col)(newline)
	  (if (= row (+ 1 hrr))
		  (append (list col (+ 1 hrr)) sl)	;; no need to modify sl yet.
		  (if (null? sl)
			  (list col row (cons (- row 1) (+ 1 hrr)))	;; new sl created
			  (split-t-col-sl col row hrr sl))))))

;; sample col#4. the range is for spaces
;; (4 16 (11.10) (1.1))
;;    from 12 to 16 is rocks,
;;    from 2 to 9 is rocks.
;;    form 0 to 0 is rocks.

(define is-space?
  (lambda (t-col row)
	(let ((hrr (t-col-get-hrr t-col))
		  (sl (t-col-get-space-list t-col)))
	  (if (> row hrr)
		  #t
		  (let loop ((sl sl))
			(if (null? sl)
				#f
				(let ((range (car sl)))
				  (if (is-row-in-range? range row)
					  #t
					  (loop (cdr sl))))))))))

;; shape is a list of (row . col)
(define stop-shape-at-pos
  (lambda (tower pos-list)
	(if (null? pos-list)
		tower
		(let* ((pos (car pos-list))
			   (rest (cdr pos-list))
			   (row (car pos))
			   (col (cdr pos))
			   (t-col (assoc col tower))
			   (t-col (add-rock t-col row))
			   (tower (cons t-col tower)))
		  (stop-shape-at-pos tower rest)))))

;; pos-list is the unorded list of (row . col) pairs for a rock
;; we need all such pos to be space.
;; if col < 0 || col > 6 || row < 0, can't move
;; if any (row . col) pair is not space, can't move.

(define is-valid-pos-list?
  (lambda (tower pos-list)
	;;(display "ivpl:")(display pos-list)(newline)
	(if (null? pos-list)
		#t
		(let* ((pos (car pos-list))
			   (rest (cdr pos-list))
			   (row (car pos))
			   (col (cdr pos)))
		  (if (or (< col 0)
				  (< row 0)
				  (> col 6))
			  #f
			  (let ((t-col (assoc col tower)))
				(if (is-space? t-col row)
					(is-valid-pos-list? tower rest)
					#f)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(define shapes '(minus plus ell stick square))
(define get-tower-hrr
  (lambda (tower thrr)
	(if (null? tower)
		thrr
		(let* ((t-col (car tower))
			   (rest (cdr tower))
			   (hrr (t-col-get-hrr t-col)))
		  (get-tower-hrr rest (max thrr hrr))))))

(define get-next-shape-pos
  (lambda (tower)
	(let ((thrr (get-tower-hrr tower -1)))
	  (cons (+ 4 thrr) 2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define push-sideways
  (lambda (pos step)
	(let ((row (car pos))
		  (col (cdr pos)))
	  (if (eqv? step 'left)
		  (cons row (- col 1))
		  (cons row (+ col 1))))))

(define push-down
  (lambda (pos)
	(let ((row (car pos))
		  (col (cdr pos)))
	  (cons (- row 1) col))))

;; return (tower new-pos)
(define push-drop-one-step
  (lambda (tower type pos-0 step)
	(let* (;;(x (display "shape-type:"))
		   ;;(x (display type))
		   ;;(x (newline))
		   ;;(x (display "shape-at:"))
		   ;;(x (display pos-0))
		   ;;(x (newline))
		   ;;(x (display "step:"))
		   ;;(x (display step))
		   ;;(x (newline))
		   (shape-0 (build-shape-at-pos type pos-0))

		   ;;(x (display "before-push-shape:"))
		   ;;(x (display shape-0))
		   ;;(x (newline))

		   (pos-1 (push-sideways pos-0 step))
		   (shape-1 (build-shape-at-pos type pos-1))
		   (sh-1-valid (is-valid-pos-list? tower shape-1))

		   (pos-0 (if sh-1-valid pos-1 pos-0))
		   (shape-0 (if sh-1-valid shape-1 shape-0))

		   ;;(x (display "after-push-shape:"))
		   ;;(x (display shape-0))
		   ;;(x (newline))

		   (pos-1 (push-down pos-0))
		   (shape-1 (build-shape-at-pos type pos-1)))
	  (if (is-valid-pos-list? tower shape-1)	;; down is valid
		  (cons tower pos-1)
		  (let* (#;(x (display "stop-at:"))
				 ;;(x (display pos-0))
				 ;;(x (newline))
				 (tower (stop-shape-at-pos tower shape-0)))
			;;(display "after:\n")
			;;(display-tower tower)
			(cons tower 'stopped))))))

(define drop-new-shape
  (lambda (tower steps step-ix type)
	(let loop ((pos (get-next-shape-pos tower))
			   (tower tower)
			   (ix step-ix))
	  (if (= ix (vector-length steps))
		  (loop pos tower 0)
		  (let* ((res (push-drop-one-step tower type pos (vector-ref steps ix)))
				 (tower (car res))
				 (pos (cdr res)))
			(if (eqv? pos 'stopped)
				(cons tower (+ 1 ix))
				(loop pos tower (+ 1 ix))))))))

(define get-hrr-diff
  (lambda (tower)
	(let ((hrr0 (t-col-get-hrr (assoc 0 tower))))
	  (let loop ((col 0)
				 (res '()))
		(if (= col 7)
			(reverse res)
			(let* ((hrr (t-col-get-hrr (assoc col tower)))
				   (diff (- hrr0 hrr)))
			  (loop (+ 1 col) (cons diff res))))))))

(define run-part-1
  (lambda (tower steps)
	(let loop ((ix 0)
			   (tower tower)
			   (num 0)
			   (type 0))
	  (if (= num 2022)
		  (+ 1 (get-tower-hrr tower -1))
		  (let* ((res (drop-new-shape tower steps ix type))
				 (tower (car res))
				 (ix (cdr res))
				 (num (+ 1 num))
				 (type (modulo (+ 1 type) 5)))
			(loop ix tower num type))))))

(define run-part-2-partial
  (lambda (tower steps ix max-num type)
	(let loop ((ix ix)
			   (tower tower)
			   (num 0)
			   (type type))
	  (if (= num max-num)
		  (+ 1 (get-tower-hrr tower -1))
		  (let* ((res (drop-new-shape tower steps ix type))
				 (tower (car res))
				 (ix (cdr res))
				 (num (+ 1 num))
				 (type (modulo (+ 1 type) 5)))
			(loop ix tower num type))))))

;; maintain a pattern as key-value pairs
;; key: (ix type hrr-diffs)
;; value: num-done thrr

(define run-part-2
  (lambda (tower steps)
	(let loop ((ix 0)
			   (tower tower)
			   (num 0)
			   (type 0)
			   (pat-list '()))
	  ;;(display "dropping #: ")(display num)(newline)
	  (let* ((res (drop-new-shape tower steps ix type))
			 (tower (car res))
			 (ix (cdr res))
			 (num (+ 1 num))
			 (type (modulo (+ 1 type) 5))
			 (thrr (get-tower-hrr tower -1))
			 (pat-1-key (append (list ix type) (get-hrr-diff tower)))
			 (pat-1-val (list num thrr))
			 (pat-1 (cons pat-1-key pat-1-val))
			 (pat-0 (assoc pat-1-key pat-list)))
		(if pat-0
			(let* ((num-done-1 (list-ref pat-1 1))
				   (num-done-0 (list-ref pat-0 1))
				   (num-done-diff (- num-done-1 num-done-0))
				   (thrr-1 (list-ref pat-1 2))
				   (thrr-0 (list-ref pat-0 2))
				   (thrr-diff (- thrr-1 thrr-0))
				   (rept-fact (- 1000000000000 num-done-0))
				   (rept-fact (floor-quotient rept-fact num-done-diff))
				   (num-left (+ num-done-0 (* rept-fact num-done-diff)))
				   (num-left (- 1000000000000 num-left))
				   (height (+ thrr-0 (* rept-fact thrr-diff)))
				   (h-diff (run-part-2-partial tower steps ix num-left type))
				   (h-diff (- h-diff thrr))
				   (height (+ height h-diff)))
			  height)
			(loop ix tower num type (cons pat-1 pat-list)))))))

			;;(display "next-type:") (display type)(newline)
			;;(display "thrr:")(display thrr)(newline)
			;;(display "next-step-ix:")(display ix)(newline)
			#;(if (= 0 (modulo (- num 28) 35))
				(display-tower tower))
			;;(display "next-step-ix:")(display ix)(display " ")
			;;(display "next-type:")(display type)(display " ")
			;;(display (get-hrr-diff tower))(newline)
			#;(if (all-cols-same-hrr? tower)
				(begin (display "acsr:")
					   (display "next-step-ix:")(display ix)(newline)
					   (display "num-done:")(display num)(newline)
					   (display "next-type:")(display type)(newline)
					   (display "thrr:")(display (get-tower-hrr tower -1))
			(newline)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define map-step-char
  (lambda (ch) (if (char=? ch #\<) 'left 'right)))

(define display-tower
  (lambda (tower)
	(let loop ((col 0))
	  (if (= col 7)
		  #f
		  (begin (display (assoc col tower))
				 (newline)
				 (loop (+ 1 col)))))))

(define run-file
  (lambda (file-name)
	(let* ((lines (call-with-input-file file-name read-lines))
		   (steps (string->list (car lines)))
		   (steps (map map-step-char steps))
		   (steps (list->vector steps))
		   (tower (build-tower)))
	  (display "part1:")
	  (display (run-part-1 tower steps))
	  (newline)

	  (display "part2:")
	  (display (run-part-2 tower steps))
	  (newline))))

(run-file "sample.txt")
(run-file "input.txt")

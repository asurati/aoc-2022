;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

(import (scheme base)
		(scheme file)
		(scheme write)
		(scheme cxr)
		(srfi 1)
		(srfi 13)
		(srfi 14)
		(aoc-2022))

(include "ranges.scm")

(define fold-min-col
  (lambda (a b)
	(if (< (car a) b)
		(car a)
		b)))

(define fold-max-col
  (lambda (a b)
	(if (> (car a) b)
		(car a)
		b)))

(define calc-min-col
  (lambda (rocks)
	(fold fold-min-col 1000000 rocks)))

(define calc-max-col
  (lambda (rocks)
	(fold fold-max-col 0 rocks)))

;; TODO filter list
(define calc-max-row-for-line
  (lambda (line)
	(let loop ((line line)
			   (max-row 0))
	  (if (null? line)
		  max-row
		  (loop (cddr line) (max max-row (cadr line)))))))

(define calc-max-row
  (lambda (lines)
	(let loop ((lines lines)
			   (max-row 0))
	  (if (null? lines)
		  max-row
		  (let* ((line (string-tokenize (car lines) char-set:digit))
				 (line (map string->number line))
				 (row (calc-max-row-for-line line)))
			(loop (cdr lines) (max max-row row)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define insert-rock
  (lambda (rocks col row)
	;;(display "insert-rock:") (display (list row col)) (newline)
	(let* ((entry (assoc col rocks))
		   (ranges (if (eqv? #f entry) '((0 . 999)) (cdr entry)))
		   (ranges (insert-in-range-list ranges row))
		   (entry (cons col ranges)))
	  ;;(display "rr:")(display ranges)(newline)
	  (cons entry rocks))))

(define add-rocks-horizontal
  (lambda (rocks row s-col e-col)
	(let ((s-col (min s-col e-col))
		  (e-col (max s-col e-col)))
	  (let loop ((col s-col)
				 (rocks rocks))
		(if (> col e-col)
			rocks
			(loop (+ 1 col) (insert-rock rocks col row)))))))

(define add-rocks-vertical
  (lambda (rocks s-row e-row col)
	(let ((s-row (min s-row e-row))
		  (e-row (max s-row e-row)))
	  (let loop ((row s-row)
				 (rocks rocks))
		(if (> row e-row)
			rocks
			(loop (+ 1 row) (insert-rock rocks col row)))))))

(define add-rocks-start-end
  (lambda (rocks s-row s-col e-row e-col)
	(if (= s-row e-row)
		(add-rocks-horizontal rocks s-row s-col e-col)
		(if (= s-col e-col)
			(add-rocks-vertical rocks s-row e-row e-col)
			#f))))

(define add-rocks-for-line
  (lambda (rocks line)
	(let loop ((line line)
			   (rocks rocks))
	  (if (null? (cddr line))
		  rocks
		  (let* ((s-col (car line))
				 (s-row (cadr line))
				 (e-col (caddr line))
				 (e-row (cadddr line))
				 (rocks (add-rocks-start-end rocks s-row s-col e-row e-col)))
			(loop (cddr line) rocks))))))

(define build-rocks
  (lambda (lines)
	(let loop ((lines lines)
			   (rocks '()))
	  (if (null? lines)
		  rocks
		  (let* ((line (string-tokenize (car lines) char-set:digit))
				 (line (map string->number line))
				 (rocks (add-rocks-for-line rocks line)))
			(loop (cdr lines) rocks))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; closed interval for spaces
;; alist entry (col (r0 . r1) (r2 . r3) ...)

(define find-range-end
  (lambda (ranges pos)
	(let loop ((ranges ranges))
	  (if (null? ranges)
		  #f
		  (let* ((range (car ranges))
				 (left (car range))
				 (right (cdr range)))
			(if (and (<= left pos)
					 (<= pos right))
				right
				(find-range-end (cdr ranges) pos)))))))

;; returns #t if the sand partcile reached the void
;; when range is presnet for a col:
;; curr-row is at 0.
;; 1st entry (left right)
;; if the start-row != left, done.
;; else curr-row for the particle is = right.
;; is there space on the left-side with space available at > curr-row?
(define drop-one-sand-part-1
  (lambda (rocks col row)
	;;(display "dropping from:")(display (list row col))(newline)
	(let ((entry (assoc col rocks)))
	  (if (eqv? #f entry)
		  'void	;; reached the void
		  ;; else we have a rock/sand on this column
		  (let* ((ranges (cdr entry))
				 (end-row (find-range-end ranges row)))
			;;(display "ranges:")(display ranges)(newline)
			(cond ((eqv? #f end-row)
				   'blocked)
				  ((= end-row 999)
				   ;;(display "999:")(display (list row col))(newline)
				   ;;(display "end-row:")(display end-row)(newline)
				   'void)
				  (else
				   (let* ((l-col (- col 1))
						  (r-col (+ col 1))
						  (down (+ 1 end-row)))
				  ;;(display "curr-pos:")(display (list row col))(newline)
					 (let ((res (drop-one-sand-part-1 rocks l-col down)))
					   (if (or (eqv? res 'void)
							   (list? res))
						   res
						   (let ((res (drop-one-sand-part-1 rocks r-col down)))
							 (if (or (eqv? res 'void)
									 (list? res))
								 res
								 (insert-rock rocks col end-row)))))))))))))

;; closed interval
;; alist entry (col (r0 . r1) (r2 . r3) ...)

(define run-part-1
  (lambda (rocks num-sand)
	;;(display "num-sand:") (display num-sand) (newline)
	(let ((result (drop-one-sand-part-1 rocks 500 0)))
	  (if (eqv? result 'void)	;; void
		  num-sand
		  (run-part-1 result (+ 1 num-sand))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define drop-one-sand-part-2
  (lambda (rocks col row floor)
	;;(display "dropping from:")(display (list row col))(newline)
	(let ((entry (assoc col rocks)))
	  (if (eqv? #f entry)
		  ;; if the column isn't present, extend the rocks
		  ;; by adding a rock at the (floor col) location.
		  (drop-one-sand-part-2 (insert-rock rocks col floor) col row floor)
		  (let* ((ranges (cdr entry))
				 (end-row (find-range-end ranges row)))
			;;(display "ranges:")(display ranges)(newline)
			(if (eqv? #f end-row)
				'blocked
				(let* ((l-col (- col 1))
					   (r-col (+ col 1))
					   (down (+ 1 end-row)))
				  ;;(display "curr-pos:")(display (list row col))(newline)
				  (let ((res (drop-one-sand-part-2 rocks l-col down floor)))
					(if (list? res)
						res
						(let ((res (drop-one-sand-part-2 rocks r-col down floor)))
						  (if (list? res)
							  res
							  (if (and (= col 500)
									   (= end-row 0))
								  'src-blocked
								  (insert-rock rocks col end-row)))))))))))))

;; closed interval
;; alist entry (col (r0 . r1) (r2 . r3) ...)

(define run-part-2
  (lambda (rocks floor num-sand)
	;;(display "num-sand:") (display num-sand) (newline)
	(let ((result (drop-one-sand-part-2 rocks 500 0 floor)))
	  (if (eqv? result 'src-blocked)
		  (+ 1 num-sand)
		  (run-part-2 result floor (+ 1 num-sand))))))

(define add-floor
  (lambda (rocks min-col max-col floor)
	(if (> min-col max-col)
		rocks
		(let ((entry (assoc min-col rocks)))
		  (if (eqv? #f entry)	;; add during run-part-2
			  (add-floor rocks
						 (+ 1 min-col)
						 max-col
						 floor)
			  (add-floor (insert-rock rocks min-col floor)
						 (+ 1 min-col)
						 max-col
						 floor))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(define fold-is-in-range
  (lambda (r pos)
	(if (eqv? #t pos)
		#t
		(if (eq? #t (is-pos-in-range? pos r))
			#t
			pos))))

(define is-in-ranges?
  (lambda (ranges pos)
	(let ((res (fold fold-is-in-range pos ranges)))
	  (if (integer? res) #f #t))))

(define ranges-to-string
  (lambda (ranges)
	(let loop ((pos 0)
			   (string ""))
	  (if (= pos 200)
		  (string-append string "\n")
		  (if (is-in-ranges? ranges pos)
			  (loop (+ 1 pos) (string-append string " "))
			  (loop (+ 1 pos) (string-append string "#")))))))

(define rocks-to-strings
  (lambda (rocks min-col max-col)
	(let loop ((col max-col)
			   (string ""))
	  (if (< col min-col)
		  string
		  (let ((ranges (cdr (assoc col rocks))))
			(if (eqv? #f ranges)
				(loop (- col 1) string)
				(loop (- col 1) (string-append string (ranges-to-string ranges)))))))))


(define display-rocks
  (lambda (rocks)
	(if (not (null? rocks))
		(begin (display (car rocks))(newline)
			   (display-rocks (cdr rocks))))))
|#

(define run-file
  (lambda (file-name)
	(let* ((lines (call-with-input-file file-name read-lines))
		   (max-row (calc-max-row lines))
		   #;(rocks (run-part-1 rocks 0))
		   #;(min-col (calc-min-col rocks))
		   #;(max-col (calc-max-col rocks)))
	  ;;(display (rocks-to-strings rocks min-col max-col))
	  ;;(display (merge-i-with-ilist i ilist))
	  ;;(display max-row)
	  ;;(display-rocks rocks)
	  ;;(display (assoc 9 rocks))
	  (let ((rocks (build-rocks lines)))
		(display "part1:")
		(display (run-part-1 rocks 0))
		(newline))
	  (let* ((rocks (build-rocks lines))
			 (floor (+ 2 max-row))
			 (min-col (calc-min-col rocks))
			 (max-col (calc-max-col rocks))
			 (rocks (add-floor rocks min-col max-col floor)))
		(display "part2:")
		(display (run-part-2 rocks floor 0))
		(newline)))))

(run-file "sample.txt")
(newline)
(display "This single-threaded solution to part-2 relies on immutable lists of ranges.\n")
(display "As a result, it runs very slow on input.txt.\n")
(display "Its design will be revisited when time permits.\n")

(display "Uncomment the run-file call, and\n")
(display "    wait for at least 13 minutes (Gauche-gosh timing on my machine).\n")
(display "    wait for at least  3 minutes (Gambit-gsi timing on my machine).\n")
;;(run-file "input.txt")

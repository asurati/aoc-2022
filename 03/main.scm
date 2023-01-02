;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

(import (scheme base)
		(scheme file)
		(scheme write)
		(scheme char)
		(scheme cxr)
		(aoc-2022))

;; SRFI 113 is a suitable option here, but
;; it seems Gambit doesn't implement it,
;; unless one uses Gerbil, which, although
;; dependent upon Gambit, does provide the
;; srfi.

(define get-priority
  (lambda (ch)
	(if (char-lower-case? ch)
		(+ 1 (- (char->integer ch) (char->integer #\a)))
		(+ 27 (- (char->integer ch) (char->integer #\A))))))

;; Convert the strnig into a vector of chars, divide the
;; vector into two equal lists, intersect the lists,
;; and calculate the priority.
(define run-part1
  (lambda (lines)
	(let loop ((lines lines)
			   (sum 0))
	  (if (null? lines)
		  sum
		  (let* ((items (string->vector (car lines)))
				 (length (/ (vector-length items) 2))
				 (left (vector->list items 0 length))
				 (right (vector->list items length))
				 (isect (set-intersect left right))
				 (prio (get-priority (car isect))))
			(loop (cdr lines) (+ sum prio)))))))

(define run-part2
  (lambda (lines)
	(let loop ((lines lines)
			   (sum 0))
	  (if (null? lines)
		  sum
		  (let* ((items0 (string->list (car lines)))
				 (items1 (string->list (cadr lines)))
				 (items2 (string->list (caddr lines)))
				 (isect (set-intersect items0 items1))
				 (isect (set-intersect isect items2))
				 (prio (get-priority (car isect))))
			(loop (cdddr lines) (+ sum prio)))))))

(define run-file
  (lambda (file-name)
	(let* ((lines (call-with-input-file file-name read-lines)))
	  (display "part1: ")
	  (display (run-part1 lines))
	  (newline)
	  (display "part2: ")
	  (display (run-part2 lines))
	  (newline))))

(run-file "sample.txt")
(newline)
(run-file "input.txt")

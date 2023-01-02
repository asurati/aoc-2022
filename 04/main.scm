;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

(import (scheme base)
		(scheme file)
		(scheme write)
		(scheme cxr)
		(srfi 13)
		(srfi 14)
		(aoc-2022))

;; ranges as (2 4 6 8), etc.
(define is-fully-contained?
  (lambda (pairs)
	(let ((l0 (car pairs))
		  (r0 (cadr pairs))
		  (l1 (caddr pairs))
		  (r1 (cadddr pairs)))
	  (or (and (<= l0 l1) (<= r1 r0))
		  (and (<= l1 l0) (<= r0 r1))))))

;; l0<=r0 < l1<=r1
;; l1<=r1 < l0<=r0
(define does-overlap?
  (lambda (pairs)
	(let ((l0 (car pairs))
		  (r0 (cadr pairs))
		  (l1 (caddr pairs))
		  (r1 (cadddr pairs)))
	  (not (or (< r0 l1) (< r1 l0))))))

(define decode-ranges
  (lambda (line)
	(let ((items (string-tokenize line char-set:digit)))
	  (map string->number items))))

(define run-part
  (lambda (lines pred)
	(let loop ((lines lines)
			   (sum 0))
	  (if (null? lines)
		  sum
		  (let ((ranges (decode-ranges (car lines))))
			(if (pred ranges)
				(loop (cdr lines) (+ 1 sum))
				(loop (cdr lines) sum)))))))

(define run-file
  (lambda (file-name)
	(let* ((lines (call-with-input-file file-name read-lines)))
	  (display "part1: ")
	  (display (run-part lines is-fully-contained?))
	  (newline)
	  (display "part2: ")
	  (display (run-part lines does-overlap?))
	  (newline))))

(run-file "sample.txt")
(newline)
(run-file "input.txt")

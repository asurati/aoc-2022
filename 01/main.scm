;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

(import (scheme base)
		(scheme file)
		(scheme write)
		(scheme cxr)
		(srfi 132)
		(aoc-2022))

;; To execute with Gambit:
;; gsi -:search=../lib,r7rs main.scm
;; To execute with Gauche:
;; gosh -A../lib -r7 main.scm

(define calc-sums
  (lambda (lines)
	(let loop ((lines lines)
			   (sum 0)
			   (sums '()))
	  (if (null? lines)
		  (list-sort > (reverse (cons sum sums)))
		  (let ((line (car lines)))
			(if (string=? line "")
				(loop (cdr lines) 0 (cons sum sums))
				(loop (cdr lines) (+ sum (string->number line)) sums)))))))

(define run-file
  (lambda (file-name)
	(let* ((lines (call-with-input-file file-name read-lines))
		   (sorted-sums (calc-sums lines))
		   (part1 (car sorted-sums))
		   (part2 (+ part1 (cadr sorted-sums) (caddr sorted-sums))))
	  (display "part1: ")
	  (display part1)
	  (newline)
	  (display "part2: ")
	  (display part2)
	  (newline))))

(run-file "sample.txt")
(newline)
(run-file "input.txt")

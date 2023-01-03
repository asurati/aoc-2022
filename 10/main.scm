;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

(import (scheme base)
		(scheme file)
		(scheme write)
		(srfi 13)
		(aoc-2022))

(define cycle-cb-1
  (lambda (cycle x)
	(if (member cycle '(20 60 100 140 180 220))
		(* cycle x)
		0)))

(define run-part-1
  (lambda (lines)
	(let loop ((lines lines)
			   (x 1)
			   (cycle 1)
			   (sum 0))
	  (if (null? lines)
		  sum
		  (let* ((line (string-tokenize (car lines)))
				 (cmd (car line))
				 (sigstr (cycle-cb-1 cycle x)))
			(if (string=? cmd "noop")
				(loop (cdr lines) x (+ 1 cycle) (+ sigstr sum))
				(let ((val (string->number (cadr line)))
					  (sigstr (+ sigstr (cycle-cb-1 (+ 1 cycle) x))))
				  (loop (cdr lines) (+ x val) (+ 2 cycle) (+ sum sigstr)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define display-screen
  (lambda (screen)
	(cond ((not (null? screen))
		   (display (car screen)) (newline)
		   (display-screen (cdr screen))))))

(define build-screen
  (lambda (num-rows num-cols)
	(let loop ((num-rows num-rows)
			   (result '()))
	  (if (= 0 num-rows)
		  (reverse result)
		  (loop (- num-rows 1) (cons (make-list num-cols #\space) result))))))

(define build-screen-row
  (lambda (row col)
	(let loop ((row row)
			   (ix 0)
			   (result '()))
	  (cond ((null? row)
			 (reverse result))
			((not (= col ix))
			 (loop (cdr row) (+ 1 ix) (cons (car row) result)))
			(else
			 (loop (cdr row) (+ 1 ix) (cons "#" result)))))))

(define cycle-cb-2
  (lambda (screen cycle x) screen
	(let ((row (floor-quotient (- cycle 1) 40))
		  (col (modulo (- cycle 1) 40))
		  (range (list (- x 1) x (+ x 1))))
	  (if (not (member col range))
		  screen
		  (let loop ((scr screen)
					 (ix 0)
					 (result '()))
			(cond ((null? scr)
				   (reverse result))
				  ((not (= row ix))
				   (loop (cdr scr)
						 (+ 1 ix)
						 (cons (car scr) result)))
				  (else
				   (loop (cdr scr)
						 (+ 1 ix)
						 (cons (build-screen-row (car scr) col) result)))))))))

(define run-part-2
  (lambda (lines screen)
	(let loop ((lines lines)
			   (x 1)
			   (cycle 1)
			   (screen screen))
	  (if (null? lines)
		  (display-screen screen)
		  (let* ((line (string-tokenize (car lines)))
				 (cmd (car line))
				 (screen (cycle-cb-2 screen cycle x)))
			(if (string=? cmd "noop")
				(loop (cdr lines) x (+ 1 cycle) screen)
				(let ((val (string->number (cadr line)))
					  (screen (cycle-cb-2 screen (+ 1 cycle) x)))
				  (loop (cdr lines) (+ x val) (+ 2 cycle) screen))))))))

(define run-file
  (lambda (file-name)
	(let ((lines (call-with-input-file file-name read-lines))
		  (screen (build-screen 6 40)))
	  (display "part1: ")
	  (display (run-part-1 lines))
	  (newline)
	  (display "part2:\n")
	  (run-part-2 lines screen)
	  (newline))))

(run-file "sample.txt")
(newline)
(run-file "input.txt")

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

(import (scheme base)
		(scheme file)
		(scheme write)
		(scheme cxr)
		(srfi 13)
		(aoc-2022))

;; Note that the solution uses the custom set
;; implementation that doesn't rely on mutation.
;; The solution is expected to be sluggish.

(define create-rope
  (lambda (num-knots)
	(let loop ((num-knots num-knots)
			   (result '()))
	  (if (= 0 num-knots)
		  (reverse result)
		  (loop (- num-knots 1) (cons '(0 0) result))))))

(define move-head-one-step
  (lambda (head dir)
	(let ((x (car head))
		  (y (cadr head)))
	  (cond ((string=? dir "R")
			 (list (+ x 1) y))
			((string=? dir "L")
			 (list (- x 1) y))
			((string=? dir "U")
			 (list x (- y 1)))
			(else
			 (list x (+ y 1)))))))

(define are-adjacent?
  (lambda (head tail)
	(let ((diff-x (abs (- (car head) (car tail))))
		  (diff-y (abs (- (cadr head) (cadr tail)))))
	  (and (< diff-x 2) (< diff-y 2)))))

(define get-tail-move-dir
  (lambda (head tail)
	(let ((diff-x (- (car head) (car tail)))
		  (diff-y (- (cadr head) (cadr tail))))
	  (cond ((= diff-x 0)
			 (if (< diff-y 0) "U" "D"))
			((= diff-y 0)
			 (if (< diff-x 0) "L" "R"))
			(else "G")))))

(define move-tail-diagonally
  (lambda (head tail)
	(let* ((x (car tail))
		   (y (cadr tail))
		   (nw (list (- x 1) (- y 1)))
		   (ne (list (+ x 1) (- y 1)))
		   (sw (list (- x 1) (+ y 1)))
		   (se (list (+ x 1) (+ y 1))))
	  (cond ((are-adjacent? head nw) nw)
			((are-adjacent? head ne) ne)
			((are-adjacent? head sw) sw)
			((are-adjacent? head se) se)
			(else #f)))))

(define move-tail-one-step
  (lambda (head tail)
	(if (are-adjacent? head tail)
		tail
		(let ((dir (get-tail-move-dir head tail)))
		  (if (string=? dir "G")
			  (move-tail-diagonally head tail)
			  (move-head-one-step tail dir))))))

(define move-rope-one-step
  (lambda (rope dir)
	(let loop ((rope rope)
			   (result '()))
	  (cond ((null? rope)
			 (reverse result))
			((null? result)
			 (loop (cdr rope)
				   (cons (move-head-one-step (car rope) dir) result)))
			(else
			 (loop (cdr rope)
				   (cons (move-tail-one-step (car result) (car rope)) result)))))))

(define move-rope
  (lambda (rope dir count)
	(let loop ((rope rope)
			   (count count)
			   (set '()))
	  (if (= count 0)
		  (list rope set)
		  (let* ((rope (move-rope-one-step rope dir))
				 (tail-pos (car (reverse rope)))
				 (set (set-insert-item set tail-pos)))
			(loop rope (- count 1) set))))))

(define run-part
  (lambda (rope lines)
	(let loop ((lines lines)
			   (rope rope)
			   (set '()))
	  (if (null? lines)
		  (length set)
		  (let* ((dir (caar lines))
				 (count (cadar lines))
				 (result (move-rope rope dir count))
				 (rope (car result))
				 (lset (cadr result)))
			(loop (cdr lines) rope (set-append-set set lset)))))))

;; rope (x0 y0) (x1 y1) ...
(define run-file
  (lambda (file-name)
	(let* ((lines (call-with-input-file file-name read-lines))
		   (lines (map string-tokenize lines))
		   (lines (map (lambda (row)
						 (list (car row) (string->number (cadr row)))) lines))
		   (rope-1 (create-rope 2))
		   (rope-2 (create-rope 10)))
	  (display "part1: ")
	  (display (run-part rope-1 lines))
	  (newline)
	  (display "part2: ")
	  (display (run-part rope-2 lines))
	  (newline))))

(run-file "sample.0.txt")
(newline)
(run-file "sample.1.txt")
(newline)
(run-file "input.txt")

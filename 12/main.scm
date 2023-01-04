;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

(import (scheme base)
		(scheme file)
		(scheme write)
		(srfi 13)
		(aoc-2022))

(define build-grid
  (lambda (lines)
	(let loop ((lines lines)
			   (result '()))
	  (if (null? lines)
		  (list->vector (reverse result))
		  (loop (cdr lines) (cons (string->vector (car lines)) result))))))

(define get-height
  (lambda (grid row col)
	(let ((ht (vector-ref (vector-ref grid row) col)))
	  (cond ((char=? ht #\S) 0)
			((char=? ht #\E) 25)
			(else (- (char->integer ht) (char->integer #\a)))))))

;; find the location of S and E
(define find-char
  (lambda (lines char)
	(if (null? lines)
		#f
		(let* ((line (car lines))
			   (col (string-index line char)))
		  (if (eqv? col #f)
			  (find-char (cdr lines) char)
			  (list (length lines) col))))))

(define get-nbour
  (lambda (dir pos)
	(let ((row (car pos))
		  (col (cadr pos)))
	  (case dir
		((left) (list row (- col 1)))
		((right) (list row (+ col 1)))
		((up) (list (- row 1) col))
		(else (list (+ row 1) col))))))

(define get-height-diff
  (lambda (grid from to)
	(let* ((s-row (car from))
		   (s-col (cadr from))
		   (e-row (car to))
		   (e-col (cadr to))
		   (s-ht (get-height grid s-row s-col))
		   (e-ht (get-height grid e-row e-col)))
	  (- e-ht s-ht))))

;; can go from from to nbour?
(define can-go?
  (lambda (grid from nbour)
	(let* ((row (car nbour))
		   (col (cadr nbour)))
	  (not (or (< row 0)
			   (< col 0)
			   (>= row (vector-length grid))
			   (>= col (vector-length (vector-ref grid 0)))
			   (> (get-height-diff grid from nbour) 1))))))

;; nbours is a list of (row col) entries
;; queue is a list of ((row col) distance) entries
(define add-nbours
  (lambda (grid queue done entry nbours)
	(let ((from (car entry))
		  (dist (cadr entry)))
	  (let loop ((add-to-q '())
				 (nbours nbours))
		(if (null? nbours)
			(append queue (reverse add-to-q))
			(let ((nbour (car nbours)))
			  (if (or (assoc nbour queue)
					  (assoc nbour done)
					  (not (can-go? grid from nbour)))
				  (loop add-to-q (cdr nbours))
				  (loop (cons (list nbour (+ 1 dist)) add-to-q) (cdr nbours)))))))))

;; queue, done are lists of pairs
(define bfs
  (lambda (grid queue done end limit)
	(if (null? queue)
		#f
		(if (assoc end queue)	;; is there a qentry with end?
			(cadr (assoc end queue))
			(let* ((entry (car queue))
				   (dist (cadr entry)))
			  (if (< dist limit)
				  (bfs-one-step grid queue done end limit)
				  (let (#;(x (display "skipping\n")))
					(bfs grid (cdr queue) done end limit))))))))

(define bfs-one-step
  (lambda (grid queue done end limit)
	(let* ((entry (car queue))
		   (queue (cdr queue))	;; remove the first item from the queue
		   (curr-pos (car entry))

		   (left (get-nbour 'left curr-pos))
		   (right (get-nbour 'right curr-pos))
		   (up (get-nbour 'up curr-pos))
		   (down (get-nbour 'down curr-pos))
		   (nbours (list left right up down))

		   (queue (add-nbours grid queue done entry nbours))
		   (done (cons entry done)))
	  (bfs grid queue done end limit))))

(define run-part
  (lambda (grid queue end limit)
	(bfs grid queue '() end limit)))

(define find-all-zero-heights
  (lambda (grid)
	(let ((num-rows (vector-length grid))
		  (num-cols (vector-length (vector-ref grid 0))))
	  (let loop0 ((row 0)
				  (srcs '()))
		(if (= row num-rows)
			(reverse srcs)
			(let loop1 ((col 0)
						(srcs srcs))
			  (if (= col num-cols)
				  (loop0 (+ 1 row) srcs)
				  (let* ((src (list row col))
						 (entry (list src 0))
						 (ht (get-height grid row col)))
					(if (= 0 ht)
						(loop1 (+ 1 col) (cons entry srcs))
						(loop1 (+ 1 col) srcs))))))))))

(define run-part-2
  (lambda (grid end limit)
	(let ((queue (find-all-zero-heights grid)))
	  (run-part grid queue end limit))))

;; list of pairs
;; ((row col) (prev-row col))
;; ((row col) (prev-row col))
;; ((row col) (prev-row col))
;; ((row col) (prev-row col))

(define run-file
  (lambda (file-name)
	(let* ((lines (call-with-input-file file-name read-lines))
		   (grid (build-grid lines))
		   (result (find-char lines #\S))
		   (s-row (- (length lines) (car result)))
		   (s-col (cadr result))
		   (result (find-char lines #\E))
		   (e-row (- (length lines) (car result)))
		   (e-col (cadr result))
		   (end (list e-row e-col))
		   (start (list s-row s-col))
		   (s-entry (list start 0))	;; ((row col) distance)
		   (queue (list s-entry))
		   (part1 (run-part grid queue end 2147483647)))
	  (display "part1:")
	  (display part1)
	  (newline)
	  (display "part2:")
	  (display (run-part-2 grid end part1))
	  (newline))))

(run-file "sample.txt")
(newline)
(run-file "input.txt")

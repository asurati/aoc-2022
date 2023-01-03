;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

(import (scheme base)
		(scheme file)
		(scheme write)
		(srfi 13)
		(srfi 1)
		(aoc-2022))

;; vectors and loops might be more efficient, though
;; we rely here on map and reduce.

(define char->number
  (lambda (ch)
	(- (char->integer ch) (char->integer #\0))))

(define build-map
  (lambda (lines)
	(let loop ((lines lines)
			   (result '()))
	  (if (null? lines)
		  (reverse result)
		  (let ((ht-map (map char->number (string->list (car lines)))))
			(loop (cdr lines) (cons ht-map result)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We only try looking from left and right.
;; Looking from up == looking from left of CCW 90deg rotation
;; Looking from down == looking from right of CCW 90deg rotation

(define look-from-the-left-minor
  (lambda (line)
	(let loop ((line line)
			   (curr-ht -1)
			   (result '()))
	  (if (null? line)
		  (reverse result)
		  (if (< curr-ht (car line))
			  (loop (cdr line) (car line) (cons 1 result))
			  (loop (cdr line) curr-ht (cons 0 result)))))))

(define look-from-the-left
  (lambda (ht-map)
	(map look-from-the-left-minor ht-map)))

(define look-from-the-right
  (lambda (ht-map)
	(map reverse (look-from-the-left (map reverse ht-map)))))

(define rotate-90-ccw
  (lambda (ht-map)
	(let loop ((ht-map ht-map)
			   (result '()))
	  (if (null? (car ht-map))
		  result
		  (loop (map cdr ht-map) (cons (map car ht-map) result))))))

(define look-from-the-top-bottom
  (lambda (ht-map-rot func)
	(let* ((result (func ht-map-rot))
		   (result (rotate-90-ccw result)))
	  (reverse (map reverse result)))))

(define look-from-the-top
  (lambda (ht-map-rot)
	(look-from-the-top-bottom ht-map-rot look-from-the-left)))

(define look-from-the-bottom
  (lambda (ht-map-rot)
	(look-from-the-top-bottom ht-map-rot look-from-the-right)))

(define view-map-minor
  (lambda (l r t b)
	;;(display "view-map-minor\n")
	;;(display l)(newline)
	;;(display r)(newline)
	;;(display t)(newline)
	;;(display b)(newline)
	(if (> (+ l r t b) 0) 1 0)))

(define view-map-major
  (lambda (l r t b)
	;;(display "view-map-major\n")
	;;(display l)(newline)
	;;(display r)(newline)
	;;(display t)(newline)
	;;(display b)(newline)
	(map view-map-minor l r t b)))

(define count-major
  (lambda (row)
	(fold + 0 row)))

(define run-part-1
  (lambda (ht-map ht-map-rot)
	(let* ((left-view (look-from-the-left ht-map))
		   (right-view (look-from-the-right ht-map))
		   (top-view (look-from-the-top ht-map-rot))
		   (bottom-view (look-from-the-bottom ht-map-rot))
		   (result (map view-map-major left-view right-view top-view bottom-view))
		   (result (count-major (map count-major result))))
	  result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define count-visible-trees
  (lambda (row)
	(let ((ht (car row))
		  (row (cdr row)))
	  (let loop ((row row)
				 (count 0))
		(cond ((null? row)
			   count)
			  ((<= ht (car row))
			   (loop '() (+ 1 count)))
			  (else
			   (loop (cdr row) (+ 1 count))))))))

(define score-to-the-right-minor
  (lambda (line)
	(let loop ((line line)
			   (result '()))
	  (if (null? line)
		  (reverse result)
		  (let ((count (count-visible-trees line)))
			(loop (cdr line) (cons count result)))))))

(define score-to-the-right
  (lambda (ht-map)
	(map score-to-the-right-minor ht-map)))

(define score-to-the-left
  (lambda (ht-map)
	(map reverse (score-to-the-right (map reverse ht-map)))))

(define score-to-the-top-bottom
  (lambda (ht-map-rot func)
	(let* ((result (func ht-map-rot))
		   (result (rotate-90-ccw result)))
	  (reverse (map reverse result)))))

(define score-to-the-top
  (lambda (ht-map-rot)
	(score-to-the-top-bottom ht-map-rot score-to-the-left)))

(define score-to-the-bottom
  (lambda (ht-map-rot)
	(score-to-the-top-bottom ht-map-rot score-to-the-right)))

(define score-map-minor
  (lambda (l r t b)
	(* l r t b)))

(define score-map-major
  (lambda (l r t b)
	(map score-map-minor l r t b)))

(define max-major
  (lambda (row)
	(fold max 0 row)))

(define run-part-2
  (lambda (ht-map ht-map-rot)
	(let* ((left-score (score-to-the-left ht-map))
		   (right-score (score-to-the-right ht-map))
		   (top-score (score-to-the-top ht-map-rot))
		   (bottom-score (score-to-the-bottom ht-map-rot))
		   (result (map score-map-major left-score right-score top-score bottom-score))
		   (result (max-major (map max-major result))))
	  result)))

(define run-file
  (lambda (file-name)
	(let* ((lines (call-with-input-file file-name read-lines))
		   (ht-map (build-map lines))
		   (ht-map-rot (rotate-90-ccw ht-map)))
	  (display "part1: ")
	  (display (run-part-1 ht-map ht-map-rot))
	  (newline)
	  (display "part2: ")
	  (display (run-part-2 ht-map ht-map-rot))
	  (newline))))

(run-file "sample.txt")
(newline)
(run-file "input.txt")

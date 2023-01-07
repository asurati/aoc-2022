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

;; right-handed system.
;; the (x,y,z) coordinates are for the
;; bottomm-left-near corner of the cube.
;; create a graph of all cubes + spaces
;; enclosed inside the cubes + 1 special node
;; called outside.

;; create 3 types of lists
;; rock-list for all cubes which are rocks.

;; trapped-list for all cubes whose sides
;; all are in direct line-of-site to a rock.
;; this is the super-set of the cubes we need.

;; space-list for all cubes whose at least
;; one side sees to infinity
;; now move a cube from trapped-list to
;; space-list if that cube is adjacent to some
;; cube in space-list

(define line-to-list
  (lambda (line)
	(string-tokenize line char-set:digit)))

(define list-to-rock
  (lambda (line)
	(map string->number line)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sum of diffs is 1
(define count-open-sides-one
  (lambda (rock all-rocks)
	(let loop ((rocks all-rocks)
			   (num 6))
	  (if (null? rocks)
		  num
		  (if (equal? rock (car rocks))
			  (loop (cdr rocks) num)
			  (let* ((other (car rocks))
					 (diffs (map (lambda (a b) (abs (- a b))) rock other))
					 (sum (fold + 0 diffs)))
				(if (= sum 1)
					(loop (cdr rocks) (- num 1))
					(loop (cdr rocks) num))))))))

(define count-open-sides
  (lambda (all-rocks)
	(let loop ((rocks all-rocks)
			   (num 0))
	  (if (null? rocks)
		  num
		  (loop (cdr rocks) (+ num (count-open-sides-one (car rocks) all-rocks)))))))

(define run-part-1
  (lambda (rocks)
	(count-open-sides rocks)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define create-fold-func
  (lambda (func index)
	(lambda (cube b)
	  (func (list-ref cube index) b))))

;;min-max (list min-x max-x min-y max-y min-z max-z)
(define is-left-open?
  (lambda (cube rocks min-max)
	(let ((min-x (list-ref min-max 0))
		  (x (car cube))
		  (y (cadr cube))
		  (z (caddr cube)))
	  (let loop ((x x))
		(if (< x min-x)
			#t
			(if (member (list x y z) rocks)
				#f
				(loop (- x 1))))))))

(define is-right-open?
  (lambda (cube rocks min-max)
	(let ((max-x (list-ref min-max 1))
		  (x (car cube))
		  (y (cadr cube))
		  (z (caddr cube)))
	  (let loop ((x x))
		(if (> x max-x)
			#t
			(if (member (list x y z) rocks)
				#f
				(loop (+ x 1))))))))

(define is-bottom-open?
  (lambda (cube rocks min-max)
	(let ((min-y (list-ref min-max 2))
		  (x (car cube))
		  (y (cadr cube))
		  (z (caddr cube)))
	  (let loop ((y y))
		(if (< y min-y)
			#t
			(if (member (list x y z) rocks)
				#f
				(loop (- y 1))))))))

(define is-top-open?
  (lambda (cube rocks min-max)
	(let ((max-y (list-ref min-max 3))
		  (x (car cube))
		  (y (cadr cube))
		  (z (caddr cube)))
	  (let loop ((y y))
		(if (> y max-y)
			#t
			(if (member (list x y z) rocks)
				#f
				(loop (+ y 1))))))))

(define is-far-open?
  (lambda (cube rocks min-max)
	(let ((min-z (list-ref min-max 4))
		  (x (car cube))
		  (y (cadr cube))
		  (z (caddr cube)))
	  (let loop ((z z))
		(if (< z min-z)
			#t
			(if (member (list x y z) rocks)
				#f
				(loop (- z 1))))))))

(define is-near-open?
  (lambda (cube rocks min-max)
	(let ((max-z (list-ref min-max 5))
		  (x (car cube))
		  (y (cadr cube))
		  (z (caddr cube)))
	  (let loop ((z z))
		(if (> z max-z)
			#t
			(if (member (list x y z) rocks)
				#f
				(loop (+ z 1))))))))

;; within straight line of sight
(define is-trapped?
  (lambda (cube rocks min-max)
	(not (or (is-left-open? cube rocks min-max)
			 (is-right-open? cube rocks min-max)
			 (is-top-open? cube rocks min-max)
			 (is-bottom-open? cube rocks min-max)
			 (is-near-open? cube rocks min-max)
			 (is-far-open? cube rocks min-max)))))

(define calc-lists
  (lambda (min-x max-x min-y max-y min-z max-z rocks)
	(let ((min-max (list min-x max-x min-y max-y min-z max-z)))
	  (let loop ((x min-x)
				 (y min-y)
				 (z min-z)
				 (t-list '())
				 (s-list '()))
		(cond ((> z max-z)
			   (loop x (+ 1 y) min-z t-list s-list))
			  ((> y max-y)
			   (loop (+ 1 x) min-y z t-list s-list))
			  ((> x max-x)
			   (cons t-list s-list))
			  ((member (list x y z) rocks)
			   (loop x y (+ 1 z) t-list s-list))
			  ((is-trapped? (list x y z) rocks min-max)
			   (loop x y (+ 1 z) (cons (list x y z) t-list) s-list))
			  (else
			   (loop x y (+ 1 z) t-list (cons (list x y z) s-list))))))))

(define calc-a-nbour
  (lambda (cube diff)
	(list (+ (car cube) (car diff))
		  (+ (cadr cube) (cadr diff))
		  (+ (caddr cube) (caddr diff)))))

(define get-nbours
  (lambda (cube)
	(let* ((cubes (make-list 6 cube))
		   (diffs '((-1 0 0) (1 0 0) (0 -1 0) (0 1 0) (0 0 -1) (0 0 1)))
		   (res (map calc-a-nbour cubes diffs)))
	  res)))

(define create-s-list-check-func
  (lambda (s-list)
	(lambda (cube) (if (member cube s-list) 1 0))))

(define move-from-t-to-s-list
  (lambda (t-list s-list)
	(let loop ((t-list t-list)
			   (s-list s-list)
			   (res '()))
	  (if (null? t-list)
		  res
		  (let* ((t (car t-list))
				 (t-nbours (get-nbours t))
				 (func (create-s-list-check-func s-list))
				 (in-s? (map func t-nbours))
				 (sum (fold + 0 in-s?)))
			;;(display "t:")(display t)(newline)
			;;(display t-nbours)(newline)
			;;(display "s-list:")(display s-list)(newline)
			;;(display "in-s?")(display in-s?)(newline)
			(if (> sum 0)
				(loop (cdr t-list) (cons t s-list) res)
				(loop (cdr t-list) s-list (cons t res))))))))

(define run-file
  (lambda (file-name)
	(let* ((lines (call-with-input-file file-name read-lines))
		   (lines (map line-to-list lines))
		   (rocks (map list-to-rock lines))
		   (min-x-func (create-fold-func min 0))
		   (max-x-func (create-fold-func max 0))
		   (min-y-func (create-fold-func min 1))
		   (max-y-func (create-fold-func max 1))
		   (min-z-func (create-fold-func min 2))
		   (max-z-func (create-fold-func max 2))
		   (min-x (fold min-x-func 2147483647 rocks))
		   (min-y (fold min-y-func 2147483647 rocks))
		   (min-z (fold min-z-func 2147483647 rocks))
		   (max-x (fold max-x-func 0 rocks))
		   (max-y (fold max-y-func 0 rocks))
		   (max-z (fold max-z-func 0 rocks))
		   (part-1 (run-part-1 rocks)))
	  (display "part1:")
	  (display part-1)
	  (newline)

	  (let* ((res (calc-lists min-x max-x min-y max-y min-z max-z rocks))
			 (trapped-list (car res))
			 (space-list (cdr res))
			 (trapped-list (move-from-t-to-s-list trapped-list space-list))
			 (part-2 (run-part-1 trapped-list))
			 (part-2 (- part-1 part-2)))
		(display "part2:")(display part-2)(newline)))))



(run-file "sample.txt")
(run-file "input.txt")

;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

(define-library (aoc-2022)
  (export read-lines set-insert-item set-append-set set-intersect
		  list-head)
  (import (scheme base))
  (begin
	;; take k elements from the start of the list
	(define list-head
	  (lambda (list k)
		(reverse (list-tail (reverse list) (- (length list) k)))))

	;; A tiny set implementation.
	(define set-append-set
	  (lambda (set0 set1)
		(let loop ((set0 set0)
				   (set1 set1))
		  (if (null? set1)
			  set0
			  (loop (set-insert-item set0 (car set1)) (cdr set1))))))

	(define set-insert-item
	  (lambda (set item)
		(if (eq? #f (member item set))
			(cons item set)
			set)))
	(define set-intersect
	  (lambda (set0 set1)
		(let loop ((set0 set0)
				   (result '()))
		  (if (null? set0)
			  result
			  (if (not (eq? #f (member (car set0) set1)))
				  (loop (cdr set0) (cons (car set0) result))
				  (loop (cdr set0) result))))))
	(define read-lines
	  (lambda (file)
		(let loop ((line (read-line file))
				   (lines '()))
		  (if (eof-object? line)
			  (reverse lines)
			  (loop (read-line file) (cons line lines))))))))

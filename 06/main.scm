;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

(import (scheme base)
		(scheme file)
		(scheme write)
		(aoc-2022))

;; search from left for the position which
;; has a duplicate to its right. The next
;; search should begin from that postion+1
;; vector
(define find-index-of-duplicate
  (lambda (group group-len)
	(let loop ((chars group)
			   (dup-ix -1))
	  (if (null? chars)
		  dup-ix
		  (if (member (car chars) (cdr chars))
			  (loop (cdr chars) (- (- group-len (length (cdr chars))) 1))
			  (loop (cdr chars) dup-ix))))))

;; vector number
(define run-part
  (lambda (chars group-len)
	(let loop ((ix 0))
	  (if (> ix (- (vector-length chars) group-len))
		  -1
		  (let* ((group (vector-copy chars ix (+ ix group-len)))
				 (dup-ix (find-index-of-duplicate (vector->list group) group-len)))
			(if (< dup-ix 0)
				(+ ix group-len)
				(loop (+ ix dup-ix 1))))))))

(define run-file
  (lambda (file-name)
	(let* ((lines (call-with-input-file file-name read-lines))
		   (chars (string->vector (car lines))))
	  (display "part1: ")
	  (display (run-part chars 4))
	  (newline)
	  (display "part2: ")
	  (display (run-part chars 14))
	  (newline))))

(run-file "sample.0.txt")
(newline)
(run-file "sample.1.txt")
(newline)
(run-file "sample.2.txt")
(newline)
(run-file "sample.3.txt")
(newline)
(run-file "sample.4.txt")
(newline)
(run-file "input.txt")

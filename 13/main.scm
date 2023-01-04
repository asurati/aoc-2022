;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

(import (scheme base)
		(scheme file)
		(scheme write)
		(scheme read)
		(srfi 13)
		(srfi 1)
		(srfi 132)
		(aoc-2022))

(define map-chars
  (lambda (char)
	(cond ((char=? char #\[) #\()
		  ((char=? char #\]) #\))
		  ((char=? char #\,) #\space)
		  (else char))))

(define str-to-list
  (lambda (line)
	(let* ((line (list->string (map map-chars (string->list line))))
		   (str-port (open-input-string line)))
	  (read str-port))))

(define build-lists
  (lambda (lines)
	(let loop ((lines lines)
			   (result '()))
	  (if (null? lines)
		  (map str-to-list (reverse result))
		  (if (string=? (car lines) "")
			  (loop (cdr lines) result)
			  (loop (cdr lines) (cons (car lines) result)))))))

;; take k elements from the start of the list
(define list-head
  (lambda (list k)
	(reverse (list-tail (reverse list) (- (length list) k)))))

;; a is new, b is prev value
(define fold-res
  (lambda (a b)
	(if (= b 0)
		a
		b)))

(define compare-lists
  (lambda (a b)
	(let* ((len-a (length a))
		   (len-b (length b))
		   (min-len (min len-a len-b))
		   (ma (list-head a min-len))
		   (mb (list-head b min-len))
		   #;(x (display ma))
		   (res (map compare-two ma mb))
		   #;(x (display res))
		   #;(x (newline))
		   (res (fold fold-res 0 res)))
	  (if (or (not (= 0 res))
			  (= len-a len-b))
		  res
		  (if (< len-a len-b) 1 -1)))))

;; correct-order +1
;; wrong-order -1
;; continue comparing 0

(define compare-two
  (lambda (a b)
	;;(display "comparing:")
	;;(display a)(display "")(display b)(newline)
	(cond ((and (integer? a) (integer? b))
		   (if (< a b)
			   1
			   (if (> a b) -1 0)))
		  ((and (list? a) (list? b))
		   (compare-lists a b))
		  ((list? a)	;; b is integer
		   (compare-lists a (list b)))
		  ((list? b)
		   (compare-lists (list a) b))
		  (else #f))))

(define compare-pair
  (lambda (pair)
	(compare-two (car pair) (cadr pair))))

(define create-pairs
  (lambda (lists)
	(let loop ((lists lists)
			   (pairs '()))
	  (if (null? lists)
		  (reverse pairs)
		  (let ((a (car lists))
				(b (cadr lists)))
			(loop (cddr lists) (cons (list a b) pairs)))))))

(define run-part-1
  (lambda (lists)
	(let* ((pairs (create-pairs lists))
		   (res (map compare-pair pairs)))
	  (let loop ((res res)
				 (ix 1)
				 (sum 0))
		(if (null? res)
			sum
			(if (= 1 (car res))
				(loop (cdr res) (+ 1 ix) (+ ix sum))
				(loop (cdr res) (+ 1 ix) sum)))))))

(define sort-two
  (lambda (a b)
	(let ((res (compare-two a b)))
	  (> res 0))))

(define display-lists
  (lambda (lists)
	(if (not (null? lists))
		(begin (display (car lists))
			   (newline)
			   (display-lists (cdr lists))))))

(define run-part-2
  (lambda (lists)
	(let* ((lists (list-sort sort-two lists))
		   (pos0 (length (member '((2)) lists)))
		   (pos1 (length (member '((6)) lists)))
		   (pos0 (- (length lists) pos0))
		   (pos1 (- (length lists) pos1)))
	  (* (+ 1 pos0) (+ 1 pos1)))))

(define run-file
  (lambda (file-name)
	(let* ((lines (call-with-input-file file-name read-lines))
		   (lists (build-lists lines)))
	  (display "part1:")
	  (display (run-part-1 lists))
	  (newline)
	  (display "part2:")
	  (display (run-part-2 (append lists '(((2)) ((6))))))
	  (newline))))

(run-file "sample.txt")
(newline)
(run-file "input.txt")

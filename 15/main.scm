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

;; each report: (s-row s-col b-row b-col distance)
(define build-reports
  (lambda (lines reports)
	(if (null? lines)
		(reverse reports)
		(let* ((line (car lines))
			   (line (string-tokenize line (char-set-union (char-set #\-) char-set:digit)))
			   (s-col (string->number (car line)))
			   (s-row (string->number (cadr line)))
			   (line (cddr line))
			   (b-col (string->number (car line)))
			   (b-row (string->number (cadr line)))
			   (col-diff (abs (- s-col b-col)))
			   (row-diff (abs (- s-row b-row)))
			   (dist (+ col-diff row-diff))
			   (report (list s-row s-col b-row b-col dist)))
		  (build-reports (cdr lines) (cons report reports))))))

(define build-beacons
  (lambda (reports result)
	(if (null? reports)
		(reverse result)
		(let* ((r (car reports))
			   (b-row (list-ref r 2))
			   (b-col (list-ref r 3))
			   (b-r (cons b-row b-col)))
		  (if (not (member b-r result))
			  (build-beacons (cdr reports) (cons b-r result))
			  (build-beacons (cdr reports) result))))))

;; for each sensor, print the range.
;; col +- (dist-diff)

(define get-r-for-row
  (lambda (report row)
	;;(display "grfr: report:") (display report) (newline)
	(let* ((s-row (list-ref report 0))
		   (s-col (list-ref report 1))
		   (dist (list-ref report 4))
		   (row-diff (abs (- row s-row)))
		   (col-diff (- dist row-diff)))
	  (if (> row-diff dist)
		  '()
		  (cons (- s-col col-diff) (+ s-col col-diff))))))

(define get-r-list-for-row
  (lambda (reports row out-r-list)
	(if (null? reports)
		(reverse out-r-list)
		(let ((range (get-r-for-row (car reports) row)))
		  (get-r-list-for-row (cdr reports) row
							  (cons range out-r-list))))))

;; if r-list is null, merge r with out
;; else check merging of r with (car r-list)
;; if merged, add the result itnto out and return out
;; if not merged, call agani wit r and (cdr r-list)

;; a.l <= b.l <= a.r then a.l (max a.r b.r)
;; a.l <= b.r <= a.r then (min a.l b.l) a.r
;; a.r + 1 == b.l then a.l b.r
(define merge-r-pair
  (lambda (a b)
	(let ((a-l (car a))
		  (a-r (cdr a))
		  (b-l (car b))
		  (b-r (cdr b)))
	  (cond ((and (<= a-l b-l)
				  (<= b-l a-r))
			 (cons a-l (max a-r b-r)))
			((and (<= a-l b-r)
				  (<= b-r a-r))
			 (cons (min a-l b-l) a-r))
			((= (+ 1 a-r) b-l)
			 (cons a-l b-r))
			(else #f)))))

;; returns a fully merged r-list
;; if one merge is successful, retry with rest
(define merge-r-with-r-list
  (lambda (a r-list out-r-list)
	;;(display "mrwrl:\n") (display a) (newline)
	;;(display r-list) (newline)
	;;(display out-r-list) (newline)
	(if (null? r-list)
		(begin ;;(display "ret:") (display (cons a out-r-list))
			   ;;(newline)
			   (cons a out-r-list))
		(let* ((b (car r-list))
			   (rest (cdr r-list))
			   (res-ab (merge-r-pair a b))
			   (res-ba (merge-r-pair b a))
			   ;;(x (display "ab:"))
			   ;;(x (display res-ab))
			   ;;(x (newline))
			   ;;(x (display "ba:"))
			   ;(x (display res-ba))
			   ;;(;x (newline))
			   (res (if (pair? res-ab)
						res-ab
						(if (pair? res-ba)
							res-ba
							#f))))
		  ;; if a merge is successful,
		  ;; call again as res#, out-r-list (cdr-rlist)
		  ;; if merge is not successful,
		  ;; add b into out-r-list and
		  ;; call with a, (cdr-list) out-r-list
		  (if (pair? res)
			  (let ((r-list (append out-r-list rest)))
				(merge-r-with-r-list res r-list '()))
			  (let ((out-r-list (cons b out-r-list)))
				(merge-r-with-r-list a rest out-r-list)))))))

(define fold-r-with-r-list
  (lambda (r r-list)
	(if (null? r)
		r-list
		(merge-r-with-r-list r r-list '()))))

(define merge-r-list
  (lambda (r-list)
	(fold fold-r-with-r-list '() r-list)))

(define count-pos-for-r
  (lambda (r)
	(let* ((lt (car r))
		   (rt (cdr r))
		   (diff (abs (- lt rt))))
	  (+ 1 diff))))

(define count-pos-for-r-list
  (lambda (r-list num)
	(if (null? r-list)
		num
		(let ((pos (count-pos-for-r (car r-list))))
		  (count-pos-for-r-list (cdr r-list) (+ pos num))))))

(define count-beacons-for-row
  (lambda (beacons row num)
	(if (null? beacons)
		num
		(let ((b-row (caar beacons)))
		  (if (= b-row row)
			  (count-beacons-for-row (cdr beacons) row (+ 1 num))
			  (count-beacons-for-row (cdr beacons) row num))))))

(define run-part-1
  (lambda (reports beacons row)
	(let* ((r-list (get-r-list-for-row reports row '()))
		   (r-list (merge-r-list r-list))
		   (pos-r-list (count-pos-for-r-list r-list 0))
		   (num-beacons (count-beacons-for-row beacons row 0)))
	  (- pos-r-list num-beacons))))

(define get-r-lists-for-rows
  (lambda (reports min-row max-row result)
	(if (> min-row max-row)
		(reverse result)
		(let* ((r-list (get-r-list-for-row reports min-row '()))
			   (result (cons r-list result)))
		  (get-r-lists-for-rows reports (+ 1 min-row) max-row result)))))

(define calc-freq-for-r-pair
  (lambda (a b row)
	(if (< (car b) (car a))
		(calc-freq-for-r-pair b a row)
		(+ row (* 4000000 (+ 1 (cdr a)))))))

(define calc-freq-for-r-list
  (lambda (r-list row)
	(let ((a (car r-list))
		  (b (cadr r-list)))
	  (calc-freq-for-r-pair a b row))))

(define run-part-2
  (lambda (reports beacons min-row max-row)
	(if (> min-row max-row)
		#f
		(let* ((r-list (get-r-list-for-row reports min-row '()))
			   (r-list (merge-r-list r-list)))
		  (if (= 1 (length r-list))
			  (run-part-2 reports beacons (+ 1 min-row) max-row)
			  (calc-freq-for-r-list r-list min-row))))))

(define run-file
  (lambda (file-name part1-row part2-up part2-down)
	(let* ((lines (call-with-input-file file-name read-lines))
		   (reports (build-reports lines '()))
		   (beacons (build-beacons reports '())))
	  ;;(display reports)
	  (display "part1:")
	  (display (run-part-1 reports beacons part1-row))
	  (newline)

	  (display "part2:")
	  (display (run-part-2 reports beacons part2-up part2-down))
	  ;;(display (run-part-2 rocks floor 0))
	  (newline))))

(run-file "sample.txt" 10 0 20)
(newline)
(display "Because the solution to part-2 is a simple\n")
(display "iteration of part-1, it is slow on input.txt.\n")
(display "Please wait for at least\n")
(display "    1 minute on Gauche-gosh,\n")
(display "    4 minutes on Gambit-gsi.\n\n")
(run-file "input.txt" 2000000 0 4000000)

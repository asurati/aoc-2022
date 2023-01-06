;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

(import (scheme base)
		(scheme file)
		(scheme write)
		(srfi 13)
		(srfi 14)
		(aoc-2022))

;; Each valve (name rate names)

(define infinity 2147483647)

(define build-valves
  (lambda (lines result)
	(if (null? lines)
		(reverse result)
		(let* ((line (car lines))
			   (rate (string-tokenize line char-set:digit))
			   (rate (string->number (car rate)))
			   (names (string-tokenize line char-set:upper-case))
			   (name (cadr names))
			   (links (cddr names))
			   (valve (cons rate links))
			   (valve (cons name valve)))
		  (build-valves (cdr lines) (cons valve result))))))

(define build-working-valves
  (lambda (valves result)
	(if (null? valves)
		(reverse result)
		(let* ((v (car valves))
			   (rest (cdr valves))
			   (v-rate (get-valve-rate v)))
		  (if (= 0 v-rate)
			  (build-working-valves rest result)
			  (build-working-valves rest (cons v result)))))))

(define create-map-routine
  (lambda (a-name)
	(lambda (b) (cons a-name (get-valve-name b)))))

(define remove-self-loops
  (lambda (pair-list result)
	(if (null? pair-list)
		(reverse result)
		(let* ((pair (car pair-list))
			   (rest (cdr pair-list))
			   (lt (car pair))
			   (rt (cdr pair)))
		  (if (string=? lt rt)
			  (remove-self-loops rest result)
			  (remove-self-loops rest (cons pair result)))))))
			   
(define build-working-valve-pairs
  (lambda (valves all-valves result)
	(if (null? valves)
		(remove-self-loops (reverse result) '())
		(let* ((a (car valves))
			   (rest (cdr valves))
			   (a-name (get-valve-name a))
			   (fr (create-map-routine a-name))
			   (pair-list (map fr all-valves))
			   (result (append pair-list result)))
		  (build-working-valve-pairs rest all-valves result)))))

(define get-valve-name
  (lambda (v) (car v)))
(define get-valve-rate
  (lambda (v) (cadr v)))
(define get-valve-links
  (lambda (v) (cddr v)))

;; for each pair of names we have a dist
;; ((name . name) . dist)
;; if there's a link, dist is 1
;; if no link, dist is infinity
;; if self-loop dist is 0
(define build-dist-for-valve
  (lambda (a valves result)
	(if (null? valves)
		result
		(let* ((b (car valves))
			   (rest (cdr valves))
			   (a-links (get-valve-links a))
			   (a-name (get-valve-name a))
			   (b-name (get-valve-name b))
			   (ab-name (cons a-name b-name))
			   (entry (if (member b-name a-links)
						  (cons ab-name 1)
						  (if (string=? a-name b-name)
							  (cons ab-name 0)
							  (cons ab-name infinity)))))
		  (build-dist-for-valve a rest (cons entry result))))))

;; for each valve, add all-valves distances
;; if there a link, the dist is 1.
;; if both valves are 
(define build-dist-for-valves
  (lambda (all-valves valves result)
	(if (null? valves)
		result
		(let* ((v (car valves))
			   (rest (cdr valves))
			   (d (build-dist-for-valve v all-valves '())))
		  (build-dist-for-valves all-valves rest (append d result))))))

(define add-dist
  (lambda (dists a-name b-name dist)
	(let* ((ab-name (cons a-name b-name))
		   (entry (cons ab-name dist)))
	  (cons entry dists))))

(define get-dist
  (lambda (dists a-name b-name)
	(let ((entry (assoc (cons a-name b-name) dists)))
	  (cdr entry))))

(define display-dists
  (lambda (dists)
	(if (not (null? dists))
		(begin (display (car dists)) (newline)
			   (display-dists (cdr dists))))))

;; each entry in the output
;; (name (name . dist) (name . dist) ...)
(define floyd-warshall
  (lambda (all-valves)
	(let* ((num-verts (length all-valves))
		   (dists (build-dist-for-valves all-valves all-valves '())))
	  (let loop0 ((vk all-valves)
				  (dists dists))
		(if (null? vk)
			dists
			(let loop1 ((vi all-valves)
						(dists dists))
			  (if (null? vi)
				  (loop0 (cdr vk) dists)
				  (let loop2 ((vj all-valves)
							  (dists dists))
					(if (null? vj)
						(loop1 (cdr vi) dists)
						(let* ((i-name (get-valve-name (car vi)))
							   (j-name (get-valve-name (car vj)))
							   (k-name (get-valve-name (car vk)))
							   (ij-dist (get-dist dists i-name j-name))
							   (ik-dist (get-dist dists i-name k-name))
							   (kj-dist (get-dist dists k-name j-name))
							   (sum (+ ik-dist kj-dist)))
						  (if (< sum ij-dist)
							  (loop2 (cdr vj) (add-dist dists i-name j-name sum))
							  (loop2 (cdr vj) dists))))))))))))

;; at the start, calculate the benefit of opening a single valve
;; first. Suppose the minute is x (0 <= x <= 29). Opening a valve
;; at minute mark 29 is of no use, since by the time the valve
;; is open (minute mark 30), the time is up.

;;Suppose the minute mark is m, and we are at valve v.
;; the benefit of opening the valve v is (30 - m - 1) * v.rate.
;; at m-mark 0, we are at A, open-valves is ().
;; go to each non-opened valve and see which gives the max benefit
(define calc-benefit
  (lambda (m-mark v tot-time)
	(let* ((rate (get-valve-rate v))
		   (time-left (- tot-time (+ 1 m-mark)))
		   (benefit (* time-left rate)))
	  benefit)))

;; we are at valve v at beginning of m-mark
(define open-valve
  (lambda (all-valves dists m-mark a open-list score)
	;;(display "ov:@")
	;;(display m-mark)(display a)(display open-list)
	;;(newline)
	(if (>= m-mark 29)
		score
		(let* ((a-name (get-valve-name a))
			   (benefit (calc-benefit m-mark a 30))
			   (score (+ score benefit))
			   (open-list (cons a-name open-list))
			   (m-mark (+ 1 m-mark)))	;; time taken to open a
		  
		  (let loop ((valves all-valves)
					 (a-score 0))
			(if (null? valves)
				(let ((score (+ a-score score)))
				  ;;(display "score:") (display score)(newline)
				  score)
				(let* ((b (car valves))
					   (b-name (get-valve-name b)))
				  (if (member b-name open-list)
					  (loop (cdr valves) a-score)
					  (let* ((dist (get-dist dists a-name b-name))
							 (mark (+ dist m-mark))
							 (val (open-valve all-valves dists mark b open-list 0)))
						(loop (cdr valves) (max val a-score)))))))))))

;;dbjhec
(define run-part-1
  (lambda (all-valves valves dists score)
	(if (null? valves)
		score
		(let* ((b (car valves))
			   (b-name (get-valve-name b))
			   (m-mark (get-dist dists "AA" b-name))
			   (val (open-valve all-valves dists m-mark b '() 0))
			   (max-score (max val score)))
		  (if (not (= max-score score))
			  (begin (display "score-until-now:")
					 (display max-score)(newline)))
		  (run-part-1 all-valves (cdr valves) dists max-score)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; we are at valve v at beginning of m-mark
;; if ab is in open-list, exclude both ab and ba.
;; m-pairs have numbers, other pairs contain strings
(define open-v-pair
  (lambda (all-valves all-v-pairs dists ab-m-pair ab-pair open-list score)
	;;(display "ovp") (display ab-pair)(display ab-m-pair)(newline)
	;;(display open-list)
	;;(newline)
	(let* ((a-mark (car ab-m-pair))
		   (b-mark (cdr ab-m-pair)))
	  (if (or (>= a-mark 25)
			  (>= b-mark 25))
		  score
		  (let* ((a-name (car ab-pair))
				 (b-name (cdr ab-pair))
				 (a (assoc a-name all-valves))
				 (b (assoc b-name all-valves))
				 (a-benefit (calc-benefit a-mark a 26))
				 (b-benefit (calc-benefit b-mark b 26))
				 (score (+ score a-benefit b-benefit))
				 (open-list (cons a-name open-list))
				 (open-list (cons b-name open-list)))
						
			(let loop ((v-pairs all-v-pairs)
					   (a-score 0))
			  (if (null? v-pairs)
				  (+ a-score score)
				  (let* ((cd-pair (car v-pairs))
						 (c-name (car cd-pair))
						 (d-name (cdr cd-pair)))
					(if (or (member c-name open-list)
							(member d-name open-list))
						(loop (cdr v-pairs) a-score)
						(let* ((ac-dist (get-dist dists a-name c-name))
							   (bd-dist (get-dist dists b-name d-name))
							   (c-mark (+ 1 a-mark ac-dist))
							   (d-mark (+ 1 b-mark bd-dist))
							   (cd-m-pair (cons c-mark d-mark))
							   (val (open-v-pair all-valves all-v-pairs dists
												 cd-m-pair cd-pair open-list 0)))
						  (loop (cdr v-pairs) (max val a-score))))))))))))

(define run-part-2
  (lambda (all-valves all-v-pairs v-pairs dists score)
	(if (null? v-pairs)
		score
		(let* ((ab-pair (car v-pairs))
			   (a-name (car ab-pair))
			   (b-name (cdr ab-pair))
			   (a-mark (get-dist dists "AA" a-name))
			   (b-mark (get-dist dists "AA" b-name))
			   (ab-m-pair (cons a-mark b-mark))
			   (val (open-v-pair all-valves all-v-pairs dists
								 ab-m-pair ab-pair '() 0))
			   (max-score (max val score)))
		  (if (not (= max-score score))
			  (begin (display "score-until-now:")
					 (display max-score)(newline)))
		  (run-part-2 all-valves all-v-pairs (cdr v-pairs) dists max-score)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		  
			   
;; TODO build a table for open valves from start. m-mark is just after last
;; restrict the # of entries in table to sum (i = 1 to 10) i!
;; valve open.
;; (a, m-mark) -> benefit
;; Then agument the table:
;; (ab, m-mark) -> benefit
;; (ba, m-mark) -> benefit
;; (abc, m-mark) -> benefit ,etc.
;; the table can be built upon itself

(define run-file
  (lambda (file-name)
	(let* ((lines (call-with-input-file file-name read-lines))
		   (valves (build-valves lines '()))
		   (w-valves (build-working-valves valves '()))
		   (w-v-pairs (build-working-valve-pairs w-valves w-valves '()))
		   (x (display "running Floyd-Warshall ...\n\n"))
		   (dists (floyd-warshall valves)))
	  ;;(display valves)
	  ;;(display-dists dists)
	  (display "running part-1 ...\n")
	  (let ((score (run-part-1 w-valves w-valves dists 0)))
		(display "part1:")
		(display score)
		(newline)(newline))

	  (display "running part-2 ...\n")
	  (let ((score (run-part-2 w-valves w-v-pairs w-v-pairs dists 0)))
		(display "part2:")
		(display score)
		(newline)(newline)))))

(display "This single-threaded solution uses brute-force.\n")
(display "Uncomment the run-file call on input.txt and prepare to wait.\n")
(display "  Part-1: at least 4 minutes on Gambit-gsi.\n")
(display "  Part-1: at least 2 minutes on Gauche-gosh.\n")
(display "  Part-2: at least # minutes on Gambit-gsi.\n")
(display "  Part-2: at least 2 hours on Gauche-gosh.\n")

(run-file "sample.txt")
;;(run-file "input.txt")

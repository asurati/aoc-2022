;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

(import (scheme base)
		(scheme file)
		(scheme write)
		(scheme cxr)
		(aoc-2022))

;; To execute with Gambit:
;; gsi -:search=../lib,r7rs main.scm
;; To execute with Gauche:
;; gosh -A../lib -r7 main.scm

;; R < P < S < R
;; 0 < 1 < 2 < 0
;; get-loser(m): m-1 mod 3
;; get-winner(m): m+1 mod 3

;; (their mine) => (1 2), etc.
(define calc-score
  (lambda (result)
	(let* ((theirs (car result))
		   (mine (cadr result))
		   (diff (- theirs mine)))
	  (cond ((= diff 0)			;; draw
			 (+ 1 mine 3))
			((or (= diff -1)
				 (= diff 2))	;; win
			 (+ 1 mine 6))
			(else				;; lose
			 (+ 1 mine 0))))))

;; (A Y) to (0 1), etc.
(define part1-transform
  (lambda (line)
	(let* ((chars (string->list line))
		   (theirs (char->integer (car chars)))
		   (mine (char->integer (caddr chars)))
		   (theirs (- theirs (char->integer #\A)))
		   (mine (- mine (char->integer #\X))))
	  (list theirs mine))))


;; (A Y) to (1 1), etc.
(define part2-transform
  (lambda (line)
	(let* ((chars (string->list line))
		   (theirs (char->integer (car chars)))
		   (mine (caddr chars))
		   (theirs (- theirs (char->integer #\A))))
	  (cond ((char=? mine #\Y)		;; draw
			 (list theirs theirs))
			((char=? mine #\X)		;; lose
			 (list theirs (modulo (- theirs 1) 3)))
			(else					;; win
			 (list theirs (modulo (+ theirs 1) 3)))))))

(define run-part
  (lambda (lines transform)
	(let loop ((lines lines)
			   (score 0))
	  (if (null? lines)
		  score
		  (let ((result (transform (car lines))))
			(loop (cdr lines) (+ score (calc-score result))))))))

(define run-file
  (lambda (file-name)
	(let* ((lines (call-with-input-file file-name read-lines)))
	  (display "part1: ")
	  (display (run-part lines part1-transform))
	  (newline)
	  (display "part2: ")
	  (display (run-part lines part2-transform))
	  (newline))))

(run-file "sample.txt")
(newline)
(run-file "input.txt")

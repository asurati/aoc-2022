;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

(import (scheme base)
		(scheme file)
		(scheme write)
		(scheme char)
		(scheme cxr)
		(srfi 13)
		(srfi 14)
		(aoc-2022))

;; the stack contents lie at offsets
;; 1 + 4 * stack-id

(define calc-num-stacks
  (lambda (line)
	(/ (+ 1 (string-length line)) 4)))

;; each stack is a list, but the array of stacks is a vctor
(define build-stack
  (lambda (id lines)
	(let ((pos (+ 1 (* 4 id))))
	  (let loop ((lines lines)
				 (stack '()))
		(let ((item (vector-ref (string->vector (car lines)) pos)))
		  (cond ((char-numeric? item)
				 (reverse stack))
				((char-upper-case? item)
				 (loop (cdr lines) (cons item stack)))
				(else
				 (loop (cdr lines) stack))))))))

(define build-stacks
  (lambda (num-stacks lines)
	(let loop ((id 0)
			   (stacks '()))
	  (if (= id num-stacks)
		  (list->vector (reverse stacks))
		  (loop (+ 1 id) (cons (build-stack id lines) stacks))))))

;; look for an empty line
(define skip-stack-desc
  (lambda (lines)
	(if (string=? (car lines) "")
		(cdr lines)
		(skip-stack-desc (cdr lines)))))

;; (vector)
(define read-tops
  (lambda (stacks)
	(let loop ((stacks (vector->list stacks))
			   (tos '()))
	  (if (null? stacks)
		  (list->string (reverse tos))
		  (loop (cdr stacks) (cons (caar stacks) tos))))))

;; (vector number number list list)
(define create-new-stacks
  (lambda (stacks from-id to-id from to)
	(let loop ((stks (vector->list stacks))
			   (id 0)
			   (result '()))
	  (cond ((= id (vector-length stacks))
			 (list->vector (reverse result)))
			((= id from-id)
			 (loop (cdr stks) (+ 1 id) (cons from result)))
			((= id to-id)
			 (loop (cdr stks) (+ 1 id) (cons to result)))
			(else
			 (loop (cdr stks) (+ 1 id) (cons (car stks) result)))))))

;; (list list number)
(define move-one-at-a-time
  (lambda (from to count)
	(if (= count 0)
		(list from to)
		(move-one-at-a-time (cdr from) (cons (car from) to) (- count 1)))))

;; simulate at-once by running one-at-a-time;
;; one-at-a-time from temp count
;; one-at-a-time temp to count
;; (list list number)
(define move-at-once
  (lambda (from to count)
	(let* ((result (move-one-at-a-time from '() count))
		   (from (car result))
		   (result (move-one-at-a-time (cadr result) to count))
		   (to (cadr result)))
	  (list from to))))

;; instead of using set! etc., we recreate the stacks during recursion.
(define run-part
  (lambda (stacks lines move-func)
	(if (null? lines)
		(read-tops stacks)
		(let* ((cmd (string-tokenize (car lines) char-set:digit))
			   (count (string->number (car cmd)))
			   (from-id (string->number (cadr cmd)))
			   (to-id (string->number (caddr cmd)))
			   (from-id (- from-id 1))
			   (to-id (- to-id 1))
			   (from (vector-ref stacks from-id))
			   (to (vector-ref stacks to-id))
			   (result (move-func from to count))
			   (from (car result))
			   (to (cadr result))
			   (new-stacks (create-new-stacks stacks from-id to-id from to)))
		  (run-part new-stacks (cdr lines) move-func)))))

(define run-file
  (lambda (file-name)
	(let* ((lines (call-with-input-file file-name read-lines))
		   (num-stacks (calc-num-stacks (car lines)))
		   (stacks (build-stacks num-stacks lines))
		   (lines (skip-stack-desc lines)))
	  (display "part1: ")
	  (display (run-part stacks lines move-one-at-a-time))
	  (newline)
	  (display "part2: ")
	  (display (run-part stacks lines move-at-once))
	  (newline))))

(run-file "sample.txt")
(newline)
(run-file "input.txt")

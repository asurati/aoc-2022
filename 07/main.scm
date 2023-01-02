;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

(import (scheme base)
		(scheme file)
		(scheme write)
		(scheme cxr)
		(srfi 13)
		(srfi 14)
		(srfi 1)
		(srfi 132)
		(aoc-2022))

;; To tokenize a file path
;; cs = (char-set-union char-set:letter (char-set #\.))
;; (string-tokenize "a/b/c.txt" cs)

(define find-dir
  (lambda (paths path)
	(cond ((null? paths)
		   #f)
		  ((and (string? (car paths))
				(string=? (car paths) path))
		   path)
		  (else
		   (find-dir (cdr paths) path)))))

(define fold-dirs
  (lambda (a b)
	(string-append a "/" b)))

;; supports only a single path-element or .. on cd
(define change-dir
  (lambda (paths curr-dir name)
#;	(display (string-append "cd:" curr-dir "," name "\n"))
	(cond ((string=? name "/")
		   name)

		  ((string=? name "..")
		   (if (string=? curr-dir "/")
			   curr-dir
			   (let* ((elements (string-tokenize curr-dir char-set:letter))
					  (elements (reverse (cdr (reverse elements)))))
				 (string-append "/" (fold-right fold-dirs "" elements)))))
		  (else
		   (find-dir paths (string-append curr-dir name "/"))))))

(define list-dir
  (lambda (curr-dir lines)
#;	(display (string-append "ls:" curr-dir "\n"))
	(let loop ((lines lines)
			   (result '()))
	  ;;exit if EOF or ls listing has ended
	  (if (or (null? lines)
			  (eqv? (string-ref (car lines) 0) #\$))
		  (list (reverse result) lines)
		  (let* ((cmd (string-tokenize (car lines)))
				 (path (string-append curr-dir (cadr cmd))))
			(if (string=? (car cmd) "dir")
				(loop (cdr lines) (cons (string-append path "/") result))
				(loop (cdr lines) (cons (list path (string->number (car cmd))) result))))))))

(define build-paths
  (lambda (lines)
	(let loop ((paths (list "/"))
			   (lines lines)
			   (curr-dir "/"))
	  (if (null? lines)
		  paths
		  (let ((cmd (string-tokenize (car lines))))
			(cond ((string=? (cadr cmd) "cd")
				   (loop paths (cdr lines) (change-dir paths curr-dir (caddr cmd))))
				  ((string=? (cadr cmd) "ls")
				   (let ((ls-res (list-dir curr-dir (cdr lines))))
					 (loop (append paths (car ls-res)) (cadr ls-res) curr-dir)))))))))

;; look at each file entry and match with the dir prefix.
(define calc-file-sizes
  (lambda (paths dir-path)
	(let loop ((paths paths)
			   (size 0))
	  (cond ((null? paths)
;;			 (display (string-append "calc-file-sizes:" dir-path "=" (number->string size) "\n"))
			 size)
			((and (list? (car paths))
				  (string-prefix? (string-append dir-path) (caar paths)))
			 #|
			 /bfqzjjct/phslrcw
			 (/bfqzjjct/phslrcw.ljl 240839)

			 The string-prefix match without the trailing / will match the dir with a file
			 not contained (but peer) within the dir. That will be incorrect
			 |#
			 (loop (cdr paths) (+ size (cadar paths))))
			(else
			 (loop (cdr paths) size))))))

;; returns a list of (dir size) entries
(define calc-dir-sizes
  (lambda (paths)
	  (let loop ((p paths)
				 (result '()))
		(cond ((null? p)
			   result)
			  ((list? (car p))
			   (loop (cdr p) result))
			  (else
			   (let ((size (calc-file-sizes paths (car p))))
				 (loop (cdr p) (cons (list (car p) size) result))))))))

(define run-part-1
  (lambda (dir-sizes)
	(let loop ((ds dir-sizes)
			   (sum 0))
	  (cond ((null? ds)
			 sum)
			((<= (cadar ds) 100000)
			 (loop (cdr ds) (+ sum (cadar ds))))
			(else
			 (loop (cdr ds) sum))))))

(define run-part-2
  (lambda (dir-sizes min-size)
	(if (null? dir-sizes)
		0
		(let ((size (cadar dir-sizes)))
		  (if (>= size min-size)
			  size
			  (run-part-2 (cdr dir-sizes) min-size))))))

(define sort-dirs
  (lambda (dir0 dir1)
	(if (> (cadr dir0) (cadr dir1))
		#t
		#f)))

#|
(define display-paths
  (lambda (paths)
	(cond ((not (null? paths))
		   (display (car paths))(newline)
		   (display-paths (cdr paths))))))

(define display-dir-sizes
  (lambda (ds)
	(cond ((not (null? ds))
		   (display (car ds))(newline)
		   (display-dir-sizes (cdr ds))))))
|#

;; file-system is a list of paths.
;; two kinds of paths: "/a/b/c/" for dirs, ("/a/b/c" size) for files.
(define run-file
  (lambda (file-name)
	(let* ((lines (call-with-input-file file-name read-lines))
		   (paths (build-paths lines))
		   (dir-sizes (calc-dir-sizes paths))
		   (dir-sizes (list-sort sort-dirs dir-sizes))
		   (free-space (- 70000000 (cadar dir-sizes)))
		   (to-delete (- 30000000 free-space))
		   (dir-sizes (reverse dir-sizes)))
	  #;(display-paths paths)
	  #;(display-dir-sizes dir-sizes)
	  (display "part1: ")
	  (display (run-part-1 dir-sizes))
	  (newline)
	  (display "part2: ")
	  (display (run-part-2 dir-sizes to-delete))
	  (newline))))

(run-file "sample.txt")
(newline)
(run-file "input.txt")

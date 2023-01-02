;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

(define-library (aoc-2022)
  (export read-lines)
  (import (scheme base))
  (begin
	(define read-lines
	  (lambda (file)
		(let loop ((line (read-line file))
				   (lines '()))
		  (if (eof-object? line)
			  (reverse lines)
			  (loop (read-line file) (cons line lines))))))))

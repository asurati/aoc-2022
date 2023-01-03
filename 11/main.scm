;; SPDX-License-Identifier: MIT
;; Copyright (c) 2023 Amol Surati

(import (scheme base)
		(scheme file)
		(scheme write)
		(scheme cxr)
		(srfi 13)
		(srfi 14)
		(srfi 132)
		(aoc-2022))

(define calc-num-monkeys
  (lambda (lines)
	(let loop ((lines lines)
			   (sum 0))
	  (if (null? lines)
		  sum
		  (if (string-contains (car lines) "Monkey")
			  (loop (cdr lines) (+ 1 sum))
			  (loop (cdr lines) sum))))))

;; Each monkey is:
;; (meta-data items)
;; (id op oprnd divby t-tgt f-tgt num-insp items)

;; items id   operator   operand    divby true-target false-target
;; list  int   +/*/-     int/old    int      int         int

;; The Monkey line is already parsed.
(define build-monkey
  (lambda (lines id)
	(let* ((result id)
		   (items-line (car lines))
		   (lines (cdr lines))

		   (op-line (car lines))
		   (lines (cdr lines))

		   (divby-line (car lines))
		   (lines (cdr lines))

		   (t-tgt-line (car lines))
		   (lines (cdr lines))

		   (f-tgt-line (car lines))
		   (lines (cdr lines))
		   ;; either at an empty line or EOF

		   (lines (if (null? lines) lines (cdr lines)))

		   (items (string-tokenize items-line char-set:digit))
		   (items (map string->number items))	;; list

		   (cs (char-set-union (char-set #\+) (char-set #\*)))
		   (symbols (string-tokenize op-line cs))
		   (operand (string-tokenize op-line char-set:digit))
		   (symbol (car symbols))
		   (operand (if (null? operand)
						"old"
						(string->number (car operand))))
		   (result (append result (list symbol operand)))
		   ;; (id op operand)

		   (divby (string-tokenize divby-line char-set:digit))
		   (divby (string->number (car divby)))
		   (result (append result (list divby)))
		   ;; (id op operand divby)

		   (t-tgt (string-tokenize t-tgt-line char-set:digit))
		   (t-tgt (string->number (car t-tgt)))
		   (result (append result (list t-tgt)))
		   ;; (id op operand divby t-tgt)

		   (f-tgt (string-tokenize f-tgt-line char-set:digit))
		   (f-tgt (string->number (car f-tgt)))
		   (result (append result (list f-tgt 0)))
		   ;; (id op operand divby t-tgt f-tgt num-insp)

		   (result (append result items)))
	  (list lines result))))

;; (id op oprnd divby t-tgt f-tgt num-insp items)
(define build-monkeys
  (lambda (lines)
	(let loop ((lines lines)
			   (monkeys '()))
	  (if (null? lines)
		  (reverse monkeys)
		  (let* ((id (string-tokenize (car lines) char-set:digit))
				 (id (map string->number id))
				 (result (build-monkey (cdr lines) id))
				 (lines (car result))
				 (monkey (cadr result)))
			(loop lines (cons monkey monkeys)))))))

;; (id op oprnd divby t-tgt f-tgt num-insp items)
;;  0  1   2     3     4     5      6

(define get-monkey-num-insp
  (lambda (monkey)
	(list-ref monkey 6)))

(define get-monkey-false-target
  (lambda (monkey)
	(list-ref monkey 5)))

(define get-monkey-true-target
  (lambda (monkey)
	(list-ref monkey 4)))

(define get-monkey-divby
  (lambda (monkey)
	(list-ref monkey 3)))

(define get-monkey-operand
  (lambda (monkey)
	(list-ref monkey 2)))

(define get-monkey-opcode
  (lambda (monkey)
	(list-ref monkey 1)))

(define get-monkey-id
  (lambda (monkey)
	(list-ref monkey 0)))

(define get-monkey-items
  (lambda (monkey)
	(list-tail monkey 7)))

(define calc-modulus
  (lambda (monkeys)
	(let loop ((monkeys monkeys)
			   (result 1))
	  (if (null? monkeys)
		  result
		  (loop (cdr monkeys)
				(* result (get-monkey-divby (car monkeys))))))))

;; (id op oprnd divby t-tgt f-tgt num-insp items)
(define run-monkey
  (lambda (monkey relief modulus)
	;;(display "rm:")
	;;(display monkey)(newline)
	(let* ((num-insp (get-monkey-num-insp monkey))
		   (f-tgt (get-monkey-false-target monkey))
		   (t-tgt (get-monkey-true-target monkey))
		   (divby (get-monkey-divby monkey))
		   (operand (get-monkey-operand monkey))
		   (opcode (get-monkey-opcode monkey))
		   (id (get-monkey-id monkey))
		   (items (get-monkey-items monkey))
		   (num-items (length items)))
	  (let loop ((items items)
				 (t-list '())
				 (f-list '()))
		(if (null? items)	;; send new-monkey t-list and f-list
			(let* ((ni (+ num-insp num-items))
				   (monkey (list id opcode operand divby t-tgt f-tgt ni)))
			  (list monkey (reverse t-list) (reverse f-list)))
			(let* ((item (car items))
				   (operand (if (string? operand) item operand))
				   (result (if (string=? opcode "*")
							   (* item operand)
							   (+ item operand)))
				   (result (if (= 1 relief)
							   (modulo result modulus)
							   (floor-quotient result relief))))
			  (if (= 0 (modulo result divby))
				  (loop (cdr items) (cons result t-list) f-list)
				  (loop (cdr items) t-list (cons result f-list)))))))))

;; (id op oprnd divby t-tgt f-tgt num-insp items)
(define append-monkey-items
  (lambda (monkey tf-list)
	(let* ((num-insp (get-monkey-num-insp monkey))
		   (f-tgt (get-monkey-false-target monkey))
		   (t-tgt (get-monkey-true-target monkey))
		   (divby (get-monkey-divby monkey))
		   (operand (get-monkey-operand monkey))
		   (opcode (get-monkey-opcode monkey))
		   (id (get-monkey-id monkey))
		   (items (get-monkey-items monkey))
		   (monkey (list id opcode operand divby t-tgt f-tgt num-insp)))
	  (append monkey items tf-list))))

;; result = (new-monkey t-list f-list)
(define rebuild-monkeys
  (lambda (monkeys result)
	(let* ((monkey (car result))
		   (t-list (cadr result))
		   (f-list (caddr result))
		   (id (get-monkey-id monkey))
		   (t-tgt (get-monkey-true-target monkey))
		   (f-tgt (get-monkey-false-target monkey)))
	  (let loop ((m monkeys)
				 (new-m '()))
		(if (null? m)
			(reverse new-m)
			(let ((m-id (get-monkey-id (car m))))
			  (cond ((= m-id id)	;; replace the old with the new monkey
					 (loop (cdr m) (cons monkey new-m)))
					((= m-id t-tgt)	;; append the t-list to this monkey
					 (let* ((monkey (car m))
							(monkey (append-monkey-items monkey t-list)))
					   (loop (cdr m) (cons monkey new-m))))
					((= m-id f-tgt)
					 (let* ((monkey (car m))
							(monkey (append-monkey-items monkey f-list)))
					   (loop (cdr m) (cons monkey new-m))))
					(else
					 (loop (cdr m) (cons (car m) new-m))))))))))

(define run-part
  (lambda (monkeys relief modulus num-rounds)
	(if (= 0 num-rounds)
		monkeys
		(let loop ((ix 0)
				   (monkeys monkeys))
		  (if (= ix (length monkeys))
			  (run-part monkeys relief modulus (- num-rounds 1))
			  (let* ((result (run-monkey (list-ref monkeys ix) relief modulus))
					 (monkeys (rebuild-monkeys monkeys result)))
				;;(display "rp:")(display result)(newline)
				;;(display "rb:")(display monkeys)(newline)
				(loop (+ 1 ix) monkeys)))))))

(define sort-monkeys
  (lambda (m0 m1)
	(> (get-monkey-num-insp m0) (get-monkey-num-insp m1))))

(define calc-monkey-business
  (lambda (monkeys)
	(let* ((m0 (car monkeys))
		   (m1 (cadr monkeys))
		   (m0 (get-monkey-num-insp m0))
		   (m1 (get-monkey-num-insp m1)))
	  (* m0 m1))))

(define run-file
  (lambda (file-name)
	(let* ((lines (call-with-input-file file-name read-lines))
		   (lines (map string-trim-both lines))
		   (num-monkeys (calc-num-monkeys lines))
		   (monkeys (build-monkeys lines))
		   (modulus (calc-modulus monkeys)))

	  (let* ((m1 (run-part monkeys 3 1 20))	;; relief modulus #rounds
			 (m1 (list-sort sort-monkeys m1)))
		(display "part1:")
		(display (calc-monkey-business m1))
		(newline))

	  (let* ((m2 (run-part monkeys 1 modulus 10000))	;; relief modulus #rounds
			 (m2 (list-sort sort-monkeys m2)))
		(display "part2:")
		(display (calc-monkey-business m2))
		(newline)))))

(run-file "sample.txt")
(newline)
(run-file "input.txt")

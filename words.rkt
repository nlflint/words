#lang racket
(require rackunit)

;read words from file
;====================
(define in (open-input-file "dictionary.txt"))

(define (readlines file acc)
   (let ([line (read-line file)])
     (if (eof-object? line)
         acc
         (readlines file (cons line acc)))))

(define words (take (reverse (readlines in `())) 1000))

(check-equal? (car words) "AA")
(check-equal? (car (reverse words)) "ZZZ")
(check-equal? (length words) 178691)

;build hashset from words
;========================
(define word-set (list->set words))

(check-equal? (set-member? word-set "ZZZ") #t)
(check-equal? (set-member? word-set "1928309") #f)
(check-equal? (set-member? word-set "HOTEL") #t)


;rearrange a word
;=================
(define (arragements word)
  (remove-duplicates (map list->string
                          (apply append
                                 (map combinations
                                      (permutations (string->list word)))))))

(define (is left)
  (lambda (right) (equal? left right)))

(check-equal? (count (is "DO") (arragements "DOG")) 1)
(check-equal? (count (is "OGD") (arragements "DOG")) 1)
(length (arragements "AARDVAR"))

;score a word
;============

(define (is-word word)
  (set-member? word-set word))

(define (score word)
  (count is-word (arragements word)))

(check-equal? (score "ALLY") 7)

;find highest scoring word
;=========================
;(map score words)

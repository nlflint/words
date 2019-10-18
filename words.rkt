#lang racket
(require rackunit)
(require racket/date)
(require future-visualizer)

;read words from file
;====================
(define in (open-input-file "dictionary.txt"))

(define (reverse-word word)
  (list->string (reverse (string->list word))))

(define (readlines file acc)
   (let ([line (read-line file)])
     (if (eof-object? line)
         acc
         (readlines file (cons (reverse-word line) acc)))))

  
(define words (reverse (readlines in `())))

(check-equal? (car words) "AA")
(check-equal? (car (reverse words)) "ZZZ")
(check-equal? (length words) 178691)

;build hashset from subwords
;===========================
(define (build-sub-words-rec letters word)
  (if (empty? letters)
      `()
      (cons
       (list->string (cons (car letters) word))
       (build-sub-words-rec (cdr letters) (cons (car letters) word)))))
                          
(define (build-sub-words word)
  (build-sub-words-rec (reverse (string->list word)) `()))


(check-equal? (build-sub-words "OLLEH") `("H" "EH" "LEH" "LLEH" "OLLEH"))

(define sub-words (list->set (apply append (map build-sub-words words))))

(check-true (set-member? sub-words "ETOH"))
(check-true (set-member? sub-words "RAVDRAA"))

;build hashset from full words
;========================
(define word-set (list->set words))

(check-equal? (set-member? word-set "ZZZ") #t)
(check-equal? (set-member? word-set "1928309") #f)
(check-equal? (set-member? word-set "LETOH") #t)

;score word
;=======================

(define (is-word word)
  (set-member? word-set (list->string word)))

(define (is-sub-word word)
  (set-member? sub-words (list->string word)))


(define (find-words-rec letters word)
  (let ([is-a-word (is-word word)])
    
    (cond [(and (empty? letters) is-a-word) (list (list->string word))]
          [(empty? letters) `()]
          [(not (is-sub-word word)) `()]
          [else (apply append
                       (if is-a-word (list (list->string word)) `())
                       (map (lambda (letter) (find-words-rec (remove letter letters) (cons letter word))) letters))])))
       
(define (find-words word)
  (let ([letters (string->list word)])
    (list->set (apply append (map (lambda (letter) (find-words-rec (remove letter letters) (list letter))) letters)))))

(define (score-word word)
  (set-count (find-words word)))

(check-equal? (score-word (reverse-word "ALLY")) 7)
(check-equal? (score-word (reverse-word "OVERSPECULATING")) 7925)

;try a few
;==============
(define start (current-seconds))
(apply max (map (lambda (word) (score-word word)) (take words 1000)))
(- (current-seconds) start)

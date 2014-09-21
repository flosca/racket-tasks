#lang racket

(require "libs/dicts.rkt")
(require "libs/graphs.rkt")

; #1
; Two Teams
; http://acm.timus.ru/problem.aspx?space=1&num=1106
(define (timus1106 input output)
  

(define graph (read-input input))
(define n (caar graph))
(define chains (rest graph))

(define (iter i friend-list graph)
  (if (null? (flatten chains)) null
  (if (null? friend-list) graph
      (let ([changed-dict (foldl (λ (x y) (dict-concat y (cons x 2))) 
                       (dict-append graph (cons i 1))
                       (first friend-list))])
        (if (dict-has-key? graph i)
            (iter (add1 i)
                  (rest friend-list)
                  (foldl (λ (x y) (dict-append y (cons x (- 3 (cdar y)))))
                          graph    (first friend-list)))
            (next (add1 i) (rest friend-list) changed-dict))))))

(define (next i fr-l g)
  (if (> i n) g
      (if (dict-has-key? g i)
          (next (add1 i) (rest fr-l) g)
          (iter i fr-l g))))

(define result
  (map car (filter (λ [x] (eq? (cdr x) 1)) (sort (iter 1 chains null) #:key car <))))

(define out (open-output-file output #:exists 'replace))
 (begin
   (displayln (length result) out)
   (for-each (λ (num) (display num out)
                      (display " " out)) result)
   (close-output-port out)))





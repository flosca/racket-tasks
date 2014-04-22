#lang racket

; #1
; Two Teams
; http://acm.timus.ru/problem.aspx?space=1&num=1106
(define (timus1106 path-in path-out)
  
(define (dict-concat dict pair)
  (let ([key   (first pair)]
        [value (rest pair)])
    
    (define (iter lst res)
      (if (null? lst) (append dict (list res))
          (if (eq? (caar lst) key)
              (if (eq? (cdar lst) value)  dict
                  (let ((res (cons key value)))
                    (append (remove (first lst) dict) (list res))))
              (iter (rest lst) res))))    
    (iter dict pair)))
  
(define (read-input filename)
  (map (λ (a) (map (λ (x) (- x 48)) a))
       (map (λ (x) (map char->integer x)) 
            (map (λ (x) (remove* '(#\space #\0) x))
                 (map string->list (file->lines filename))))))

(define graph (read-input path-in))
(define n (caar graph))
(define chains (rest graph))

(define (iter i fr-l gr)
  (if (null? (flatten chains)) null
  (if (null? fr-l) gr
      (let ([changed-dict (foldl (λ (x y) (dict-concat y (cons x 2))) 
                       (dict-concat gr (cons i 1))
                       (first fr-l))])
        (if (dict-has-key? gr i)
            (iter (add1 i)
                  (rest fr-l)
                  (foldl (λ (x y) (dict-concat y (cons x (- 3 (cdar y)))))
                          gr    (first fr-l)))
            (next (add1 i) (rest fr-l) changed-dict))))))

(define (next i fr-l g)
  (if (> i n) g
      (if (dict-has-key? g i)
          (next (add1 i) (rest fr-l) g)
          (iter i fr-l g))))

(define result
  (map car (filter (λ [x] (eq? (cdr x) 1)) (sort (iter 1 chains null) #:key car <))))

(define out (open-output-file path-out #:exists 'replace))
 (begin
   (displayln (length result) out)
   (for-each (λ (num) (display num out)
                      (display " " out)) result)
   (close-output-port out)))





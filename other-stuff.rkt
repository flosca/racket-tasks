#lang racket

;; Find substring with hash-function

(define (find-substring-by-hash sub str)
  
  (define (string-sum str)
    ; hash-function
  (foldl (lambda (x s)
           (+ s (char->integer x)))  0
                                     (string->list str)))
      
  (define hashed-sub (string-sum sub))
  (define sub-len (string-length sub))
  (define str-len (string-length str))
  
  (define (iter str hashed-string i)
    (if (equal? hashed-sub hashed-string)
        (if (equal? sub (substring str 0 sub-len)) i
            (if (>= i (- str-len sub-len)) #f
                (iter (substring str 1)
                      (+ (- hashed-string  (char->integer (string-ref str 0)))
                         (char->integer (string-ref str sub-len)))
                      (add1 i))))
        (if (>= i (- str-len sub-len)) #f
            (iter (substring str 1)
                  (+ (- hashed-string   (char->integer (string-ref str 0)))
                     (char->integer (string-ref str sub-len)))
                  (add1 i)))))
  
  
  (if (> sub-len str-len) #f
      iter str (string-sum (substring str 0 sub-len)) 0))) 




;; All subsets of a set

(define (subsets lst)
  
  (define (combinations lst i)
  (cond [(zero? i) (list null)]
        [(null? lst) null]
  (else (append (map (λ (x) (cons (first lst) x))
                            (combinations (rest lst) (sub1 i)))
                (combinations(rest lst) i)))))
  
  (let* ([set (set->list (list->set lst))]
         [m (length set)])
    
  (define (iter i res)
    (if (> i m) res
        (iter (add1 i) (append res (combinations i set)))))
    
  (iter 1 (list null))))

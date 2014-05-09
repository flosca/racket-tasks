#lang racket

(define (read-input filename)
; nice parsing of a textfile
; from A B
;      C D E
;      etc.
; function evaluates a list: '((A B) (C D E) ...)
  (map (λ (a) (map (λ (x) (- x 48)) a))
       (map (λ (x) (map char->integer x)) 
            (map (λ (x) (remove* '(#\space) x))
                 (map string->list (file->lines filename))))))


(define (dfs a b graph)
  ; depth-first search
  (define (iter res lst)
    ; res - mutating graph, lst - kinda stack
    (if (positive? (list-ref res a)) lst 
        (if (null? lst) false 
            (let* ([i (car lst)]
                   [next (foldl (λ (x y)
                                  (if (equal? y false) 
                                      (if (zero? (list-ref res x)) x false) y))
                                false (list-ref graph i))]
                   [step (list-ref res i)])
              (if (false? next)
                  (iter res (cdr lst))
                  (iter (append (take res next)
                                (cons (add1 step) (drop res (add1 next))))
                        (cons next lst)))))))
                  
  (iter (build-list (length graph) (λ (i) (if (= i b) 1 0))) (list b)))


(define (bfs a b graph)
  ; breadth-first search
  (define (iter res queue) 
    (if (positive? (list-ref res a)) true
        (if (null? queue) false            
            (let* ([i (car queue)]                  
                   [next (filter (λ (x) (zero? (list-ref res x))) 
                                 (list-ref graph i))]              
                   [step (list-ref res i)])
              (if (eq? next false)
                  (iter res (cdr queue))
                  (iter (map (λ (i) (if (zero? (list-ref res i)) 
                                        (if (false? (member i next)) 0 
                                            (add1 step)) 
                                        (list-ref res i)))
                             (build-list (length graph) values))
                        (append (cdr queue) next)))))))
  (iter (build-list (length graph) (λ (i) (if (= i b) 1 0))) (list b)))

(provide (all-defined-out))

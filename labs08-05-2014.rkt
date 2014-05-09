#lang racket

(require "graphs.rkt")

(define (read-input filename)
  (map (λ (a) (map (λ (x) (- x 48)) a))
       (map (λ (x) (map char->integer x)) 
            (map (λ (x) (remove* '(#\space) x))
                 (map string->list (file->lines filename))))))

; #1

(define (check a b graph)
  (or (= (add1 (length (dfs a b graph))) (length (dfs b a graph)))
      (= (sub1 (length (dfs a b graph))) (length (dfs b a graph)))))


(define (odd-even-graph? filename)
  ; Граф задается списками смежности
  (define input (read-input filename))
  (if (false? (ormap (λ (s) (if (check 0 s (cdr input)) s #f)) (range 0 (caar input)))) false true))




; #2
(define (rev x)
  ; changes the rotation of a gear
  (if (eq? x 'l) 'r 'l))

(define (dict-append dict pair)
  ; appends a new pair of linked gears
  (let ([key   (car pair)]
        [value (cdr pair)])
    
    (define (concat lst res)
      (if (null? lst) (append dict (list res))
          (if (eq? (caar lst) key)
              (if (eq? (cdar lst) value)  dict
                  false)
              (concat (rest lst) res))))
    
    (concat dict pair)))

(define (update-positions pair dict)
  ; changes the positions in dict
  (let ([fst (car pair)]
        [snd (cdr pair)])
    (if (false? dict) false
        (if (dict-has-key? dict fst)
            (if (dict-has-key? dict snd)
                (if (equal? (dict-ref dict fst)
                            (dict-ref dict snd)) false dict)
                (dict-append dict (append (cdr pair) (rev (dict-ref dict fst)))))
            dict))))

; main
(define (can-move-gears? filename)
  ; Граф задается смежными ребрами
  (define (iter lst dict)
    (if (null? lst) (if (false? dict) false
                        true)
        (iter (cdr lst) (update-positions (car lst) dict))))
  (iter (cdr (read-input filename)) '((1 . r))))






; #3

(define (chain-has-signal? filename)
  ; Граф задается списками смежности
  (let* ([in (read-input filename)]
         [len (caar in)]
         [k (caar  (reverse in))]
         [l (cadar (reverse in))]
         [graph (take (rest in) len)])
    (if (false? (bfs k l 
                     (map (λ (s) (sort s >)) graph))) false true)))
    
    
 

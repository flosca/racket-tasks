#lang racket

(provide (all-defined-out))

(define (dict-append dict pair)
  (let ([key   (car pair)]
        [value (cdr pair)])
    
    (define (concat lst res)
      (if (null? lst) (append dict (list res))
          (if (eq? (caar lst) key)
              (if (eq? (cdar lst) value)  dict
                  false)
              (concat (rest lst) res))))
    
    (concat dict pair)))

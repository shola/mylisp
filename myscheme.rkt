#lang typed/racket

(provide lat?)
(provide member?)
(provide rember)
(provide multirember)
(provide firsts)
(provide insertR)
(provide multiinsertR)
(provide insertL)
(provide subst)
(provide subst2)

; lat is a list of atoms.

; Returns true if 'list' is a list of atoms
(: lat? (-> (Listof Any) Boolean))
(define (lat? list)
  (cond
    [(null? list) true]
    [(not (list? (car list)))
     (lat? (cdr list))]
    [else false]))

; Returns true if 'item' is in 'lat'
(: member? (-> Any (Listof Any) Boolean))
(define (member? item lat)
  (cond
    [(null? lat) false]
    [(eq? (car lat) item) true]
    [else (member? item (cdr lat))]))

; Removes 'item' from 'lat' if it exists
(: rember (-> Any (Listof Any) (Listof Any)))
(define (rember item lat)
  (cond
    [(null? lat) '()]
    [(eq? item (car lat))
     (cdr lat)]
    [else
     (cons (car lat)
           (rember item (cdr lat)))]))

; Removes all 'item' from 'lat'
(: multirember (-> Any (Listof Any) (Listof Any)))
(define (multirember item lat )
  (cond
    [(null? lat) '()]
    [else (cond
            [(eq? item (car lat))
             (multirember item (cdr lat))]
            [else
             (cons (car lat)
                   (multirember item (cdr lat)))])]))
(define filter multirember)

; Returns a list of the car's of each sublist
(: firsts (-> (Listof (Listof Any)) (Listof Any)))
(define (firsts list)
  (cond
    [(null? list) '()]
    [else (cons (car (car list))
                (firsts (cdr list)))]))

; Returns 'list' with 'new' inserted after the value 'old'
(: insertR (-> Any Any (Listof Any) (Listof Any)))
(define (insertR new old lat)
  (cond
    [(null? lat) '()]
    [else (cond
            [(eq? old (car lat))
             (cons old (cons new (cdr lat)))]
            [else (cons (car lat)
                        (insertR new old (cdr lat)))])]))

; Returns 'list' with 'new' inserted after each 'old'
(: multiinsertR (-> Any Any (Listof Any) (Listof Any)))
(define (multiinsertR new old lat)
  (cond
    [(null? lat) '()]
    [else (cond
            [(eq? old (car lat))
             (cons old
                   (cons new
                         (multiinsertR new old (cdr lat))))]
            [else (cons (car lat)
                        (multiinsertR new old (cdr lat)))])]))

; Returns 'list' with 'new' inserted before the value 'old'
(: insertL (-> Any Any (Listof Any) (Listof Any)))
(define (insertL new old lat)
  (cond
    [(null? lat) '()]
    [else (cond
            [(eq? old (car lat))
             (cons new lat)]
            [else (cons (car lat)
                        (insertL new old (cdr lat)))])]))

; Returns 'list' with 'old' replaced with 'new'
(: subst (-> Any Any (Listof Any) (Listof Any)))
(define (subst new old lat)
  (cond
    [(null? lat) '()]
    [else (cond
            [(eq? old (car lat))
             (cons new (cdr lat))]
            [else (cons (car lat)
                        (subst new old (cdr lat)))])]))

; Returns 'list' with 'old1' or 'old2' replaced with 'new'
; (whichever comes first)
(: subst2 (-> Any Any Any (Listof Any) (Listof Any)))
(define (subst2 new old1 old2 lat)
  (cond
    [(null? lat) '()]
    [else (cond
            [(or (eq? old1 (car lat)) (eq? old2 (car lat)))
             (cons new (cdr lat))]
            [else (cons (car lat)
                        (subst2 new old1 old2 (cdr lat)))])]))
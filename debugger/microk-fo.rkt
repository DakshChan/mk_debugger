#lang racket
(provide
 (all-from-out "common.rkt")
 (struct-out disj)
 (struct-out conj)
 (struct-out relate)
 (struct-out prim)
 (struct-out mplus)
 (struct-out bind)
 (struct-out pause)
 (struct-out pop)
 pp-map
 pp-map-reset!
 pp-map-add!
 failed-lst
 failed-count
 failed-lst-reset!
 step
 mature
 mature?)

(require "common.rkt")

(define pp-map (make-hash '()))

(define (pp-map-reset!)
  (set! pp-map (make-hash '())))

(define (pp-map-add! stx)
  (let ((ref (hash-ref pp-map stx #f)))
    (if ref
        (hash-set! pp-map stx (+ ref 1))
        (hash-set! pp-map stx 1))))

(define failed-lst '())
(define failed-count 0)

(define (failed-lst-reset!)
  (set! failed-lst '()))

; same as state->stream but uniformly randomly tracks failed states
; in a list with a maximum capacity of n (where n>=0)
(define (state->stream/log-fails! st oldst n)
  (let ((s (state->stream st))
        (len (length failed-lst)))
    (cond ((not s)
           (set! failed-count (+ failed-count 1))
           (if n
               (if (< len n)
                   (add-failed! oldst)
                   (cond ((> n (* (random) failed-count))) ; eqv to (n/failed-count) chance of success
                         (replace-random-failed! oldst)))
               (add-failed! oldst))))
    s))

(define (replace-random-failed! x)
  (set! failed-lst (cons x (car (shuffle failed-lst)))))

(define (add-failed! x)
  (set! failed-lst (cons x failed-lst)))

;; first-order microKanren
(struct disj     (g1 g2)                  #:prefab)
(struct conj     (g1 g2)                  #:prefab)
(struct relate   (thunk description stx)  #:prefab)
(struct prim     (name ts stx)            #:prefab)
(struct bind     (s g)                    #:prefab)
(struct mplus    (s1 s2)                  #:prefab)
(struct pause    (st g)                   #:prefab)
(struct pop      ()                       #:prefab)

(define (mature? s) (or (not s) (pair? s)))
(define (mature s n)
  (if (mature? s) s (mature (step s n) n)))

(define (start st g n)
  (match g
    ((disj g1 g2)
     (step (mplus (pause st g1)
                  (pause st g2))
           n))
    ((conj g1 g2)
     (step (bind (pause st g1) g2) n))
    ((relate thunk _ stx)
     (begin
       (pp-map-add! stx)
       ;(println "Adding syntax to stack...")
       ;(printf "~s -> ~s\n"
       ;        (map (lambda (stx) (syntax->datum stx)) (state-stack st))
       ;        (map (lambda (stx) (syntax->datum stx)) (cons stx (state-stack st))))
       (pause (extend-state-path/stack st stx) (conj (thunk) (pop)))))
    ((pop)
     (begin
       ;(println "Popping syntax from stack...")
       ;(printf "~s -> ~s\n"
       ;        (map (lambda (stx) (syntax->datum stx)) (state-stack st))
       ;        (map (lambda (stx) (syntax->datum stx)) (cdr (state-stack st))))
       (state->stream (pop-state-stack st))))
    ((prim type ts stx)
     (begin
       (pp-map-add! stx)
       (let ((newst (extend-state-path st stx)))
         (state->stream/log-fails!
          (match* (type ts)
            (('==          (list t1 t2)) (unify t1 t2 newst))
            (('=/=         (list t1 t2)) (disunify t1 t2 newst))
            (('symbolo     (list t))     (typify t symbol? newst))
            (('stringo     (list t))     (typify t string? newst))
            (('numbero     (list t))     (typify t number? newst))
            (('not-symbolo (list t))     (distypify t symbol? newst))
            (('not-stringo (list t))     (distypify t string? newst))
            (('not-numbero (list t))     (distypify t number? newst)))
          newst
          n))))))

(define (step s n)
  (match s
    ((mplus s1 s2)
     (let ((s1 (if (mature? s1) s1 (step s1 n))))
       (cond ((not s1) s2)
             ((pair? s1)
              (cons (car s1)
                    (mplus s2 (cdr s1))))
             (else (mplus s2 s1)))))
    ((bind s g)
     (let ((s (if (mature? s) s (step s n))))
       (cond ((not s) #f)
             ((pair? s)
              (step (mplus (pause (car s) g)
                           (bind (cdr s) g))
                    n))
             (else (bind s g)))))
    ((pause st g) (start st g n))
    (_            s)))

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
 reset-globals!
 paused-solns
 set-paused-solns!
 paused-stream
 set-paused-stream!
 pp-map
 failed-lst
 failed-lst-size
 set-failed-lst-size!
 step
 mature
 mature?)

(require "common.rkt")

; Debugging

(define (reset-globals!)
  (pp-map-reset!)
  (failed-lst-reset!)
  (set! paused-stream #f)
  (set! paused-solns '()))

(define paused-solns '())
(define (set-paused-solns! lst)
  (set! paused-solns lst))

(define paused-stream #f)
(define (set-paused-stream! s)
  (set! paused-stream s))

(define pp-map (make-hash '()))
(define (pp-map-reset!)
  (set! pp-map (make-hash '())))
(define (pp-map-add-count! stx)
  (let ((ref (hash-ref pp-map stx #f)))
    (if ref
        (hash-set! pp-map stx (hash-set ref 'count (+ 1 (hash-ref ref 'count #f))))
        (hash-set! pp-map stx (make-immutable-hash '((count . 1) (successes . 0) (fails . 0)))))))
(define (pp-map-add-successes! stx)
  (let ((ref (hash-ref pp-map stx))) ; we will never fail to reference a succeeding program point
    (hash-set! pp-map stx (hash-set ref 'successes (+ 1 (hash-ref ref 'successes #f))))))
(define (pp-map-add-fails! stx)
  (let ((ref (hash-ref pp-map stx))) ; we will never fail to reference a rejected program point
    (hash-set! pp-map stx (hash-set ref 'fails (+ 1 (hash-ref ref 'fails #f))))))

(define failed-lst '())
(define (failed-lst-reset!)
  (set! failed-lst '()))

(define failed-lst-size #f)
(define (set-failed-lst-size! n)
  (set! failed-lst-size n))

(define failed-count 0)

(define (state->stream/log st oldst cx)
  (pp-map-add-count! (last-from-state-path oldst))
  (let ((s (state->stream st))
        (len (length failed-lst)))
    (cond ((not s)
           (set! failed-count (+ failed-count 1))
           (pp-map-add-fails! (car (state-path oldst)))
           (for-each (lambda (stx) (pp-map-add-fails! stx))
                     (state-stack oldst))
           (if failed-lst-size
               (if (< len failed-lst-size)
                   (add-failed! (cons oldst cx))
                   (cond ((> failed-lst-size (* (random) failed-count))) ; eqv to (n/failed-count) chance of success
                         (replace-random-failed! (cons oldst cx))))
               (add-failed! (cons oldst cx))))
          (else (pp-map-add-successes! (car (state-path oldst)))))
    s))

(define (replace-random-failed! x)
  (set! failed-lst (cons x (car (shuffle failed-lst)))))

(define (add-failed! x)
  (set! failed-lst (cons x failed-lst)))

;; First Order microKanren

(struct disj     (g1 g2)       #:prefab)
(struct conj     (g1 g2)       #:prefab)
(struct relate   (thunk stx)   #:prefab)
(struct prim     (name ts stx) #:prefab)
(struct bind     (s g)         #:prefab)
(struct mplus    (s1 s2)       #:prefab)
(struct pause    (st g)        #:prefab)
(struct pop      ()            #:prefab)

(define (mature? s) (or (not s) (pair? s)))
(define (mature s)
  (if (mature? s) s (mature (step s))))

(define (start st g)
  (match g
    ((disj g1 g2)
     (step (mplus (pause st g1)
                  (pause st g2))))
    ((conj g1 g2)
     (step (bind (pause st g1) g2)))
    ((relate thunk stx)
     (begin
       (pp-map-add-count! stx)
       (pause (extend-state-path/stack st stx) (conj (thunk) (pop)))))
    ((pop)
     (pp-map-add-successes! (car (state-stack st)))
     (state->stream (pop-state-stack st)))
    ((prim type ts stx)
     (let ((st (extend-state-path st stx)))
       (state->stream/log
        (match* (type ts)
          (('==          (list t1 t2)) (unify t1 t2 st))
          (('=/=         (list t1 t2)) (disunify t1 t2 st))
          (('symbolo     (list t))     (typify t symbol? st))
          (('stringo     (list t))     (typify t string? st))
          (('numbero     (list t))     (typify t number? st))
          (('not-symbolo (list t))     (distypify t symbol? st))
          (('not-stringo (list t))     (distypify t string? st))
          (('not-numbero (list t))     (distypify t number? st)))
        st (append (list type) ts))))))

(define (step s)
  (match s
    ((mplus s1 s2)
     (let ((s1 (if (mature? s1) s1 (step s1))))
       (cond ((not s1) s2)
             ((pair? s1)
              (cons (car s1)
                    (mplus s2 (cdr s1))))
             (else (mplus s2 s1)))))
    ((bind s g)
     (let ((s (if (mature? s) s (step s))))
       (cond ((not s) #f)
             ((pair? s)
              (step (mplus (pause (car s) g)
                           (bind (cdr s) g))))
             (else (bind s g)))))
    ((pause st g) (start st g))
    (_            s)))


;; Low-level goals

(define succeed (== #t #t))
(define fail    (== #f #t))
(define-syntax conj*
  (syntax-rules ()
    ((_)                succeed)
    ((_ g)              g)
    ((_ gs ... g-final) (conj (conj* gs ...) g-final))))

(define-syntax disj*
  (syntax-rules ()
    ((_)           fail)
    ((_ g)         g)
    ((_ g0 gs ...) (disj g0 (disj* gs ...)))))

;; Primitive goals

(define-syntax define-prim
  (syntax-rules ()
    ((_ name param ...)
     (define-syntax (name stx)
       (syntax-case stx ()
         ((_ param ...)
          #`(prim 'name (list param ...) #'#,stx)))))))

(define-prim ==          t1 t2)
(define-prim =/=         t1 t2)
(define-prim symbolo     t)
(define-prim stringo     t)
(define-prim numbero     t)
(define-prim not-symbolo t)
(define-prim not-stringo t)
(define-prim not-numebro t)

;; User-defined relations

(define-syntax define-relation
  (syntax-rules ()
    ((_ (name param ...) g ...)
     (begin
       (define (relation param ... stx)
         (relate (lambda () (fresh () g ...)) stx))
       (... (define-syntax (name stx)
              (syntax-case stx ()
                ((_ args ...)
                 #`(relation args ... #'#,stx)))))))))

;; High level goals

(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 gs ...)
     (let ((x (var/fresh 'x)) ...) (conj* g0 gs ...)))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g gs ...) (h hs ...) ...)
     (disj* (conj* g gs ...) (conj* h hs ...) ...))))

;; Queries

(define-syntax query
  (syntax-rules ()
    ((_ (x ...) g0 gs ...)
     (let ((goal (fresh (x ...) (== (list x ...) initial-var) g0 gs ...)))
       (pause empty-state goal)))))

(define-syntax query/fresh
  (syntax-rules ()
    ((_ (x ...) g0 gs ...)
     (let* ((x (var/fresh 'x)) ...
            (st (car (car (stream-take #f #f (pause empty-state (== (list x ...) initial-var))))))
            (st (empty-state-path st))
            (g (conj* g0 gs ...)))
       (reset-globals!)
       (pause st g)))))

(define (stream-take-helper n i s acc) ; n(solns) i(steps)
  (cond ((or (and n (= n 0)) (and i (= i 0)) (not s)) `(,(reverse acc) . ,s))
        ((not (mature? s)) (stream-take-helper n (and i (- i 1)) (step s) acc))
        (else (stream-take-helper (and n (- n 1)) i (cdr s) (cons (car s) acc)))))

(define (stream-take n i s)
  (stream-take-helper n i s '()))

(define (stream-take/format-out n m i j s)
  (set-failed-lst-size! m)
  (let*-values (((out cpu real gc) (time-apply stream-take (list n i s)))
                ((out)   (car out))
                ((solns) (car out))
                ((s)     (cdr out)))
    (set-paused-solns! (append paused-solns solns))
    (set-paused-stream! s)
    (if j
        (debug/json paused-solns pp-map failed-lst (list cpu real gc))
        (map reify/initial-var paused-solns))))

; n -- number of results to take (or #f for all)
; m -- number of failures to track (or #f for all)
; i -- number of steps to take (or #f for all)
; j -- #t to format output as JSON, #f otherwise
(define-syntax run
  (syntax-rules ()
    ((_ n m i j body ...) (stream-take/format-out n m i j (query/fresh body ...)))))

(define (resume n m i j)
  (stream-take/format-out n m i j paused-stream))

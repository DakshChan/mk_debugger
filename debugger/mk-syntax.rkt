(define-syntax define-relation
  (syntax-rules ()
    ((_ (name param ...) g ...)
     (begin
       (define (relation param ... stx)
         (relate (lambda () (fresh () g ...)) `() stx))
       (... (define-syntax (name stx)
              (syntax-case stx ()
                ((_ args ...)
                 #`(relation args ... #'#,stx)))))))))

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
(define-syntax (== stx)
  (syntax-case stx ()
    ((_ t1 t2)
     #`(prim '== (list t1 t2) #'#,stx))))

(define-syntax (=/= stx)
  (syntax-case stx ()
    ((_ t1 t2)
     #`(prim '=/= (list t1 t2) #'#,stx))))

(define-syntax (symbolo stx)
  (syntax-case stx ()
    ((_ t)
     #`(prim 'symbolo (list t) #'#,stx))))

(define-syntax (stringo stx)
  (syntax-case stx ()
    ((_ t)
     #`(prim 'stringo (list t) #'#,stx))))

(define-syntax (numbero stx)
  (syntax-case stx ()
    ((_ t)
     #`(prim 'numbero (list t) #'#,stx))))

(define-syntax (not-symbolo stx)
  (syntax-case stx ()
    ((_ t)
     #`(prim 'not-symbolo (list t) #'#,stx))))

(define-syntax (not-stringo stx)
  (syntax-case stx ()
    ((_ t)
     #`(prim 'not-stringo (list t) #'#,stx))))

(define-syntax (not-numbero stx)
  (syntax-case stx ()
    ((_ t)
     #`(prim 'not-numbero (list t) #'#,stx))))

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
     (begin
       (set-step-count #f)
       (let* ((x (var/fresh 'x)) ...
              (st (car (stream-take #f 0 (pause empty-state (== (list x ...) initial-var)))))
              (st (empty-state-path st))
              (g (conj* g0 gs ...)))
         (pp-map-reset!)
         (failed-lst-reset!)
         (pause st g))))))

(define (stream-take n m s)
  (if (eqv? 0 n)
      '()
      (let ((s (mature s m)))
        (if (pair? s) 
            (cons (car s) (stream-take (and n (- n 1)) m (cdr s)))
            '()))))

(define (stream-take/format-out n m i j s)
  (set-step-count i)
  (let ((out (stream-take n m s)))
    (if j
        (debug/json out pp-map failed-lst)
        (map reify/initial-var out))))

; n -- number of results to take (or #f for all)
; m -- number of failures to track (or #f for all)
; i -- number of steps to take (or #f for all)
; j -- #t to format output as JSON, #f otherwise
(define-syntax run
  (syntax-rules ()
    ((_ n m i j body ...) (stream-take/format-out n m i j (query/fresh body ...)))))

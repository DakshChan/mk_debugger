#lang racket
(provide
 (struct-out var)
 initial-var
 var/fresh
 (struct-out state)
 empty-state
 extend-state-path
 clear-state-path
 extend-state-path/stack
 pop-state-stack
 state->stream
 unify
 disunify
 typify
 distypify
 walk*
 reify
 reify/initial-var
 debug/json)

(require json)

;; Logic variables
(struct var (name index) #:prefab)
(define (var=? x1 x2)
  (= (var-index x1) (var-index x2)))
(define initial-var (var #f 0))
(define var/fresh
  (let ((index 0))
    (lambda (name) (set! index (+ 1 index))
      (var name index))))
;; States
(define empty-sub '())
(define empty-diseq '())
(define empty-types '())
(define empty-distypes '())
(define empty-path '())
(define empty-stack '())

(define (walk t sub)
  (let ((xt (and (var? t) (assf (lambda (x) (var=? t x)) sub))))
    (if xt (walk (cdr xt) sub) t)))

(define (occurs? x t sub)
  (cond ((pair? t) (or (occurs? x (walk (car t) sub) sub)
                       (occurs? x (walk (cdr t) sub) sub)))
        ((var? t)  (var=? x t))
        (else      #f)))

(define (var-type-ref t types)
  (let* ((xt (assf (lambda (x) (var=? t x)) types)))
    (and xt (cdr xt))))

(define (extend-sub x t sub)
  (and (not (occurs? x t sub)) `((,x . ,t) . ,sub)))

(define (extend-diseq =/=s diseq)
  (cons =/=s diseq))

(define (extend-types x t types)
  `((,x . ,t) . ,types))

(define (extend-distypes distype distypes)
  (cons distype distypes))

(define (extend-state-path st stx)
  (state (state-sub st) (state-diseq st) (state-types st) (state-distypes st) (cons stx (state-path st)) (state-stack st)))

(define (clear-state-path st)
  (state (state-sub st) (state-diseq st) (state-types st) (state-distypes st) empty-path (state-stack st)))

(define (extend-state-path/stack st stx)
  (extend-state-stack (extend-state-path st stx) stx))

(define (extend-state-stack st stx)
  (state (state-sub st) (state-diseq st) (state-types st) (state-distypes st) (state-path st) (cons stx (state-stack st))))

(define (pop-state-stack st)
  (state (state-sub st) (state-diseq st) (state-types st) (state-distypes st) (state-path st) (cdr (state-stack st))))

(define (var-type-remove t types)
  (remove t types (lambda (v type-constraint) (eq? v (car type-constraint)))))

(struct state (sub diseq types distypes path stack) #:prefab)
(define empty-state (state empty-sub empty-diseq empty-types empty-distypes empty-path empty-stack))

(define (state->stream state)
  (if state (cons state #f) #f))

;; Unification
(define (assign-var u v st)
  (let* ((types (state-types st))
         (u-type (var-type-ref u types))
         (types (if u-type (var-type-remove u types) types))
         (new-sub (extend-sub u v (state-sub st))))
    (and new-sub (let ((st (state new-sub (state-diseq st) types (state-distypes st) (state-path st) (state-stack st))))
                   (if u-type
                       (typify u u-type st)
                       (state-simplify st))))))

(define (unify u v st)
  (let* ((sub (state-sub st))
         (u (walk u sub))
         (v (walk v sub)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) st)
      ((var? u)                            (assign-var u v st))
      ((var? v)                            (assign-var v u st))
      ((and (pair? u) (pair? v))           (let ((st (unify (car u) (car v) st)))
                                             (and st (unify (cdr u) (cdr v) st))))
      (else                                (and (eqv? u v) st)))))

;; Type constraints
(define (typify u type? st)
  (let ((u (walk u (state-sub st))))
    (if (var? u)
        (let ((u-type (var-type-ref u (state-types st))))
          (if u-type
              (and (eqv? type? u-type) st)
              (state-simplify (state (state-sub st)
                                     (state-diseq st)
                                     (extend-types u type? (state-types st))
                                     (state-distypes st)
                                     (state-path st)
                                     (state-stack st)))))
        (and (type? u) st))))

;; Negation Constraints
(define (diff-prefix x newx acc)
  (if (eqv? x newx)
      (reverse acc)
      (diff-prefix x (cdr newx) (cons (car newx) acc))))

(define (extend-state/negated-diff newst st mode)
  (let* ((sub (state-sub st))
         (types (state-types st))
         (diseq (state-diseq st))
         (distypes (state-distypes st))
         (newsub (and newst (state-sub newst)))
         (newtypes (and newst (state-types newst)))
         (path (state-path st))
         (stack (state-stack st)))
    (cond
      ((not newsub) st)
      ((and (eq? mode 'sub) (not (eq? newsub sub)))
       (state sub
              (extend-diseq (diff-prefix sub newsub '()) diseq)
              types
              distypes
              path
              stack)) 
      ((and (eq? mode 'types) (not (eq? newtypes types)))
       (state sub
              diseq
              types
              (extend-distypes (car (diff-prefix types newtypes '())) distypes)
              path
              stack))
      (else #f))))

(define (state-simplify st)
  (let* ((st (and st (diseq-simplify st)))
         (st (and st (distype-simplify st))))
    st))

(define (diseq-simplify st)
  (let* ((sub (state-sub st))
         (diseq (state-diseq st))
         (types (state-types st))
         (distypes (state-distypes st))
         (path (state-path st))
         (stack (state-stack st))
         (st (state sub empty-diseq types distypes path stack)))
    (foldl/and (lambda (=/=s st) (disunify (map car =/=s) (map cdr =/=s) st)) st diseq)))

(define (distype-simplify st)
  (let* ((sub (state-sub st))
         (diseq (state-diseq st))
         (types (state-types st))
         (distypes (state-distypes st))
         (path (state-path st))
         (stack (state-stack st))
         (st (state sub diseq types empty-distypes path stack)))
    (foldl/and (lambda (not-type st) (distypify (car not-type) (cdr not-type) st)) st distypes)))

(define (foldl/and proc acc lst)
  (if (null? lst)
      acc
      (let ((new-acc (proc (car lst) acc)))
        (and new-acc (foldl/and proc new-acc (cdr lst))))))

;; Disunification
(define (disunify u v st)
  (extend-state/negated-diff (unify u v st) st 'sub))

;; Distypification
(define (distypify u type? st)
  (extend-state/negated-diff (typify u type? st) st 'types))

;; Reification
(struct Ans (term constraint) #:prefab)

(define (walk* tm st)
  (let* ((sub (state-sub st)) (tm (walk tm sub)))
    (if (pair? tm)
        `(,(walk* (car tm) st) .  ,(walk* (cdr tm) st))
        tm)))

(define (reified-index index)
  (string->symbol
   (string-append "_." (number->string index))))

;; stylizes output state
;; 1. substitutes variables with stylized index 
;; 2. simplifies state
;; 3. removes unused fresh variables
;; 4. stylizes rest of state
(define (reify tm st)
  (define index -1)
  (let ((results (let loop ((tm tm) (st st))
                   (define t (walk tm (state-sub st)))
                   (cond ((pair? t) (loop (cdr t) (loop (car t) st)))
                         ((var? t)  (set! index (+ 1 index))
                                    (state (extend-sub t (reified-index index) (state-sub st))
                                           (state-diseq st)
                                           (state-types st)
                                           (state-distypes st)
                                           (state-path st)
                                           (state-stack st)))
                         (else      st)))))
    (let* ((walked-sub (walk* tm results))
           (diseq (map (lambda (=/=) (cons (length =/=) =/=)) (state-diseq st)))
           (diseq (map cdr (sort diseq (lambda (x y) (< (car x) (car y))))))
           (st (state-simplify (state (state-sub st) diseq (state-types st) (state-distypes st) (state-path st) (state-stack st))))
           (diseq (walk* (state-diseq st) results))
           (diseq (map pretty-diseq (filter-not contains-fresh? diseq)))
           (diseq (map (lambda (=/=) (sort =/= term<?)) diseq))
           (diseq (if (null? diseq) '() (list (cons '=/= diseq))))
           (types (walk* (map pretty-types (state-types st)) results))
           (types (filter-not contains-fresh? types))
           (distypes (walk* (map pretty-distypes (state-distypes st)) results))
           (distypes (filter-not contains-fresh? distypes))
           (cxs (append types diseq distypes))
           (path (reverse (map (lambda (stx) (syntax->datum stx)) (state-path st)))))
      (if (null? cxs)
          (list walked-sub path)
          (list (Ans walked-sub (sort cxs term<?)) path)))))

(define (reify/initial-var st)
  (reify initial-var st))

(define (term<? u v)
  (eqv? (term-compare u v) -1))

;; Returns -1 if u < v, 0 if u = v, 1 if u > v
;; Used for stylization
(define (term-compare u v)
  (cond
    ((eqv? u v) 0)
    ((null? u) -1)
    ((null? v) 1)
    ((not u) -1)
    ((not v) 1)
    ((eqv? u #t) -1)
    ((eqv? v #t) 1)
    ((symbol? u) (if (symbol? v) (if (symbol<? u v) -1 1) -1))
    ((symbol? v) 1)
    ((number? u) (if (number? v) (if (< u v) -1 1) -1))
    ((number? v) 1)
    ((string? u) (if (string? v) (if (string<? u v) -1 (if (string=? u v) 0 1)) -1))
    ((string? v) 1)
    ((pair? u) (if (pair? v) 
                   (let ((compared-cars (term-compare (car u) (car v))))
                     (if (eqv? compared-cars 0)
                         (term-compare (cdr u) (cdr v))
                         compared-cars))
                   -1))
    ((pair? v) 1)
    (else 1)))

(define (contains-fresh? x)
  (if (pair? x)
      (or (contains-fresh? (car x)) (contains-fresh? (cdr x)))
      (var? x)))

(define (pretty-diseq =/=s) 
  (map (lambda (=/=) (let ((x (car =/=)) (y (cdr =/=)))
                       (if (term<? x y) (list x y) (list y x))))
       =/=s))

(define (pretty-types constraint) (list (type-check->sym (cdr constraint)) (car constraint)))

(define (type-check->sym pred)
  (cond
    ((eq? pred symbol?) 'sym)
    ((eq? pred string?) 'str)
    ((eq? pred number?) 'num)
    (error "Invalid type")))

(define (pretty-distypes constraint) (list (distype-check->sym (cdr constraint)) (car constraint)))

(define (distype-check->sym pred)
  (cond
    ((eq? pred symbol?) 'not-sym)
    ((eq? pred string?) 'not-str)
    ((eq? pred number?) 'not-num)
    (error "Invalid type")))

(define (debug/json sts pp-map failed-lst)
  (write-json (debug/jsexpr sts pp-map failed-lst)))

(define (debug/jsexpr sts pp-map failed-lst)
  (let ((solutions (map state/jsexpr sts))
        (rejected-states (map state/jsexpr failed-lst))
        (program-points (program-points/jsexpr pp-map)))
    (hash 'solutions solutions
          'rejected-states rejected-states
          'program-points program-points)))

(define (state/jsexpr st)
  (let ((sub      (map sub/jsexpr     (state-sub st)))
        (diseq    (map diseq/jsexpr   (state-diseq st)))
        (types    (map type/jsexpr    (state-types st)))
        (distypes (map distype/jsexpr (state-distypes st)))
        (path     (map syntax/jsexpr  (state-path st)))
        (stack    (map syntax/jsexpr  (state-stack st))))
    (hash 'sub      sub
          'diseq    diseq
          'types    types
          'distypes distypes
          'path     path
          'stack    stack)))

(define (program-points/jsexpr pp-map)
  (map (lambda (key) (let ((syntax (syntax/jsexpr key))
                           (count  (hash-ref pp-map key)))
                       (hash 'syntax syntax
                             'count  count)))
       (hash-keys pp-map)))

(define (syntax/jsexpr syntax)
  (let* ((source   (syntax-source syntax))
         (source   (cond
                     ((symbol? source) (symbol->string source))
                     ((path?   source) (path->string   source))
                     ((string? source) source)))
         (line     (syntax-line syntax))
         (column   (syntax-column syntax))
         (position (syntax-position syntax))
         (content  (~s (syntax->datum syntax))))
    (hash 'source   source
          'line     line
          'column   column
          'position position
          'content  content)))

(define (sub/jsexpr sub)
  (let ((t1 (term/jsexpr (car sub)))
        (t2 (term/jsexpr (cdr sub))))
    (hash 't1 t1
          't2 t2)))

(define (diseq/jsexpr diseq)
  (let ((t1 (term/jsexpr (car diseq)))
        (t2 (term/jsexpr (cdr diseq))))
    (hash 't1 t1
          't2 t2)))

(define (type/jsexpr type)
  (let ((var  (lvar/jsexpr (car type)))
        (type (cond
                ((eqv? (cdr type) number?) "num")
                ((eqv? (cdr type) symbol?) "sym")
                ((eqv? (cdr type) string?) "str"))))
    (hash 'var  var
          'type type)))

(define (distype/jsexpr distype)
  (let ((var  (lvar/jsexpr (car distype)))
        (distype (cond
                   ((eqv? (cdr distype) number?) "num")
                   ((eqv? (cdr distype) symbol?) "sym")
                   ((eqv? (cdr distype) string?) "str"))))
    (hash 'var     var
          'distype distype)))

(define (term/jsexpr term)
  (let* ((type  (cond
                  ((number? term) "num")
                  ((string? term) "str")
                  ((symbol? term) "sym")
                  ((var?    term) "var")
                  ((list?   term) "lst")
                  ((pair?   term) "lst")))
         (value  (cond
                   ((eqv? type "num") term)
                   ((eqv? type "str") term)
                   ((eqv? type "sym") (symbol->string term))
                   ((eqv? type "var") (lvar/jsexpr term))
                   ((eqv? type "lst") (if (list? term)
                                          (map term/jsexpr term)
                                          (list (term/jsexpr (car term)) (term/jsexpr (cdr term))))))))
    (hash 'type  type
          'value value)))

(define (lvar/jsexpr var)
  (let* ((var-name (var-name var))
         (name (if var-name
                   (symbol->string var-name)
                   #f)))
    (hash 'name name)))

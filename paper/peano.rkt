(define ZERO '())

(define (successor n)
  (cons 's n))

(define (peano-num i)
  (if (= i 0)
      ZERO
      (successor (peano-num (- i 1)))))

(define-relation (successoro n1 n2)
  (== n2 (successor n1)))

(define-relation (pluso n1 n2 out)
  (conde
   ((== n1 ZERO)
    (== n2 out))
   ((fresh (n1- out-)
           (successoro n1- n1)
           (successoro out- out)
           (pluso n2 n1- out-)))))

(define-relation (<=o n1 n2)
  (fresh (m)
         (pluso n1 m n2)))

; selection sort

(define-relation (same-lengtho lst1 lst2)
  (conde
   ((== lst1 '()) (== lst2 '()))
   ((fresh (fst1 rst1 fst2 rst2)
           (== lst1 (cons fst1 rst1))
           (== lst2 (cons fst2 rst2))
           (same-lengtho rst1 rst2)))))

(define-relation (remove-firsto lst x out)
  (conde
   ((== lst (cons x out)))
   ((fresh (lst-fst lst-rst out-fst out-rst)
           (== lst (cons lst-fst lst-rst))
           (== out (cons out-fst out-rst))
           (=/= lst-fst x)
           (== out-fst lst-fst)
           (remove-firsto lst-rst x out-rst)))))

(define-relation (<=firsto x lst)
  (conde
   ((== lst '()))
   ((fresh (lst-fst lst-rst)
           (== lst (cons lst-fst lst-rst))
           (<=o x lst-fst)))))

(define-relation (selection-sorto lst out)
  (conde
   ((== lst '()) (== out '()))
   ((fresh (out-fst out-rst sublst)
           (== out (cons out-fst out-rst))
           ;(same-lengtho lst out)
           (remove-firsto lst out-fst sublst)
           (<=firsto out-fst out-rst)
           (selection-sorto sublst out-rst)))))

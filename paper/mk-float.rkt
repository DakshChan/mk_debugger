; this code was taken from
; https://github.com/malaikazaidi/relational-float

(define-relation (appendo xs ys xsys)
  (conde ((== xs '()) (== ys xsys))
         ((fresh (x zs zsys)
            (== `(,x . ,zs)   xs)
            (== `(,x . ,zsys) xsys)
            (appendo zs ys zsys)))))

;; Relational arithmetic
(define (build-num n)
  (cond
    ((odd? n)
     (cons 1
           (build-num (quotient (- n 1) 2))))
    ((and (not (zero? n)) (even? n))
     (cons 0
           (build-num (quotient n 2))))
    ((zero? n) '())))

(define-relation (zeroo n)
  (== '() n))

(define-relation (poso n)
  (fresh (a d)
    (== `(,a . ,d) n)))

(define-relation (>1o n)
  (fresh (a ad dd)
    (== `(,a ,ad . ,dd) n)))

(define-relation (full-addero b x y r c)
  (conde
    ((== 0 b) (== 0 x) (== 0 y) (== 0 r) (== 0 c))
    ((== 1 b) (== 0 x) (== 0 y) (== 1 r) (== 0 c))
    ((== 0 b) (== 1 x) (== 0 y) (== 1 r) (== 0 c))
    ((== 1 b) (== 1 x) (== 0 y) (== 0 r) (== 1 c))
    ((== 0 b) (== 0 x) (== 1 y) (== 1 r) (== 0 c))
    ((== 1 b) (== 0 x) (== 1 y) (== 0 r) (== 1 c))
    ((== 0 b) (== 1 x) (== 1 y) (== 0 r) (== 1 c))
    ((== 1 b) (== 1 x) (== 1 y) (== 1 r) (== 1 c))))

(define-relation (addero d n m r)
  (conde
    ((== 0 d) (== '() m) (== n r))
    ((== 0 d) (== '() n) (== m r)
              (poso m))
    ((== 1 d) (== '() m)
              (addero 0 n '(1) r))
    ((== 1 d) (== '() n) (poso m)
              (addero 0 '(1) m r))
    ((== '(1) n) (== '(1) m)
                 (fresh (a c)
                   (== `(,a ,c) r)
                   (full-addero d 1 1 a c)))
    ((== '(1) n) (gen-addero d n m r))
    ((== '(1) m) (>1o n) (>1o r)
                 (addero d '(1) n r))
    ((>1o n) (gen-addero d n m r))))

(define-relation (gen-addero d n m r)
  (fresh (a b c e x y z)
    (== `(,a . ,x) n)
    (== `(,b . ,y) m) (poso y)
    (== `(,c . ,z) r) (poso z)
    (addero e x y z)
    (full-addero d a b c e)
    ))

(define-relation (pluso n m k)
  (addero 0 n m k))

(define-relation (=lo n m)
  (conde
    ((== '() n) (== '() m))
    ((== '(1) n) (== '(1) m))
    ((fresh (a x b y)
       (== `(,a . ,x) n) (poso x)
       (== `(,b . ,y) m) (poso y)
       (=lo x y)))))

(define-relation (<lo n m)
  (conde
    ((== '() n) (poso m))
    ((== '(1) n) (>1o m))
    ((fresh (a x b y)
       (== `(,a . ,x) n) (poso x)
       (== `(,b . ,y) m) (poso y)
       (<lo x y)))))

(define-relation (<o n m)
  (conde
    ((<lo n m))
    ((=lo n m)
     (fresh (x)
       (poso x)
       (pluso n x m)))))

(define-relation (<=o n m)
  (conde
    ((== n m))
    ((<o n m))))

; Constants used for the entire system
(define PRECISION 16) ; CHANGE THIS TO CHANGE THE PRECISION OF THE RELATION SYSTEM

(define ZERO-MANTISSA (make-list PRECISION 0))
(define FULL-EXP '(1 1 1 1  1 1 1 1))

; =====================
; | Reifier Base Code |
; =====================

; Constant for reporting a reification error
(define FP-ERR 'FP-ERR)

; Constants for floats that aren't numbers
(define POS-INFINITY 'positive-infinity)
(define NEG-INFINITY 'negative-infinity)
(define NaN 'NaN)

; Numerical constants used for reification.
(define EXPONENT-SHIFT 127) ; The shifting factor for the exponenet field.
(define SMALLEST-PRECISION-EXP 149) ; The smallest representable exponent for the LSB.

; ==============================================
; | MiniKanren Relational Arithmetic Base Code |
; ==============================================

(define BIAS (build-num 127))
(define UNIT-MANTISSA (append (make-list (- PRECISION 1) 0) '(1)))

; -----------------------
; | Basic bit relations |
; -----------------------

#|
(noto a not-a)
    a: 0 or 1
    not-a: The result of ~a.
    Negates the bit a, i.e negation relation.
|#  
(define-relation (noto a not-a)
    (conde ((== a 1) (== not-a 0))
           ((== a 0) (== not-a 1))))


; ------------------------------------------
; | Length checking/manipulating relations |
; ------------------------------------------


#|
(expo-lengtho expo)
    expo: A Oleg number.

    Ensures that expo contains no more than 8 bits.
|#
(define-relation (expo-lengtho expo) 
    (fresh (r b b0 b1 b2 b3 b4 b5 b6 b7)
        (== b (list b0 b1 b2 b3 b4 b5 b6 b7)); b is a list of 8 bits.
        (appendo expo r b))) ; ensure that expo (++) r = b

#|
(mantissa-lengtho mantissa)
    mantissa: A Oleg number.

    Ensures that the mantissa contains exactly 16 bits.
|#
(define-relation (mantissa-lengtho mantissa)
    (fresh (b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15)
        (== mantissa
            (take (list b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) PRECISION))))

#|
(drop-leastsig-bito mantissa rmantissa)
    mantissa: The mantissa being created that may have more than 24 bits.
    rmantissa: The MKFP mantissa that takes the 24 most significant bits from mantissa.

Drops least significant bit in the mantissa, where cap is 24 bits.
|#
(define-relation (drop-leastsig-bito mantissa rmantissa bit)
    (fresh ()
        (mantissa-lengtho rmantissa)
        (appendo bit rmantissa mantissa)))


; ----------------------------------
; | Value Representation relations |
; ----------------------------------


#|
(fp-overflowo expo mant rmant)
    expo: A MKFP oleg exponent
    mant: A MKFP mantissa
    rmant: A MKFP mantissa

    If expo is '(1 1 1 1  1 1 1 1), equate rmant to '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 1)
    otherwise mant == rmant
|#
(define-relation (fp-overflowo expo mant rexpo rmant)
    (conde 
        ((<o expo FULL-EXP) (=/= expo '()) (== expo rexpo) (== mant rmant))
        ((<=o FULL-EXP expo)
         (== rexpo FULL-EXP) (== rmant UNIT-MANTISSA))))

#|
(fp-zeroo sign expo mantissa)
    sign: The sign bit of a MKFP number.
    expo: The exponent of a MKFP number.
    mantissa: The mantissa of a MKFP number.

Ensures that the floating point number given by the arguments -- sign, expo, and mantissa -- represent
zero.
|#
(define-relation (fp-zeroo sign expo mantissa)
    (fresh ()
        (conde 
            ((== sign 0))
            ((== sign 1)))
        (== expo '())
        (== mantissa ZERO-MANTISSA)))

#|
(fp-nonzeroo  sign expo mantissa)
    sign: The sign bit of a MKFP number.
    expo: The exponent of a MKFP number.
    mantissa: The mantissa of a MKFP number.

Ensures that the floating point number given by the arguments -- sign, expo, and mantissa -- represent
a positive or negative quantity.
|#
(define-relation (fp-nonzeroo sign expo mantissa)
    (fresh ()
        (conde 
            ((== sign 0))
            ((== sign 1)))
        (=/= expo '())
        (=/= mantissa ZERO-MANTISSA)))

#|
(fp-finiteo sign expo mantissa)
    sign: The sign bit of a MKFP number.
    expo: The exponent of a MKFP number.
    mantissa: The mantissa of a MKFP number.

Checks if the floating point number given by the arguments -- sign, expo, and mantissa --
do not represent an infinity/NaN (i.e a special value).
|#
(define-relation (fp-finiteo sign expo mantissa)
    (fresh ()
        (conde 
            ((== sign 0))
            ((== sign 1)))
        (=/= expo FULL-EXP)))

#|
(fp-infiniteo sign expo mantissa)
    sign: The sign bit of a MKFP number.
    expo: The exponent of a MKFP number.
    mantissa: The mantissa of a MKFP number.

Checks if the floating point number given by the arguments -- sign, expo, and mantissa --
represent an infinity.
|#
(define-relation (fp-infiniteo sign expo mantissa)
    (fresh ()
        (conde 
            ((== sign 0))
            ((== sign 1)))
        (== expo FULL-EXP)
        (== mantissa UNIT-MANTISSA)))

;; data structure decomposition relations

#|
Decomposes fp number into sign, exponent, and mantissa
|#
(define-relation (fp-decompo fp sign expo mantissa)
    (fresh (mantissa-head)
        (== fp (list sign expo mantissa))
        (mantissa-lengtho mantissa)
        (conde 
            ((=/= expo '()) (=/= expo FULL-EXP)
             (appendo mantissa-head (list 1) mantissa)) ; These numbers are non-zero but finite.

            ((== expo FULL-EXP) (== mantissa UNIT-MANTISSA)) ; Only allow infinities
            ((== expo '())  (== mantissa ZERO-MANTISSA))))) ; Only allow normalized numbers + 0.


; ---------------------------
; | Base Addition relations |
; ---------------------------


#|
(mantissa-shifto mantissa n result)
    mantissa: The mantissa of a MKFP number. 
    n: the number of least-significant bits from mantissa to remove.
    result: The result of removing n of the least significant bits from mantissa

    Removes n of the least significant bits from mantissa and equates that to result.
|#
(define-relation (mantissa-shifto mantissa n result)
    (fresh (template remain b0 b1 b2 b3)
        (== template (list b0 b1 b2 b3))

        (conde 
            ((appendo n remain template) ;captures when length n <= 4
             (shifto-helper mantissa n result 1))
            
            ((=/= remain '()); Remainder of list cannot be empty
             (appendo template remain n); Together with above captures length n > 4 -> n >= 16
             (== result '())))))

#|
(advance-bit#o bit next-bit)
    curr-bit: The current bit of an oleg number we are iterating over.
    next-bit: The next bit of an oleg number we are iterating over.
    (== nextbit (+ 1 bit))
|#
(define-relation (advance-bit#o bit next-bit)
    (fresh () 
        (conde 
            ((== bit 1) (== next-bit 2))
            ((== bit 2) (== next-bit 3))
            ((== bit 3) (== next-bit 4))
            ((== bit 4) (== next-bit 5)))))

#|
(shifto-helper mantissa n result curr-bit)
    mantissa: The mantissa of a MKFP number. 
    n: the number of least-significant bits from mantissa to remove.
    result: The result of removing n of the least significant bits from mantissa
    curr-bit: The current bit of n that we are iterating on.

    Removes n of the least significant bits from mantissa and equates that to result.
|#
(define-relation (shifto-helper mantissa n result curr-bit)
  (conde 
        ((== n '())
         (== mantissa result))

        ((fresh (n-first n-rest next-bit next-n next-mantissa)
         (== n (cons n-first n-rest))
         (conde 
            ((== n-first 0) (== next-mantissa mantissa) (== next-n n-rest))

            ((== n-first 1)
                (conde
                ((== curr-bit 1) ; removing 1 binary digit
                    (fresh (remain b0) 
                        (conde 
                            ((== mantissa (cons b0 remain)) ; Captures when (length mantissa) >= 1
                                (== next-mantissa remain)
                                (== next-n n-rest))
                                                            
                            ((== mantissa '()) 
                                (== next-mantissa '())
                                (== next-n '())))))
                        
                ((== curr-bit 2) ; remove 2 binary digits
                    (fresh (template remain b0 b1)
                        (== template (list b0 b1))
                        (conde 
                            ((appendo template remain mantissa) ; Captures when (length mantissa) >= 2
                                (== next-mantissa remain)
                                (== next-n n-rest))

                            ((=/= remain '())
                                (appendo mantissa remain template) ; Captures when (length mantissa) < 2 
                                (== next-mantissa '())
                                (== next-n '())))))
                
                ((== curr-bit 3) ; remove 4 binary digits
                    (fresh (template remain b0 b1 b2 b3) 
                        (== template (list b0 b1 b2 b3))
                        (conde
                            ((appendo template remain mantissa) ; Captures when (length mantissa) >= 4
                                (== next-mantissa remain)
                                (== next-n n-rest)) 
                                
                            ((=/= remain '())
                                (appendo mantissa remain template) ; Captures when (length mantissa) < 4
                                (== next-mantissa '())
                                (== next-n '())))))
                
                ((== curr-bit 4) ; remove 8 binary digits
                    (fresh (template remain b0 b1 b2 b3 b4 b5 b6 b7)
                        (== template (list b0 b1 b2 b3 b4 b5 b6 b7)) 
                        (conde 
                            ((appendo template remain mantissa) ; Captures when (length mantissa) >= 4
                                (== next-mantissa remain)
                                (== next-n n-rest))

                            ((=/= remain '())
                                (appendo mantissa remain template) ; Captures when (length mantissa) < 4
                                (== next-mantissa '())
                                (== next-n '()))))))))
        
            (advance-bit#o curr-bit next-bit)
            
            (shifto-helper next-mantissa next-n result next-bit) ))))


#|
(fp-swapo sign1 expo1 mant1 sign2 expo2 mant2 rsign rexpo rmant)
    sign1: The sign of the MKFP number with the smaller exponent
    expo1: The exponent of the MKFP number with the smaller exponent
    mant1: The mantissa of the MKFP number with the smaller exponent
    sign2: The sign of the MKFP number with the larger exponent
    expo2: The exponent of the MKFP number with the larger exponent
    mant2: The mantissa of the MKFP number with the larger exponent
    rsign: The resulting sign of the addition f1 + f2
    rexpo: The resulting exponent of the addition f1 + f2
    rmant: The resulting mantissa of the addition f1 + f2

    Calls fp-samesignaddero so that the number out of f1 and f2 with the
    smaller exponent is entered first.
|#
(define-relation (fp-swapo expo1 mant1 expo2 mant2 rexpo rmant)
    (fresh (expo-diff)  
        (conde
            ((pluso expo-diff expo1 expo2) ; Exponent 2 >= Expoenent 1
             (fp-samesignaddero mant1 expo2 mant2 expo-diff rexpo rmant))
            
            ((=/= expo1 expo2)
             (pluso expo-diff expo2 expo1); Exponent 2 < Exponent 1
             (fp-samesignaddero mant2 expo1 mant1 expo-diff rexpo rmant)))))

#|
(fp-samesignaddero mant1 sign2 expo2 mant2 expo-diff rsign rexpo rmant)
    sign1: The sign of the MKFP number with the smaller exponent
    expo1: The exponent of the MKFP number with the smaller exponent
    mant1: The mantissa of the MKFP number with the smaller exponent
    sign2: The sign of the MKFP number with the larger exponent
    expo2: The exponent of the MKFP number with the larger exponent
    mant2: The mantissa of the MKFP number with the larger exponent
    expo-diff: An Oleg number that equals (expo2 - expo1)
    rsign: The resulting sign of the addition f1 + f2
    rexpo: The resulting exponent of the addition f1 + f2
    rmant: The resulting mantissa of the addition f1 + f2

    Floating-Point Addition for same signs
|#
(define-relation (fp-samesignaddero mant1 expo2 mant2 expo-diff rexpo rmant)
    (fresh (shifted-mant1 mant-sum pre-rmant bit pre-rexpo)
        ; Shift the mantissa of the SMALLER exponent
        (mantissa-shifto mant1 expo-diff shifted-mant1)        

        ; oleg number addition
        (pluso shifted-mant1 mant2 mant-sum)
        
        ; Rounding by chopping
        (drop-leastsig-bito mant-sum pre-rmant bit)

        ; Add 1 to the exponent as necessary
        (conde ((== bit '()) (== pre-rexpo expo2))
               ((=/= bit '()) (pluso '(1) expo2 pre-rexpo)))

        ; Check for overflow
        (fp-overflowo pre-rexpo pre-rmant rexpo rmant)
))
           

#|
(fp-specialpluso sign1 expo1 mant1 sign2 expo2 mant2 rsign rexpo rmant)
    sign1: The sign of the MKFP number
    expo1: The exponent of the MKFP number
    mant1: The mantissa of the MKFP number
    sign2: The sign of the MKFP number
    expo2: The exponent of the MKFP number 
    mant2: The mantissa of the MKFP number
    rsign: The resulting sign of the addition f1 + f2
    rexpo: The resulting exponent of the addition f1 + f2
    rmant: The resulting mantissa of the addition f1 + f2

    Performs the special case additions. i.e. addition with 0 and infinity.
|#
(define-relation (fp-specialpluso sign1 expo1 mant1 sign2 expo2 mant2 rsign rexpo rmant)
    (conde
        ; 0 + 0 = 0
        ((fp-zeroo sign1 expo1 mant1)
         (fp-zeroo sign2 expo2 mant2)
         (fp-zeroo rsign rexpo rmant))

        ; x + 0 = x (x \in R)
        ((fp-nonzeroo sign1 expo1 mant1) (fp-finiteo sign1 expo1 mant1)
         (fp-zeroo sign2 expo2 mant2)
         (== rsign sign1) (== rexpo expo1) (== rmant mant1))

        ; 0 + y = y (y \in R)
        ((fp-nonzeroo sign2 expo2 mant2) (fp-finiteo sign2 expo2 mant2)
         (fp-zeroo sign1 expo1 mant1)
         (== rsign sign2) (== rexpo expo2) (== rmant mant2))

        ((noto sign1 sign2) ; (x) + (-x) = 0 (x \in R)
         (fp-nonzeroo sign1 expo1 mant1) (fp-finiteo sign1 expo1 mant1)
         (fp-nonzeroo sign2 expo2 mant2) (fp-finiteo sign2 expo2 mant2)
         (== expo1 expo2) (== mant1 mant2)
         (fp-zeroo rsign rexpo rmant))

        ((== sign1 sign2)  (== rsign sign2);  (+/- \inf) + (+/- \inf) = (+/- \inf)
         (fp-infiniteo sign1 expo1 mant1)
         (fp-infiniteo sign2 expo2 mant2)
         (fp-infiniteo rsign rexpo rmant))

        ((== sign2 rsign);   c + (+/- \inf) = (+/- \inf) (c \in R)
         (fp-finiteo sign1 expo1 mant1)
         (fp-infiniteo sign2 expo2 mant2)
         (fp-infiniteo rsign rexpo rmant))

        ((== sign1 rsign);   (+/- \inf) + c = (+/- \inf) (c \in R)
         (fp-infiniteo sign1 expo1 mant1)
         (fp-finiteo sign2 expo2 mant2)
         (fp-infiniteo rsign rexpo rmant))))

; ==================================================
; | MiniKanren Floating Point Arithmetic Relations |
; ==================================================

#|
(fp-pluso f1 f2 r)
    f1: A MKFP number.
    f2: A MKFP number.
    r: A MKFP number that satisfies f1 + f2 = r (under the rules of floating point addition)

    General Floating-Point Addition
|#
(define-relation (fp-pluso f1 f2 r)
    (fresh (sign1 expo1 mant1 sign2 expo2 mant2 rsign rexpo rmant)
        (fp-decompo f1 sign1 expo1 mant1)
        (fp-decompo f2 sign2 expo2 mant2)
        (fp-decompo r rsign rexpo rmant)
        (conde
            ((fp-specialpluso sign1 expo1 mant1 sign2 expo2 mant2 rsign rexpo rmant))
            ;(+/- x) + (+/- y) = (+/- z) (x, y \in R) (z \in R \cup {+/- \inf})
            ((fp-nonzeroo sign1 expo1 mant1) (fp-finiteo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2) (fp-finiteo sign2 expo2 mant2)
             (fp-nonzeroo rsign rexpo rmant)

             (conde 
                ((== sign1 sign2) (== sign2 rsign)
                 (fp-swapo expo1 mant1 expo2 mant2 rexpo rmant)) 
                
                ; Approach when signs are opposite
                ; When r has the same sign as f1 (+)
                ; f1+(-f2) = r -> f1 = r + f2
                ; fp-pluso (-f2, r, f1)
                ; When r has the same sign as f2 (-)
                ; f1 + (-f2) = -r -> -f2 = -r + (-f1)
                ; fp-pluso (r, -f1, f2)
                ((noto sign1 sign2) (== sign1 rsign)
                 (fp-swapo expo2 mant2 rexpo rmant expo1 mant1)) 
                
                ((noto sign1 sign2) (== sign2 rsign)
                 (fp-swapo expo1 mant1 rexpo rmant expo2 mant2)))))
             
        (expo-lengtho expo1)
        (expo-lengtho expo2)
        (expo-lengtho rexpo)))    

(define one `(0 ,(build-num 127) ,(append (make-list 15 0) '(1))))
(define pi '(0 (0 0 0 0 0 0 0 1) (1 0 1 0 1 1 1 1 0 0 0 1 0 0 1 1)))

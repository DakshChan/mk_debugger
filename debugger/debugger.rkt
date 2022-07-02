#lang racket

(require "mk-fo.rkt")
(require "microk-fo.rkt")


(define (stream->choices s)
  (match s
    ((pause st g)              (stream->choices (start-shallow st g)))  ; resume computation and continue searching for choices
    ((mplus s1 s2)             (list s1 s2))                            ; s1 s2 are each a choice
    ((bind s g)                (foldl (lambda (c acc) (if (state? c)    ; search for choices in s, then for each choice:
                                                          (append (stream->choices (pause c g)) acc) ; if it is a state (result), apply g directly
                                                          (cons (bind c g) acc)))                    ; if it is a stream, bind g to the stream
                                      '()
                                      (stream->choices s)))             
    (`(,st . ,s)               (cons st (stream->choices s)))           ; st is a choice, continue searching for choices in s
    (#f                        '())))

(define (explore/stream display qvars s)
  (let loop ((s (stream->choices s)) (undo '()))
    (define previous-choice
      (and (pair? undo)
           (let* ((i.s (car undo)) (i (car i.s)) (s (cdr i.s)))
             (list-ref (filter (lambda (x) (not (state? x))) s) (- i 1))))) ; not sure if all results are guaranteed to appear before choices
    (define results (filter state? s))                                      ; so takef and dropf have been temporarily replaced
    (define choices (filter (lambda (x) (not (state? x))) s))               ; with `filter` on these three lines
    (define i (display `(("prev" ,previous-choice)
                         ("results" ,results)
                         ("choices" ,choices)
                         ("num-choices" ,(length choices)))))
    (cond ((eof-object? i) (newline))
          ((and (or (eq? i 'u) (eq? i 'undo)) (pair? undo))
           (loop (cdar undo) (cdr undo)))
          ((and (integer? i) (<= 1 i) (<= i (length choices)))
           (loop (stream->choices (list-ref choices (- i 1)))
                 (cons (cons i s) undo)))
          (else (loop s undo)))
    ))

(define-syntax explore
  (syntax-rules (query)
    ((_ display (query (qvars ...) body ...))
     (begin (printf "Using display procedure: ~s\nExploring query:\n~s\n"
                    'display '(query (qvars ...) body ...))
            (explore/stream display '(qvars ...) (query/fresh (qvars ...) body ...))))))

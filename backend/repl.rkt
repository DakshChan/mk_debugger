#lang racket

(define-namespace-anchor replAnchor)
(define replNamespace (namespace-anchor->namespace replAnchor))

(require json)
(require (file "C:\\Users\\daksh\\WebstormProjects\\mk_debugger\\debugger\\mk-fo.rkt"))
(include (file "C:\\Users\\daksh\\Downloads\\proofo.rkt"))
; potentially other includes possible here for multi file debugging

(define (input-loop)
  (define command (read-line (current-input-port) 'any))
  (define json-command (string->jsexpr command))
  (cond [(not (hash-has-key? json-command 'command)) (exit 1)]
        [(string=? (hash-ref json-command 'command) "run")
         (let ((solutions (hash-ref json-command 'solutions))
               (steps (hash-ref json-command 'steps))
               (samples (hash-ref json-command 'samples))
               (query (hash-ref json-command 'query)))

              (eval (read (open-input-string (string-append "(run " solutions " " samples " " steps " #t " query")"))) replNamespace)
              (flush-output))
         (input-loop)]
        [(string=? (hash-ref json-command 'command) "resume")
         (let ((solutions (hash-ref json-command 'solutions))
               (steps (hash-ref json-command 'steps))
               (samples (hash-ref json-command 'samples)))

              (eval (read (open-input-string (string-append "(resume " solutions " " samples " " steps " #t)"))) replNamespace)
              (flush-output))
         (input-loop)]
        [(string=? (hash-ref json-command 'command) "exit") (exit 0)] ; not really necessary
        [else (exit 2)]))

(input-loop)

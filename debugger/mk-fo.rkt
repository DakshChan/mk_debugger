#lang racket
(provide
 
 define-prim
 define-relation
 ==
 =/=
 symbolo
 stringo
 numbero
 not-symbolo
 not-stringo
 not-numbero
 
 fresh
 conde
 query
 query/fresh
 run
 resume

 stream-take
 conj*
 disj*)

(require "microk-fo.rkt")
(include "mk-syntax.rkt")

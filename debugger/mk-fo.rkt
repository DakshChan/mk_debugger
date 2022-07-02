#lang racket
(provide
 ==
 =/=
 symbolo
 stringo
 numbero
 not-symbolo
 not-stringo
 not-numbero

 define-relation
 fresh
 conde
 query
 query/fresh
 run
 run*
 run*/debug
 run*/debug*

 stream-take
 conj*
 disj*)

(require "microk-fo.rkt")
(include "mk-syntax.rkt")

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
 run/json
 run*/json
 run*/debug/json
 run*/debug*/json

 stream-take
 conj*
 disj*)

(require "microk-fo.rkt")
(include "mk-syntax.rkt")

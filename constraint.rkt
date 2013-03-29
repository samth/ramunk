#lang racket/base

(require racket/class
         
         "chipmunk.rkt"
         "vector.rkt")

(provide constraint%)

(define constraint%
  (class object%
    
    (super-new)))
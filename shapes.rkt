#lang racket/base

(require racket/class
         
         "chipmunk.rkt"
         "vector.rkt"
         "body.rkt")

(provide circle-shape%
         poly-shape%
         segment-shape%
         box-shape%)

(define shape%
  (class object%
    
    (init-field cstruct)
    (init [elasticity 0.0])
    
    (set-elasticity elasticity)
    
    (super-new)
    
    (define/public-final (get-elasticity)
      (cp-shape-get-elasticity cstruct))
    
    (define/public-final (get-body)
      (cp-shape-get-body cstruct))
    
    (define/public-final (set-elasticity amount)
      (cp-shape-set-elasticity cstruct amount))
    
    (define/public-final (set-body body)
      (cp-shape-set-body cstruct (get-field cstruct body)))
    
    (set-cp-shape-data! cstruct this)))

(define circle-shape%
  (class shape%
    (init body radius [offset (vector 0.0 0.0)])
    (super-new
     [cstruct
      (cp-circle-shape-new
       (get-field cstruct body)
       radius
       (vector->cpv offset))])))

(define segment-shape%
  (class shape%
    (init body start end [radius 0.0])
    (super-new
     [cstruct
      (cp-segment-shape-new
       (get-field cstruct body)
       (vector->cpv start)
       (vector->cpv end)
       radius)])))

(define poly-shape%
  (class shape%
    (init body points [offset (vector 0.0 0.0)])
    (super-new
     [cstruct
      (cp-poly-shape-new
       body
       (length points)
       (map vector->cpv points)
       (vector->cpv offset))])))

(define box-shape%
  (class poly-shape%
    (init body width height)
    (super-new
     [cstruct
      (cp-box-shape-new body width height)])))

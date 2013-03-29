#lang racket/base

(require racket/class
         
         "chipmunk.rkt"
         "vector.rkt"
         "body.rkt"
         "shapes.rkt"
         "constraint.rkt")

(provide space%)

(define space%
  (class object%
    
    (init [gravity (vector 0.0 9.81)]
          [damping 1.0]
          [iterations 10]
          )
    
    (field [cstruct (cp-space-new)])
    
    (set-gravity gravity)
    (set-iterations iterations)
    (set-damping damping)
    
    (field
     [static-body
      (make-object body-wrapper%
        (cp-space-get-static-body cstruct))])
    
    (super-new)
    
    (define/public-final (destroy)
      (cp-space-destroy cstruct))
    
    (define/public-final (free)
      (cp-space-free cstruct))
    
    (define/public-final (locked?)
      (cp-space-is-locked cstruct))
    
    (define/public-final (set-default-collision-handler begin-proc pre-solve post-solve separate data)
      (cp-space-set-default-collision-handler cstruct begin-proc pre-solve post-solve separate data))
    
    (define/public-final (add-collision-handler collision-a collision-b begin-proc pre-solve post-solve separate data)
      (cp-space-add-collision-handler cstruct collision-a collision-b begin-proc pre-solve post-solve separate data))
    
    (define/public-final (remove-collision-handler collision-a collision-b)
      (cp-space-remove-collision-handler cstruct collision-a collision-b))
    
    (define/public-final (get-iterations)
      (cp-space-get-iterations cstruct))
    
    (define/public-final (get-gravity)
      (cp-space-get-gravity cstruct))
    
    (define/public-final (get-damping)
      (cp-space-get-damping cstruct))
    
    (define/public-final (set-gravity v)
      (cp-space-set-gravity cstruct (vector->cpv v)))
    
    (define/public-final (set-iterations amount)
      (cp-space-set-iterations cstruct amount))
    
    (define/public-final (set-damping amount)
      (cp-space-set-damping cstruct amount))
    
    (define/public-final (add-shape shape)
      (cp-space-add-shape cstruct (get-field cstruct shape))
      shape)
    
    (define/public-final (add-static-shape shape)
      (cp-space-add-static-shape cstruct (get-field cstruct shape))
      shape)
    
    (define/public-final (add-body body)
      (cp-space-add-body cstruct (get-field cstruct body))
      body)
    
    (define/public-final (add-constraint constraint)
      (cp-space-add-constraint cstruct (get-field cstruct constraint))
      constraint)
    
    (define/public-final (remove-shape shape)
      (cp-space-remove-shape cstruct (get-field cstruct shape))
      shape)
    
    (define/public-final (remove-static-shape shape)
      (cp-space-remove-static-shape cstruct (get-field cstruct shape))
      shape)
    
    (define/public-final (remove-body body)
      (cp-space-remove-body cstruct (get-field cstruct body))
      body)
    
    (define/public-final (remove-constraint constraint)
      (cp-space-remove-constraint cstruct (get-field cstruct constraint))
      constraint)
   
    (define/public-final (contains-shape? shape)
      (cp-space-contains-shape cstruct (get-field cstruct shape)))
    
    (define/public-final (contains-body? body)
      (cp-space-contains-body cstruct (get-field cstruct body)))
    
    (define/public-final (contains-constraint? constraint)
      (cp-space-contains-constraint cstruct (get-field cstruct constraint)))
    
    (define/public-final (add-post-step-callback callback key data)
      (cp-space-add-post-step-callback cstruct callback key data))
    
    (define/public-final (point-query point layers group callback data)
      (cp-space-point-query cstruct (vector->cpv point) layers group callback data))
    
    (define/public-final (point-query-first point layers group)
      (cp-space-point-query-first cstruct (vector->cpv point) layers group))
    
    (define/public-final (nearest-point-query start end layers group callback data)
      (cp-space-nearest-point-query start end layers group callback data))
    
    (define/public-final (nearest-point-query-nearest point max-distance layers group out)
      (cp-space-nearest-point-query-nearest cstruct (vector->cpv point) max-distance layers group out))
    
    (define/public-final (segment-query start end layers group callback data)
      (cp-space-segment-query cstruct (vector->cpv start) (vector->cpv end) layers group callback data))
    
    (define/public-final (segment-query-first start end layers group out)
      (cp-space-segment-query-first cstruct (vector->cpv start) (vector->cpv end) layers group out))
    
    (define/public-final (bb-query bb layers group callback data)
      (cp-space-bb-query cstruct bb layers group callback data))
    
    (define/public-final (activate-shapes-touching-shape shape)
      (cp-space-activate-shapes-touching-shape cstruct shape))
    
    (define/public-final (each-body callback data)
      (cp-space-each-body cstruct callback data))
    
    (define/public-final (each-shape callback data)
      (cp-space-each-shape cstruct callback data))
    
    (define/public-final (each-constraint callback data)
      (cp-space-each-constraint cstruct callback data))
    
    (define/public-final (reindex-static)
      (cp-space-reindex-static cstruct))
    
    (define/public-final (reindex-shape shape)
      (cp-space-reindex-shape cstruct (get-field cstruct shape)))
    
    (define/public-final (reindex-shapes-for-body body)
      (cp-space-reindex-shapes-for-body cstruct (get-field cstruct body)))
    
    (define/public-final (use-spatial-hash dimension count)
      (cp-space-use-spatial-hash cstruct dimension count))
    
    (define/public-final (step delta)
      (cp-space-step cstruct delta))
    
    (set-cp-space-data! cstruct this)))
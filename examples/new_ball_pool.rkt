#lang racket/base

(require "../main.rkt"
         2htdp/image
         2htdp/universe)

(define scene-width 800.0)
(define scene-height 600.0)
(define the-space (new space% [gravity (vector 0.0 6.0)]))
(define the-empty-scene (empty-scene scene-width scene-height))

;; Instellingen voor de physics

(define ground-edge
  (new segment-shape%
       [body (new body%)]
       [start (vector 0.0 scene-height)]
       [end (vector scene-width scene-height)]
       [elasticity 1.0]))

(define left-edge
  (new segment-shape%
       [body (new body%)]
       [start (vector 0.0 0.0)]
       [end (vector 0.0 scene-height)]
       [elasticity 1.0]))

(define right-edge
  (new segment-shape%
       [body (new body%)]
       [start (vector scene-width 0.0)]
       [end (vector scene-width scene-height)]
       [elasticity 1.0]))

(send the-space add-static-shape ground-edge)
(send the-space add-static-shape left-edge)
(send the-space add-static-shape right-edge)

(define-syntax send-all
  (syntax-rules ()
    [(_ objs-expr method-id arg ...)
     (for-each (lambda (obj)
                 (send obj method-id arg ...))
               objs-expr)]
    [(_ objs-expr method-if arg ... . arg-list-expr)
     (for-each (lambda (obj)
                 (send obj method-id arg ... . arg-list-expr))
               objs-expr)]))

(define (remove-each els lst [proc eq?])
  (if (null? els)
      lst
      (remove-each
       (cdr els)
       (remove (car els) lst)
       proc)))

(define bag%
  (class object%
    
    (define elements '())
    
    (super-new)
    
    (define/pubment (add . els)
      (set! elements (append els elements))
      (inner void add . els))
    
    (define/pubment (remove . els)
      (set! elements (remove-each els elements))
      (inner void add . els))
    
    (define/public-final (each proc)
      (for-each proc elements))
    
    (define/public-final (fold proc init)
      (foldl proc init elements))))

(define layer%
  (class bag%
    
    (inherit/super fold)
    
    (super-new)
    
    (define/augment-final (add . objects)
      (send-all objects enable))
    
    (define/augment-final (remove . objects)
      (send-all objects disable))
    
    (define/public-final (draw [scene the-empty-scene])
      (fold (lambda (ball scene)
              (send ball draw scene))
            scene))))

(define ball%
  (class object%
    
    (init-field position
                [space the-space]
                [velocity (vector 0.0 0.0)]
                [mass 1.0]
                [inertia 5.0]
                [radius 50.0]
                [appearance "solid"]
                [color "blue"])
    
    (field
     [body
      (new body%
           [mass mass]
           [inertia inertia]
           [position position]
           [velocity velocity])])
    
    (define shape
      (new circle-shape%
           [body body]
           [radius radius]
           [elasticity 0.8]))
    
    (define image (circle radius appearance color))
    
    (super-new)
    
    (define/public-final (enable)
      (send space add-shape shape)
      (send space add-body body))
    
    (define/public-final (disable)
      (send space remove-body body)
      (send space remove-shape shape))
    
    (define/public-final (draw [scene the-empty-scene])
      (let ((position (send body get-position)))
        (place-image image
                     (vector-x position)
                     (vector-y position)
                     scene)))))

(define aimer%
  (class object%
    
    (init-field [strength 0.5]
                [color "black"])
    
    (define start (vector 0.0 0.0))
    (define end (vector 0.0 0.0))
    (field [aiming? #f])
    
    (define dot (circle 5 "solid" color))
    
    (super-new)
    
    (define/public-final (aim v)
      (set! start (exact-vector->inexact-vector v))
      (set! end (exact-vector->inexact-vector v))
      (set! aiming? #t))
    
    (define/public-final (correct v)
      (set! end (exact-vector->inexact-vector v)))
    
    (define/public-final (loose)
      (set! aiming? #f))
    
    (define/public-final (position)
      start)
    
    (define/public-final (velocity)
      (vector-scale (vector-sub end start) strength))
    
    (define/public-final (draw [scene the-empty-scene])
      (if aiming?
          (place-image dot
                       (vector-x start)
                       (vector-y start)
                       (add-line scene
                                 (vector-x start)
                                 (vector-y start)
                                 (vector-x end)
                                 (vector-y end)
                                 color))
          scene))))

(define balls (new layer%))
(define aimer (new aimer%))

(big-bang
 0
 [on-tick (lambda (ticks)
            (send the-space step 0.5))]
 [on-draw (lambda (ticks)
            (send aimer draw (send balls draw )))]
 [on-mouse (lambda (ticks x y  event)
             (case (string->symbol event)
               ((drag)
                (send aimer correct (vector x y)))
               ((button-down)
                (send aimer aim (vector x y)))
               ((button-up)
                (send aimer loose)
                (send balls add
                      (new ball%
                           [position (send aimer position)]
                           [velocity (send aimer velocity)])))))])
#lang racket/base

(require racket/class
         racket/bool
         "chipmunk.rkt")

(provide body-wrapper% body%)

(define body-wrapper%
  (class object%
    
    (init-field cstruct)
    
    (super-new)
    
    (define/public-final (get-position)
      (cpv->vector (cp-body-get-pos cstruct)))
    
    (define/public-final (get-velocity)
      (cpv->vector (cp-body-get-vel cstruct)))
    
    (define/public-final (get-angle)
      (cp-body-get-angle cstruct))
    
    (define/public-final (get-angular-velocity)
      (cp-body-get-ang-vel cstruct))
    
    (define/public-final (get-mass)
      (cp-body-get-mass cstruct))
    
    (define/public-final (get-moment)
      (cp-body-get-moment cstruct))
    
    (define/public-final (set-position v)
      (cp-body-set-pos cstruct (vector->cpv v)))
    
    (define/public-final (set-velocity v)
      (cp-body-set-vel cstruct (vector->cpv v)))
    
    (define/public-final (set-angle a)
      (cp-body-set-angle cstruct a))
    
    (define/public-final (set-angular-velocity a)
      (cp-body-set-ang-vel cstruct a))
    
    (define/public-final (set-mass m)
      (cp-body-set-mass cstruct m))
    
    (define/public-final (set-moment m)
      (cp-body-set-moment m))
    
    (define/public-final (destroy)
      (cp-body-destroy cstruct))
    
    (define/public-final (free)
      (cp-body-free cstruct))
    
    (define/public (activate [filter #f])
      (if filter
          (cp-body-activate-static cstruct filter)
          (cp-body-activate cstruct)))
    
    (define/public-final (sleep [group #f])
      (if group
          (cp-body-sleep-with-group cstruct group)
          (cp-body-sleep cstruct)))
    
    (define/public-final (sleeping?)
      (cp-body-is-sleeping cstruct))
    
    (define/public-final (static?)
      (cp-body-is-static cstruct))
    
    (define/public-final (rogue?)
      (cp-body-is-rogue cstruct))
    
    (define/public-final (update-velocity gravity damping delta)
      (cp-body-update-velocity gravity damping delta))
    
    (define/public-final (local->world v)
      (cpv->vector
       (cp-body-local-to-world cstruct (vector->cpv v))))
    
    (define/public-final (world->local v)
      (cpv->vector
       (cp-body-world-to-local cstruct (vector->cpv v))))
    
    (define/public-final (reset-forces)
      (cp-body-reset-forces cstruct))
    
    (define/public-final (apply-force force centre)
      (cp-body-apply-force (vector->cpv force) (vector->cpv centre)))
    
    (define/public-final (apply-impulse impulse centre)
      (cp-body-apply-impulse (vector->cpv impulse) (vector->cpv centre)))
    
    (define/public-final (get-velocity-at-world-point v)
      (cpv->vector
       (cp-body-get-vel-at-world-point (vector->cpv v))))
    
    (define/public-final (get-velocity-at-local-point v)
      (cpv->vector
       (cp-body-get-vel-at-local-point (vector->cpv v))))
    
    (define/public-final (kinetic-energy)
      (cp-body-kinetic-energy cstruct))
    
    (define/public-final (each-shape proc)
      (cp-body-each-shape cstruct proc))
    
    (define/public-final (each-constraint proc)
      (cp-body-each-constraint cstruct proc))
    
    (define/public-final (each-arbiter proc)
      (cp-body-each-arbiter cstruct proc))    

    (set-cp-body-data! cstruct this)))

(define body%
  (class body-wrapper%
    
    (inherit/super set-position set-velocity set-angle set-angular-velocity)
    
    (init [mass #f]
          [inertia #f]
          [position (vector 0.0 0.0)]
          [velocity (vector 0.0 0.0)]
          [angle 0.0]
          [angular-velocity 0.0])
    
    (when (xor mass inertia)
      (error 'body% "a body can have either a mass and an inertia or nothing at all"))
    
    (super-new
     [cstruct
      (if (and mass inertia)
          (cp-body-new mass inertia)
          (cp-body-new-static))])
    
    (set-position position)
    (set-velocity velocity)
    (set-angle angle)
    (set-angular-velocity angular-velocity)))
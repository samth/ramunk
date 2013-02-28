#lang racket

(require "../chipmunk-ffi.rkt"
         2htdp/image
         2htdp/universe)

(define (cpVectPrint v)
  (display (format "(~a,~a)" (cpVect-x v) (cpVect-y v)))
  (newline))

; Working with intervals

(struct interval (min max))

(define (random-in interval)
   (+ (interval-min interval)
      (random (inexact->exact (interval-max interval)))))

; Working with inexact vectors

(define (make-cpv x y)
  (cpv (exact->inexact x)
       (exact->inexact y)))

; Parameters for the space

(define space-gravity (cpv 0.0 9.81))
(define space-framerate 120)
(define space-width 800.0)
(define space-height 800.0)
(define space-background (rectangle space-width space-height "solid" "gray"))
(define space-elasticity 0.7)

; Parameters for the particles

(define particle-origin (cpv 10.0 10.0))
(define particle-diameter (interval 40.0 50.0))
(define particle-mass (interval 2.0 10.0))
(define particle-velocity (interval 5.0 20.0))
(define particle-color "blue")

; Defining screen boundaries

(define (wall space start end [elasticity 0.1])
  (let ((shape (cpSegmentShapeNew (cpSpaceGetStaticBody space) start end 0.0)))
    (cpShapeSetElasticity shape elasticity)
    (cpSpaceAddShape space shape)
    shape))

(define (wrap space topleft topright bottomleft bottomright [elasticity 0.1])
  (wall space topleft topright elasticity)
  (wall space topright bottomright elasticity)
  (wall space bottomright bottomleft elasticity)
  (wall space bottomleft topleft elasticity))

(define (box space width height [elasticity 1.0])
  (wrap space (cpvzero) (cpv width 0.0) (cpv 0.0 height) (cpv width height) elasticity))

; Defining the physical space

(define the-space (cpSpaceNew))
(define the-space-interval (/ 1 space-framerate))
(cpSpaceSetGravity the-space space-gravity)
(box the-space space-width space-height space-elasticity)

; Definining a particle

(struct particle (body shape radius))

(define (make-particle position velocity mass diameter)
  (let*
      ((radius (/ diameter 2))
       (body (cpBodyNew mass (cpMomentForCircle mass 0.0 radius (cpvzero))))
       (shape (cpCircleShapeNew body radius (cpvzero))))
    (cpBodySetPos body position)
    (cpBodySetVel body velocity)
    (cpShapeSetElasticity shape 1.0)
    (cpSpaceAddBody the-space body)
    (cpSpaceAddShape the-space shape)
    (particle body shape radius)))

(define (make-random-particle position velocity)
  (make-particle position
                 velocity
                 (random-in particle-mass)
                 (random-in particle-diameter)))

(define (make-particles amount)
  (build-list amount
              (lambda (i)
                (make-random-particle particle-origin
                                      (make-cpv (random-in particle-velocity)
                                                (random-in particle-velocity))))))

; Keeping track of the interactions

(define particles (make-particles 5))
(define mouse-origin #f)
(define mouse-target (cpvzero))

(define (add-particle! particle)
  (set! particles (cons particle particles)))

(define (remove-particle! particle)
  (set! particles (remove particle particles)))

; Drawing the environment

(define (draw-particle particle scene)
  (let* ((position (cpBodyGetPos (particle-body particle))))
    (place-image (circle (particle-radius particle) "solid" particle-color)
                 (cpVect-x position)
                 (cpVect-y position)
                 scene)))

; Mixing everything together

(big-bang
 0
 [on-tick (lambda (ticks)
            (cpSpaceStep the-space (exact->inexact (* the-space-interval 10)))
            (+ ticks 1))
          the-space-interval]
 [on-draw (lambda (ticks)
            (let ((scene
                   (foldl draw-particle
                          space-background
                          particles)))
              (if mouse-origin
                  (add-line scene
                            (cpVect-x mouse-origin)
                            (cpVect-y mouse-origin)
                            (cpVect-x mouse-target)
                            (cpVect-y mouse-target)
                            "black")
                  scene)))]
 [on-mouse (lambda (y x ticks event)
             (case (string->symbol event)
               ((drag)
                (set! mouse-target (make-cpv x y)))
               ((button-down)
                (set! mouse-origin (make-cpv x y)))
               ((button-up)
                (add-particle!
                 (make-random-particle mouse-origin (cpvsub (make-cpv x y) mouse-origin)))
                (set! mouse-origin #f)))
             ticks)])
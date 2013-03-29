#lang racket

(require "../chipmunk.rkt"
         
         2htdp/image
         2htdp/universe)

(define (cpVectPrint v)
  (display (format "(~a,~a)" (cp-vect-x v) (cp-vect-y v)))
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
  (let ((shape (cp-segment-shape-new (cp-space-get-static-body space) start end 0.0)))
    (cp-shape-set-elasticity shape elasticity)
    (cp-space-add-shape space shape)
    shape))

(define (wrap space topleft topright bottomleft bottomright [elasticity 0.1])
  (wall space topleft topright elasticity)
  (wall space topright bottomright elasticity)
  (wall space bottomright bottomleft elasticity)
  (wall space bottomleft topleft elasticity))

(define (box space width height [elasticity 1.0])
  (wrap space (cpvzero) (cpv width 0.0) (cpv 0.0 height) (cpv width height) elasticity))

; Defining the physical space

(define the-space (cp-space-new))
(define the-space-interval (/ 1 space-framerate))
(cp-space-set-gravity the-space space-gravity)
(box the-space space-width space-height space-elasticity)

; Definining a particle as a cpShape

(struct particle (radius))

(define (make-particle position velocity mass diameter)
  (let*
      ((radius (/ diameter 2))
       (body (cp-body-new mass (cp-moment-for-circle mass 0.0 radius (cpvzero))))
       (shape (cp-circle-shape-new body radius (cpvzero))))
    (cp-body-set-pos body position)
    (cp-body-set-vel body velocity)
    (cp-shape-set-data shape (particle radius))
    (cp-shape-set-elasticity shape 1.0)
    shape))

(define (make-random-particle position velocity)
  (make-particle position
                 velocity
                 (random-in particle-mass)
                 (random-in particle-diameter)))

(define (make-particles amount)
  (build-list amount
              (lambda (i)
                (particle-enable 
                 (make-random-particle particle-origin
                                       (make-cpv (random-in particle-velocity)
                                                 (random-in particle-velocity)))
                 the-space))))

(define (particle-enable particle space)
  (cp-space-add-body the-space (cp-shape-get-body particle))
  (cp-space-add-shape the-space particle))

(define (particle-disable particle space)
  (cp-space-remove-body the-space (cp-shape-get-body particle))
  (cp-space-remove-shape the-space particle))


; Keeping track of the interactions

(define particles (make-particles 5))
(define mouse-origin #f)
(define mouse-particle #f)
(define mouse-target (cpvzero))

(define (add-particle! particle)
  (set! particles (cons particle particles))
  particle)

(define (remove-particle! particle)
  (set! particles (remove particle particles))
  particle)

; Drawing the environment

(define (draw-particle particle scene)
  (let* ((position (cp-body-get-pos (cp-shape-get-body particle))))
    (place-image (circle (particle-radius (cp-shape-get-data particle)) "solid" particle-color)
                 (cp-vect-x position)
                 (cp-vect-y position)
                 scene)))

; Mixing everything together

(big-bang
 0
 [on-tick (lambda (ticks)
            (cp-space-step the-space (exact->inexact (* the-space-interval 10)))
            (+ ticks 1))
          the-space-interval]
 [on-draw (lambda (ticks)
            (let ((scene
                   (foldl draw-particle
                          space-background
                          particles)))
              (if mouse-origin
                  (add-line scene
                            (cp-vect-x mouse-origin)
                            (cp-vect-y mouse-origin)
                            (cp-vect-x mouse-target)
                            (cp-vect-y mouse-target)
                            "black")
                  scene)))]
 [on-mouse (lambda (y x ticks event)
             (case (string->symbol event)
               ((drag)
                (set! mouse-target (make-cpv x y)))
               ((button-down)
                (let ((shape
                       (or (cp-space-point-query-first the-space (make-cpv x y) cp-all-layers cp-no-group)
                           (add-particle! (make-random-particle (make-cpv x y) (cpvzero))))))
                  (set! mouse-origin (make-cpv x y))
                  (set! mouse-particle shape)))
               ((button-up)
                (cp-body-set-vel (cp-shape-get-body mouse-particle) (cpvsub (make-cpv x y) mouse-origin))
                (particle-enable mouse-particle the-space)
                (add-particle! mouse-particle)
                (set! mouse-origin #f)))
             ticks)])
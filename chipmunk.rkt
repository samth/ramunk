#lang racket/base

(require (for-syntax racket/base)
         ffi/unsafe
         "ffi-utils.rkt"
         "chipmunk-ffi.rkt")

(define-syntax-rule (with (var val) expr ...)
  (let ((var val)) expr ... var))

(define-syntax (provide/ffi stx)
  (syntax-case stx ()
    [(_ id ...)
     #`(provide (rename-out
                 #,@(map (lambda (id)
                           (list id (c-id->racket-id id)))
                         (syntax-e #'(id ...)))))]))

(define the-empty-immobile-cell (malloc-immobile-cell (void)))

(define (immobile-cell-contents cell)
  (ptr-ref cell _racket))

(define (immobilize-mutator mutator-proc)
  (lambda (cstruct val)
    (mutator-proc cstruct (malloc-immobile-cell val))))

;; Vector operations

(define/provide (vector->cpv v)
  (cpv (vector-ref v 0)
       (vector-ref v 1)))

(define/provide (cpv->vector v)
  (vector (cpVect-x v)
          (cpVect-y v)))

(provide/ffi cpVect-x
             cpVect-y
             cpv
             cpvzero 
             cpvslerp
             cpvslerpconst
             cpvstr
             cpveql
             cpvadd
             cpvsub
             cpvneg
             cpvmult
             cpvcross
             cpvdot
             cpvperp
             cpvrperp
             cpvproject
             cpvforangle
             cpvtoangle
             cpvrotate
             cpvunrotate
             cpvlength
             cpvlerp
             cpvnormalize
             cpvnormalize_safe
             cpvclamp
             cpvlerpconst
             cpvdist
             cpvdistsq
             cpvnear
             cpvlengthsq
             )

;; Body operations

(provide (rename-out
          [cpBodyLocal2World cp-body-local-to-world]
          [cpBodyWorld2Local cp-body-world-to-local]))

(define/provide set-cp-body-data! (immobilize-mutator set-cpBody-data!))

(define/provide (cp-body-get-data cpBody)
  (immobile-cell-contents (cpBody-data cpBody)))

(define/provide (cp-body-set-data cpBody val)
  (free-immobile-cell (cpBody-data cpBody))
  (set-cp-body-data! cpBody val))

(define (lift-body-constructor constructor)
  (lambda args
    (with (body (apply constructor args))
      (set-cpBody-data! body the-empty-immobile-cell))))

(define/provide cp-body-alloc (lift-body-constructor cpBodyAlloc))
(define/provide cp-body-new (lift-body-constructor cpBodyNew))
(define/provide cp-body-new-static (lift-body-constructor cpBodyNewStatic))

(provide/ffi cpBodyGetMass
             cpBodyGetMoment
             cpBodyGetPos
             cpBodyGetVel
             cpBodyGetForce
             cpBodyGetAngle
             cpBodyGetAngVel
             cpBodyGetTorque
             cpBodyGetVelLimit
             cpBodyGetAngVelLimit
             cpBodyGetSpace
             cpBodyGetUserData
             
             cpBodySetMass
             cpBodySetMoment
             cpBodySetPos
             cpBodySetVel
             cpBodySetForce
             cpBodySetAngle
             cpBodySetAngVel
             cpBodySetTorque
             cpBodySetVelLimit
             cpBodySetAngVelLimit
             cpBodySetUserData
             
             cpBodyInit
             cpBodyInitStatic
             cpBodyDestroy
             cpBodyFree
             cpBodySanityCheck
             cpBodyActivate
             cpBodyActivateStatic
             cpBodySleep
             cpBodySleepWithGroup
             cpBodyIsSleeping
             cpBodyIsStatic
             cpBodyIsRogue
             cpBodyUpdateVelocity
             cpBodyLocal2World
             cpBodyWorld2Local
             cpBodyResetForces
             cpBodyApplyForce
             cpBodyApplyImpulse
             cpBodyGetVelAtWorldPoint
             cpBodyGetVelAtLocalPoint
             cpBodyKineticEnergy
             cpBodyEachShape
             cpBodyEachConstraint
             cpBodyEachArbiter
             )

;; Shape operations

(define/provide set-cp-shape-data! (immobilize-mutator set-cpShape-data!))

(define/provide (cp-shape-get-data shape val)
  (immobile-cell-contents (cpShape-data shape)))

(define/provide (cp-shape-set-data shape val)
  (free-immobile-cell (cpShape-data shape))
  (set-cp-shape-data! shape val))

(define (lift-shape-constructor constructor)
  (lambda args
    (with (shape (apply constructor args))
      (set-cpShape-data! shape the-empty-immobile-cell))))

(define/provide cp-circle-shapee-alloc (lift-shape-constructor cpCircleShapeAlloc))
(define/provide cp-segment-shape-alloc (lift-shape-constructor cpSegmentShapeAlloc))
(define/provide cp-poly-shape-alloc (lift-shape-constructor cpPolyShapeAlloc))

(define/provide cp-circle-shape-new (lift-shape-constructor cpCircleShapeNew))
(define/provide cp-segment-shape-new (lift-shape-constructor cpSegmentShapeNew))
(define/provide cp-poly-shape-new (lift-shape-constructor cpPolyShapeNew))
(define/provide cp-box-shape-new (lift-shape-constructor cpBoxShapeNew))
(define/provide cp-box-shape-new2 (lift-shape-constructor cpBoxShapeNew2))

(provide/ffi cpShapeGetBody
             cpShapeGetBB
             cpShapeGetSensor
             cpShapeGetElasticity
             cpShapeGetFriction
             cpShapeGetSurfaceVelocity
             cpShapeGetCollisionType
             cpShapeGetGroup
             cpShapeGetLayers
             cpShapeGetSpace
             cpShapeGetUserData
             
             cpShapeSetBody
             cpShapeSetSensor
             cpShapeSetElasticity
             cpShapeSetFriction
             cpShapeSetSurfaceVelocity
             cpShapeSetCollisionType
             cpShapeSetGroup
             cpShapeSetLayers
             cpShapeSetUserData
             
             cpShapeDestroy
             cpShapeFree
             cpShapeCacheBB
             cpShapeUpdate
             cpShapePointQuery
             cpShapeNearestPointQuery
             cpShapeSegmentQuery
             cpSegmentQueryHitPoint
             cpSegmentQueryHitDist
             cpResetShapeIdCounter
             
             cpCircleShapeInit
             cpCircleShapeGetOffset
             cpCircleShapeGetRadius
             
             cpSegmentShapeInit
             cpSegmentShapeGetA
             cpSegmentShapeGetB
             cpSegmentShapeGetNormal
             cpSegmentShapeGetRadius
             
             cpPolyShapeInit
             cpPolyValidate
             cpPolyShapeGetNumVerts
             cpPolyShapeGetVert
             
             cpBoxShapeInit
             cpBoxShapeInit2
             )

;; Space operations

(define/provide set-cp-space-data! (immobilize-mutator set-cpSpace-data!))

(define/provide (cp-space-get-data cpSpace)
  (immobile-cell-contents (cpSpace-data cpSpace)))

(define/provide (cp-space-set-data cpSpace val)
  (free-immobile-cell (cpSpace-data cpSpace))
  (set-cp-space-data! cpSpace val))

(define (lift-space-constructor constructor)
  (lambda args
    (with (space (apply constructor args))
      (set-cpSpace-data! space the-empty-immobile-cell))))
  
(define/provide cp-space-new (lift-space-constructor cpSpaceNew))
(define/provide cp-space-alloc (lift-space-constructor cpSpaceAlloc))

(provide/ffi cpSpaceGetIterations
             cpSpaceGetGravity
             cpSpaceGetDamping
             cpSpaceGetIdleSpeedThreshold
             cpSpaceGetSleepTimeThreshold
             cpSpaceGetCollisionSlop
             cpSpaceGetCollisionBias
             cpSpaceGetCollisionPersistence
             cpSpaceGetEnableContactGraph
             cpSpaceGetCurrentTimeStep
             cpSpaceGetUserData
             cpSpaceGetStaticBody
             
             cpSpaceSetIterations
             cpSpaceSetGravity
             cpSpaceSetDamping
             cpSpaceSetIdleSpeedThreshold
             cpSpaceSetSleepTimeThreshold
             cpSpaceSetCollisionSlop
             cpSpaceSetCollisionBias
             cpSpaceSetCollisionPersistence
             cpSpaceSetEnableContactGraph
             cpSpaceSetUserData
             
             cpSpaceInit
             cpSpaceDestroy
             cpSpaceFree
             cpSpaceIsLocked
             cpSpaceSetDefaultCollisionHandler
             cpSpaceAddCollisionHandler
             cpSpaceRemoveCollisionHandler
             cpSpaceAddShape
             cpSpaceAddStaticShape
             cpSpaceAddBody
             cpSpaceAddConstraint
             cpSpaceRemoveShape
             cpSpaceRemoveStaticShape
             cpSpaceRemoveBody
             cpSpaceRemoveConstraint
             cpSpaceContainsShape
             cpSpaceContainsBody
             cpSpaceContainsConstraint
             cpSpaceAddPostStepCallback
             cpSpacePointQuery
             cpSpacePointQueryFirst
             cpSpaceNearestPointQuery
             cpSpaceNearestPointQueryNearest
             cpSpaceSegmentQuery
             cpSpaceSegmentQueryFirst
             cpSpaceBBQuery
             cpSpaceShapeQuery
             cpSpaceActivateShapesTouchingShape
             cpSpaceEachBody
             cpSpaceEachShape
             cpSpaceEachConstraint
             cpSpaceReindexStatic
             cpSpaceReindexShape
             cpSpaceReindexShapesForBody
             cpSpaceUseSpatialHash
             cpSpaceStep
             )

;; Miscellaneous operations

(provide/ffi cpEnableSegmentToSegmentCollisions
             cpMomentForCircle
             cpAreaForCircle
             cpMomentForSegment
             cpAreaForSegment 
             cpMomentForPoly 
             cpAreaForPoly 
             cpCentroidForPoly 
             cpRecenterPoly
             cpMomentForBox
             cpMomentForBox2
             cpConvexHull
             )
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

;; Vector operations

(define (vector->cpv v)
  (cpv (vector-ref v 0)
       (vector-ref v 1)))

(define (cpv->vector v)
  (vector (cpVect-x v)
          (cpVect-y v)))

(provide vector->cpv
         cpv->vector)

(provide/ffi cpv
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

(provide cpBody-data
         (rename-out
          [cpBodyLocal2World cp-body-local-to-world]
          [cpBodyWorld2Local cp-body-world-to-local]))

(define/provide (cp-body-get-data cpBody)
  (immobile-cell-contents (cpBody-data cpBody)))

(define/provide (cp-body-set-data cpBody val)
  (free-immobile-cell (cpBody-data cpBody))
  (set-cpBody-data! cpBody (malloc-immobile-cell val)))

(define/provide (cp-body-new mass intertia)
  (with (cpBody (cpBodyNew mass intertia))
    (set-cpBody-data! cpBody the-empty-immobile-cell)))

(provide/ffi cpBodyAlloc
             cpBodyInit
             cpBodyInitStatic
             cpBodyNewStatic
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
             cpBodyResetForces
             cpBodyApplyForce
             cpBodyApplyImpulse
             cpBodyGetVelAtWorldPoint
             cpBodyGetVelAtLocalPoint
             cpBodyKineticEnergy
             cpBodyEachShape
             cpBodyEachConstraint
             cpBodyEachArbiter
             
             cpBodyGetMass 
             cpBodyGetMoment 
             cpBodyGetPos 
             cpBodyGetAngle
             cpBodyGetVel 
             cpBodyGetAngVel 
             
             cpBodySetMass 
             cpBodySetMoment
             cpBodySetPos 
             cpBodySetAngle 
             cpBodySetVel 
             cpBodySetAngVel 
             )

;; Space operations

(define/provide (cp-space-get-data cpSpace)
  (immobile-cell-contents (cpSpace-data cpSpace)))

(define/provide (cp-space-set-data cpSpace val)
  (free-immobile-cell (cpSpace-data cpSpace))
  (set-cpSpace-data! cpSpace (malloc-immobile-cell val)))

(define/provide (cp-space-new)
  (with (cpSpace (cpSpaceNew))
    (set-cpSpace-data! cpSpace the-empty-immobile-cell)))

(provide/ffi cpSpaceDestroy 
             cpSpaceFree
             cpSpaceAddShape 
             cpSpaceAddStaticShape
             cpSpaceAddBody
             cpSpaceAddConstraint
             cpSpaceAddPostStepCallback
             cpSpaceRemoveShape
             cpSpaceRemoveStaticShape 
             cpSpaceRemoveBody 
             cpSpaceRemoveConstraint
             cpSpaceGetIterations 
             cpSpaceSetIterations 
             cpSpaceGetGravity 
             cpSpaceSetGravity
             cpSpaceGetDamping
             cpSpaceSetDamping
             cpSpaceGetIdleSpeedThreshold 
             cpSpaceSetIdleSpeedThreshold 
             cpSpaceGetSleepTimeThreshold 
             cpSpaceSetSleepTimeThreshold
             cpSpaceGetCollisionSlop 
             cpSpaceSetCollisionSlop
             cpSpaceGetCollisionBias 
             cpSpaceSetCollisionBias 
             cpSpaceGetCollisionPersistence 
             cpSpaceSetCollisionPersistence 
             cpSpaceGetEnableContactGraph 
             cpSpaceSetEnableContactGraph 
             cpSpaceGetUserData 
             cpSpaceSetUserData 
             cpSpaceGetStaticBody 
             cpSpaceGetCurrentTimeStep 
             cpSpaceSetDefaultCollisionHandler
             cpSpaceAddCollisionHandler
             cpSpaceRemoveCollisionHandler
             cpSpaceIsLocked
             cpSpaceContainsShape
             cpSpaceContainsBody
             cpSpaceContainsConstraint
             cpSpacePointQuery 
             cpSpacePointQueryFirst 
             cpSpaceNearestPointQuery 
             cpSpaceNearestPointQueryNearest
             cpSpaceSegmentQuery 
             cpSpaceSegmentQueryFirst
             cpSpaceBBQuery
             ;cpSpaceShapeQuery
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


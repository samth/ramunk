#lang racket

; TODOTODO: Make ffi bindings lazy to reduce startup time.
; TODOTODO: Add some missing structures.
; FIXMEFIXME: Some functions have been commented out because their pointers were not found.

(require ffi/unsafe
         ffi/unsafe/define
         rnrs/arithmetic/bitwise-6
         racket/runtime-path)

(define-runtime-path chipmunk-binary (build-path "bin" (number->string (system-type 'word)) "libchipmunk"))
(define chipmunk (ffi-lib chipmunk-binary))
(define-ffi-definer define-chipmunk chipmunk)

(define-syntax (defchipmunk stx)
  (syntax-case stx ()
    [(defchipmunk name #:ptr type)
     #`(begin (provide name)
              (define name
                (let ()
                  (define-chipmunk ptr _pointer
                    #:c-id #,(datum->syntax
                              #'name
                              (string->symbol
                               (format "_~a" (syntax->datum #'name)))))
                  (function-ptr ptr type))))]
    [(defchipmunk name type)
     #'(begin (provide name)
              (define-chipmunk name type))]))

(define (sint32->uint32 v)
  (bitwise-and #xFFFFFFFF v))

; ***********************************************
; * Start of Chipmunk type definitions
; ***********************************************

(define _cpFloat _double)
(define cpFloat? real?)
(define _cpDataPointer _racket)
(define _cpKeyPointer _pointer)
(define _size_t _ulong)
(define _cpHashValue _size_t)
(define _cpBool _int)
(define cpTrue 1)
(define cpFalse 0)
(define _cpTimeStamp _uint)
(define _cpCollisionType _uint)
(define _cpGroup _uint)
(define _cpLayers _uint)

(define GRABABLE_MASK (sint32->uint32 (arithmetic-shift 1 31)))
(define NOT_GRABABLE_MASK (sint32->uint32 (bitwise-not GRABABLE_MASK)))
(define CP_NO_GROUP 0)
(define CP_ALL_LAYERS 0)
(define cpfexp exp)

; ***********************************************
; * End of Chipmunk type definitions
; ***********************************************

; -----------------------------------------------

; ***********************************************
; * Start of Chipmunk struct definitions
; ***********************************************

(define-cstruct _cpVect
  ([x _cpFloat]
   [y _cpFloat]))

(define _cpBodyVelocityFunc (_fun _pointer _cpVect _cpFloat _cpFloat -> _void))
(define _cpBodyPositionFunc (_fun _pointer _cpFloat -> _void))

(define-cstruct _cpBody
  ([velocity_func _cpBodyVelocityFunc]
   [position_func _cpBodyPositionFunc]
   [m _cpFloat]
   [m_inv _cpFloat]
   [i _cpFloat]
   [i_inv _cpFloat]
   [p _cpVect]
   [v _cpVect]
   [f _cpVect]
   [a _cpFloat]
   [w _cpFloat]
   [t _cpFloat]
   [rot _cpVect]
   [data _cpDataPointer]
   [v_bias _cpVect]
   [w_bias _cpFloat]))

(define-cstruct _cpSpace
  ([iterations _int]
   [gravity _cpVect]
   [damping _cpFloat]
   [idleSpeedThreshold _cpFloat]
   [sleepTimeThreshold _cpFloat]
   [collisionSlop _cpFloat]
   [collisionBias _cpFloat]
   [collisionPersistence _cpFloat]
   [enableContactGraph _cpBool]
   [data _cpDataPointer]
   [staticBody _cpBody-pointer]))

(define-cstruct _cpArbiter
  ([e _cpFloat]
   [u _cpFloat]
   [surface_vr _cpVect]))

(define-cstruct _cpBB
  ([l _cpFloat]
   [b _cpFloat]
   [r _cpFloat]
   [t _cpFloat]))

(define-cstruct _cpShape
  ([body _cpBody-pointer]
   [bb _cpBB]
   [sensor _cpBool]
   [e _cpFloat]
   [u _cpFloat]
   [surface_v _cpVect]
   [data _cpDataPointer]
   [collision_type _cpCollisionType]
   [group _cpGroup]
   [layers _cpLayers]))

(define _cpConstraintPreSolveFunc (_fun _cpSpace-pointer -> _void))
(define _cpConstraintPostSolveFunc (_fun _cpSpace-pointer -> _void))

(define-cstruct _cpConstraint
  ([a _cpBody]
   [b _cpBody]
   [maxForce _cpFloat]
   [errorBias _cpFloat]
   [minForce _cpFloat]
   [preSolve _cpConstraintPreSolveFunc]
   [postSolve _cpConstraintPostSolveFunc]
   [data _cpDataPointer]))

; ********
; Funtion types start
; ********

(define _cpCollisionBeginFunc (_fun _cpArbiter-pointer _cpSpace-pointer _pointer -> _cpBool))
(define _cpCollisionPreSolveFunc (_fun _cpArbiter-pointer _cpSpace-pointer _pointer -> _cpBool))
(define _cpCollisionPostSolveFunc (_fun _cpArbiter-pointer _cpSpace-pointer _pointer -> _void))
(define _cpCollisionSeparateFunc (_fun _cpArbiter-pointer _cpSpace-pointer _pointer -> _void))

(define _cpPostStepFunc (_fun _cpSpace-pointer _cpKeyPointer _cpDataPointer -> _void))
(define _cpSpacePointQueryFunc (_fun _cpShape-pointer _cpDataPointer -> _void))
(define _cpSpaceNearestPointQueryFunc (_fun _cpShape-pointer _cpFloat _cpVect _cpDataPointer -> _void))
(define _cpSpaceSegmentQueryFunc (_fun _cpShape-pointer _cpFloat _cpVect _cpDataPointer -> _void))
(define _cpSpaceBBQueryFunc (_fun _cpShape-pointer _cpDataPointer -> _void))
(define _cpSpaceBodyIteratorFunc (_fun _cpBody-pointer _cpDataPointer -> _void))
(define _cpSpaceShapeIteratorFunc (_fun _cpShape-pointer _cpDataPointer -> _void))
(define _cpSpaceConstraintIteratorFunc (_fun _cpConstraint-pointer _cpDataPointer -> _void))

; ********
; Function types end
; ********

; ***********************************************
; * End of Chipmunk struct definitions
; ***********************************************

; -----------------------------------------------

; ***********************************************
; * Start of Chipmunk Space definitions
; ***********************************************

; ***
; cpSpace creation functions
; ***

(defchipmunk cpSpaceAlloc (_fun -> _cpSpace-pointer))
(defchipmunk cpSpaceInit (_fun _cpSpace-pointer -> _cpSpace-pointer))
(defchipmunk cpSpaceNew (_fun -> _cpSpace-pointer))

; ***
; cpSpace destruction functions
; ***

(defchipmunk cpSpaceDestroy (_fun _cpSpace-pointer -> _void))
(defchipmunk cpSpaceFree (_fun _cpSpace-pointer -> _void))
(defchipmunk cpSpaceAddShape (_fun _cpSpace-pointer _cpShape-pointer -> _cpShape-pointer))
(defchipmunk cpSpaceAddStaticShape (_fun _cpSpace-pointer _cpShape-pointer -> _cpShape))
(defchipmunk cpSpaceAddBody (_fun _cpSpace-pointer _cpBody-pointer -> _cpBody-pointer))
(defchipmunk cpSpaceAddConstraint (_fun _cpSpace-pointer _cpConstraint-pointer -> _cpConstraint-pointer))
(defchipmunk cpSpaceAddPostStepCallback (_fun _cpSpace-pointer (_or-null _cpPostStepFunc) _cpKeyPointer _cpDataPointer -> _cpBool))
(defchipmunk cpSpaceRemoveShape (_fun _cpSpace-pointer _cpShape-pointer -> _void))
(defchipmunk cpSpaceRemoveStaticShape (_fun _cpSpace-pointer _cpShape-pointer -> _cpShape))
(defchipmunk cpSpaceRemoveBody (_fun _cpSpace-pointer _cpBody-pointer -> _void))
(defchipmunk cpSpaceRemoveConstraint (_fun _cpSpace-pointer _cpConstraint-pointer -> _cpConstraint-pointer))

; ********
; Getters and Setters Start
; ********

(defchipmunk cpSpaceGetIterations #:ptr (_fun _cpSpace-pointer -> _int))
(defchipmunk cpSpaceSetIterations #:ptr (_fun _cpSpace-pointer _int -> _void))
(defchipmunk cpSpaceGetGravity #:ptr (_fun _cpSpace-pointer -> _cpVect))
(defchipmunk cpSpaceSetGravity #:ptr (_fun _cpSpace-pointer _cpVect -> _void))
(defchipmunk cpSpaceGetDamping #:ptr (_fun _cpSpace-pointer -> _cpFloat))
(defchipmunk cpSpaceSetDamping #:ptr (_fun _cpSpace-pointer _cpFloat -> _void))
(defchipmunk cpSpaceGetIdleSpeedThreshold #:ptr (_fun _cpSpace-pointer -> _cpFloat))
(defchipmunk cpSpaceSetIdleSpeedThreshold #:ptr (_fun _cpSpace-pointer _cpFloat -> _void))
(defchipmunk cpSpaceGetSleepTimeThreshold #:ptr (_fun _cpSpace-pointer -> _cpFloat))
(defchipmunk cpSpaceSetSleepTimeThreshold #:ptr (_fun _cpSpace-pointer _cpFloat -> _void))
(defchipmunk cpSpaceGetCollisionSlop #:ptr (_fun _cpSpace-pointer -> _cpFloat))
(defchipmunk cpSpaceSetCollisionSlop #:ptr (_fun _cpSpace-pointer _cpFloat -> _void))
(defchipmunk cpSpaceGetCollisionBias #:ptr (_fun _cpSpace-pointer -> _cpFloat))
(defchipmunk cpSpaceSetCollisionBias #:ptr (_fun _cpSpace-pointer _cpFloat -> _void))
(defchipmunk cpSpaceGetCollisionPersistence #:ptr (_fun _cpSpace-pointer -> _cpTimeStamp))
(defchipmunk cpSpaceSetCollisionPersistence #:ptr (_fun _cpSpace-pointer _cpTimeStamp -> _void))
(defchipmunk cpSpaceGetEnableContactGraph #:ptr (_fun _cpSpace-pointer -> _cpBool))
(defchipmunk cpSpaceSetEnableContactGraph #:ptr (_fun _cpSpace-pointer _cpBool -> _void))
(defchipmunk cpSpaceGetUserData #:ptr (_fun _cpSpace-pointer -> _cpDataPointer))
(defchipmunk cpSpaceSetUserData #:ptr (_fun _cpSpace-pointer _cpDataPointer -> _void))
(defchipmunk cpSpaceGetStaticBody #:ptr (_fun _cpSpace-pointer -> _cpBody-pointer))
(defchipmunk cpSpaceGetCurrentTimeStep #:ptr (_fun _cpSpace-pointer -> _cpFloat))

; ********
; Getters and Setters End
; ********
; Collision Handlers Start
; ********

(defchipmunk cpSpaceSetDefaultCollisionHandler
  (_fun _cpSpace-pointer
        _cpCollisionBeginFunc
        _cpCollisionPreSolveFunc
        _cpCollisionPostSolveFunc
        _cpCollisionSeparateFunc
        _pointer
        -> _void))

(defchipmunk cpSpaceAddCollisionHandler
  (_fun _cpSpace-pointer
        _cpCollisionType
        _cpCollisionType
        _cpCollisionBeginFunc
        _cpCollisionPreSolveFunc
        _cpCollisionPostSolveFunc
        _cpCollisionSeparateFunc
        _pointer
        -> _void))

(defchipmunk cpSpaceRemoveCollisionHandler
  (_fun _cpSpace-pointer
        _cpCollisionType
        _cpCollisionType
        -> _void))


; ********
; Collision Handlers End
; ********

(defchipmunk cpSpaceIsLocked #:ptr (_fun _cpSpace-pointer -> _cpBool))
(defchipmunk cpSpaceContainsShape (_fun _cpSpace-pointer _cpShape-pointer -> _cpBool))
(defchipmunk cpSpaceContainsBody (_fun _cpSpace-pointer _cpBody-pointer -> _cpBool))
(defchipmunk cpSpaceContainsConstraint (_fun _cpSpace-pointer _cpConstraint-pointer -> _cpBool))
(defchipmunk cpSpacePointQuery (_fun _cpSpace-pointer _cpVect _cpLayers _cpGroup _cpSpacePointQueryFunc _cpDataPointer -> _void))
(defchipmunk cpSpacePointQueryFirst (_fun _cpSpace-pointer _cpVect _cpLayers _cpGroup -> (_or-null _cpShape-pointer)))
(defchipmunk cpSpaceNearestPointQuery (_fun _cpSpace-pointer _cpVect _cpFloat _cpLayers _cpGroup _cpSpaceNearestPointQueryFunc _cpDataPointer -> _void))
(defchipmunk cpSpaceNearestPointQueryNearest (_fun _cpSpace-pointer _cpVect _cpFloat _cpLayers _cpGroup _cpSpaceNearestPointQueryFunc _cpDataPointer -> _void))
(defchipmunk cpSpaceSegmentQuery (_fun _cpSpace-pointer _cpVect _cpVect _cpLayers _cpGroup _cpGroup _cpSpaceSegmentQueryFunc -> _void))
;(defchipmunk cpSpaceSegmentQueryFirst (_fun _cpSpace-pointer _cpVect _cpVect _cpLayers _cpGroup _cpSegmenQueryInfo -> _cpShape-pointer))
(defchipmunk cpSpaceBBQuery (_fun _cpSpace-pointer _cpBB-pointer _cpLayers _cpGroup _cpSpaceBBQueryFunc _cpDataPointer -> _void))
;(defchipmunk cpSpaceShapeQuery (_fun _cpSpace-pointer _cpShape-pointer _cpSpaceShapeQueryFunc _cpDataPointer -> _cpBool))
(defchipmunk cpSpaceActivateShapesTouchingShape (_fun _cpSpace-pointer _cpShape-pointer -> _void))
(defchipmunk cpSpaceEachBody (_fun _cpSpace-pointer _cpSpaceBodyIteratorFunc -> _void))
(defchipmunk cpSpaceEachShape (_fun _cpSpace-pointer _cpSpaceShapeIteratorFunc -> _void))
(defchipmunk cpSpaceEachConstraint (_fun _cpSpace-pointer _cpSpaceConstraintIteratorFunc -> _void))
(defchipmunk cpSpaceReindexStatic (_fun _cpSpace-pointer -> _void))
(defchipmunk cpSpaceReindexShape (_fun _cpSpace-pointer _cpShape-pointer -> _void))
(defchipmunk cpSpaceReindexShapesForBody (_fun _cpSpace-pointer _cpBody-pointer -> _void))
(defchipmunk cpSpaceUseSpatialHash (_fun _cpSpace-pointer _cpFloat _int -> _void))
(defchipmunk cpSpaceStep (_fun _cpSpace-pointer _cpFloat -> _void))

; ***********************************************
; * End of Chipmunk Space definitions
; ***********************************************

; -----------------------------------------------

; ***********************************************
; * Start of Chipmunk Bounding Box operation definitions.
; ***********************************************

(defchipmunk cpBBNew #:ptr (_fun _cpFloat _cpFloat _cpFloat _cpFloat -> _cpBB))

; ********
; Getters and Setters Start
; ********
; ********
; Getters and Setters End
; ********
; ***********************************************
; * End of Chipmunk Bounding Box operation definitions.
; ***********************************************

; -----------------------------------------------

; ***********************************************
; * Start of Chipmunk Body operation definitions.
; ***********************************************

(defchipmunk cpBodyAlloc (_fun -> _cpBody-pointer))
(defchipmunk cpBodyInit (_fun _cpBody-pointer _cpFloat _cpFloat -> _cpBody-pointer))
(defchipmunk cpBodyNew (_fun _cpFloat _cpFloat -> _cpBody-pointer))
(defchipmunk cpBodyInitStatic (_fun _cpBody-pointer -> _cpBody-pointer))
(defchipmunk cpBodyNewStatic (_fun -> _cpBody-pointer))
(defchipmunk cpBodyDestroy (_fun _cpBody-pointer -> _cpBody-pointer))
(defchipmunk cpBodyFree (_fun _cpBody-pointer -> _void))
(defchipmunk cpBodySanityCheck (_fun _cpBody-pointer -> _void))
(defchipmunk cpBodyActivate (_fun _cpBody-pointer -> _void))
(defchipmunk cpBodyActivateStatic (_fun _cpBody-pointer _cpShape-pointer -> _void))
(defchipmunk cpBodySleep (_fun _cpBody-pointer -> _void))
(defchipmunk cpBodySleepWithGroup (_fun _cpBody-pointer _cpBody-pointer -> _void))
(defchipmunk cpBodyIsSleeping #:ptr (_fun _cpBody-pointer -> _cpBool))
(defchipmunk cpBodyIsStatic #:ptr (_fun _cpBody-pointer -> _cpBool))
(defchipmunk cpBodyIsRogue #:ptr (_fun _cpBody-pointer -> _cpBool))
(defchipmunk cpBodyUpdateVelocity (_fun _cpBody-pointer _cpVect _cpFloat _cpFloat -> _void))
(defchipmunk cpBodyLocal2World #:ptr (_fun _cpBody-pointer _cpVect -> _cpVect))
(defchipmunk cpBodyWorld2Local #:ptr (_fun _cpBody-pointer _cpVect -> _cpVect))
;(defchipmunk cpBodyResetForces #:ptr (_fun _cpBody-pointer -> _void))
;(defchipmunk cpBodyApplyForce #:ptr (_fun _cpBody-pointer _cpVect _cpVect -> _void))
;(defchipmunk cpBodyApplyImpulse #:ptr (_fun _cpBody-pointer _cpVect _cpVect -> _void))
;(defchipmunk cpBodyGetVelAtWorldPoint #:ptr (_fun _cpBody-pointer _cpVect -> _cpVect))
;(defchipmunk cpBodyGetVelAtLocalPoint #:ptr (_fun _cpBody-pointer _cpVect -> _cpVect))
(defchipmunk cpBodyKineticEnergy #:ptr (_fun _cpBody-pointer -> _cpFloat))
;(defchipmunk cpBodyEachShape (_fun _cpBody-pointer _cpBodyShapeIteratorFunc _cpDataPointer -> void))
;(defchipmunk cpBodyEachConstraint (_fun _cpBody-pointer _cpBodyConstraintIteratorFunc _cpDataPointer -> void))
;(defchipmunk cpBodyEachArbiter (_fun _cpBody-pointer _cpBodyArbiterIteratorFunc _cpDataPointer -> void))

; ********
; Getters and Setters Start
; ********

(defchipmunk cpBodyGetMass #:ptr (_fun _cpBody-pointer -> _cpFloat))
(defchipmunk cpBodyGetMoment #:ptr (_fun _cpBody-pointer -> _cpFloat))
(defchipmunk cpBodyGetPos #:ptr (_fun _cpBody-pointer -> _cpVect))
(defchipmunk cpBodyGetAngle #:ptr (_fun _cpBody-pointer -> _cpFloat))
(defchipmunk cpBodyGetVel #:ptr (_fun _cpBody-pointer -> _cpVect))
(defchipmunk cpBodyGetAngVel #:ptr (_fun _cpBody-pointer -> _cpFloat))

(defchipmunk cpBodySetMass (_fun _cpBody-pointer _cpFloat -> _void))
(defchipmunk cpBodySetMoment (_fun _cpBody-pointer _cpFloat -> _void))
(defchipmunk cpBodySetPos (_fun _cpBody-pointer _cpVect -> _void))
(defchipmunk cpBodySetAngle (_fun _cpBody-pointer _cpFloat -> _void))
(defchipmunk cpBodySetVel #:ptr (_fun _cpBody-pointer _cpVect -> _void))
(defchipmunk cpBodySetAngVel #:ptr (_fun _cpBody-pointer _cpFloat -> _void))

(define (cpBodyGetData cpBody)
  (ptr-ref (cpBody-data cpBody) _cpDataPointer))

(define (cpBodySetData cpBody val)
  (let ((data (malloc-immobile-cell val)))
    (free-immobile-cell (cpBody-data cpBody))
    (set-cpBody-data! cpBody data)))

; ********
; Getters and Setters End
; ********

; ***********************************************
; * End of Chipmunk Body operation definitions.
; ***********************************************

; -----------------------------------------------

; ***********************************************
; * Start of Chipmunk Shape operation definitions.
; ***********************************************

(defchipmunk cpShapeDestroy (_fun _cpShape-pointer -> _void))
(defchipmunk cpShapeFree (_fun _cpShape-pointer -> _void))
(defchipmunk cpShapeCacheBB (_fun _cpShape-pointer -> _cpBB))
(defchipmunk cpShapeUpdate (_fun _cpShape-pointer _cpVect _cpVect -> _cpBB))
(defchipmunk cpShapePointQuery (_fun _cpShape-pointer _cpVect -> _cpBool))

(defchipmunk cpSegmentShapeNew (_fun _cpBody-pointer _cpVect _cpVect _cpFloat -> _cpShape-pointer))
(defchipmunk cpCircleShapeNew (_fun _cpBody-pointer _cpFloat _cpVect -> _cpShape-pointer))

; ********
; Getters and Setters Start
; ********

(defchipmunk cpShapeGetBody #:ptr (_fun _cpShape-pointer -> _cpBody-pointer))
(defchipmunk cpShapeSetBody #:ptr (_fun _cpShape-pointer _cpBody-pointer -> _void))
(defchipmunk cpShapeGetBB #:ptr (_fun _cpShape-pointer -> _cpBB))
(defchipmunk cpShapeGetSensor #:ptr (_fun _cpShape-pointer -> _cpBool))
(defchipmunk cpShapeSetSensor #:ptr (_fun _cpShape-pointer _cpBool -> _void))
(defchipmunk cpShapeGetElasticity #:ptr (_fun _cpShape-pointer -> _cpFloat))
(defchipmunk cpShapeSetElasticity #:ptr (_fun _cpShape-pointer _cpFloat -> _void))
(defchipmunk cpShapeGetFriction #:ptr (_fun _cpShape-pointer -> _cpFloat))
(defchipmunk cpShapeSetFriction #:ptr (_fun _cpShape-pointer _cpFloat -> _void))
(defchipmunk cpShapeGetSurfaceVelocity #:ptr (_fun _cpShape-pointer -> _cpVect))
(defchipmunk cpShapeSetSurfaceVelocity #:ptr (_fun _cpShape-pointer _cpVect -> _cpVect))
(defchipmunk cpShapeGetUserData #:ptr (_fun _cpShape-pointer -> _cpDataPointer))
(defchipmunk cpShapeSetUserData #:ptr (_fun _cpShape-pointer _cpDataPointer -> _void))
(defchipmunk cpShapeGetCollisionType #:ptr (_fun _cpShape-pointer -> _cpCollisionType))
(defchipmunk cpShapeSetCollisionType #:ptr (_fun _cpShape-pointer _cpCollisionType -> _void))
(defchipmunk cpShapeGetGroup #:ptr (_fun _cpShape-pointer -> _cpGroup))
(defchipmunk cpShapeSetGroup #:ptr (_fun _cpShape-pointer _cpGroup -> _void))
(defchipmunk cpShapeGetLayers #:ptr (_fun _cpShape-pointer -> _cpLayers))
(defchipmunk cpShapeSetLayers #:ptr (_fun _cpShape-pointer _cpLayers -> _void))

; ********
; Getters and Setters End
; ********

; ***********************************************
; * End of Chipmunk Shape operation definitions.
; ***********************************************

; -----------------------------------------------

; ***********************************************
; * Start of vector operation definitions.
; ***********************************************

;(define (cpv x y)
;(make-cpVect x y))
(defchipmunk cpv #:ptr (_fun _cpFloat _cpFloat -> _cpVect))
(define (cpvzero) (cpv 0.0 0.0))
(defchipmunk cpvslerp (_fun _cpVect _cpVect _cpFloat -> _cpVect))
(defchipmunk cpvslerpconst (_fun _cpVect _cpVect _cpFloat -> _cpVect))
(defchipmunk cpvstr (_fun _cpVect -> _string))
(defchipmunk cpveql #:ptr (_fun _cpVect _cpVect -> _bool))
(defchipmunk cpvadd #:ptr (_fun _cpVect _cpVect -> _cpVect))
(defchipmunk cpvsub #:ptr (_fun _cpVect _cpVect -> _cpVect))
(defchipmunk cpvneg #:ptr (_fun _cpVect -> _cpVect))
(defchipmunk cpvmult #:ptr (_fun _cpVect _cpFloat -> _cpVect))
(defchipmunk cpvcross #:ptr (_fun _cpVect _cpVect -> _cpFloat))
(defchipmunk cpvdot #:ptr (_fun _cpVect _cpVect -> _cpFloat))
(defchipmunk cpvperp #:ptr (_fun _cpVect -> _cpVect))
(defchipmunk cpvrperp #:ptr (_fun _cpVect -> _cpVect))
(defchipmunk cpvproject #:ptr (_fun _cpVect _cpVect -> _cpVect))
;(defchipmunk cpvforangle #:ptr (_fun _cpFloat -> _cpVect))
;(defchipmunk cpvtoangle #:ptr (_fun _cpVect -> _cpFloat))
(defchipmunk cpvrotate #:ptr (_fun _cpVect _cpVect -> _cpVect))
(defchipmunk cpvunrotate #:ptr (_fun _cpVect _cpVect -> _cpVect))
;(defchipmunk cpvlength #:ptr (_fun _cpVect -> _cpFloat))
(defchipmunk cpvlerp #:ptr (_fun _cpVect _cpVect -> _cpFloat))
(defchipmunk cpvnormalize #:ptr (_fun _cpVect -> _cpVect))
(defchipmunk cpvnormalize_safe #:ptr (_fun _cpVect -> _cpVect))
(defchipmunk cpvclamp #:ptr (_fun _cpVect _cpFloat -> _cpVect))
(defchipmunk cpvlerpconst #:ptr (_fun _cpVect _cpVect -> _cpVect))
(defchipmunk cpvdist #:ptr (_fun _cpVect _cpVect -> _cpFloat))
(defchipmunk cpvdistsq #:ptr (_fun _cpVect _cpVect -> _cpFloat))
(defchipmunk cpvnear #:ptr (_fun _cpVect _cpVect -> _cpFloat))
(defchipmunk cpvlengthsq #:ptr (_fun _cpVect -> _cpFloat))

; ***********************************************
; * End of vector operation definitions.
; ***********************************************

; -----------------------------------------------

; ***********************************************
; * Start of PolyShape operation definitions.
; ***********************************************

(defchipmunk cpBoxShapeNew (_fun _cpBody-pointer _cpFloat _cpFloat -> _cpShape-pointer))
(defchipmunk cpBoxShapeNew2 (_fun _cpBody-pointer _cpBB -> _cpShape-pointer))
(defchipmunk cpPolyShapeGetNumVerts (_fun _cpShape-pointer -> _int))
(defchipmunk cpPolyShapeGetVert (_fun _cpShape-pointer _int -> _cpVect))

; ***********************************************
; * End of PolyShape operation definitions.
; ***********************************************

; -----------------------------------------------

; ***********************************************
; * Start of Arbiter operation definitions.
; ***********************************************
; HACKHACK: Not sure how to express a "_cpShape-pointer-pointer", so this
;  currently just uses a raw pointer. Demo code mallocs an 'atomic
;  _cpShape-pointer (with correct size), and an 'atomic _pointer, then
;  copies the first (ctype-sizeof _pointer) bytes of the _cpShape-pointer
;  to the raw pointer, which is passed in to this. Either this needs to
;  be expressed better/correctly, or it needs to be macro-ized so that
;  you don't need to actually do all that junk.
(defchipmunk cpArbiterGetShapes
#:ptr (_fun _cpArbiter-pointer
              (out1 : (_ptr o _cpShape-pointer))
              (out2 : (_ptr o _cpShape-pointer))
              -> _void
              -> (values out1 out2)))

; ***********************************************
; * End of Arbiter operation definitions.
; ***********************************************

; -----------------------------------------------

; ***********************************************
; * Start of various operation definitions.
; ***********************************************

(defchipmunk cpMomentForPoly (_fun _cpFloat _int _cpVect-pointer _cpVect -> _cpFloat))
(defchipmunk cpAreaForPoly (_fun _int _cpVect-pointer -> _cpFloat))
(defchipmunk cpCentroidForPoly (_fun _int _cpVect-pointer -> _cpVect))
(defchipmunk cpMomentForCircle (_fun _cpFloat _cpFloat _cpFloat _cpVect -> _cpFloat))
(defchipmunk cpMomentForBox (_fun _cpFloat _cpFloat _cpFloat -> _cpFloat))
(defchipmunk cpfabs #:ptr (_fun _cpFloat -> _cpFloat))

; ********
; constraints/util.h
; ********

; FIXME: k_scalar_body does not seem to be defined in
; the dll for some reason. Would be nice to find out
; why. For now, it is implemented in pure Racket.
(define (k_scalar_body *body r n)
  (let ([rcn (cpvcross r n)])
    (+ (cpBody-m_inv *body)
       (* (cpBody-i_inv *body)
          rcn
          rcn))
    )
  )
(define (apply_impulse *body j r)
  (set-cpBody-v! *body (cpvadd (cpBody-v *body)
                               (cpvmult j (cpBody-m_inv *body))))
  (set-cpBody-w! *body (* (cpBody-i_inv *body)
                          (cpvcross r j)))
  _void
  )

; ***********************************************
; * Start of various operation definitions.
; ***********************************************

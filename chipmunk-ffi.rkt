#lang racket/base

(require (for-syntax racket/base)
         
         racket/runtime-path
         
         ffi/unsafe
         ffi/unsafe/define
         
         "ffi-utils.rkt")

(define-runtime-path chipmunk-binary (build-path "bin" (number->string (system-type 'word)) "libchipmunk"))
(define chipmunk (ffi-lib chipmunk-binary))
(define-ffi-definer* defchipmunk chipmunk #:provide provide-protected)

; **********************************
; * CHIPMUNK TYPE DEFINITIONS
; **********************************

(define _cpFloat _double)
(define cpFloat? real?)
(define _cpDataPointer _pointer)
(define _cpKeyPointer _pointer)
(define _size_t _ulong)
(define _cpHashValue _size_t)
(define _cpBool _int)
(define _cpTimeStamp _uint)
(define _cpCollisionType _uint)
(define _cpGroup _uint)
(define _cpLayers _uint)

(define/provide cpTrue 1)
(define/provide cpFalse 0)
(define/provide CP_NO_GROUP 0)
(define/provide CP_ALL_LAYERS 0)

;; STRUCT DEFINITIONS ;;

(define-cstruct* _cpVect
  ([x _cpFloat]
   [y _cpFloat]))

(define _cpBodyVelocityFunc (_fun _pointer _cpVect _cpFloat _cpFloat -> _void))
(define _cpBodyPositionFunc (_fun _pointer _cpFloat -> _void))

(define-cstruct* _cpBody
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
   [data _pointer]
   [v_bias _cpVect]
   [w_bias _cpFloat]))

(define-cstruct* _cpSpace
  ([iterations _int]
   [gravity _cpVect]
   [damping _cpFloat]
   [idleSpeedThreshold _cpFloat]
   [sleepTimeThreshold _cpFloat]
   [collisionSlop _cpFloat]
   [collisionBias _cpFloat]
   [collisionPersistence _cpFloat]
   [enableContactGraph _cpBool]
   [data _pointer]
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

(define-cstruct* _cpShape
  ([body _cpBody-pointer]
   [bb _cpBB]
   [sensor _cpBool]
   [e _cpFloat]
   [u _cpFloat]
   [surface_v _cpVect]
   [data _pointer]
   [collision_type _cpCollisionType]
   [group _cpGroup]
   [layers _cpLayers]))

(define-cstruct* _cpCircleShape
  ([shape _cpShape]
   [c _cpVect]
   [tc _cpVect]
   [r _cpFloat]))

(define-cstruct* _cpSegmentShape
  ([shape _cpShape]
   [a _cpVect]
   [b _cpVect]
   [n _cpVect]
   [ta _cpVect]
   [tb _cpVect]
   [tn _cpVect]
   [r _cpFloat]
   [a_tangent _cpVect]
   [b_tangent _cpVect]))

(define-cstruct* _cpSplittingPlane
  ([n _cpVect]
   [d _cpFloat]))

(define-cstruct* _cpPolyShape
  ([shape _cpShape]
   [numVerts _int]
   [verts _cpVect]
   [tVerts _cpVect]
   [planes _cpSplittingPlane]
   [tPlanes _cpSplittingPlane]))

(define _cpConstraintPreSolveFunc (_fun _cpSpace-pointer -> _void))
(define _cpConstraintPostSolveFunc (_fun _cpSpace-pointer -> _void))

(define-cstruct* _cpConstraint
  ([a _cpBody]
   [b _cpBody]
   [maxForce _cpFloat]
   [errorBias _cpFloat]
   [minForce _cpFloat]
   [preSolve _cpConstraintPreSolveFunc]
   [postSolve _cpConstraintPostSolveFunc]
   [data _pointer]))

(define-cstruct* _cpContactPointSet
  ([count _int]
   [point _cpVect]
   [norml _cpVect]
   [dist _cpFloat]))

(define-cstruct* _cpSegmentQueryInfo
  ([shape _cpShape]
   [t _cpFloat]
   [n _cpVect]))

(define-cstruct* _cpNearestPointQueryInfo
  ([shape _cpShape]
   [p _cpVect]
   [d _cpVect]))

;; FUNCTION TYPE DEFINITIONS ;;

(define _cpCollisionBeginFunc (_fun _cpArbiter-pointer _cpSpace-pointer _pointer -> _cpBool))
(define _cpCollisionPreSolveFunc (_fun _cpArbiter-pointer _cpSpace-pointer _pointer -> _cpBool))
(define _cpCollisionPostSolveFunc (_fun _cpArbiter-pointer _cpSpace-pointer _pointer -> _void))
(define _cpCollisionSeparateFunc (_fun _cpArbiter-pointer _cpSpace-pointer _pointer -> _void))

(define _cpBodyShapeIteratorFunc (_fun _cpBody-pointer _cpShape-pointer _cpDataPointer -> _void))
(define _cpBodyConstraintIteratorFunc (_fun _cpBody-pointer _cpConstraint-pointer _cpDataPointer -> _void))
(define _cpBodyArbiterIteratorFunc (_fun _cpBody-pointer _cpArbiter-pointer _cpDataPointer -> _void))

(define _cpPostStepFunc (_fun _cpSpace-pointer _cpKeyPointer _cpDataPointer -> _void))
(define _cpSpacePointQueryFunc (_fun _cpShape-pointer _cpDataPointer -> _void))
(define _cpSpaceNearestPointQueryFunc (_fun _cpShape-pointer _cpFloat _cpVect _cpDataPointer -> _void))
(define _cpSpaceSegmentQueryFunc (_fun _cpShape-pointer _cpFloat _cpVect _cpDataPointer -> _void))
(define _cpSpaceBBQueryFunc (_fun _cpShape-pointer _cpDataPointer -> _void))
(define _cpSpaceShapeQueryFunc (_fun _cpShape-pointer _cpContactPointSet-pointer _cpDataPointer -> _void))
(define _cpSpaceBodyIteratorFunc (_fun _cpBody-pointer _cpDataPointer -> _void))
(define _cpSpaceShapeIteratorFunc (_fun _cpShape-pointer _cpDataPointer -> _void))
(define _cpSpaceConstraintIteratorFunc (_fun _cpConstraint-pointer _cpDataPointer -> _void))

; **********************************
; * BASIC OPERATIONS DEFINITIONS
; **********************************

(defchipmunk cpfmax #:ptr (_fun _cpFloat _cpFloat -> _cpFloat))
(defchipmunk cpfmin #:ptr (_fun _cpFloat _cpFloat -> _cpFloat))
(defchipmunk cpfabs #:ptr (_fun _cpFloat -> _cpFloat))
(defchipmunk cpfclamp #:ptr (_fun _cpFloat _cpFloat -> _cpFloat))
(defchipmunk cpfclamp01 #:ptr (_fun _cpFloat -> _cpFloat))
(defchipmunk cpflerp #:ptr (_fun _cpFloat _cpFloat _cpFloat -> _cpFloat))
(defchipmunk cpflerpconst #:ptr (_fun _cpFloat _cpFloat _cpFloat -> _cpFloat))

; ***********************************
; * VECTOR MODULE DEFINITIONS
; ***********************************

(defchipmunk cpv #:ptr (_fun _cpFloat _cpFloat -> _cpVect))
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
(defchipmunk cpvforangle #:ptr (_fun _cpFloat -> _cpVect))
(defchipmunk cpvtoangle #:ptr (_fun _cpVect -> _cpFloat))
(defchipmunk cpvrotate #:ptr (_fun _cpVect _cpVect -> _cpVect))
(defchipmunk cpvunrotate #:ptr (_fun _cpVect _cpVect -> _cpVect))
(defchipmunk cpvlength #:ptr (_fun _cpVect -> _cpFloat))
(defchipmunk cpvlerp #:ptr (_fun _cpVect _cpVect -> _cpFloat))
(defchipmunk cpvnormalize #:ptr (_fun _cpVect -> _cpVect))
(defchipmunk cpvnormalize_safe #:ptr (_fun _cpVect -> _cpVect))
(defchipmunk cpvclamp #:ptr (_fun _cpVect _cpFloat -> _cpVect))
(defchipmunk cpvlerpconst #:ptr (_fun _cpVect _cpVect -> _cpVect))
(defchipmunk cpvdist #:ptr (_fun _cpVect _cpVect -> _cpFloat))
(defchipmunk cpvdistsq #:ptr (_fun _cpVect _cpVect -> _cpFloat))
(defchipmunk cpvnear #:ptr (_fun _cpVect _cpVect -> _cpFloat))
(defchipmunk cpvlengthsq #:ptr (_fun _cpVect -> _cpFloat))

(define/provide cpvzero (cpv 0.0 0.0))

; **********************************
; * BOUNDING BOX MODULE DEFINITIONS
; **********************************

(defchipmunk cpBBNew #:ptr (_fun _cpFloat _cpFloat _cpFloat _cpFloat -> _cpBB))

; **********************************
; * BODY MODULE DEFINITIONS
; **********************************

(defchipmunk cpBodyGetMass #:ptr (_fun _cpBody-pointer -> _cpFloat))
(defchipmunk cpBodyGetMoment #:ptr (_fun _cpBody-pointer -> _cpFloat))
(defchipmunk cpBodyGetPos #:ptr (_fun _cpBody-pointer -> _cpVect))
(defchipmunk cpBodyGetVel #:ptr (_fun _cpBody-pointer -> _cpVect))
(defchipmunk cpBodyGetForce #:ptr (_fun _cpBody-pointer -> _cpVect))
(defchipmunk cpBodyGetAngle #:ptr (_fun _cpBody-pointer -> _cpFloat))
(defchipmunk cpBodyGetAngVel #:ptr (_fun _cpBody-pointer -> _cpFloat))
(defchipmunk cpBodyGetTorque #:ptr (_fun _cpBody-pointer -> _cpFloat))
(defchipmunk cpBodyGetVelLimit #:ptr (_fun _cpBody-pointer -> _cpFloat))
(defchipmunk cpBodyGetAngVelLimit #:ptr (_fun _cpBody-pointer -> _cpFloat))
(defchipmunk cpBodyGetSpace #:ptr (_fun _cpBody-pointer -> _cpSpace-pointer))
(defchipmunk cpBodyGetUserData #:ptr (_fun _cpBody-pointer -> _cpDataPointer))

(defchipmunk cpBodySetMass (_fun _cpBody-pointer _cpFloat -> _void))
(defchipmunk cpBodySetMoment (_fun _cpBody-pointer _cpFloat -> _void))
(defchipmunk cpBodySetPos (_fun _cpBody-pointer _cpVect -> _void))
(defchipmunk cpBodySetVel #:ptr (_fun _cpBody-pointer _cpVect -> _void))
(defchipmunk cpBodySetForce #:ptr (_fun _cpBody-pointer _cpVect -> _void))
(defchipmunk cpBodySetAngle (_fun _cpBody-pointer _cpFloat -> _void))
(defchipmunk cpBodySetAngVel #:ptr (_fun _cpBody-pointer _cpFloat -> _void))
(defchipmunk cpBodySetTorque #:ptr (_fun _cpBody-pointer _cpFloat -> _void))
(defchipmunk cpBodySetVelLimit #:ptr (_fun _cpBody-pointer _cpFloat -> _void))
(defchipmunk cpBodySetAngVelLimit #:ptr (_fun _cpBody-pointer _cpFloat -> _void))
(defchipmunk cpBodySetUserData #:ptr (_fun _cpBody-pointer _cpDataPointer -> _void))

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
(defchipmunk cpBodyResetForces (_fun _cpBody-pointer -> _void))
(defchipmunk cpBodyApplyForce (_fun _cpBody-pointer _cpVect _cpVect -> _void))
(defchipmunk cpBodyApplyImpulse (_fun _cpBody-pointer _cpVect _cpVect -> _void))
(defchipmunk cpBodyGetVelAtWorldPoint (_fun _cpBody-pointer _cpVect -> _cpVect))
(defchipmunk cpBodyGetVelAtLocalPoint (_fun _cpBody-pointer _cpVect -> _cpVect))
(defchipmunk cpBodyKineticEnergy #:ptr (_fun _cpBody-pointer -> _cpFloat))
(defchipmunk cpBodyEachShape (_fun _cpBody-pointer _cpBodyShapeIteratorFunc _cpDataPointer -> _void))
(defchipmunk cpBodyEachConstraint (_fun _cpBody-pointer _cpBodyConstraintIteratorFunc _cpDataPointer -> _void))
(defchipmunk cpBodyEachArbiter (_fun _cpBody-pointer _cpBodyArbiterIteratorFunc _cpDataPointer -> _void))

; **********************************
; * SHAPE MODULE DEFINITIONS
; **********************************

(defchipmunk cpShapeGetBody #:ptr (_fun _cpShape-pointer -> _cpBody-pointer))
(defchipmunk cpShapeGetBB #:ptr (_fun _cpShape-pointer -> _cpBB-pointer))
(defchipmunk cpShapeGetSensor #:ptr (_fun _cpShape-pointer -> _cpBool))
(defchipmunk cpShapeGetElasticity #:ptr (_fun _cpShape-pointer -> _cpFloat))
(defchipmunk cpShapeGetFriction #:ptr (_fun _cpShape-pointer -> _cpFloat))
(defchipmunk cpShapeGetSurfaceVelocity #:ptr (_fun _cpShape-pointer -> _cpVect))
(defchipmunk cpShapeGetCollisionType #:ptr (_fun _cpShape-pointer -> _cpCollisionType))
(defchipmunk cpShapeGetGroup #:ptr (_fun _cpShape-pointer -> _cpGroup))
(defchipmunk cpShapeGetLayers #:ptr (_fun _cpShape-pointer -> _cpLayers))
(defchipmunk cpShapeGetSpace #:ptr (_fun _cpShape-pointer -> _cpSpace-pointer))
(defchipmunk cpShapeGetUserData #:ptr (_fun _cpShape-pointer -> _cpDataPointer))

(defchipmunk cpShapeSetBody #:ptr (_fun _cpShape-pointer _cpBody-pointer -> _void))
(defchipmunk cpShapeSetSensor #:ptr (_fun _cpShape-pointer _cpBool -> _void))
(defchipmunk cpShapeSetElasticity #:ptr (_fun _cpShape-pointer _cpFloat -> _void))
(defchipmunk cpShapeSetFriction #:ptr (_fun _cpShape-pointer _cpFloat -> _void))
(defchipmunk cpShapeSetSurfaceVelocity #:ptr (_fun _cpShape-pointer _cpVect -> _cpVect))
(defchipmunk cpShapeSetCollisionType #:ptr (_fun _cpShape-pointer _cpCollisionType -> _void))
(defchipmunk cpShapeSetGroup #:ptr (_fun _cpShape-pointer _cpGroup -> _void))
(defchipmunk cpShapeSetLayers #:ptr (_fun _cpShape-pointer _cpLayers -> _void))
(defchipmunk cpShapeSetUserData #:ptr (_fun _cpShape-pointer _cpDataPointer -> _void))

(defchipmunk cpShapeDestroy (_fun _cpShape-pointer -> _void))
(defchipmunk cpShapeFree (_fun _cpShape-pointer -> _void))
(defchipmunk cpShapeCacheBB (_fun _cpShape-pointer -> _cpBB-pointer))
(defchipmunk cpShapeUpdate (_fun _cpShape-pointer _cpVect _cpVect -> _cpBB-pointer))
(defchipmunk cpShapePointQuery (_fun _cpShape-pointer _cpVect -> _cpBool))
(defchipmunk cpShapeNearestPointQuery (_fun _cpShape-pointer _cpVect _cpNearestPointQueryInfo -> _cpFloat))
(defchipmunk cpShapeSegmentQuery (_fun _cpShape-pointer _cpVect _cpVect _cpSegmentQueryInfo -> _cpBool))
(defchipmunk cpSegmentQueryHitPoint #:ptr (_fun _cpVect _cpVect _cpSegmentQueryInfo -> _cpVect))
(defchipmunk cpSegmentQueryHitDist #:ptr (_fun _cpVect _cpVect _cpSegmentQueryInfo -> _cpFloat))
(defchipmunk cpResetShapeIdCounter (_fun -> _void))

(defchipmunk cpCircleShapeAlloc (_fun -> _cpCircleShape-pointer))
(defchipmunk cpCircleShapeInit (_fun _cpCircleShape-pointer _cpBody-pointer _cpFloat _cpVect -> _cpCircleShape-pointer))
(defchipmunk cpCircleShapeNew (_fun _cpBody-pointer _cpFloat _cpVect -> _cpShape-pointer))
(defchipmunk cpCircleShapeGetOffset (_fun _cpShape-pointer -> _cpVect))
(defchipmunk cpCircleShapeGetRadius (_fun _cpShape-pointer -> _cpFloat))

(defchipmunk cpSegmentShapeAlloc (_fun -> _cpSegmentShape-pointer))
(defchipmunk cpSegmentShapeInit (_fun _cpSegmentShape-pointer _cpBody-pointer _cpVect _cpVect _cpFloat -> _cpSegmentShape-pointer))
(defchipmunk cpSegmentShapeNew (_fun _cpBody-pointer _cpVect _cpVect _cpFloat -> _cpShape-pointer))
(defchipmunk cpSegmentShapeGetA (_fun _cpShape-pointer -> _cpVect))
(defchipmunk cpSegmentShapeGetB (_fun _cpShape-pointer -> _cpVect))
(defchipmunk cpSegmentShapeGetNormal (_fun _cpShape-pointer -> _cpVect))
(defchipmunk cpSegmentShapeGetRadius (_fun _cpShape-pointer -> _cpFloat))

(defchipmunk cpPolyShapeAlloc (_fun -> _cpPolyShape-pointer))
(defchipmunk cpPolyShapeInit (_fun _cpPolyShape-pointer _cpBody-pointer _int _cpVect _cpVect -> _cpPolyShape-pointer))
(defchipmunk cpPolyShapeNew (_fun _cpBody-pointer _int _cpVect _cpVect -> _cpShape-pointer))
(defchipmunk cpPolyValidate (_fun _cpVect _int -> _cpBool))
(defchipmunk cpPolyShapeGetNumVerts (_fun _cpShape-pointer -> _int))
(defchipmunk cpPolyShapeGetVert (_fun _cpShape-pointer _int -> _cpVect))

(defchipmunk cpBoxShapeInit (_fun _cpPolyShape-pointer _cpBody-pointer _cpFloat _cpFloat -> _cpPolyShape-pointer))
(defchipmunk cpBoxShapeInit2 (_fun _cpPolyShape-pointer  _cpBody-pointer _cpBB-pointer -> _cpPolyShape-pointer))
(defchipmunk cpBoxShapeNew (_fun _cpBody-pointer _cpFloat _cpFloat -> _cpShape-pointer))
(defchipmunk cpBoxShapeNew2 (_fun _cpBody-pointer _cpBB-pointer -> _cpShape-pointer))

; **********************************
; * SPACE MODULE DEFINITIONS
; **********************************

(defchipmunk cpSpaceGetIterations #:ptr (_fun _cpSpace-pointer -> _int))
(defchipmunk cpSpaceGetGravity #:ptr (_fun _cpSpace-pointer -> _cpVect))
(defchipmunk cpSpaceGetDamping #:ptr (_fun _cpSpace-pointer -> _cpFloat))
(defchipmunk cpSpaceGetIdleSpeedThreshold #:ptr (_fun _cpSpace-pointer -> _cpFloat))
(defchipmunk cpSpaceGetSleepTimeThreshold #:ptr (_fun _cpSpace-pointer -> _cpFloat))
(defchipmunk cpSpaceGetCollisionSlop #:ptr (_fun _cpSpace-pointer -> _cpFloat))
(defchipmunk cpSpaceGetCollisionBias #:ptr (_fun _cpSpace-pointer -> _cpFloat))
(defchipmunk cpSpaceGetCollisionPersistence #:ptr (_fun _cpSpace-pointer -> _cpTimeStamp))
(defchipmunk cpSpaceGetEnableContactGraph #:ptr (_fun _cpSpace-pointer -> _cpBool)) ; undocumented
(defchipmunk cpSpaceGetCurrentTimeStep #:ptr (_fun _cpSpace-pointer -> _cpFloat))
(defchipmunk cpSpaceGetUserData #:ptr (_fun _cpSpace-pointer -> _cpDataPointer))
(defchipmunk cpSpaceGetStaticBody #:ptr (_fun _cpSpace-pointer -> _cpBody-pointer))

(defchipmunk cpSpaceSetIterations #:ptr (_fun _cpSpace-pointer _int -> _void))
(defchipmunk cpSpaceSetGravity #:ptr (_fun _cpSpace-pointer _cpVect -> _void))
(defchipmunk cpSpaceSetDamping #:ptr (_fun _cpSpace-pointer _cpFloat -> _void))
(defchipmunk cpSpaceSetIdleSpeedThreshold #:ptr (_fun _cpSpace-pointer _cpFloat -> _void))
(defchipmunk cpSpaceSetSleepTimeThreshold #:ptr (_fun _cpSpace-pointer _cpFloat -> _void))
(defchipmunk cpSpaceSetCollisionSlop #:ptr (_fun _cpSpace-pointer _cpFloat -> _void))
(defchipmunk cpSpaceSetCollisionBias #:ptr (_fun _cpSpace-pointer _cpFloat -> _void))
(defchipmunk cpSpaceSetCollisionPersistence #:ptr (_fun _cpSpace-pointer _cpTimeStamp -> _void))
(defchipmunk cpSpaceSetEnableContactGraph #:ptr (_fun _cpSpace-pointer _cpBool -> _void))
(defchipmunk cpSpaceSetUserData #:ptr (_fun _cpSpace-pointer _cpDataPointer -> _void))

(defchipmunk cpSpaceAlloc (_fun -> _cpSpace-pointer))
(defchipmunk cpSpaceInit (_fun _cpSpace-pointer -> _cpSpace-pointer))
(defchipmunk cpSpaceNew (_fun -> _cpSpace-pointer))
(defchipmunk cpSpaceDestroy (_fun _cpSpace-pointer -> _void))
(defchipmunk cpSpaceFree (_fun _cpSpace-pointer -> _void))
(defchipmunk cpSpaceIsLocked #:ptr (_fun _cpSpace-pointer -> _cpBool))
(defchipmunk cpSpaceSetDefaultCollisionHandler (_fun _cpSpace-pointer _cpCollisionBeginFunc _cpCollisionPreSolveFunc _cpCollisionPostSolveFunc _cpCollisionSeparateFunc _pointer -> _void))
(defchipmunk cpSpaceAddCollisionHandler (_fun _cpSpace-pointer _cpCollisionType _cpCollisionType _cpCollisionBeginFunc _cpCollisionPreSolveFunc _cpCollisionPostSolveFunc _cpCollisionSeparateFunc _pointer -> _void))
(defchipmunk cpSpaceRemoveCollisionHandler (_fun _cpSpace-pointer _cpCollisionType _cpCollisionType -> _void))
(defchipmunk cpSpaceAddShape (_fun _cpSpace-pointer _cpShape-pointer -> _cpShape-pointer))
(defchipmunk cpSpaceAddStaticShape (_fun _cpSpace-pointer _cpShape-pointer -> _cpShape))
(defchipmunk cpSpaceAddBody (_fun _cpSpace-pointer _cpBody-pointer -> _cpBody-pointer))
(defchipmunk cpSpaceAddConstraint (_fun _cpSpace-pointer _cpConstraint-pointer -> _cpConstraint-pointer))
(defchipmunk cpSpaceRemoveShape (_fun _cpSpace-pointer _cpShape-pointer -> _void))
(defchipmunk cpSpaceRemoveStaticShape (_fun _cpSpace-pointer _cpShape-pointer -> _cpShape))
(defchipmunk cpSpaceRemoveBody (_fun _cpSpace-pointer _cpBody-pointer -> _void))
(defchipmunk cpSpaceRemoveConstraint (_fun _cpSpace-pointer _cpConstraint-pointer -> _cpConstraint-pointer))
(defchipmunk cpSpaceContainsShape (_fun _cpSpace-pointer _cpShape-pointer -> _cpBool))
(defchipmunk cpSpaceContainsBody (_fun _cpSpace-pointer _cpBody-pointer -> _cpBool))
(defchipmunk cpSpaceContainsConstraint (_fun _cpSpace-pointer _cpConstraint-pointer -> _cpBool))
(defchipmunk cpSpaceAddPostStepCallback (_fun _cpSpace-pointer (_or-null _cpPostStepFunc) _cpKeyPointer _cpDataPointer -> _cpBool))
(defchipmunk cpSpacePointQuery (_fun _cpSpace-pointer _cpVect _cpLayers _cpGroup _cpSpacePointQueryFunc _cpDataPointer -> _void))
(defchipmunk cpSpacePointQueryFirst (_fun _cpSpace-pointer _cpVect _cpLayers _cpGroup -> (_or-null _cpShape-pointer)))
(defchipmunk cpSpaceNearestPointQuery (_fun _cpSpace-pointer _cpVect _cpFloat _cpLayers _cpGroup _cpSpaceNearestPointQueryFunc _cpDataPointer -> _void))
(defchipmunk cpSpaceNearestPointQueryNearest (_fun _cpSpace-pointer _cpVect _cpFloat _cpLayers _cpGroup _cpSpaceNearestPointQueryFunc _cpDataPointer -> _void))
(defchipmunk cpSpaceSegmentQuery (_fun _cpSpace-pointer _cpVect _cpVect _cpLayers _cpGroup _cpGroup _cpSpaceSegmentQueryFunc -> _void))
(defchipmunk cpSpaceSegmentQueryFirst (_fun _cpSpace-pointer _cpVect _cpVect _cpLayers _cpGroup _cpSegmentQueryInfo -> _cpShape-pointer))
(defchipmunk cpSpaceBBQuery (_fun _cpSpace-pointer _cpBB-pointer _cpLayers _cpGroup _cpSpaceBBQueryFunc _cpDataPointer -> _void))
(defchipmunk cpSpaceShapeQuery (_fun _cpSpace-pointer _cpShape-pointer _cpSpaceShapeQueryFunc _cpDataPointer -> _cpBool))
(defchipmunk cpSpaceActivateShapesTouchingShape (_fun _cpSpace-pointer _cpShape-pointer -> _void))
(defchipmunk cpSpaceEachBody (_fun _cpSpace-pointer _cpSpaceBodyIteratorFunc _cpDataPointer -> _void))
(defchipmunk cpSpaceEachShape (_fun _cpSpace-pointer _cpSpaceShapeIteratorFunc _cpDataPointer -> _void))
(defchipmunk cpSpaceEachConstraint (_fun _cpSpace-pointer _cpSpaceConstraintIteratorFunc _cpDataPointer -> _void))
(defchipmunk cpSpaceReindexStatic (_fun _cpSpace-pointer -> _void))
(defchipmunk cpSpaceReindexShape (_fun _cpSpace-pointer _cpShape-pointer -> _void))
(defchipmunk cpSpaceReindexShapesForBody (_fun _cpSpace-pointer _cpBody-pointer -> _void))
(defchipmunk cpSpaceUseSpatialHash (_fun _cpSpace-pointer _cpFloat _int -> _void))
(defchipmunk cpSpaceStep (_fun _cpSpace-pointer _cpFloat -> _void))

; **********************************
; * ARBITER MODULE DEFINITIONS
; **********************************

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

; **********************************
; * MISCELLANEOUS OPERATIONS
; **********************************

(defchipmunk cpEnableSegmentToSegmentCollisions (_fun -> _void))
(defchipmunk cpMomentForCircle (_fun _cpFloat _cpFloat _cpFloat _cpVect -> _cpFloat))
(defchipmunk cpAreaForCircle (_fun _cpFloat _cpFloat -> _cpFloat))
(defchipmunk cpMomentForSegment (_fun _cpFloat _cpVect _cpVect -> _cpFloat))
(defchipmunk cpAreaForSegment (_fun _cpVect _cpVect _cpFloat -> _cpFloat))
(defchipmunk cpMomentForPoly (_fun _cpFloat _int _cpVect-pointer _cpVect -> _cpFloat))
(defchipmunk cpAreaForPoly (_fun _int _cpVect-pointer -> _cpFloat))
(defchipmunk cpCentroidForPoly (_fun _int _cpVect-pointer -> _cpVect))
(defchipmunk cpRecenterPoly (_fun _int _cpVect -> _void))
(defchipmunk cpMomentForBox (_fun _cpFloat _cpFloat _cpFloat -> _cpFloat))
(defchipmunk cpMomentForBox2 (_fun _cpFloat _cpBB-pointer -> _cpFloat))
(defchipmunk cpConvexHull (_fun _cpVect _cpVect _int _cpFloat -> _int))

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

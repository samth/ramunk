#lang racket

(require "lib.rkt")

; **********************************
; * SPACE MODULE DEFINITIONS
; **********************************


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
(defchipmunk cpSpaceAddStaticShape (_fun _cpSpace-pointer _cpShape-pointer -> _cpShape-pointer))
(defchipmunk cpSpaceAddBody (_fun _cpSpace-pointer _cpBody-pointer -> _cpBody-pointer))
(defchipmunk cpSpaceAddConstraint (_fun _cpSpace-pointer _cpConstraint-pointer -> _cpConstraint-pointer))
(defchipmunk cpSpaceRemoveShape (_fun _cpSpace-pointer _cpShape-pointer -> _void))
(defchipmunk cpSpaceRemoveStaticShape (_fun _cpSpace-pointer _cpShape-pointer -> _cpShape-pointer))
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
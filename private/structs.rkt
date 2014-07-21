#lang racket

(require "lib.rkt")

(provide (all-defined-out))
         

; **********************************
; * SPACE MODULE DEFINITIONS
; **********************************

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


(define-cstruct _cpArbiter
  ([e _cpFloat]
   [u _cpFloat]
   [surface_vr _cpVect]))

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

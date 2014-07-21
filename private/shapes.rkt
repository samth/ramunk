#lang racket

(require "lib.rkt"
         "structs.rkt"
         "body.rkt")

; **********************************
; * SHAPE MODULE DEFINITIONS
; **********************************

(defchipmunk cpShapeGetBody (_fun _cpShape-pointer -> _cpBody-pointer))
(defchipmunk cpShapeGetBB (_fun _cpShape-pointer -> _cpBB-pointer))
(defchipmunk cpShapeGetSensor (_fun _cpShape-pointer -> _cpBool))
(defchipmunk cpShapeGetElasticity (_fun _cpShape-pointer -> _cpFloat))
(defchipmunk cpShapeGetFriction (_fun _cpShape-pointer -> _cpFloat))
(defchipmunk cpShapeGetSurfaceVelocity (_fun _cpShape-pointer -> _cpVect))
(defchipmunk cpShapeGetCollisionType (_fun _cpShape-pointer -> _cpCollisionType))
(defchipmunk cpShapeGetSpace (_fun _cpShape-pointer -> _cpSpace-pointer))
(defchipmunk cpShapeGetUserData (_fun _cpShape-pointer -> _cpDataPointer))

(defchipmunk cpShapeSetBody (_fun _cpShape-pointer _cpBody-pointer -> _void))
(defchipmunk cpShapeSetSensor (_fun _cpShape-pointer _cpBool -> _void))
(defchipmunk cpShapeSetElasticity (_fun _cpShape-pointer _cpFloat -> _void))
(defchipmunk cpShapeSetFriction (_fun _cpShape-pointer _cpFloat -> _void))
(defchipmunk cpShapeSetSurfaceVelocity (_fun _cpShape-pointer _cpVect -> _cpVect))
(defchipmunk cpShapeSetCollisionType (_fun _cpShape-pointer _cpCollisionType -> _void))
(defchipmunk cpShapeSetUserData (_fun _cpShape-pointer _cpDataPointer -> _void))

(defchipmunk cpShapeDestroy (_fun _cpShape-pointer -> _void))
(defchipmunk cpShapeFree (_fun _cpShape-pointer -> _void))
(defchipmunk cpShapeCacheBB (_fun _cpShape-pointer -> _cpBB-pointer))
(defchipmunk cpShapeUpdate (_fun _cpShape-pointer _cpVect _cpVect -> _cpBB-pointer))
(defchipmunk cpShapePointQuery (_fun _cpShape-pointer _cpVect -> _cpBool))
(defchipmunk cpShapeSegmentQuery (_fun _cpShape-pointer _cpVect _cpVect _cpSegmentQueryInfo -> _cpBool))

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
(defchipmunk cpPolyShapeGetVert (_fun _cpShape-pointer _int -> _cpVect))

(defchipmunk cpBoxShapeInit (_fun _cpPolyShape-pointer _cpBody-pointer _cpFloat _cpFloat -> _cpPolyShape-pointer))
(defchipmunk cpBoxShapeInit2 (_fun _cpPolyShape-pointer  _cpBody-pointer _cpBB-pointer -> _cpPolyShape-pointer))
(defchipmunk cpBoxShapeNew (_fun _cpBody-pointer _cpFloat _cpFloat -> _cpShape-pointer))
(defchipmunk cpBoxShapeNew2 (_fun _cpBody-pointer _cpBB-pointer -> _cpShape-pointer))
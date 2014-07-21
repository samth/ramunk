#lang racket

(require "lib.rkt"
         "structs.rkt")

(provide cpvzero)

; ***********************************
; * VECTOR MODULE DEFINITIONS
; ***********************************



(defchipmunk cpv (_fun _cpFloat _cpFloat -> _cpVect))
(defchipmunk cpvslerp (_fun _cpVect _cpVect _cpFloat -> _cpVect))
(defchipmunk cpvslerpconst (_fun _cpVect _cpVect _cpFloat -> _cpVect))
(defchipmunk cpveql (_fun _cpVect _cpVect -> _bool))
(defchipmunk cpvadd (_fun _cpVect _cpVect -> _cpVect))
(defchipmunk cpvsub (_fun _cpVect -> _cpVect))
(defchipmunk cpvneg (_fun _cpVect -> _cpVect))
(defchipmunk cpvmult (_fun _cpVect _cpFloat -> _cpVect))
(defchipmunk cpvcross (_fun _cpVect _cpVect -> _cpFloat))
(defchipmunk cpvdot (_fun _cpVect _cpVect -> _cpFloat))
(defchipmunk cpvperp (_fun _cpVect -> _cpVect))
(defchipmunk cpvrperp (_fun _cpVect -> _cpVect))
(defchipmunk cpvproject (_fun _cpVect _cpVect -> _cpVect))
(defchipmunk cpvforangle (_fun _cpFloat -> _cpVect))
(defchipmunk cpvtoangle (_fun _cpVect -> _cpFloat))
(defchipmunk cpvrotate (_fun _cpVect _cpVect -> _cpVect))
(defchipmunk cpvunrotate (_fun _cpVect _cpVect -> _cpVect))
(defchipmunk cpvlength (_fun _cpVect -> _cpFloat))
(defchipmunk cpvlerp (_fun _cpVect _cpVect -> _cpFloat))
(defchipmunk cpvnormalize (_fun _cpVect -> _cpVect))
(defchipmunk cpvclamp (_fun _cpVect _cpFloat -> _cpVect))
(defchipmunk cpvlerpconst (_fun _cpVect _cpVect -> _cpVect))
(defchipmunk cpvdist (_fun _cpVect _cpVect -> _cpFloat))
(defchipmunk cpvdistsq (_fun _cpVect _cpVect -> _cpFloat))
(defchipmunk cpvnear (_fun _cpVect _cpVect -> _cpFloat))
(defchipmunk cpvlengthsq (_fun _cpVect -> _cpFloat))


(define cpvzero (cpv 0.0 0.0))

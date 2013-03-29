#lang racket/base

(require (for-syntax racket/base racket/syntax)
         
         racket/vector)

(provide vector-accessor
         vector-mutator
         vector-overwrite!
         vector-fold
         vector-merge!
         vector-merge
         
         define-vector-space
         
         vector-axis-add!
         vector-axis-sub!
         vector-axis-mul!
         vector-axis-div!
         
         vector-add!
         vector-sub!
         vector-mul!
         vector-div!
         
         exact-vector->inexact-vector!
         vector-scale!
         vector-shrink!
         
         vector-add
         vector-mul
         vector-sub
         vector-div
         
         exact-vector->inexact-vector
         vector-scale
         vector-shrink
         
         vector-dot-product
         vector-magnitude
         vector-distance
         vector-angle
         
         origin x y
         
         vector-x
         vector-y
         
         set-vector-x!
         set-vector-y!
         
         vector-angle
         )

;; General vector utilities

(define (vector-accessor i)
  (lambda (v) (vector-ref v i)))

(define (vector-mutator i)
  (lambda (v x) (vector-set! v i x)))

(define (vector-ref* vectors i)
  (map (vector-accessor i) vectors))

(define (vector-overwrite! v . values)
  (do ((i 0 (+ i 1))
       (lst values (cdr lst)))
    ((null? lst) v)
    (vector-set! v i (car lst))))

(define (rebuild-vector! v proc)
  (do ((i (- (vector-length v) 1) (- i 1)))
    ((= (+ i 1) 0) v)
    (vector-set! v i (proc i))))

(define (vector-fold op init . vectors)
  (let loop
    ((i (- (vector-length (car vectors)) 1)))
    (if (=  (+ i 1) 0)
        init
        (apply op (loop (- i 1))
               (vector-ref* vectors i)))))

(define (vector-merged-ref vectors i op)
  (apply op (vector-ref* vectors i)))

(define (vector-merge! op vectors)
  (rebuild-vector! (car vectors)
                   (lambda (i)
                     (vector-merged-ref vectors i op))))

(define (vector-merge op vectors)
  (build-vector (vector-length (car vectors))
                (lambda (i)
                  (vector-merged-ref vectors i op))))

;; Wiskundige uitbreidingen op vectoren

(define vector-dimension vector-length)

; eenplaatsige operaties

(define (exact-vector->inexact-vector v)
  (vector-map exact->inexact v))

(define (vector-scale v c)
  (vector-map (lambda (x) (* x c)) v))

(define (vector-shrink v c)
  (vector-scale v (/ 1.0 c)))

(define (exact-vector->inexact-vector! v)
  (vector-map! exact->inexact v))

(define (vector-scale! v c)
  (vector-map! (lambda (x) (* x c)) v))

(define (vector-shrink! v c)
  (vector-scale! v (/ 1.0 c)))

; operaties op een specifieke as

(define (vector-map-axis! v axis proc)
  (vector-set! v axis (proc (vector-ref v axis))))

(define (vector-map-axis v axis proc)
  (let ((new-v (vector-copy v)))
    (vector-map-axis! v axis proc)))

(define (make-axis-operator op)
  (lambda (v axis . args)
    (vector-map-axis! v axis (lambda (c) (apply op c args)))))

(define vector-axis-add! (make-axis-operator +))
(define vector-axis-sub! (make-axis-operator -))
(define vector-axis-mul! (make-axis-operator *))
(define vector-axis-div! (make-axis-operator /))

; meerplaatsige operaties

(define (vector-add! . vectors)
  (vector-merge! + vectors))

(define (vector-sub! . vectors)
  (vector-merge! - vectors))

(define (vector-mul! . vectors)
  (vector-merge! * vectors))

(define (vector-div! . vectors)
  (vector-merge! / vectors))

(define (vector-add . vectors)
  (vector-merge + vectors))

(define (vector-sub . vectors)
  (vector-merge - vectors))

(define (vector-mul . vectors)
  (vector-merge * vectors))

(define (vector-div . vectors)
  (vector-merge / vectors))

; accumulatoren op vectoren

(define (vector-dot-product a b)
  (vector-fold + 0.0 (vector-mul a b)))

(define (vector-magnitude v)
  (sqrt (vector-dot-product v v)))

(define (vector-distance a b)
  (abs (vector-magnitude (vector-sub a b))))

(define-syntax (define-vector-space stx)
  (syntax-case stx ()
    [(_ name axis-exps)
     (datum->syntax
      #'name
      (list*
       'begin
       (let loop
         ((axis-ids (syntax-e #'axis-exps))
          (idx 0))
         (if (null? axis-ids)
             '()
             (let ((axis-id (car axis-ids)))
               (list*
                #`(define #,axis-id #,idx)
                #`(define #,(format-id #'name "vector-~a" axis-id) (vector-accessor #,idx))
                #`(define #,(format-id #'name "set-vector-~a!" axis-id) (vector-mutator #,idx))
                (loop (cdr axis-ids) (+ idx 1))))))))]))

;; Positie en rotatie als vectoren

(define-vector-space 2d (x y))

(define origin (vector 0.0 0.0))

(define (vector-angle v)
  (atan (vector-y v) (vector-x v)))

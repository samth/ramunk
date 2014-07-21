#lang racket

(require (for-syntax racket/base racket/syntax racket/list)
         racket/runtime-path
         ffi/unsafe
         ffi/unsafe/define)

(provide  (all-from-out ffi/unsafe)
          
          defchipmunk
          define-cstruct*
          
          _cpFloat
          _cpDataPointer
          _cpKeyPointer
          _size_t
          _cpHashValue
          _cpBool
          _cpTimeStamp
          _cpCollisionType
          _cpGroup
          _cpLayers)



(begin-for-syntax
  
  (define (identifier->string id)
    (symbol->string (syntax-e id)))
  
  (define (string->identifier ctx str)
    (datum->syntax ctx (string->symbol str)))
  
  (define (strings->identifiers ctx strs)
    (map (lambda (str)
           (string->identifier ctx str))
         strs))
  
  (define (c-name-chunks id)
    (regexp-split #rx"(?<!^)(?=[A-Z])(?![A-Z][A-Z][a-z])"
                  (identifier->string id)))
  
  (define (c-lower-name-chunks id)
    (map string-downcase (c-name-chunks id)))
  
  (define (string-merge infix . strs)
    (foldl (lambda (str result)
             (string-append result infix str))
           (car strs)
           (cdr strs)))
  
  (define (c-token->racket-token id)
    (apply string-merge "-" (c-lower-name-chunks id)))
  
  (define (c-name->racket-method-name id)
    (apply string-merge "-" (cddr (c-lower-name-chunks id))))
  
  (define (make-id-transformer proc)
    (lambda (id)
      (datum->syntax
       id
       (string->symbol
        (proc id)))))
  
  (define c-id->racket-id
    (make-id-transformer c-token->racket-token)))

(define-syntax (provide-racket stx)
  (syntax-case stx ()
    [(x c ...)
     #`(provide (rename-out 
                 #,@(map (lambda (id)
                           #`(#,id #,(c-id->racket-id id)))
                         (syntax-e #'(c ...)))))]))

(define-syntax (define-cstruct* stx)
  (syntax-case stx ()
    [(_ name fields options ...)
     (let ((cname (string->identifier #'name (substring (identifier->string #'name) 1))))
       #`(begin
           (define-cstruct name fields options ...)
           #,@(map (lambda (field-stx)
                     (let ((field-id (first (syntax-e field-stx))))
                       #`(provide #,(format-id #'name "~a-~a" cname field-id)
                                  #,(format-id #'name "set-~a-~a!" cname field-id))))
                   (syntax-e #'fields))
           (provide #,(format-id cname "~a?" cname)
                    #,(format-id cname "_~a" cname)
                    #,(format-id cname "make-~a" cname))))]))

(define-runtime-path chipmunk-binary (build-path "bin" (string-append "libchipmunk-x" (number->string (system-type 'word)))))
(define chipmunk (ffi-lib chipmunk-binary))
(define-ffi-definer defchipmunk chipmunk
  #:provide provide-racket)


(define _cpFloat _double)
(define _cpDataPointer _pointer)
(define _cpKeyPointer _pointer)
(define _size_t _ulong)
(define _cpHashValue _size_t)
(define _cpBool _int)
(define _cpTimeStamp _uint)
(define _cpCollisionType _uint)
(define _cpGroup _uint)
(define _cpLayers _uint)

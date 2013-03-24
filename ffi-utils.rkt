#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/keyword
                     racket/list)
         
         ffi/unsafe
         ffi/unsafe/define)

(provide  (for-syntax identifier->string
                      string->identifier
                      
                      options-search-row
                      options-search-value
                      options-remove
                      options-flatten
                      
                      c-name-chunks
                      c-lower-name-chunks
                      
                      c-id->racket-id)
          
          define/provide
          define-cstruct*
          define-ffi-definer*)

(define-syntax define/provide
  (syntax-rules ()
    [(_ (name args ... . rest) expr ...)
     (begin (define (name args ... . rest) expr ...)
            (provide name))]
    [(_ (name args ...) expr ...)
     (begin (define (name args ...) expr ...)
            (provide name))]
    [(_ name val)
     (begin (define name val)
            (provide name))]))

(begin-for-syntax
  
  (define (options-search-row options keyword)
    (options-select-row options keyword #:default #f))
  
  (define (options-search-value options keyword)
    (options-select-value options keyword #:default #f))
  
  (define (options-remove options . keywords)
    (filter-not (lambda (option)
                  (member (first option) keywords))
                options))
  
  (define (options-flatten options)
    (flatten (map cdr options)))
  
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
  
  (define (c-name->racket-name id)
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
    (make-id-transformer c-name->racket-name)))

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
                    #,(format-id cname "make-~a" cname))))]))

(define (make-pointer-wrapper type)
  (lambda (ffi)
    (function-ptr ffi type)))

(define (make-initializer proc)
  (lambda (ffi)
    (lambda args
      (let ((instance (apply ffi args)))
        (proc instance)
        instance))))

(define-for-syntax ffi-keywords-list
  (list
   
   (list '#:ptr)
   
   (list '#:c-id check-identifier)
   (list '#:wrap check-expression)
   (list '#:make-fail check-expression)
   (list '#:fail check-expression)
   
   ))

(define-for-syntax (parse-ffi-keyword-options options-stx)
  (parse-keyword-options
   options-stx
   ffi-keywords-list
   #:incompatible '((#:c-id #:ptr))
   ;#:on-not-in-table void ; broken
   ))

(define-syntax (parse-ffi-definer stx)
  (syntax-case stx ()
    [(_ ffi-definer name options ...)
     (identifier? #'name)
     (let-values
         ([(bind-options type-expr)
           (parse-ffi-keyword-options #'(options ...))])
       (if (options-search-row bind-options '#:ptr)
           #`(parse-ffi-definer
              ffi-definer name
              #:c-id #,(format-id #'name "_~a" #'name)
              #:wrap (lambda (ffi) (function-ptr ffi #,@type-expr))
              #,@(options-flatten (options-remove bind-options '#:ptr))
              _pointer)
           (let ((init-exps (map first (options-select bind-options '#:init)))
                 (wrap-exps (map first (options-select bind-options '#:wrap))))
             #`(ffi-definer
                name #,@type-expr
                #:wrap #,(if (null? init-exps)
                             #`(compose #,@wrap-exps)
                             #`(compose
                                (make-initializer (compose #,@init-exps))
                                #,@wrap-exps))
                #,@(options-flatten (options-remove bind-options '#:wrap '#:init))))))]))

(define-syntax (define-ffi-definer* stx)
  (syntax-case stx ()
    [(_ define-id ffi-lib-expr options ...)
     (let ((real-ffi-definer
            (generate-temporary
             (format-id #'define-id "~a-helper" #'define-id))))
       #`(begin
           (define-ffi-definer #,real-ffi-definer ffi-lib-expr options ...)
           (define-syntax-rule (define-id name type specification (... ...))
             (parse-ffi-definer #,real-ffi-definer name type specification (... ...)))))]))

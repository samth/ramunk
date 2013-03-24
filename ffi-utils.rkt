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
                      
                      c-id->racket-id
                      c-id->racket-method-id)
          
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
    (regexp-split #rx"(?<!^)(?=[A-Z])" 
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
    (make-id-transformer c-name->racket-name))
  
  (define c-id->racket-method-id
    (make-id-transformer c-name->racket-method-name))
  (define (cstruct-id->initializer-id cstruct-id)
    (format-id cstruct-id "init-~a" cstruct-id))
  
  (define (cstruct-id->constrcutor-id cstruct-id)
    (format-id cstruct-id "make-~a" cstruct-id))
  
  (define (cstruct-id->immobile-constructor-id cstruct-id)
    (format-id cstruct-id "new-~a" cstruct-id))
  
  (define (cproc-id->cstruct-id cproc-id)
    (let ((chunks (c-name-chunks cproc-id)))
      (string->identifier
       cproc-id
       (string-append (first chunks)
                      (second chunks))))))

(define the-empty-immobile-cell (malloc-immobile-cell (void)))

(define-syntax (define-cstruct* stx)
  
  (define (get-field-id field-stx)
    (first (syntax-e field-stx)))
  
  (define (get-field-type field-stx)
    (second (syntax-e field-stx)))
  
  (define (get-field-options field-stx)
    (datum->syntax field-stx (rest (rest (syntax-e field-stx)))))
  
  (define cstruct-keywords-list
    (list
     (list '#:public)))
  
  (define field-keywords-list
    (list
     (list '#:immobile)
     (list '#:public)))
  
  (define (parse-cstruct-keyword-options option-stx)
    (parse-keyword-options
     option-stx
     cstruct-keywords-list
     #:no-duplicates? #t))
  
  (define (parse-field-keyword-options option-stx)
    (parse-keyword-options/eol
     option-stx
     field-keywords-list
     #:no-duplicates? #t))
  
  (syntax-case stx ()
    [(_ name fields options ...)
     (let ((cname (string->identifier #'name (substring (identifier->string #'name) 1))))
       
       (define (field-id->immobile-getter-id field-id)
         (format-id #'name "~a-immobile-~a" cname field-id))
       
       (define (field-id->immobile-setter-id field-id)
         (format-id #'name "set-~a-immobile-~a!" cname field-id))
       
       (define (field-id->getter-id field-id)
         (format-id #'name "~a-~a" cname field-id))
       
       (define (field-id->setter-id field-id)
         (format-id #'name "set-~a-~a!" cname field-id))
       
       (let
           ((cstruct-initializer
             (cstruct-id->initializer-id cname))
            (cstruct-constructor
             (cstruct-id->constrcutor-id cname))
            (cstruct-immobile-constructor
             (cstruct-id->immobile-constructor-id cname)))
         
         (let-values ([(cstruct-options cstruct-properties)
                       (parse-cstruct-keyword-options #'(options ...))])
           
           (let ((cstruct-public-option (options-search-row cstruct-options '#:public)))
             
             (define (normalize-field field-stx)
               (syntax-case field-stx ()
                 [(field-id field-type options ...)
                  (and (identifier? #'field-id)
                       (identifier? #'field-type))
                  #'(field-id field-type)]))
             
             (define (compile-field-options field-stx field-options)
               (let ((field-id (get-field-id field-stx)))
                 (let
                     ((field-getter
                       (field-id->getter-id field-id))
                      (field-setter
                       (field-id->setter-id field-id))
                      (field-immobile-option
                       (options-search-row field-options '#:immobile))
                      (field-public-option
                       (options-search-row field-options '#:public)))
                   
                   (cond
                     (field-immobile-option
                      (let
                          ((field-immobile-getter
                            (field-id->immobile-getter-id field-id))
                           (field-immobile-setter
                            (field-id->immobile-setter-id field-id)))
                        #`(begin
                            (define (#,field-immobile-getter a-cstruct)
                              (ptr-ref (#,field-getter a-cstruct) _racket))
                            (define (#,field-immobile-setter a-cstruct val)
                              (free-immobile-cell (#,field-getter a-cstruct))
                              (#,field-setter a-cstruct (malloc-immobile-cell val)))
                            #,(when (or cstruct-public-option field-public-option)
                                #`(provide #,field-immobile-getter
                                           #,field-getter
                                           #,field-immobile-setter
                                           #,field-setter)))))
                     ((or cstruct-public-option field-public-option)
                      #`(provide #,field-getter #,field-setter))))))
             
             (define (compile-field-initializer field-stx field-options)
               (let ((immobile-option (options-search-row field-options '#:immobile)))
                 (and immobile-option
                      #`(#,(field-id->setter-id (get-field-id field-stx))
                         instance
                         the-empty-immobile-cell))))
             
             (let*
                 ((fields (syntax-e #'fields))
                  (fields-options
                   (map parse-field-keyword-options (map get-field-options fields))))
               
               #`(begin
                   
                   (define-cstruct name
                     #,(map normalize-field fields)
                     #,@cstruct-properties)
                   
                   #,@(map compile-field-options
                           fields fields-options)
                   
                   #,(when cstruct-public-option
                       #`(provide #,(format-id cname "~a?" cname)))
                   
                   (define (#,cstruct-initializer instance)
                     #,@(filter-map compile-field-initializer
                                    fields fields-options)
                     instance)
                   
                   ;(define (#,cstruct-immobile-constructor . args)
                   ;  (#,cstruct-initializer (apply #,cstruct-constructor args)))
                   
                   ))))))]))

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

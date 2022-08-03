#lang racket
(provide (except-out (all-from-out racket)
                     #%top #%app))

;;;
;;; Field and Method Access, Method Calls
;;;

;; Goal
;; ----

;; 1. o.f                       access field f of object o
;; 2. o.f1.f2                   access field f2 of object o.f1

;; 3. (o .m a ...)              invoke method m on object o wth arguments a...
;; 4. (o .m1 a1 ... .m2 a2 ...) same as ((o .m1 a1 ...) .m2 a2 ...)
;; 5. (o.m a ...)               invoke method m on object o wth arguments a... 
;; 6. (o.m a ... .m1 a1 ...)    invoke method m1 on resultof object (o.m a ...) with arguments a2 ... 


;; Ad 1. and 3.
;;   - use #%top to rewrite
;;       o.f       into (get-field f o)
;;       o.f1.f2   into (get-field f2 (get-field f1 o))

;; Ad 2. and 3.
;;   - use #%app to rewrite applications with method calls


;; Implementation
;; --------------

;; We are going to define our own versions of #%app and #%top named .app and .top.
;; We will export them under the names #%app and #%top.

(provide (rename-out [.app #%app]))
(provide (rename-out [.top #%top]))


;; Utilities for working with identifiers

(require (for-syntax racket/base racket/syntax syntax/parse syntax/stx
                     racket/string racket/list))
(require racket/match racket/format)

(begin-for-syntax
  (define (identifier->string id)
    (cond
      [(string? id)                           id]
      [(symbol? id) (symbol->string           id)]
      [else         (symbol->string (syntax-e id))]))

  (define (string->identifier ctx src str)
    (when (syntax? str) (set! str (syntax-e str)))
    (datum->syntax ctx (string->symbol str) src))

  (define (identifier-append ctx srcloc . ids)
    (define (-> x) (datum->syntax ctx x srcloc))
    (-> (string->symbol (string-append* (map identifier->string ids)))))

  (define (identifier-append* ctx srcloc ids)
    (apply identifier-append ctx srcloc ids))

  (define (identifier-split id sep)
    ; Like string-split, but for identifiers.
    ; Returns a syntax object with a list of strings.
    (define str    (identifier->string id))
    (define parts  (string-split str sep))
    (define ctx    id)
    (define srcloc id)
    (define (-> x) (datum->syntax ctx x srcloc))
    (-> (map -> parts)))

  (define (identifier-contains? id contained-str)
    (string-contains? (identifier->string id)
                      contained-str))

  (define (identifier-begins-with? id start-ch)
    (unless (char? start-ch)
      (error 'identifier-begins-with? "expected a character as start-ch"))
    (define str (identifier->string id))
    (and (not (zero? (string-length str)))
         (eqv? (string-ref str 0) start-ch)))

  (define (dot-identifier? id)
    (eqv? (string-ref (identifier->string id) 0) #\.))

  (define (method-identifier? id)
    (and (identifier-begins-with? id #\.)
         (not (identifier-contains? (identifier-drop-start id) "."))))

  (define (identifier-drop-start id)
    (define str (identifier->string id))
    (define sym (string->symbol (substring str 1 (string-length str))))
    (datum->syntax id sym id id))


  (define (identifiers->dotted-name id ids)
    (when (syntax? ids) (set! ids (syntax->list ids)))
    (identifier-append* id id (add-between ids #'|.|)))

  (define (dotted-identifier->identifiers id)
    (define (-> x) (datum->syntax id x id))
    (define strs (string-split (identifier->string #'id) "."))
    (define syms (map string->symbol strs))
    (map -> syms)))

;; Syntax classes makes parsing our new forms easier.

(begin-for-syntax
  (define-syntax-class name ; an identifier without dots
    (pattern name:id
             #:when (not (identifier-contains? #'name "."))))

  (define-syntax-class method ; an identifier that begins with a dot
    (pattern dot-name:id
             #:when (identifier-begins-with? #'dot-name #\.)
             #:attr name (identifier-drop-start #'dot-name)))

  (define-syntax-class non-method
    (pattern (~not expr:method)))

  (define-syntax-class non-method-id ; an identifier that does not begin with a dot
    (pattern name:id
             #:when (not (identifier-begins-with? #'name #\.))))
             
  (define-syntax-class dotted-name
    (pattern dotted-name:id
             #:when (identifier-contains? #'dotted-name ".")
             #:attr names (identifier-split #'dotted-name "."))))

;;;
;;; #%top 
;;;

;; Ad 1. and 3.
;;   - use #%top to rewrite
;;       o.f       into (get-field f o)
;;       o.f1.f2   into (get-field f2 (get-field f1 o))

(define-syntax (.top stx)
  ; like #%top, but dotted identifiers are field or method access
  (syntax-parse stx
    ; Names (identifiers without dots) resolve as usual.
    [(_.top . id:name)
     #'(#%top . id)]
    
    [(_.top . id:dotted-name)
     (cond
       ; If the name beings with a . it is not a field or method access.
       [(identifier-begins-with? #'id #\.)
        #'(#%top . id)]

       [(identifier-contains? #'id ".")
        ; Double dots in an identifier is not allowed
        (when (identifier-contains? #'id "..")
          (raise-syntax-error '.top "two consecutive dots not allowed" #'id))
        
        (define (-> x) (string->identifier stx #'id x))
        (syntax-parse (stx-map -> #'id.names)
          ; fast path (could be omitted)
          [(o f)      (syntax/loc #'id
                        (get-field f o))]
          [(o . fs)  (define (loop o fs)
                        (syntax-parse fs
                          [()       o]
                          [(f . fs) (loop (with-syntax ([o o])
                                            (syntax/loc #'id
                                              (get-field f o)))
                                          #'fs)]))
                      (loop #'o #'fs)])])]
    [(_.top . id)
     ; better safe than sorry
     #'(#%top . id)]))

;;;
;;; #%app
;;;

;; 3. (o .m a ...)              invoke method m on object o wth arguments a...
;; 4. (o .m1 a1 ... .m2 a2 ...) same as ((o .m1 a1 ...) .m2 a2 ...)
;; 5. (o.m a ...)               invoke method m on object o with arguments a... 
;; 6. (o.m a ... .m1 a1 ...)    invoke method m1 on resultof object (o.m a ...) with arguments a2 ... 

(define-syntax (.app stx)
  (syntax-parse stx
    ; 5. (o.m a ...)
    [(_.app id:dotted-name arg:non-method ...)
     (define (-> x) (string->identifier #'id #'id x))
     (with-syntax ([(o f ... m) (stx-map -> #'id.names)])
       (with-syntax ([o.fs (identifiers->dotted-name #'id #'(o f ...))])
         (syntax/loc stx
           (let ([obj o.fs]
                 [args (list arg ...)]) ; don't duplicate arg ...
             (cond
               [(and (object? obj) (method-in-interface? 'm (object-interface obj)))
                (send/apply o m args)]
               [(and (object? obj) (field-bound? m obj))
                (apply (get-field m obj) args)]
               [(object? obj)
                (raise-syntax-error
                 '.app (~a "the object does not not have a field or method named: " 'm) #'id)]
               [else
                (raise-syntax-error
                 '.app (~a "expected an object, got: " obj) #'id)])))))]
    ; 6. (o.m a ... .m1 a1 ...)
    [(_.app id:dotted-name a:non-method ... m1:method . more)
     (syntax/loc stx
       (let ([obj (.app id a ...)])
         (send obj m1.name . more)))]
    ; (e .m a ...)
    [(_.app e:expr method:method arg:non-method ...)
     (syntax/loc stx
       (let ([o e])
         (send o method.name arg ...)))]
    ; (e .m a ... .m2 a2 ...)
    [(_.app e:expr method:method arg:non-method ... method2:method . more)
     (syntax/loc stx
       (let ((o e))
         (let ([o1 (send o method.name arg ...)])
           (.app o1 method2 . more))))]
    ; (e a ...)
    [(_.app e:expr arg:non-method ...)
     (syntax/loc stx
       (e arg ...))]))



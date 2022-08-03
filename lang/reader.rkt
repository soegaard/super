#lang racket/base
;; The function `make-meta-reader` is used to implement meta langauges
;; that adjusts an existing language.
;; We want
;;    #lang super <lang>
;; to behave mostly as <lang>, but we want to:
;;   - use #%app and #%top from super/main
;;   - adjust forms of the type:
;;        id[expr]
;;    (an expression consisting of an identifer followed directly
;;     be an expression in square brackets)
;;    The expression
;;        id[expr]
;;    is rewritten to 
;      (#%ref id expr ...).

(require (only-in syntax/module-reader make-meta-reader))

;; The procedure `make-meta-reader` produces adjusted versions
;; of `read`, `read-syntax` and `read-get-info`.

(provide (rename-out [super-read        read]
                     [super-read-syntax read-syntax]
                     [super-get-info    get-info]))


; After standard reading, we will rewrite
;      id[expr ...]
; to
;      (#%ref id expr ...).

; We will use this to index to vectors, strings and hash tables.

; Since `adjust` is called after reading, we are essentially working with
; three passes.
;   - read-syntax
;   - adjust
;   - expand

; Let's define our `adjust` pass.

(require racket/runtime-path racket/syntax
         (except-in syntax/parse char))

(define (read-string str #:source-name [source-name #f])
  (define in (open-input-string str))
  ; (port-count-lines! in)
  (read-syntax source-name in))

(define (adjust stx)
  (syntax-parse stx
    [(a . d) (adjust-dotted-list stx)]
    [_       stx]))

(define (wrap-reader reader)
  (define (adjusted-reader . args)
    (inject-new-app-and-top (adjust (apply reader args))))
  adjusted-reader)

(define (inject-new-app-and-top stx)
  (syntax-parse stx
    [(mod name lang (mod-begin . more) . even-more)
     (with-syntax ([req (datum->syntax #f '(require (only-in super/main #%app #%top #%ref)) stx)])
       (syntax/loc stx
         (mod name lang
              (mod-begin req . more)
              . even-more)))]))

(define (adjust-dotted-list stx)    
  (syntax-parse stx
    [(id:id (~and [e:expr ...] brackets)  . more)
     (cond
       [(and (eqv? (syntax-property #'brackets 'paren-shape) #\[)
             (= (+ (syntax-position #'id) (syntax-span #'id))
                (syntax-position #'brackets)))
        (let ([adjusted-more (adjust #'more)]
              [arguments     (syntax->list #'(id e ...))])
          (datum->syntax #f
                         `((#%ref ,@arguments) . ,adjusted-more)
                         stx))]
       [else
        (with-syntax ([(_ . rest) stx])
          (let ([adjusted-rest (adjust-dotted-list #'rest)])
            (datum->syntax #f
                           `(,#'id . ,adjusted-rest)
                           stx)))])]
    [(a . more)
     (let ([adjusted-a    (adjust #'a)]
           [adjusted-more (adjust #'more)])
       (datum->syntax #f
                      `(,adjusted-a . ,adjusted-more)
                      stx))]
    [_
     (raise-syntax-error 'adjust-dotted-list "expected a dotted list" stx)]))

;; Now are ready to wrap `read`, `read-syntax` and `read-get-info` from <lang>.

(define-values (super-read super-read-syntax super-get-info)
  (make-meta-reader
   ; self-sym
   'super
   ; path-desc-str
   "language path"
   ; module-path-parser
   (lambda (bstr)
     (let* ([str (bytes->string/latin-1 bstr)]
            [sym (string->symbol str)])
       (and (module-path? sym)
            (vector
             ;; try submod first:
             `(submod ,sym reader)
             ;; fall back to /lang/reader:
             (string->symbol (string-append str "/lang/reader"))))))
   ; convert-read
   wrap-reader
   ; convert-read-syntax
   wrap-reader
   ; convert-get-info
   (lambda (proc)
     (lambda (key defval)
       (define (fallback) (if proc (proc key defval) defval))
       (case key
         [(color-lexer)
          (or (fallback)
              (dynamic-require 'syntax-color/racket-lexer 'racket-lexer))]
         [else (fallback)])))))


; > (displayln (adjust (read-string "(foo[bar])")))
; #<syntax:string::2 (ref (foo bar))>

; > (displayln (adjust (read-string "(foo [bar])")))
; #<syntax:string::1 (foo (bar))>

; > (displayln (adjust (read-string "(foo v[1] bar)")))
; #<syntax:string::1 (foo (#%ref v 1) bar)>



#lang racket/base

(require syntax/parse/define
         racket/format
         (for-syntax racket/base
                     racket/string
                     racket/syntax
                     syntax/parse))

(provide (rename-out [-app #%app]
                     [-top #%top]
                     [-module-begin #%module-begin])
         (except-out (all-from-out  racket/base)
                     #%module-begin
                     #%top
                     #%app)
         ns def)

(begin-for-syntax
  (define (clj-mod-path->rkt stx)
    (define str (string-replace (symbol->string (syntax->datum stx)) "." "/"))
    (format-id stx "~a" str))

  (define-syntax-class req-subclause
    #:datum-literals (:refer)
    (pattern [mod:id]
             #:with sub (clj-mod-path->rkt #'mod))

    (pattern [mod:id :refer [name:id ...]]
             #:with sub
             #`(only-in #,(clj-mod-path->rkt #'mod) name ...)))

  (define-syntax-class ns-clause
    #:datum-literals (:require)
    (pattern (:require cl:req-subclause ...)
             #:with req
             #'(require cl.sub ...))))

(define-syntax-parser ns
  [(_ name:id cl:ns-clause ...)
   #'(begin cl.req ...)])

(define-syntax-parser -app
  [(_ x:expr ...)
   #:when (eq? (syntax-property this-syntax 'paren-shape) #\{)
   (syntax/loc this-syntax (hash x ...))]
  [(_ x:expr ...)
   #:when (eq? (syntax-property this-syntax 'paren-shape) #\[)
   (syntax/loc this-syntax (vector x ...))]
  [(_ x ...+)
   (syntax/loc this-syntax (#%app x ...))])

(define-syntax-parser -top
  [(_ . x:id)
   (define x-str (symbol->string (syntax-e #'x)))
   (cond
     [(regexp-match? #rx"^:" x-str)
      (define/syntax-parse new-x
        (format-id this-syntax (substring x-str 1)))
      #''new-x]
     [else #'(#%top . x)])])

(define-syntax (-module-begin stx)
  (syntax-parse stx
    [(_ x ...)
     #:with a-d-o (datum->syntax stx '(all-defined-out))
     #'(#%module-begin
        (provide a-d-o)
        x ...)]))

(define-syntax-parser def
  [(_ x:id e:expr) #'(define x e)])

(module reader syntax/module-reader
  frankenstein
  #:wrapper1 (λ (t)
               (parameterize ([current-readtable (make-str-readtable)])
                 (t)))

  (define (make-info key default use-default)
    (case key
      [(drracket:default-filters) '(("Frankenstein's Monster Sources" "*.crkt"))]
      [(drracket:default-extension) "frankenstein"]
      [else (use-default key default)]))

  (define (make-str-readtable #:readtable [base-readtable (current-readtable)])
    (make-readtable base-readtable
                    #\^
                    'terminating-macro
                    (λ (ch port src line col pos)
                      (cond
                        [(eq? #\{ (peek-char port))
                         (with-handlers ([exn:fail:read? (λ (e) null)])
                           (read-syntax/recursive src port #f base-readtable))
                         (make-special-comment #f)]))
                    #\, #\space #f)))

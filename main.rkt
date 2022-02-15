#lang racket/base

(require syntax/parse/define
         racket/format
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide (rename-out [-app #%app]
                     [-top #%top])
         (except-out (all-from-out  racket/base) #%top #%app))

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

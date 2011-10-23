#lang racket

(require srfi/13                  
         syntax/readerr
         rackunit
         xml)

(provide (all-defined-out))

; HELPERS

;****************************************************************
;****************************************************************

; partition*
; Partitions the-list by bucket-func. 
; If bucket-func returns #f, the element will be IGNORED. 
; (That is, there can never be a #f bucket.)
; init-keys contains initial key values that should be present
; even if no element falls into that bucket.

; using a mutable hash table for now. if switch to immutable, can fold table creation over the-list
(define/contract 
  (partition* bucket-func the-list #:init-keys [init-keys '()])
  [->* (procedure? 
        list?)
       (#:init-keys list?)  
       hash?]  
  (define result-hash (make-hash))
  
  ; initialize
  (for-each (lambda (k) (hash-set! result-hash k empty)) init-keys)
  
  (for-each (lambda (x) 
              (define bucket (bucket-func x))
              (when bucket
                (if (hash-has-key? result-hash bucket)                    
                    (hash-set! result-hash bucket (cons x (hash-ref result-hash bucket)))
                    (hash-set! result-hash bucket (list x)))) )
            the-list)
  result-hash)

; TESTS
; Need to call hash-copy because partition returns a MUTABLE hash table, but '#hash( ... ) 
; is immutable. Mut != Immut.
(check-true (equal? 
             (partition* even? 
                         '())
             (hash-copy '#hash())))

(check-true (equal? 
             (partition* (lambda (x) (remainder x 3))
                         '(1 2 3) 
                         #:init-keys (list 17))
             (hash-copy '#hash( (0 . (3)) (1 . (1)) (2 . (2)) (17 . () )))))

(check-true (equal?
             (partition* (lambda (x) (and (even? x) (remainder x 3)))
                         '(1 2 3 4 5 6 7 8 9 10))    
            (hash-copy '#hash((0 . (6)) (2 . (8 2)) (1 . (10 4))))))


;****************************************************************
;****************************************************************

(define (fold-append-with-spaces posslist)
  (fold-append-with-separator posslist " "))

; May be a list, may not be a list
(define (fold-append-with-separator posslist separator)
  (if (list? posslist)
      (foldr (lambda (s t)
               (let ([s-str (if (symbol? s)
                                (symbol->string s)
                                s)]
                     [t-str (if (symbol? t)
                                (symbol->string t)
                                t)])                  
                 (cond
                   [(string=? s-str "") t-str]
                   [(string=? t-str "") s-str]
                   [else (string-append s-str separator t-str)])))
             ""
             posslist)
      (if (symbol? posslist)
          (symbol->string posslist)
          posslist)))

(define (fold-append-with-spaces-quotes posslist)
  (fold-append-with-spaces (if (list? posslist)
                               (map symbol->quoted-string posslist)
                               posslist)))

; symbol or string -> string
; Returns the argument, quoted, as a string.
(define (symbol->quoted-string arg)
  (if (symbol? arg)
      (string-append "\"" (symbol->string arg)"\"")
      (string-append "\"" arg "\"")))

(define (->string arg)
  (cond 
    [(symbol? arg) (symbol->string arg)]
    [(number? arg) (number->string arg)]
    [(syntax? arg) (->string (syntax->datum arg))]        
    [else (format "~a" arg)]))


(define (wrap-list-parens lst)
  (fold-append-with-spaces (map (lambda (str) (string-append "(" str ")")) lst)))

(define (safe-get-margrave-collection-path)
  (with-handlers ([(lambda (e) (exn:fail:filesystem? e))
                   (lambda (e) (current-directory))])
    ; If no margrave collection, assume debug mode.
    (collection-path "margrave")))

(define (resolve-margrave-filename-keyword raw-filename)
  
  (define the-filename (cond [(path? raw-filename)
                              (path->string raw-filename)]
                             [(symbol? raw-filename)
                              (symbol->string raw-filename)]
                             [else 
                              raw-filename]))
  
  (define loc (string-contains-ci the-filename "*MARGRAVE*"))
  (define coll-path-string (path->string (safe-get-margrave-collection-path)))
  
  (define result (cond [(or (not loc) (> loc 1)) 
                        the-filename]
                      [(equal? loc 1)
                       (string-replace the-filename coll-path-string 0 11)]
                      [else 
                       (string-replace the-filename coll-path-string 0 10)]))
  
  ; Avoid confusion: prevent mixed use of / and \ in the same path.
  (path->string (simplify-path result)))


; file-exists?/error 
; filename src-syntax -> boolean
; If file does not exist, raises an error
(define (file-exists?/error file-name src-syntax [error-message (format "File did not exist: ~a~n" file-name)])
  (cond [(file-exists? file-name)
         #t]
        [(syntax? src-syntax)
         (raise-read-error 
          error-message
          (syntax-source src-syntax)
          (syntax-line src-syntax)
          (syntax-column src-syntax)
          (syntax-position src-syntax)
          (syntax-span src-syntax))]
        [else (raise-user-error error-message)]))

; filename syntax -> port
; If file does not exist, raises an exception. If syntax has been passed, will enable syntax highlighting.
(define (open-input-file/exists file-name src-syntax [error-message (format "File did not exist: ~a~n" file-name)])
  (and (file-exists?/error file-name src-syntax error-message)
       (open-input-file file-name)))

(define (path-only/same the-path)
  (define p (path-only the-path))
  (if p
      p
      'same))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This section made with help from Danny Yoo

;; This extends Racket's pattern matcher to recognize lists of syntaxes.
(define-match-expander syntax-list-quasi 
  (lambda (stx)
    (syntax-case stx ()
      [(_ elts ...)
       #'(? (lambda (x) (and (syntax? x)
                             (list? (syntax->list x))))
            (app syntax->list
                 ; swap these two lines to turn syntax-list-quasi into syntax-list
                 ;(list elts ...)))])))
                 `(elts ...)))])))

(define (make-keyword-predicate keyword)
 (lambda (stx)
   (and (identifier? stx)
        (free-identifier=? stx keyword))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helpers for formula structures and commands that use formulas

; Might be better with [children (listof m-type?)] 
; but would need a recursive contract.
(define-struct/contract m-type
  ([name string?]
   [child-names (listof string?)])
  #:transparent)

(define-struct/contract m-predicate
  ([name string?]
   [arity (listof string?)])
  #:transparent)

(define-struct/contract m-constant
  ([name string?]
   [type string?])
  #:transparent)

(define-struct/contract m-function
  ([name string?]   
   [arity (listof string?)]
   [result string?])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define reserved-words
  '(function
    constant
    predicate
    rule
    vocab
    policy
    policyset
    type ; used as synonyms for now
    sort
    
    and
    or
    iff
    implies
    not
    =
    
    list
    quote
    quasiquote))

; Function symbols must have the first character alpha lowercase
(define (valid-function? sym-or-syn)
  (define sym (if (syntax? sym-or-syn)
                  (syntax->datum sym-or-syn)
                  sym-or-syn))
  (and (symbol? sym) 
       (not (member sym reserved-words))
       (let ([str0 (string-ref (symbol->string sym) 0)])
         (and 
          (char-lower-case? str0)
          (char-alphabetic? str0)))))
(check-true (valid-function? 'f0))
(check-false (valid-function? 100))  

; Constant symbols are preceded by a quote, but otherwise same as funcs.
(define (valid-constant? sym-or-syn)
  (define sym (if (syntax? sym-or-syn)
                  (syntax->datum sym-or-syn)
                  sym-or-syn))
  (match sym
    [`(quote ,(? valid-function? sym)) #t]
    [else #f]))
(check-true (valid-constant? ''cONSTANT))
(check-false (valid-constant? 'c))
(check-false (valid-constant? '100))  
(check-false (valid-constant? ''100))  

; Predicate symbols are the same as function symbols.
; Understand difference from context.
(define (valid-predicate? sym-or-syn)
  (define sym (if (syntax? sym-or-syn)
                  (syntax->datum sym-or-syn)
                  sym-or-syn))
  (and (symbol? sym) 
       (not (member sym reserved-words))
       (let ([str0 (string-ref (symbol->string sym) 0)])
         (and 
          (char-lower-case? str0)
          (char-alphabetic? str0)))))
(check-true (valid-predicate? 'myPred))
(check-false (valid-predicate? 100))  

; Sort symbols must be capitalized.
(define (valid-sort? sym-or-syn)
  (define sym (if (syntax? sym-or-syn)
                  (syntax->datum sym-or-syn)
                  sym-or-syn))
  (and (symbol? sym) 
       (not (member sym reserved-words))
       (let ([str0 (string-ref (symbol->string sym) 0)])
         (and 
          (char-upper-case? str0)
          (char-alphabetic? str0)))))
(check-true (valid-sort? 'A))
(check-false (valid-sort? 'a))  
(check-false (valid-sort? 'constant))  
(check-false (valid-sort? 100))  

; Variables must begin with a lowercase letter.
; This is also the same as func ids and predicate ids.
(define (valid-variable? sym-or-syn)  
  (define sym (if (syntax? sym-or-syn)
                  (syntax->datum sym-or-syn)
                  sym-or-syn))
  (and (symbol? sym) 
       (not (member sym reserved-words))
       (let ([str0 (string-ref (symbol->string sym) 0)])
         (and 
          (char-lower-case? str0)
          (char-alphabetic? str0)))))
(check-true (valid-variable? 'myVar0))
(check-false (valid-variable? 'A))  
(check-false (valid-variable? 100))  

(define (valid-variable?/err sexpr)
  (cond [(valid-variable? sexpr) #t]
        [(syntax? sexpr) (raise-syntax-error 'Margrave (format "Invalid variable: ~a.~n" (->string sexpr)) #f #f (list sexpr))]
        [else (raise-user-error (format "Invalid variable: ~a.~n" (->string sexpr)))]))

(define (valid-predicate?/err sexpr)
  (cond [(valid-predicate? sexpr) #t]
        [(syntax? sexpr) (raise-syntax-error 'Margrave (format "Invalid predicate: ~a.~n" (->string sexpr)) #f #f (list sexpr))]
        [else (raise-user-error (format "Invalid predicate: ~a.~n" (->string sexpr)))]))

(define (valid-constant?/err sexpr)
  (cond [(valid-constant? sexpr) #t]
        [(syntax? sexpr) (raise-syntax-error 'Margrave (format "Invalid constant: ~a.~n" (->string sexpr)) #f #f (list sexpr))]
        [else (raise-user-error (format "Invalid constant: ~a.~n" (->string sexpr)))]))

(define (valid-function?/err sexpr)
  (cond [(valid-function? sexpr) #t]
        [(syntax? sexpr) (raise-syntax-error 'Margrave (format "Invalid function: ~a.~n" (->string sexpr)) #f #f (list sexpr))]
        [else (raise-user-error (format "Invalid function: ~a.~n" (->string sexpr)))]))

(define (valid-sort?/err sexpr)
  (cond [(valid-sort? sexpr) #t]
        [(syntax? sexpr) (raise-syntax-error 'Margrave (format "Invalid sort: ~a.~n" (->string sexpr)) #f #f (list sexpr))]
        [else (raise-user-error (format "Invalid sort: ~a.~n" (->string sexpr)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  


(define (syntax->string x)
  (symbol->string (syntax->datum x)))

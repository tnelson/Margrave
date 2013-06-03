#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Modified from Shriram's P4P syntax:
; https://github.com/shriram/p4p
; by Tim, to make our policies and vocabularies
; less parenthetically intimidating.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require (for-syntax racket syntax/stx))
(require syntax/readerr)
(require rackunit)

(require (only-in lang/htdp-advanced check-expect : signature predicate)
         (only-in test-engine/scheme-tests run-tests display-results))

(provide (except-out (all-from-out racket) #%module-begin)
        ; (rename-out [top-level #%module-begin])        
         (for-syntax process-sexp-stream)
         :-readtable
         )         
         ;Policy Vocab Theory)


; Provide the Policy form
;(require "margrave-policy-vocab.rkt")

(define-for-syntax (process-sexp-stream sexp-stream)
   
  
  ;(printf "Stream: ~v~n" sexp-stream)
  
  ; thanks to Jens Axel Søgaard (via google)
  ; thread: [racket] racket's syntax-case and read-syntax usage
  ; for use by syntax-case*
  (define (symbolic-identifier=? stx1 stx2)
    (and (identifier? stx1)
         (identifier? stx2)
         (or (symbol=? (syntax->datum stx1) (syntax->datum stx2))
             (and (symbol=? (syntax->datum stx1) 'unquote)
                  (symbol=? (syntax->datum stx2) ':-))
             (and (symbol=? (syntax->datum stx2) 'unquote)
                  (symbol=? (syntax->datum stx1) ':-)))))
  
  ; Those that can't be at the beginning of an expression 
  (define non-expr-keywords '(else: elseif: deffun: defstruct: defvar:))
  (define expr-keywords '(fun: in: if: do: let: let*: letrec:))
  (define all-keywords (append expr-keywords non-expr-keywords))
  
  (define DUMMY-ICHECK 
    (lambda (s) true))

  ; Same Line or Same Column
  (define (SLSC l1 c1 l2 c2)
    (or (= l1 l2)
        (= c1 c2)))
  
  ; Same Line or Greater Column
  (define SLGC-min-offset 2)
  (define (SLGC l1 c1 l2 c2)
    (or (= l1 l2)
        (>= (- c2 c1) SLGC-min-offset)))
  
  ; Same Line or Greater-than-or-Equal Column
  (define (SLGEC l1 c1 l2 c2)
    (or (= l1 l2)
        (<= c1 c2)))
  
  ; Top Level
  (define (TOP-I s)
    (if (and (syntax-line s) (syntax-column s))
        (or (= (syntax-column s) 0)
            (raise-syntax-error 'indentation-error "bad top level" s))
        (raise-syntax-error 'cannot-check-top-level-term "not syntax" s)))
  
  (define ((check-indent checker s1) s2)
    ; TN: disabled indentation check
    #t)
    ;(if (and (syntax-line s1) (syntax-column s1)
    ;         (syntax-line s2) (syntax-column s2))
    ;    (or (checker (syntax-line s1)
    ;                 (syntax-column s1)
    ;                 (syntax-line s2)
    ;                 (syntax-column s2))
    ;        (begin
    ;          (pretty-print s1)
    ;          (pretty-print s2)
    ;          (raise-syntax-error 'indentation-error "does not match" s2)))
    ;    (if (and (syntax-line s1) (syntax-line s2))
    ;        (raise-syntax-error 'cannot-check-indentation-follower "can't check" s1)
    ;        (raise-syntax-error 'cannot-check-indentation-leader "can't check" s2))))

  ;; NOTE: This function doesn't return the rest of the stream.  
  ;; See explanation for process-expr-sequence.
  (define (process-comma-separated-identifiers sexp-stream icheck)
    (define (process-first sexp-stream)
      (syntax-case sexp-stream ()
        [(v other-things ...)
         (identifier? #'v)
         (begin
           (icheck #'v)
           #'v)]
        [(v other-things ...)
         (raise-syntax-error 'not-an-identifier "expected to find one" #'v)]))
    (define (process-rest sexp-stream)
      (syntax-case* sexp-stream (unquote :-) symbolic-identifier=?
        [()
         empty]
        [((unquote id) other-things ...)
         (identifier? (syntax id))
         (begin
           (icheck (syntax id))
           (cons (syntax id) (process-rest (syntax (other-things ...)))))]
        [((unquote something) other-things ...)
         (raise-syntax-error 'comma-separated-list "expected to find an identifier" (syntax something))]
        [(v other-things ...)
         (identifier? (syntax v))
         (raise-syntax-error 'comma-separated-list "identifier not preceded by comma" #'v)]
        [(v other-things ...)
         (raise-syntax-error 'comma-separated-list "expected to find an identifier" (syntax v))]))
    (if (stx-null? sexp-stream)
        empty
        (cons (process-first sexp-stream)
              (process-rest (stx-cdr sexp-stream)))))

  (define (process-let/let*/letrec: p4p-construct racket-construct outer-sexp-stream icheck)
    (define sub-icheck (check-indent SLGC (stx-car outer-sexp-stream)))
    (define (process-bindings sexp-stream so-far)
      (define (rest-of-bindings sexp-stream so-far)
        (syntax-case* sexp-stream (in: =) symbolic-identifier=?
          [(in: rest-stream ...)
           (begin
             (icheck (stx-car sexp-stream))
             (values so-far #'(rest-stream ...)))]
          [((unquote var) = other-things ...)
           (identifier? #'var)
           (let-values ([(expr rest-stream) (extract-one-expression #'(other-things ...) sub-icheck)])
             (rest-of-bindings rest-stream (cons (list #'var expr) so-far)))]
          [((unquote var) other-things ...)
           (identifier? #'var)
           (raise-syntax-error p4p-construct "expected to find = after identifier" #'var)]
          [((unquote var) other-things ...)
           (raise-syntax-error p4p-construct "expected to find an identifier in LHS" #'var)]
          [(something other-things ...)
           (raise-syntax-error p4p-construct "expected to find a comma" #'something)]))
      (syntax-case sexp-stream (in: =)
        [(in: rest-stream ...)
         (begin
           (icheck (stx-car sexp-stream))
           (values #'(rest-stream ...) empty))]
        [(var = rest-stream ...)
         (identifier? #'var)
         (let-values ([(first-rhs rest-stream) (extract-one-expression #'(rest-stream ...) sub-icheck)])
           (sub-icheck #'var)
           (rest-of-bindings rest-stream (list (list #'var first-rhs))))]
        [(var rest-stream ...)
         (identifier? #'var)
         (raise-syntax-error p4p-construct "expected to find = after identifier" #'var)]
        [(var rest-stream ...)
         (raise-syntax-error p4p-construct "expected to find an identifier in LHS" #'var)]))
    (let-values ([(bindings rest-stream) (process-bindings (stx-cdr outer-sexp-stream) empty)])
      (define-values (body body-rest) (extract-one-expression rest-stream sub-icheck))
      (with-syntax ([construct-kwd racket-construct]
                    [((name val) ...) (reverse bindings)]
                    [body body])
        (values (syntax (construct-kwd ((name val) ...) body))
                body-rest))))

  (define (process-fun: sexp-stream icheck)
    (define sub-term-icheck (check-indent SLGC (stx-car sexp-stream))) ;; know stx-car must exist from calling context
    (syntax-case sexp-stream (in:)
      [(fun:-kwd argdesc in: rest-of-stream ...)
       (let-values ([(defs*+body body-rest) 
                     (extract-definition*+expression (syntax (rest-of-stream ...)) sub-term-icheck)])
         (with-syntax ([(body ...) defs*+body])
           ;; NOTE: this conditional is repeated in process-deffun:, but with different RHSes
           (cond
             [(identifier? (syntax argdesc))
              (sub-term-icheck (syntax argdesc))
              (values (syntax (lambda argdesc body ...))
                      body-rest)]
             [(stx-list? (syntax argdesc))
              (with-syntax ([(args ...) (process-comma-separated-identifiers (syntax argdesc) sub-term-icheck)])
                (values (syntax (lambda (args ...) body ...))
                        body-rest))]
             [else
              (raise-syntax-error 'argument-list "not valid syntax" (syntax argdesc))])))]))
  
  ;; icheck is for the keywords, not sub-expressions; if: has already been checked
  (define (process-if: sexp-stream icheck)
    (define sub-expr-icheck (check-indent SLGC (stx-car sexp-stream)))
    ;; stx-cdr skips over the IF: keyword
    (let*-values ([(test-expr test-rest) (extract-one-expression (stx-cdr sexp-stream) sub-expr-icheck)]
                  [(then-expr then-rest) (extract-one-expression test-rest sub-expr-icheck)])
      (let loop ([rest-stream then-rest]
                 [elseifs empty])
        (syntax-case rest-stream (elseif: else:)
          [(elseif: more ...)
           (let*-values ([(elseif-test-expr elseif-test-rest) (extract-one-expression (syntax (more ...)) sub-expr-icheck)]
                         [(elseif-then-expr elseif-then-rest) (extract-one-expression elseif-test-rest sub-expr-icheck)])
             (icheck (stx-car rest-stream)) ;; elseif: must indent the same as if:
             (loop elseif-then-rest (cons (list elseif-test-expr elseif-then-expr) elseifs)))]
          [(else: more ...)
           (let-values ([(else-expr else-rest) (extract-one-expression (syntax (more ...)) sub-expr-icheck)])
             (icheck (stx-car rest-stream)) ;; else: must indent the same as if:
             (let ([clauses (cons (list test-expr then-expr) (reverse elseifs))])
               (with-syntax ([((question answer) ...) clauses]
                             [else-answer else-expr])
                 (values (syntax (cond [question answer] ... [else else-answer]))
                         else-rest))))]
          [_ (raise-syntax-error 'if: "expected to see a complete body" sexp-stream)]))))
  
  (define (process-and/or: p4p-construct racket-construct sexp-stream icheck)
    (syntax-case sexp-stream () ;; _ is and: or or:
      [(_ (body ...) rest-of-stream ...)
       ; guard
       (let ([paren (syntax-property (stx-car (stx-cdr sexp-stream)) 'paren-shape)])
         (and paren (char=? paren #\{)))
       ; RHS
       (let ([body-exprs (process-expr-sequence (syntax (body ...)) icheck)])
         (with-syntax ([(body ...) body-exprs]
                       [construct-kwd racket-construct])
           (values (syntax (construct-kwd body ...))
                   (syntax (rest-of-stream ...)))))]
      [(_ (body ...) rest-of-stream ...)
       (raise-syntax-error p4p-construct "expected to find {braces} after keyword" sexp-stream)]))

  (define (process-do: sexp-stream icheck)
    (syntax-case sexp-stream () ;; _ is do:
      [(_ (body ...) rest-of-stream ...)
       ; guard
       (let ([paren (syntax-property (stx-car (stx-cdr sexp-stream)) 'paren-shape)])
         (and paren (char=? paren #\{)))
       ; RHS
       (let ([body-exprs (process-expr-sequence (syntax (body ...)) icheck)])
         (with-syntax ([(body ...) body-exprs])
           (values (syntax (begin body ...))
                   (syntax (rest-of-stream ...)))))]
      [(_ (body ...) rest-of-stream ...)
       (raise-syntax-error 'do: "expected to find {braces} after keyword" sexp-stream)]))
  
  (define (process-test: sexp-stream icheck)
    (syntax-case sexp-stream () ;; _ is test:
      [(_ test-expr-and-rest ...)
       (let-values ([(test-expr test-rest-stream)
                     (extract-one-expression #'(test-expr-and-rest ...) icheck)])
         (syntax-case test-rest-stream (=?)
           [(=? should-be-and-rest ...)
            (let-values ([(should-be-expr should-be-rest-stream)
                          (extract-one-expression #'(should-be-and-rest ...) icheck)])
              (with-syntax ([the-test test-expr]
                            [the-should-be should-be-expr])
                (values #'(check-expect the-test the-should-be)
                        should-be-rest-stream)))]
           [_
            (raise-syntax-error 'test: "expected to find =? followed by an expression" test-rest-stream)]))]
      [_
       (raise-syntax-error 'test: "malformed use" sexp-stream)]))    
  
  ;; NOTE: Assumes first arg has been taken care of.
  ;; NOTE: Assumes function position's indentation has already been checked.
  ;;       The provided icheck is for arguments.
  (define (process-app fun-expr arg-descr rest-stream icheck)
    ;; NOTE: arg-helper doesn't return the rest of the stream.  
    ;;       See explanation for process-expr-sequence.
    (define (arg-helper arg-stream icheck)
      (if (stx-null? arg-stream)
          empty
          (syntax-case* arg-stream (unquote :-) symbolic-identifier=?
            [((unquote something) other-things ...)
             (let-values ([(an-arg arg-rest) (extract-one-expression
                                              (syntax (something other-things ...))
                                              icheck)])
               ;(printf "arg-helper: ~v: ~v ~v~n" arg-stream an-arg arg-rest)
               (cons an-arg (arg-helper arg-rest icheck)))]
            [((x y) other-things ...)
             (printf "~v ~v ~v ~n" #'x #'y #'(other-things ...))
             (raise-syntax-error 'process-app (format "namespace/phase failure on ~v. ~v ~v ~v ~v"
                                                      #'(x y)
                                                      (free-identifier=? #'x #'unquote)
                                                      (free-identifier=? #'x #'unquote 0 1)
                                                      (free-identifier=? #'x #'unquote 1 0)
                                                      (list (identifier-binding #'x) (identifier-binding #'unquote))
                                                      ) arg-stream)]
            
            [_
             (raise-syntax-error 'process-app "argument not preceded by comma" arg-stream)])))
    
    (define (process-args arg-stream icheck)
      (if (stx-list? arg-stream)
          (if (stx-null? arg-stream)
              empty
              (let-values ([(first-arg arg-rest) (extract-one-expression arg-stream icheck)])
               ; (printf "process-args: ~v : ~v ~v~n~n" arg-stream first-arg arg-rest )
                (cons first-arg (arg-helper arg-rest icheck))))
          (raise-syntax-error 'application "not a proper argument list" arg-stream)))
    
    (define-values (out1 out2) 
      (with-syntax ([fun fun-expr]
                    [(args ...) (process-args arg-descr icheck)])
                    ;[args (process-args arg-descr icheck)])
        (values ;(syntax (#%app fun args ...))
         ; TN: Just produce the sexpression, not function application.
         (syntax (fun args ...))                
         rest-stream)))        
    (values out1 out2))

  (define (process-const const-expr const-rest icheck)
    (icheck const-expr)
    ;; Do some error-checking to make sure it's a datum
    (values const-expr const-rest))

  ;; NOTE: process-expr-sequence doesn't return the rest of the stream
  ;; because it terminates precisely when its input stream is empty.
  ;; This is because we demand that expression sequences have a delimiter,
  ;; which enables us to invoke process-expr-sequence on just the sub-stream
  ;; before the delimiter.

  #;(define (process-expr-sequence sexp-sub-stream icheck)
    (if (stx-null? sexp-sub-stream)
        empty
        (let-values ([(one-exp rest-sub-stream)
                      (extract-one-expression sexp-sub-stream icheck)])
          (cons one-exp
                (process-expr-sequence rest-sub-stream icheck)))))

  (define (process-expr-sequence sexp-sub-stream icheck)
    (define process-first extract-one-expression)
    (define (process-not-first sexp-stream)
      (syntax-case* sexp-stream (unquote :-) symbolic-identifier=?
        [() (values empty sexp-stream)]
        [((unquote stuff) other-stuff ...)
         (extract-one-expression #'(stuff other-stuff ...) icheck)]
        [_ (raise-syntax-error 'expression-sequence "expected to find a comma at the beginning" sexp-stream)]))
    (if (stx-null? sexp-sub-stream)
        empty
        (let-values ([(first rest-stream) (process-first sexp-sub-stream icheck)])
          (cons first
                (let loop ([sexp-stream rest-stream])
                  (if (stx-null? sexp-stream)
                      empty
                      (let-values ([(one-exp rest-sub-stream) (process-not-first sexp-stream)])
                        (cons one-exp (loop rest-sub-stream)))))))))

  (define (extract-one-expression sexp-stream icheck)
    (if (stx-null? sexp-stream)
        (raise-syntax-error 'looking-for-expression "program ended prematurely")
        (syntax-case* (stx-car sexp-stream) (fun: if: do: and: or: quote let: let*: letrec: test:) symbolic-identifier=?

          [let:
           (begin
             (icheck (stx-car sexp-stream))
             (process-let/let*/letrec: 'let: 'let sexp-stream (check-indent SLGEC (stx-car sexp-stream))))]
          [let*:
           (begin
             (icheck (stx-car sexp-stream))
             (process-let/let*/letrec: 'let*: 'let* sexp-stream (check-indent SLGEC (stx-car sexp-stream))))]
          [letrec:
           (begin
             (icheck (stx-car sexp-stream))
             (process-let/let*/letrec: 'letrec: 'letrec sexp-stream (check-indent SLGEC (stx-car sexp-stream))))]
          [fun:
           (begin
             (icheck (stx-car sexp-stream))
             (process-fun: sexp-stream (check-indent SLGC (stx-car sexp-stream))))]
          [if:
           (begin
             (icheck (stx-car sexp-stream))
             (process-if: sexp-stream (check-indent SLSC (stx-car sexp-stream))))]
          [do:
           (begin
             (icheck (stx-car sexp-stream))
             (process-do: sexp-stream (check-indent SLGC (stx-car sexp-stream))))]
          [and:
           (begin
             (icheck (stx-car sexp-stream))
             (process-and/or: 'and: 'and sexp-stream (check-indent SLGC (stx-car sexp-stream))))]
          [or:
           (begin
             (icheck (stx-car sexp-stream))
             (process-and/or: 'or: 'or sexp-stream (check-indent SLGC (stx-car sexp-stream))))]
          [test:
           (begin
             (icheck (stx-car sexp-stream))
             (process-test: sexp-stream (check-indent SLGC (stx-car sexp-stream))))]
          
          
          [(quote e)
           (process-const (syntax e) (stx-cdr sexp-stream) icheck)]
          
          
          [(e ...)  ;; Function position is itself a non-identifier expression
           (let ([paren (syntax-property (stx-car sexp-stream) 'paren-shape)])
             (and paren (char=? paren #\{)))
           (let-values ([(fun-expr better-be-empty)
                         (extract-one-expression (syntax (e ...)) icheck)])
             (if (not (stx-null? better-be-empty))
                 (raise-syntax-error 'application "too many terms" better-be-empty)
                 (if (and (stx-pair? (stx-cdr sexp-stream))
                          (stx-list? (stx-car (stx-cdr sexp-stream))))
                     (process-app fun-expr (stx-car (stx-cdr sexp-stream)) (stx-cdr (stx-cdr sexp-stream)) 
                                         (check-indent SLGC (stx-car sexp-stream)))
                     (raise-syntax-error 'application "not beginning with arguments" (stx-car (stx-cdr sexp-stream))))))]
          [maybe-kwd
           (and (identifier? (syntax maybe-kwd))
                (member (syntax->datum (syntax maybe-kwd)) non-expr-keywords))
           (raise-syntax-error 'keyword "found one in expression position" (syntax maybe-kwd))]
          [app-or-val
           (let ([v (syntax->datum (syntax app-or-val))])
             (if (or (number? v) (string? v) (boolean? v)) ;; missing a few!
                 (process-const (stx-car sexp-stream) (stx-cdr sexp-stream) icheck)
                 (if (symbol? v)
                     (syntax-case* (stx-cdr sexp-stream) (unquote :-) symbolic-identifier=?
                       ;; This line cost me from 2:30am to 1:30pm.
                       ;; When we're looking at the x subexpression of f(x, y) -- which the reader 
                       ;; turns into f(x (unquote y)) -- this is indistinguishable
                       ;; from f applied to x applied to something -- unless you have the below rule
                       ;; to distinguish unquotes.
                       [((unquote anything) anything-else ...)
                        ;; identifier case!
                        (process-const (stx-car sexp-stream) (stx-cdr sexp-stream) icheck)]
                       [((anything ...) anything-else ...)
                        (begin
                          (icheck (stx-car sexp-stream))
                          (process-app (stx-car sexp-stream) (stx-car (stx-cdr sexp-stream)) (stx-cdr (stx-cdr sexp-stream))
                                       (check-indent SLGC (stx-car sexp-stream))))]
                       [_
                        ;; identifier case!
                        (process-const (stx-car sexp-stream) (stx-cdr sexp-stream) icheck)])
                     (raise-syntax-error 'application "not beginning with arguments" (stx-car (stx-cdr sexp-stream))))))])))

  (define (process-deffun: sexp-stream icheck)
    (syntax-case sexp-stream (=)
      [(defun:-kwd f-name argdesc = rest-of-stream ...)
       (identifier? (syntax f-name))
       (let-values ([(defs*+expr body-rest)
                     (extract-definition*+expression (syntax (rest-of-stream ...)) 
                                                     (check-indent SLGC (syntax defun:-kwd)))])
         (icheck (syntax defun:-kwd))
         (with-syntax ([(body ...) defs*+expr])
           (cond
             [(identifier? (syntax argdesc))
              (values (syntax (define (f-name . argdesc) body ...))
                      body-rest)]
             [(stx-list? (syntax argdesc))
              (with-syntax ([(args ...) (process-comma-separated-identifiers (syntax argdesc) 
                                                                             (check-indent SLGC (syntax defun:-kwd)))])
                (values (syntax (define (f-name args ...) body ...))
                        body-rest))]
             [else
              (raise-syntax-error 'argument-list "not valid syntax" (syntax argdesc))])))]
      [_
       (raise-syntax-error 'deffun: "not proper syntax" sexp-stream)]))
  
  (define (process-defstruct: sexp-stream icheck)
    (syntax-case sexp-stream (has:)
      [(defstruct:-kwd struct-name has: (field-descr ...) rest-of-stream ...)
       (identifier? (syntax struct-name))
       (begin
         (icheck (syntax defstruct:-kwd))
         (values (with-syntax ([(fields ...)
                                (process-comma-separated-identifiers (syntax (field-descr ...)) 
                                                                     (check-indent SLGC (syntax defstruct:-kwd)))])
                   (syntax (define-struct struct-name (fields ...) #:transparent)))
                 (syntax (rest-of-stream ...))))]
      [_
       (raise-syntax-error 'defstruct: "not proper syntax" sexp-stream)]))

  (define (process-defvar: sexp-stream icheck)
    (syntax-case sexp-stream (=)
      [(defvar:-kwd v-name = rest-of-stream ...)
       (identifier? (syntax v-name))
       (let-values ([(body-expr body-rest)
                     (extract-one-expression (syntax (rest-of-stream ...))
                                             (check-indent SLGC (syntax defvar:-kwd)))])
         (icheck (syntax defvar:-kwd))
         ((check-indent SLGC (syntax defvar:-kwd)) (syntax v-name))
         (with-syntax ([body body-expr])
           (values (syntax (define v-name body))
                   body-rest)))]
      [(_ v-name rest-of-stream ...)
       (not (identifier? (syntax v-name)))
       (raise-syntax-error 'defvar: "not a variable" #'v-name)]
      [(_ v-name token rest-of-stream ...)
       (syntax-case (syntax token) (=) ;; This syntax-case is the GUARD
         [= false]
         [_ true])
       (raise-syntax-error 'defvar: "expected to find = here" #'token)]
      [_
       (raise-syntax-error 'defvar: "not proper syntax" sexp-stream)]))
  
  (define (process-defsigpred: sexp-stream icheck)
    (syntax-case sexp-stream (=)
      [(defsigpred:-kwd s-name = rest-of-stream ...)
       (identifier? #'v-name)
       (let-values ([(body-expr body-rest)
                     (extract-one-expression #'(rest-of-stream ...)
                                             (check-indent SLGC #'defsigpred:-kwd))])
         (icheck #'defsigpred:-kwd)
         ((check-indent SLGC #'defsigpred:-kwd) #'s-name)
         (with-syntax ([body body-expr])
           (values (syntax (define v-name (signature (predicate body))))
                   body-rest)))]
      [(_ v-name rest-of-stream ...)
       (not (identifier? #'v-name))
       (raise-syntax-error 'defsigpred: "not a variable" #'v-name)]
      [(_ v-name token rest-of-stream ...)
       (syntax-case (syntax token) (=) ;; This syntax-case is the GUARD
         [= false]
         [_ true])
       (raise-syntax-error 'defsigpred: "expected to find = here" #'token)]
      [_
       (raise-syntax-error 'defsigpred: "not proper syntax" sexp-stream)]))

  (define (process-sign: sexp-stream icheck)

    (define (parse-compound-sig something) 'nothing)

    (define (parse-sig sub-stream)
      (syntax-case* sub-stream (-> unquote :-) symbolic-identifier=?
        [{sig-name}
         (identifier? #'sig-name)
         #'sig-name]
        [{-> rest ...}
         (with-syntax ([e (parse-sig #'(rest ...))])
           #'(-> e))]
        [{op (arg ...) rest ...}
         (identifier? #'op)
         (with-syntax ([hd (parse-compound-sig #'(op (arg ...)))]
                       [tl (parse-sig #'(rest ...))])
           #'(hd . tl))]
        [{sig-name more ...}
         (identifier? #'sig-name)
         'do-something]
        [_
         (raise-syntax-error 'sign: "not proper syntax" sub-stream)]))

    ;; body of process-sign:
    (syntax-case sexp-stream (is:)
      [(sign:-kwd v-name is: {signature ...} rest-of-stream ...)
       (and (identifier? #'v-name)
            (let ([paren (syntax-property (stx-car (stx-cdr sexp-stream)) 'paren-shape)])
              (and paren (char=? paren #\{))))
       (let ([sig-out (parse-sig #'{signature ...})])
         (icheck #'sign:-kwd)
         ((check-indent SLGC #'sign:-kwd) #'v-name)
         (with-syntax ([signature sig-out])
           (values #'(: v-name signature)
                   #'(rest-of-stream ...))))]
      [(_ v-name is: {signature ...} rest-of-stream ...)
       (not (identifier? #'v-name))
       (raise-syntax-error 'sign: "not a variable" #'v-name)]
      [(_ v-name is: (signature ...) rest-of-stream)
       (let ([paren (syntax-property (stx-car (stx-cdr sexp-stream)) 'paren-shape)])
         (and paren (not (char=? paren #\{))))
       (raise-syntax-error 'sign: "signature must be in {brackets}" #'(signature ...))]
      [(_ v-name token rest-of-stream ...)
       (syntax-case (syntax token) (is:) ;; This syntax-case is the GUARD
         [is: true]
         [_ false])
       (raise-syntax-error 'sign: "expected to find is: here" #'token)]
      [_
       (raise-syntax-error 'sign: "not proper syntax" sexp-stream)]))

  ;; can return false if it isn't finding a definition at the head of the stream
  (define (extract-one-definition sexp-stream icheck)
    (if (stx-null? sexp-stream)
        (raise-syntax-error sexp-stream "program ended prematurely while expecting a definition or expression")
        (begin
          (icheck (stx-car sexp-stream))
          (syntax-case (stx-car sexp-stream) (deffun: defvar: defstruct: defsigpred: sign:)
            [deffun:
              (process-deffun: sexp-stream icheck)]
            [defvar:
              (process-defvar: sexp-stream icheck)]
            [defstruct:
              (process-defstruct: sexp-stream icheck)]
            [defsigpred:
              (process-defsigpred: sexp-stream icheck)]
            [sign:
             (process-sign: sexp-stream icheck)]
            [_
             (values false sexp-stream)]))))
  
  (define (extract-one-definition/expression sexp-stream icheck)
    (if (stx-null? sexp-stream)
        (raise-syntax-error sexp-stream "program ended prematurely while expecting a definition or expression")
        (let-values ([(def def-rest)
                      (extract-one-definition sexp-stream icheck)])
          (if def
              (values def def-rest)
              (extract-one-expression sexp-stream icheck))))) ;; expect sexp-stream = def-rest
  
  (define (extract-definition*+expression sexp-stream icheck)
    (define (defs-helper sexp-stream defs-so-far)
      (let-values ([(def def-rest)
                    (extract-one-definition sexp-stream icheck)])
        (if def
            (defs-helper def-rest (cons def defs-so-far))
            (let-values ([(expr expr-rest)
                          (extract-one-expression def-rest icheck)])
              ;; cons'ing the expr onto the defs rather than returning it separately
              (values (reverse (cons expr defs-so-far)) expr-rest)))))
    (defs-helper sexp-stream empty))

  (define (handle-stream sexp-stream past-objects icheck)
    (if (stx-null? sexp-stream)
        (reverse past-objects)
        (let-values ([(first-thing rest-of-sexp-stream)
                      (extract-one-definition/expression sexp-stream icheck)])
          (handle-stream rest-of-sexp-stream (cons first-thing past-objects) icheck))))
  
  ;(handle-stream sexp-stream empty TOP-I)  )
  (handle-stream sexp-stream empty DUMMY-ICHECK)  )

;(define (top-level body-exprs)
(define-syntax (top-level body-exprs)
  (syntax-case body-exprs () ;; _ is %module-begin
    [(_ bodies ...)
     (begin       
       (define bodies-stx #'(bodies ...))
       (printf "bodies-stx: ~v~n" bodies-stx)
       ;(printf "~v~n" bodies-stx)
       (define sexp-stream (syntax->list bodies-stx))              
       (printf "sexp-stream: ~v~n" sexp-stream)
       (with-syntax
           ([(processed-bodies ...)
             (process-sexp-stream sexp-stream)])
       ;(syntax (#%module-begin processed-bodies ... (run-tests) (display-results))))]))
       ; TN: Just produce the (Policy ...) for processing by our macros
         (printf "processed-bodies: ~v~n" #'(processed-bodies ...))
         (define result-syntax (syntax (#%module-begin  processed-bodies ...)))
         
         (printf "Result stx: ~v~n" result-syntax)
         
         result-syntax))]))


; p4p depends on the default reader. Thus, we cannot make :- equate to ,
; unless we slightly change the default reader:

(define parse-colon
    (case-lambda
     [(ch port)
      ; ‘read' mode
      (define n (read-char port))
      (unless (equal? #\- n)
        (error "colon must be followed by dash"))
      ; not enough just to insert 'unquote here. need to enclose the rest in a list:
      (list 'unquote (read/recursive port #f :-readtable))]
     [(ch port src line col pos)
      ; ‘read-syntax' mode
      (define n (read-char port))
      (unless (equal? #\- n)
        (raise-read-error
                 "colon must be followed by dash" 
                 src line col pos 1))
      (datum->syntax #f 
                     (list 'unquote (read-syntax/recursive src port #f :-readtable))
                     (list src line col pos 1)
                     )]))
(define :-readtable
    (make-readtable (current-readtable) #\: 'terminating-macro parse-colon))

(define test0 (open-input-string "z,(x,y)"))
(define result0 (parameterize ([current-readtable :-readtable])
  (list (read-syntax "test0" test0) (read-syntax "test0" test0))))
(define test (open-input-string "z:-(x:-y)"))
(define result (parameterize ([current-readtable :-readtable])
  (list (read-syntax "test" test) (read-syntax "test" test))))
(check-true (equal? (syntax->datum result0) (syntax->datum result)))
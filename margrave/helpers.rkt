#lang racket

(require srfi/13                  
         syntax/readerr)

(provide (all-defined-out))

;****************************************************************

; HELPERS

; using a mutable hash table for now. if switch to immutable, can fold table creation over the-list
(define (partition* bucket-func the-list #:init-keys [init-keys '()])
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


; (partition* (lambda (x) (and (even? x) (remainder x 3))) '(1 2 3 4 5 6 7 8 9 10))
; '#hash((0 . (6)) (2 . (8 2)) (1 . (10 4)))

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

(define (symbol->string/safe arg)
  (if (symbol? arg)
      (symbol->string arg)
      arg))


(define (wrap-list-parens lst)
  (fold-append-with-spaces (map (lambda (str) (string-append "(" str ")")) lst)))

(define (safe-get-margrave-collection-path)
  (with-handlers ([(lambda (e) (exn:fail:filesystem? e))
                   (lambda (e) #f)])
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
(define (file-exists?/error file-path src-syntax [error-message (format "File did not exist: ~a~n" file-path)])
  (cond [(and file-path (file-exists? file-path))
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
  (define file-path (build-path file-name))
  (define actual-file-name (path->string (file-name-from-path file-path)))
  (define actual-path (path-only/same file-path))
  (define safe-path (build-path/file-ci actual-path actual-file-name))
  (and (file-exists?/error safe-path src-syntax error-message)
       (open-input-file safe-path)))



(define (path-only/same the-path)
  (define p (path-only the-path))
  (if p
      p
      (build-path 'same)))

;;;;;;;;;;;;;;;;;;;;;;;;
; Deal with the fact that the .p file contains a vocab name, 
; and the name's capitalization may not match the file's.
; Returns #f if no such file exists.
; Does not accept 'same or 'up
(define/contract (build-path/file-ci location want-filename) 
  [path? string? . -> . (or/c path? #f)]  
  
  ;(printf "buildpath/file-ci: loc: ~v want-filename: ~v curr-dir: ~v~n" location want-filename (current-directory))
  
  (define (my-filter fullpath)
    (define fname (file-name-from-path fullpath))    
    (and fname (string-ci=? want-filename (path->string fname))))
  
  ; Don't use find-files, because that recurses and we want to be local. 
  ; (It also has problems if one of the dirs it is recurring on lacks permissions...)

  (define folder-contents-fullpaths (map (lambda (fpath) (build-path location fpath))
                                         (directory-list location)))  
  (define files-in-folder (filter file-exists? folder-contents-fullpaths))  
  ;(printf "Testing vs. files: ~v~n" files-in-folder)
  (define files-found (filter my-filter files-in-folder))  
  (cond [(< (length files-found) 1) #f]
        [(> (length files-found) 1) (raise-user-error (format "Ambiguous case-insensitive file name. Asked for ~v in ~v, but there were multiple matches: ~v" 
                                                   want-filename (path->string location)
                                                   (map path->string files-found)))]
        [else (first files-found)]))


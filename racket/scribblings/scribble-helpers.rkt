#lang racket

(require scribble/base
	 scribble/core
	 scribble/basic
	 scribble/html-properties
	 scribble/latex-properties
         
         ;; added by tim
         (except-in scribble/manual
                    code
                    math)
	 )

(provide (all-defined-out))

(define (lecstyle name)
  (make-style name (list (make-css-addition "lecnotes.css"))))

(define summarystyle (lecstyle "summaryblock"))
(define assumestyle (lecstyle "assumption"))
(define todostyle (lecstyle "todo"))
(define notestyle (lecstyle "note"))
(define lecstepstyle (lecstyle "lecstep"))
(define exercisestyle (lecstyle "exercise"))

(define (defn . t) (apply italic t))
(define (keywd k) (bold k))
(define (code . e) (apply tt e))
;(define (exercise . t) (apply nested #:style 'inset t))
(define (codedisp . e) (apply verbatim #:indent 2 e))
(define (math . e) (apply italic e))

(define (bracket L)
  (append (list "[") L (list "]")))

(define (summary . e) (apply nested #:style summarystyle e))

(define (assumption . e) (apply nested #:style assumestyle e))

(define (insert . t) (elem #:style todostyle 
			   (apply italic (bracket (cons "INSERT " t)))))

(define (fill . t) (elem #:style todostyle (bracket t)))

;(define (todo . t) (elem #:style todostyle (bracket t)))
(define (todo . t) "")

;(define (note . t) (elem #:style notestyle (bracket t)))
(define (note . t) "")


;(define (lecturestep . t) (elem #:style lecstepstyle 
;	                        (apply italic (bracket t))))

(define (lecturestep . t) "")


(define (exercise . t) (apply nested #:style exercisestyle 
			      (cons "Exercise: " t)))

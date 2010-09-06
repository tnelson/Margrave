;    Copyright (c) 2009-2010 Brown University and Worcester Polytechnic Institute.
;    
;    This file is part of Margrave.

;    Margrave is free software: you can redistribute it and/or modify
;    it under the terms of the GNU Lesser General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    Margrave is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU Lesser General Public License for more details.
;
;    You should have received a copy of the GNU Lesser General Public License
;    along with Margrave.  If not, see <http://www.gnu.org/licenses/>.


; This module loads Margrave and starts the REPL, then initializes the
; Java engine automatically. When creating a Margrave distribution 
; *without* DrRacket, do Racket -> Create Executable from *this*
; module. 

; If you have DrRacket installed, there is no reason to use
; this module. - tn

#lang racket

(require "margrave.rkt"
         "margrave-ios.rkt")

;****************************************************************
(define-namespace-anchor repl-namespace-anchor)
(define margrave-repl-namespace (namespace-anchor->namespace repl-namespace-anchor))

; Cannot auto-load because we don't know where the user has installed the engine.
;(start-margrave-engine)

; Just calling read-eval-print-loop results in an error (no #%app ...)
; Works if we give it a namespace
(parameterize ([current-namespace margrave-repl-namespace])
  (read-eval-print-loop))

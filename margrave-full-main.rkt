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


; This is the main module for Full Margrave 
; Click "Run" and enter commands at the prompt at the bottom of the screen.

#lang racket

(require "margrave.rkt"
         "margrave-ios.rkt")


;****************************************************************


; Defaults to the value of MARGRAVE_HOME environment var.
; If no such var, uses current directory.
(start-margrave-engine)





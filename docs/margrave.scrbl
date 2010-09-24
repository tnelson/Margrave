#lang scribble/manual

@; [#:style '(toc)]
@title{The @bold{Margrave} Tool for Policy Analysis}
@author+email["Tim Nelson" "tn@cs.wpi.edu"]

@; Don't need a ToC here since the sidebar on the left also gives it.
@;table-of-contents[]

@include-section["getting-started.scrbl"]

@include-section["commands.scrbl"]

@include-section["representing-policies.scrbl"]

@; Not including API yet, just basic user docs and examples
@;include-section["api.scrbl"]

@;index-section[]

@bibliography[ (bib-entry #:key "nbdfk10"
                          #:title "The Margrave Tool for Firewall Analysis"	 	 	 	                           	 
                          #:author "Timothy Nelson and Christopher Barratt and Daniel J. Dougherty and Kathi Fisler and Shriram Krishnamurthi"
                          #:location "Proceedings of the 24th USENIX Large Installation System Administration Conference (LISA 2010)"
                          #:date "2010"	 	 	 	 
                          #:url "http://web.cs.wpi.edu/~tn/Publications/lisa10-margrave-final.pdf")]
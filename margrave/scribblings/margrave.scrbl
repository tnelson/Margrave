#lang scribble/doc

@; Don't use #lang scribble/manual, or we get the current racket version in the upper-right of the document.
@(require scribble/manual)

@title{The @bold{Margrave} Tool for Policy Analysis}
@author[@author+email["Tim Nelson" "tn@cs.wpi.edu"]
         "Christopher Barratt"
         "Daniel J. Dougherty"
         "Kathi Fisler"
         "Shriram Krishnamurthi"
         "Varun Singh"]


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
                          #:url "http://web.cs.wpi.edu/~tn/publications/index.html#nbdfk10")]
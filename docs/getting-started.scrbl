#lang scribble/manual

@title{Getting Started}

@;defmodule[margrave]

Welcome to Margrave!

Margrave is a tool for analysis of access-control and configuration
policies. Margrave allows users to ask questions about the 
behavior of their policies, such as "Can a student ever access
another student's grades?" or "Does this new firewall allow any
unexpected new traffic?" 

The following examples demonstrate how to get started using Margrave.
If you have not yet installed Margrave, see @secref{install}. 
If you have an IOS firewall configuration and want to get started
immediately, see @secref{gs-ios}. 
To see a series of instructive examples, see @secref{gs-existing}. 
If you want to create a specific policy in Margrave's
intermediate language, see @secref{gs-create). 

@;The following examples demonstrate Margrave, first on a general
@;access-control policy, and then on an IOS firewall configuration.

Margrave is part of an ongoing research project. We appreciate your
feedback and suggestions (including feature requests). These can be
sent to tn@"@"cs.wpi.edu.

@;--------------------------------------------------------------------
@section[#:tag "install"]{Installing Margrave}

There are two versions of Margrave available: lite and full. The lite
version allows commands and queries to be entered at a prompt. The full
version requires a Racket installation, but provides a GUI and better
support for writing programs involving Margrave queries. 

<<< The "prompt" is really a repl, don't make it sound like they enter
    EXPLORE... directly. >>>

@subsection{Lite Margrave}

<< FILL>>>



@subsection{Full Margrave}
    
* Make sure that you have a recent version of Java installed. Margrave
requires <<<FILL>>> or later. You can download the latest version of 
Java at <<<FILL>>>.

* Download and install Racket. You can download Racket at <<<FILL>>>.

* Download the FULL Margrave distribution from <<<FILL>>>. This is 
available both as a zip file and as a tar.gz file.

* Extract the full distribution to the location of your choice.

* Set your MARGRAVE_HOME environment variable to the location of 
margrave.rkt in the distribution. 


<<< Ok, now what? Redirect to example? Actually: running margrave!>>>


Notes:

When running the full version, always invoke 
@racket{(start-margrave-engine)} before
using any of the commands in this section. 

<<< Can we make it so this is called automatically when the margrave 
module loads? Would that be good or bad? >>>

@;--------------------------------------------------------------------
@section[#:tag "running-margrave"]{Running Margrave}



  
@;--------------------------------------------------------------------
@section[#:tag "gs-ios"]{IOS in Margrave}

<< Does this section assume that the reader has at least skimmed the 
LISA paper? >>>


Margrave supports a core subset of the IOS language that involves 
<<FILL>>. To parse and load an IOS policy into Margrave, use:

@racket{(parse-and-load-ios config-file-name config-file-path)}

where @racket{config-file-name} is the file name of the configuration 
saved as a text file and @racket{config-file-path} is the directory
containing the configuration. 

Margrave will produce several intermediate policy files in
@racket{config-file-path} and load them (provided the engine has
been started; see @secref{running-margrave}). 

For instance, if you have a configuration saved to a file demo.txt in
the directory "C:\Margrave\IOS", you should invoke:

@racket{(parse-and-load-ios "demo.txt" "C:\\Margrave\\IOS")}.

To avoid the awkward double-backslash in Windows, you can use the
@racket{build-path} Racket function, e.g.:
@racket{(parse-and-load-ios "demo.txt" (build-path "C:" "Margrave" "IOS"))}.




<<<sub-policy decomposition?>>

<<queries>>


@;--------------------------------------------------------------------
@section[#:tag "gs-existing"]{Some Quick Examples}


@;--------------------------------------------------------------------
@section[#:tag "gs-create"]{Creating Your Own Policy}


 
 
 


@;--------------------------------------------------------------------
@section[#:tag "getting-started-1"]{An Access-Control Example}

First, let's consider an access-control policy for a conference management system. 

@subsection{What Does A Policy Look Like?}

A @tech{policy} is a list of @tech{rules} and a method of resolving rule
conflicts, such as @tech{first-applicable} or @tech{permit-overrides}.

A @tech{rule} asserts a @tech{decision} for certain @tech{requests}.

But what is a @tech{request} exactly, and what @tech{decision}s can the policy render? 
Don't they depend on the application?

Yes. Margrave allows users to specify these things using 
@tech{vocabularies}. For details, see @secref["vocabularies"]; for this
first example, a request contains a subject, an action, and a resource.
A decision is either permit or deny.

@subsection{Loading Policies}

<<Give sufficient context before this so the user understands what "use the load-policy function" means>>

To load a policy, use the @racket{load-policy} function. 
E.g., to load our conference-manager example:

@racket[(load-policy "tests/conference.p")]
                 
@subsection{Asking Questions}

Let's ask Margrave whether a reviewer can ever be denied access to read a paper. The following Margrave query captures this question:

<<< need to insert this into examples rkt file and test >>>

@racketblock[
EXPLORE Conference:Deny(s, a, r) AND 
        reviewer(s) AND paper(r) AND readpaper(a)
]        

For more information on Margrave's query language, see @secref["query-language"].
 
We can ask for satisfying scenarios with:

@racketblock[
SHOW ALL
]
            
One of the solutions will be this one:

@racketblock[
*** SOLUTION: Size = 3.
$s: reviewer author
$r: paper
$a: readpaper
conflicted = {[$s, $r]}
assigned = {}
]

@subsection{Understanding Scenario Output}

The block above represents a scenario where the query could be 
satisfied. "SHOW" commands format query results and display them 
in this concise format. The scenario above says:
"The query can be satisfied by when the Subject is
both a Reviewer and an Author, the Resource is a Paper, and 
the action is reading the paper, provided that
the subject is Conflicted on the Paper but not Assigned to it."

Here, $s $a and $r correspond to the variables that appear in
the query.
Size = 3 means that in this scenario, there were 3 objects. 
In this case, one is BOTH a Reviewer and an
Author, another is a Paper, and the third is the action ReadPaper.

Conflicted and Assigned are
binary @tech{predicates} mentioned in the policy. Any such facts 
will be printed after information about individual variables.

Note: When printing, only the most specific applicable 
information will be shown. E.g., you will never see
$s: reviewer subject
because a reviewer is always a subject.



@;--------------------------------------------------------------------
@section{An IOS Firewall Example}

Margrave has built-in parsers to handle certain industry-standard 
configuration languages, such as Cisco's IOS language. In this
example, we bring Margrave to bear on an IOS configuration, as well
as asking more complex queries.

<<< Don't make it sound like we support *the entire IOS language* >>>


@subsection{Loading IOS Configurations}

<<how to load >>

@subsection{Change-Impact Analysis}

<< from section 2; demo.rkt >>


@subsection{Rule Applicability}

<<_matches and _applies IDBs>>

<< rule-responsibility; section 2; demo.rkt >>

<< checking for superfluous rules >>

@subsection{Using Margrave Output in Your Programs}

The above query lists all superfluous firewall rules --- it would be even better if we could explain @italic{why} the rules are superfluous. Query output can be re-used programatically to create queries based on prior results. Instead of simply displaying the response, we save it as a racket list:

<<...>>

and then use that list to construct a new query:

<<...>>

The result of this query is a map from each superfluous rule to the set of non-superfluous rules that overlap it. Of course, this mapping can be either printed,

<<example entry in the map>>

or saved as a Racket hash table for re-use:

<<code to save>>


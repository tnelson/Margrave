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
To see a series of instructive non-IOS examples, see 
@secref{gs-existing}. 
If you want to create a specific policy in Margrave's
intermediate language, see @secref{gs-create}. 

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

@subsection{Lite Margrave}

<<< Note: need to get DrRacket on *nix to create Lite for *nix >>>

@itemlist[
  @item{Make sure that you have a recent version of Java installed.
        Margrave requires Java 6 or later. You can download the latest
        version of Java at
        
        @url{http://www.java.com/en/download/}.}
  @item{Download the LITE Margrave distribution from
        
        <<<FILL>>>.
        
        This is available for both Windows (as a zip file) and 
        *nix (as a tar.gz file).}
  @item{Extract the distribution archive to the location of your choice.}
  @item{Set your MARGRAVE_HOME environment variable to the location of
        margrave-repl.exe in the extracted distribution.}      
]




@subsection{Full Margrave}

@itemlist[
  @item{Make sure that you have a recent version of Java installed.
        Margrave requires Java 6 or later. You can download the latest
        version of Java at 
        
        @url{http://www.java.com/en/download/}.}
  @item{Download and install Racket. You can download Racket at
        
        @url{http://racket-lang.org/download/}}
  @item{Download the FULL Margrave distribution from
        
        <<<FILL>>>.
        
        This is available for both Windows (as a zip file) and 
        *nix (as a tar.gz file).}
  @item{Extract the distribution archive to the location of your choice.}
  @item{Set your MARGRAVE_HOME environment variable to the location of
        margrave.rkt in the extracted distribution.}
]


@;--------------------------------------------------------------------
@section[#:tag "running-margrave"]{Running Margrave}

@subsection{Running Lite Margrave}

Execute margrave-repl.exe. It will automatically detect where your Java
installation is located, and attempt to start the Margrave engine from
the directory you provided via the MARGRAVE_HOME environment variable.

The command prompt accepts Racket commands (in Lisp terms, it is a  
read-eval-print-loop, or REPL). For instance, entering:

@racket[(display-response (mtext "INFO"))]

will execute the @tech{INFO} Margrave command, returning information about the 
state of Margrave, including memory usage and other statistics. The 
@tech{mtext} function runs the command and the @tech{display-response}
function prints human-readable results.

To exit Lite Margrave, type @racket[(exit)] at the command prompt. If
you close Lite Margrave via ctrl-C, the Java-based engine may be left 
running.

For more information, see the files in <MARGRAVE_HOME>/examples/lite.

@subsection{Running Full Margrave}

With Full Margrave, you can use Margrave in your Racket programs. Simply
require the appropriate modules! 

@margin-note{
  @bold{Caution:}
   
  When running the full version, always invoke 
  @racket[(start-margrave-engine)] before
  attempting to load policies or execute commands.}

For more information, see the files in <MARGRAVE_HOME>/examples/full.

  
@;--------------------------------------------------------------------
@section[#:tag "gs-ios"]{IOS in Margrave}

Margrave supports a core subset of the IOS language that involves 
<<FILL>>. The IOS modules implement the abstraction given 
in <<<FILL: LISA paper>>>.

To parse and load an IOS policy into Margrave, use:

@racket[(parse-and-load-ios config-file-name config-file-path)]

where @racket[config-file-name] is the file name of the configuration 
saved as a text file and @racket[config-file-path] is the directory
containing the configuration. 

Margrave will produce several intermediate policy files in
@racket[config-file-path] and, if the engine has been started,
load them.

For instance, if you have a configuration saved to a file config.txt in
the directory "C:\Margrave\IOS", you should invoke:

@racket[(parse-and-load-ios "config.txt" "C:\\Margrave\\IOS")].

To avoid the awkward double-backslash in Windows, you can use the
@racket[build-path] Racket function, e.g.:
@racket[(parse-and-load-ios "config.txt" (build-path "C:" "Margrave" "IOS"))].


For detailed examples of running queries in IOS, see the IOS examples
in <MARGRAVE_HOME>/examples/full or <MARGRAVE_HOME>/examples/lite 
(depending on your version).

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


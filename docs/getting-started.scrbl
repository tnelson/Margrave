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
For an instructive non-IOS examples, see 
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

<<< TN Note: need to get DrRacket on *nix to create Lite for *nix >>>

@itemlist[
  @item{Make sure that you have a recent version of Java installed.
        Margrave requires Java 6 or later. You can download the latest
        version of Java at
        
        @url{http://www.java.com/en/download/}.}
  @item{Download the LITE Margrave distribution from
        
        @url{http://www.margrave-tool.org/v3}.
        
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
        
        @url{http://www.margrave-tool.org/v3}.
        
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

To load and run an example script for Margrave Lite, use the @racket[run-lite] function.
For instance, to load the example IOS firewall script, enter this at the 
REPL prompt:

@racketblock[(run-lite 
              (build-path margrave-home-path
                          "examples" "lite" "lite-ios-demo.rkt"))]

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

For more information, see the files in @italic{<MARGRAVE_HOME>/examples/full}.

  
@;--------------------------------------------------------------------
@section[#:tag "gs-ios"]{IOS in Margrave}

Margrave supports a core subset of the IOS language involving
standard and most extended ACLs, static NAT,
ACL-based and map-based dynamic NAT, static routing,
and policy-based routing. At the moment, our support for state is
limited to reflexive access-lists.

The IOS modules implement the abstraction given 
in @cite{nbdfk10}.

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
in @italic{<MARGRAVE_HOME>/examples/full} or @italic{<MARGRAVE_HOME>/examples/lite} 
(depending on your version).

@;--------------------------------------------------------------------
@section[#:tag "gs-existing"]{General Policies in Margrave}

Margrave applies to more than just IOS firewall policies. Its intermediate
policy language can express many different kinds of policy. In this section, 
we discuss how to use Margrave's intermediate policy language to express
and analyze non-IOS policies. 

@italic{What does a policy look like in Margrave?}

For details, see @secref{policies}, but for the moment
a @tech{policy} is just a list of @tech{rules} that each map certain 
@tech{request}s to @tech{decision}s. 

@;and a method of resolving rule conflicts, such as @tech{first-applicable} or @tech{permit-overrides}.
 
@italic{But what is a @tech{request} exactly, and what @tech{decision}s can the policy render?} 
 
That depends on what the policy is meant to do. 
A policy for a door lock may take keys as requests, and decide whether or not the door opens. 
A phone company policy may take source and destination phone numbers and decide whether the call is denied, toll-free, or toll. 
Margrave allows users to specify these domain-specific details about a policy using @tech{vocabularies}, which are discussed
in detail in @secref["vocabularies"].

@subsection{A Brief Example}

Let's examine one of Margrave's built-in examples, 
an access-control policy for a conference management system.
In this basic example, a request contains a subject, an action,
and a resource, and a decision is either permit or deny.

Margrave loads policies using the @racket[load-policy] function. 
@racket[load-policy] takes a single parameter: the filename of the policy file.
Since this policy is stored in the tests subfolder of Margrave, we load it by 
calling:

@racket[(load-policy (build-path margrave-home-path "tests" "conference1.p"))]

If the policy loads successfully, @racket[load-policy] returns the policy's identifier for use in queries. In this case, it returns:

@racket["ConferencePolicy"]

This policy acts based on the roles of users (Are they a reviewer? An author?) and the nature of the resource they are trying to access (A paper? A review?)

Let's ask Margrave whether a reviewer can ever be denied access to read a paper.
The following Margrave query captures this question. 

<<< need to insert this into examples rkt file and test >>>

@racketblock[
EXPLORE ConferencePolicy:Deny(s, a, r) AND 
        reviewer(s) AND paper(r) AND readpaper(a)
]        

@italic["reviewer(s)"], @italic["paper(r)"] and @italic["readpaper(a)"]
signify properties about the request: The subject is a reviewer,
etc. The term 
@italic["ConferencePolicy:Deny(s, a, r)"] states that the policy
renders the "Deny" decision for the request. @margin-note{Rendering deny is
not the same as failing to render permit!}
The @racket[EXPLORE] statement as a whole instructs Margrave to find 
scenarios that meet all of those conditions.

(For more information on Margrave's query language, see @secref["query-language"].) 
 
Once the query is created, 
we can ask for @italic{all} scenarios that satisfy the query with:

@racketblock[
SHOW ALL
]
            
whereas 

@racketblock[
SHOW ONE
]

shows only the first scenario found.

One of the solutions will be this one:

@racketblock[
*** SOLUTION: Size = 3.
$s: reviewer author
$r: paper
$a: readpaper
conflicted = {[$s, $r]}
assigned = {}
]

@subsubsection{Understanding Scenario Output}

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
@italic{$s: reviewer subject}
because a reviewer is always a subject.


@;--------------------------------------------------------------------
@section[#:tag "gs-create"]{Creating Your Own Policy}

To create your own policy, first decide on its vocabulary. What decisions should it render? What do its requests look like? What contraints always hold in the policy's domain? Then consult @secref["vocabularies"]. Create a text-file with the vocabulary expression and save it with the .v extension.

Once your vocabulary is created, consult @secref["policies"], and create your policy expression in a text-file in the same directory with the .p extension. Make sure that you refer to the correct vocabulary name in the policy file. For instance, if your vocabulary is in the file myvocab.v, make sure that the vocabulary is named @italic{myvocab} and the policy @italic{uses myvocab}.





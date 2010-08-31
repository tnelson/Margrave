#lang scribble/manual

@title{Getting Started}

@;defmodule[margrave]

Welcome to Margrave!

Margrave is a tool for analysis of access-control and configuration
policies. Margrave allows users to ask questions about the 
behavior of their policies, such as "Can a student ever access
another student's grades?" or "Does this new firewall allow any
unexpected new traffic?" 

The following examples demonstrate Margrave, first on a general
access-control policy, and then on an IOS firewall configuration.

Margrave is part of an ongoing research project. We appreciate your
feedback and suggestions (including feature requests). These can be
sent to tn@"@"cs.wpi.edu.

@;--------------------------------------------------------------------
@section[#:tag "running-margrave"]{Running Margrave}


<<< Context. DrRacket. Require Margrave. >>>


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


@;--------------------------------------------------------------------
@section{For More Information}

<< links to other example files, test files, etc. >>
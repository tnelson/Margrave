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
@section[#:tag "getting-started-1"]{"An Access-Control Example"}

First, let's consider an access-control policy for a conference management system. 

@subsection["What Does A Policy Look Like?"]

A @deftech{policy} is a list of @tech{rules} and a method of resolving rule
conflicts, such as @tech{first-applicable} or @tech{permit-overrides}.

A @deftech{rule} asserts a @tech{decision} for certain @tech{requests}.

But what is a @tech{request} exactly, and what @tech{decision}s can the policy render? 
Don't they depend on the application?

Yes. Margrave allows users to specify these things using 
@tech{vocabularies}. For details, see @secref["vocabularies"]; for this
first example, a request contains a subject, an action, and a resource.
A decision is either permit or deny.

@subsection["Loading Policies"]

<<no mention of DrRacket yet. So what's all this about functions?>>


To load a policy, use the @racket{load-policy} function. 
E.g., to load our conference-manager example:

@racket[(load-policy "tests/conference.p")]
                 
@subsection["Asking Questions"]

Let's ask Margrave whether a reviewer can ever be denied access to read a paper. The following Margrave query captures this question:

<<< need to insert this into examples rkt file and test >>>

@racketblock[
EXPLORE Conference:Deny(s, a, r) AND 
        reviewer(s) AND paper(r) AND readpaper(a)
]        

<<< for more info on the language, see... >>>
 
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

@subsection["Understanding Scenario Output"]

The block above represents a scenario where the query could be 
satisfied. "SHOW" commands format query results and display them 
in this concise format. The scenario above says:
"The query can be satisfied by when the Subject is
both a Reviewer and an Author, the Resource is a Paper, and 
the action is reading the paper, provided that
the subject is Conflicted on the Paper but not Assigned to it."

Size = 3 means that in this solution to the query, there were 3
objects in the universe. In this case, one is BOTH a Reviewer and an
Author, another is a Paper, and the third is the action ReadPaper.

Here, $s $a and $r correspond to the
variables that appear in the query. Conflicted and Assigned are
binary predicates mentioned in the policy. Any predicates that
represent an environment state (rather than a property of the
request itself) will be displayed fully.


<<< Clean up this language for a "getting started" user. ("Predicate? Huh?") >>>


Note: When printing, only the most specific applicable 
sort information will be shown. E.g., you will never see
$s: reviewer subject
because a reviewer is always a subject.




@;--------------------------------------------------------------------
@section["An IOS Firewall Example"]

Margrave has built-in parsers to handle certain industry-standard 
configuration languages, such as Cisco's IOS language. In this
example, we bring Margrave to bear on an IOS configuration, as well
as asking more complex queries.

<<< Don't make it sound like we support *the entire IOS language* >>>






<< from section 2; demo.rkt. Change-impact, Rule-responsibility, superfluous checking>>


@subsection["Using Margrave Output in Your Programs"]

<< Go from superfluous list to WHY each is superfluous >>


@;--------------------------------------------------------------------
@section["For More Information"]

<< links to other example files, test files, etc. >>
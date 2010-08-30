#lang scribble/base

@title{Getting Started}

Welcome to Margrave!

Margrave is a tool for analysis of access-control and configuration
policies. Margrave allows users to ask questions about the 
behavior of their policies, and responses with scenarios where the
behavior can occur.

<<< Maybe take better prose from LISA paper >>>




Margrave is part of an ongoing research project. We appreciate your
feedback and suggestions (including feature requests). These can be
sent to tn@"@"cs.wpi.edu.


@section["An Access-Control Example"]

<< Simple subject/action/resource example; use one from old examples script >>

@subsection["Loading Policies"]

<< load-policy >>

@subsection["Asking Questions"]

<< very simple query >>


Result:

*** SOLUTION: Size = 3.
$s: reviewer author
$r: paper
$a: readpaper
conflicted = {[$s, $r]}
assigned = {}

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



@section["An IOS Firewall Example"]

<< from section 2; demo.rkt>>


@subsection["Using Margrave Output in Your Programs"]

<< Suppose we wanted to... >>

@section["For More Information"]

<< links to other example files, test files, etc. >>
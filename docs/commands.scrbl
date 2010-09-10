#lang scribble/manual

@title{Margrave's Query and Command Languages}

@section[#:tag "query-language"]{The Query Language}

@margin-note{BNF and semantics coming later...}

Margrave's query language contains only one statement: @deftech{EXPLORE}. Explore statements instruct Margrave to look for scenarios that satisfy the statement's conditions. For instance,

@racketblock[EXPLORE administrator(x) OR (CEO(y) AND supervisor(y, x))]

tells Margrave to look for scenarios where @italic{x} is either an administrator or supervised directly by a CEO. When querying a policy, first use an EXPLORE statement to define the scenarios of interest, and then a SHOW or GET command (@secref["command-language"]) to get the results.

@tech{EXPLORE} statements have the following form:

@tech{EXPLORE} <condition> @linebreak{}
[@tech{UNDER} <policy-name>, ...] @linebreak{}
[@tech{PUBLISH} <variable-name>, ...] @linebreak{}
[@tech{TUPLING}] @linebreak{}
[@tech{INCLUDE} <id>, ...] @linebreak{}
[@tech{CEILING} <number>] @linebreak{}

Clauses enclosed in [] are optional, and can appear in any order.

The @deftech{UNDER} clause adds policies to the query's scope that do not appear in the query condition.
If a condition does not explicitly refer to a policy, the query must include an UNDER clause.

The @deftech{PUBLISH} clause dictates which variables in the query condition are published for use in later queries. 

The @deftech{TUPLING} clause activates the optional tupling optimization.

If @tech{TUPLING} is not enabled, then the @deftech{INCLUDE} clause contains a list of policy IDB names (see the Margrave firewall paper @cite{nbdfk10} for more information})that should be included explicitly in scenario output.

If @tech{TUPLING} is enabled, then the @tech{INCLUDE} clause contains a list of atomic formulas whose truth should be included explictly in scenario output.

The @deftech{CEILING} clause provides a limit on the scenario-sizes Margrave will check. In most cases, Margrave can infer its own sufficient ceiling. If not, Margrave uses the user-provided ceiling (or @italic{6}, if none is provided).



@section[#:tag "command-language"]{The Command Language}

The rest of Margrave's language consists of commands.

@subsection{Getting the Results of EXPLORE Statements}

If you just want to know if any scenarios exist to satisfy a query, follow it up with the IS POSSIBLE? statement.

To get concrete scenarios, use:

@deftech{SHOW ALL}: Returns a string containing @italic{all} satisfying scenarios, pretty-printed for Human consumption. @margin-note{Beware casual use of the SHOW and GET ALL commands, as some queries can have enormous numbers of satisfying scenarios.} 

@deftech{SHOW ONE}: Returns a string containing @italic{the first} satisfying scenario that Margrave finds, pretty-printed for Human consumption.

@deftech{SHOW NEXT}: Returns a string containing @italic{the next} satisfying scenario. Use in concert with SHOW ONE. @margin-note{Without a preceding SHOW ONE, the first SHOW NEXT will behave like SHOW ONE.}

@deftech{GET ONE}, @deftech{GET NEXT}, @deftech{GET ALL}: Same as SHOW, except returns an XML object for the scenario that can be used programatically.

If the @tech{TUPLING} optimization is enabled, the following commands also become available:

@deftech{SHOW (UN)REALIZED <atom>, ...} returns a list of all atomic formualas given that can be true in a satisfying scenario.

@deftech{SHOW (UN)REALIZED <atom>, ... FOR CASES <atom>, ...} returns a map taking each case to a separate @tech{SHOW (UN)REALIZED} list.
@margin-note{TN: Needs re-wording.}

The example files contain sample uses of each of these. SHOW REALIZED is especially useful when reasoning about interactions between rules.

@subsection{Renaming Prior Queries}

To rename a query or policy, use the @deftech{RENAME} command:

@italic{@tech{RENAME} old-identifier new-identifier}

@subsection{Getting INFO}

To get general information about the Margrave engine, including memory use and other statistics, use the @deftech{INFO} command:

@italic{@tech{INFO}}

To get information about a specific policy, vocabulary, or saved query, append the policy, vocabulary, or query identifier:

@italic{@tech{INFO} mypolicy}
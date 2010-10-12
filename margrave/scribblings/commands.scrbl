#lang scribble/manual

@(require "helper.rkt")

@title{Margrave's Command Language}

There are three kinds of Margrave commands: queries, presentation commands, and system commands.

@section[#:tag "query-language"]{The Query Sub-Language}

@subsection{@tt{EXPLORE}}

Explore statements instruct Margrave to look for scenarios that satisfy the statement's conditions. For instance, in:

@multiline-racketblock[                       
"EXPLORE ConferencePolicy1:Deny(s, a, r) AND 
        reviewer(s) AND paper(r) AND readpaper(a);"]   

@tt{reviewer(s)}, @tt{paper(r)} and @tt{readpaper(a)}
signify properties about the request: The subject is a reviewer,
etc. The term @tt{ConferencePolicy:Deny(s, a, r)} states that the policy
renders the ``Deny'' decision for the request. @margin-note{Rendering deny is
not the same as failing to render permit!}
The @tt{EXPLORE} statement as a whole instructs Margrave to find 
scenarios that meet all of those conditions.

When querying a policy, first use an EXPLORE statement to define the scenarios of interest, and then use a presentation command to get the results.

@tt{EXPLORE} statements have the following form:

@tt{EXPLORE} <condition> @linebreak{}
[@tt{UNDER} <policy-name>, ...] @linebreak{}
[@tt{PUBLISH} <variable-name>, ...] @linebreak{}
[@tt{TUPLING}] @linebreak{}
[@tt{INCLUDE} <id>, ...] @linebreak{}
[@tt{CEILING} <number>] @linebreak{}

Clauses enclosed in [] are optional, and can appear in any order.




A @deftech{vocabulary predicate} is the name of either a @tech{type} or
@tech{other predicate} in the query's @tech{vocabulary context}.
Every @tech{vocabulary predicate} has an arity no less than 1.
If a @tech{vocabulary predicate} is the name of a @tech{type}, 
its arity is 1, and it is called a @deftech{unary} predicate.

@; --------------------------------------------------------------
@; Variable vectors: explicit of <pol:req> style.

A @deftech{variable vector} is one of:

@itemlist[ @item{@tt{x}, where @tt{x} is a variable symbol;}
          @item{@tt{<id:req>}, where @tt{id} is the name of a policy or saved query;}
          @item{A comma-delimited sequence of @tech{variable vector}s.}
          ]

The @tech{variable vector} of the form @tt{<id:req>} is equivalent to explicitly stating the request vector of the vocabulary associated with @tt{id}.

Each @tech{variable vector} has an associated @italic{arity}. A single variable @tt{x} has arity 1. A @tt{<id:req>} reference has the same arity as @tt{id}'s @tech{request vector}. A comma-delimited sequence of vectors has arity equal to the sum of its sub-vectors' arities.

@; --------------------------------------------------------------
A <condition> is a boolean combination (via @tt{AND}, @tt{OR}, @tt{IFF}, @tt{IMPLIES}, @tt{NOT}, and parentheses) of @tech{atomic formula}s.

@; --------------------------------------------------------------

An @deftech{atomic formula} is one of:

@itemlist[
          @item{an @deftech{atomic vocabulary formula}, which is one of: 
                @itemlist[
                          @item{@tt{predname(X)}, 
                                 where @tt{X} is a @tech{variable vector} and @italic{predname} is a @tech{vocabulary predicate} of the same arity;}
                           
                          @item{@tt{x IN predname}, where @tt{x} is a variable symbol and 
                                 @tt{predname} is a unary @tech{vocabulary predicate};}
                          
                          @item{@tt{(X) IN predname},
                                 where @tt{X} is a @tech{variable vector} and @italic{predname} is a @tech{vocabulary predicate} of the same arity;}                          
                          
                          @item{@tt{x = typename}, where @tt{x} is a variable symbol and
                                 @tt{typename} is the name of an @tech{at-most-one}
                                 or @tech{singleton} constrained type in the
                                 statement's @tech{vocabulary context}}
                           ]}
          @item{an @deftech{atomic IDB formula}, which is one of:
                @itemlist[
                          @item{@tt{policyid:idbname(X)}, where @tt{policyid} is an identifier
                                for a @tech{policy} with a k-ary @tech{request vector}, @tt{idbname} is a valid @tech{IDB}
                                in that policy), 
                                and @tt{X} is a @tech{variable vector} of arity k;}
                           
                          @item{@tt{savedquery(X)}, where @tt{savedquery} is an identifier for a
                                k-ary saved query and @tt{X} is a @tech{variable vector} of arity k;}
                           ]}
          @item{or a @deftech{variable equality formula}: @tt{x = y}, where @tt{x} and @tt{y} are variable symbols.}
          ]


@margin-note{The saved query identifier @tt{last} always refers to the last successful @tt{EXPLORE} statement.}

@; --------------------------------------------------------------

Every @tt{EXPLORE} statement has a @deftech{vocabulary context}: the set of
vocabularies across all policies mentioned in the query's <condition>
and its @tt{UNDER} clause, if any. If the policies have incompatable vocabularies
(for example, if they both define the same predicate name, but with a
different arity) the query will produce an error. 

@;Every policy provides the following set of @deftech{IDB} names for use in queries:
@;@itemlist[
@;          @item{Each @tech{decision} that the policy can render;}
@;           @item{For each rule R, }
   @;        @item{For each rule R, }
  @;        ]

The term @deftech{IDB} is taken from the database field.
Each policy provides a set of @tech{IDB}s that represent
policy decisions, rule applicability, and more.
For details, see Section 2 of @cite{nbdfk10}. 

The @deftech{@tt{UNDER}} clause adds policies to the query's @tech{vocabulary context} that do not appear in the query <condition>.
If the <condition> does not explicitly refer to a policy, the query must include an @tt{UNDER} clause.

The @deftech{@tt{PUBLISH}} clause dictates which variables in the query condition are published for use in later queries. 

The @deftech{@tt{TUPLING}} clause activates the optional tupling optimization. In order to qualify for tupling, 
all of the query's saved-query predicates (if any) must refer to saved queries that @tt{PUBLISH}ed all of their variables. 

The syntax of the @deftech{@tt{INCLUDE}} clause differs depending on whether tupling has been enabled.



@itemlist[
          @item{If @tech{@tt{TUPLING}} is not enabled, then the @tech{@tt{INCLUDE}} clause contains a nonempty list of
                   policy @tech{IDB} predicates whose instantiations are to be included in the scenario
                   output.
                   
                   @bold{Example:}
                   @tt{INCLUDE mypolicy:permit} will cause each output scenario to include the requests that @tt{mypolicy} permits.
}
           
           
           @item{If @tech{@tt{TUPLING}} is enabled, then the @tech{@tt{INCLUDE}} clause may contain a nonempty list of IDB
                    and/or @tech{vocabulary predicate}s; however a tuple of
                    arguments must be supplied to each predicate.
                    Variable names shared between @tech{@tt{INCLUDE}} and 
                    the <condition> are assumed to refer to the same variable.
                    
                    @bold{Examples:}
                    @tt{INCLUDE mypolicy:permit(r, a, s)} causes each scenario to report if the request @tt{(r, a, s)} is permitted by mypol.
                    
                    @tt{INCLUDE mypolicy:permit(s, a, r), mypolicy:permit(r, a, s)} sets the watch on both @tt{(s, a, r)} and @tt{(r, a, s)}. }                                                                       
]


@; really is "truth" above. hundreds of INCLUDEs possible with tupling, and scenarios got very very cluttered. Consider use case: "which rule applies?" Only want to see the positives.

The @deftech{@tt{CEILING}} clause provides a limit on the scenario-sizes Margrave will check. In most cases, Margrave can infer its own sufficient ceiling (see the Margrave firewall paper @cite{nbdfk10}). If not, Margrave uses the user-provided ceiling (or @italic{6}, if none is provided).



@subsection{@tt{COMPARE}}

Margrave provides shorthand for change-impact queries in the form of the @tt{COMPARE} command. The syntax of @tt{COMPARE} is:

@tt{COMPARE} <policy> <policy> @linebreak{}
[@tt{TUPLING}] @linebreak{}
[@tt{CEILING} <number>] @linebreak{}

Running @tt{COMPARE} on a pair of policies is equivalent to writing an @tt{EXPLORE} statement that produces the scenarios in which the two policies render different decisions.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section[#:tag "presentation-language"]{The Presentation Sub-Language}

The presentation sub-language is concerned with getting the results of the previous query.
                                                                                       
@deftech{@tt{IS POSSIBLE?}}: If you just want to know if any scenarios exist to satisfy a query, follow it up with the @tt{IS POSSIBLE?} command.

To get concrete scenarios, use:

@deftech{@tt{SHOW ALL}}: Returns a string containing @italic{all} satisfying scenarios, pretty-printed for human consumption. @margin-note{Beware casual use of the SHOW ALL and GET ALL commands, as some queries can have enormous numbers of satisfying scenarios.} 

@deftech{@tt{SHOW ONE}}: Returns a string containing a single satisfying scenario that Margrave finds (in no particular order), pretty-printed for human consumption.

@deftech{@tt{SHOW NEXT}}: Returns a string containing a @italic{different} satisfying scenario, if one exists. Each successive use of @tt{SHOW NEXT} will produce a different scenario until all have been given. Using @tt{SHOW ONE} or defining a new query with @tt{EXPLORE} will cause Margrave to forget which scenarios it has already printed. @margin-note{Without a preceding @tt{SHOW ONE}, the first @tt{SHOW NEXT} will behave like @tt{SHOW ONE}.}

@deftech{@tt{GET ONE}}, @deftech{@tt{GET NEXT}}, @deftech{@tt{GET ALL}}: Same as above, except returns an XML object that represents the scenario and can be used in programs.

If the @tech{@tt{TUPLING}} optimization is enabled, the following commands also become available:




@tt{SHOW (UN)REALIZED <atom>, ...}: Given a list (``candidates'') of 
@tech{atomic IDB formula}s, @tech{atomic predicate formula}s, and
@tech{atomic type formula}s, 
returns the subset of the atomic formualas given that can be true (are never true) in a satisfying scenario. 
@bold{All candidates must be declared in the @tt{INCLUDE} clause.}

For example:

@multiline-racketinput[
"EXPLORE mypolicy:permit(s, a, r) 
INCLUDE mypolicy:rule10_matches(s, a, r)
TUPLING; 
SHOW REALIZED mypolicy:rule10_matches(s, a, r);"]


@tt{SHOW (UN)REALIZED <atom>, ... FOR CASES <atom>, ...}: 
Given two lists (``candidates'' and ``cases'') of @tech{atomic IDB formula}s, 
@tech{atomic predicate formula}s, and @tech{atomic type formula}s,
returns a map taking each case to a separate @tt{SHOW (UN)REALIZED} list
where the case was included in the <condition>.


The Margrave paper @cite{nbdfk10} contains more information on the subtleties of the @tt{SHOW REALIZED} command,
and the example files contain sample uses. @tt{SHOW REALIZED} is especially useful when reasoning about interactions between rules.




@section[#:tag "command-language"]{The Command Sub-Language}

The rest of Margrave's language comprises system commands.

@subsection{Loading Policies}

To load a policy in Margrave's intermediate language use the @tt{LOAD POLICY} command:

@margin-note{If the filename path begins with @racket[*MARGRAVE*], the @racket[*MARGRAVE*] will be replaced with Margrave's installation directory.}

@multiline-racketinput["LOAD POLICY <filename>"]

To load a Cisco IOS configuration (that uses the subset of IOS we support; see @secref["gs-ios"]) use the @tt{LOAD IOS} command:

@multiline-racketinput["LOAD IOS <filename>"]

This command will create several new policies in Margrave: @italic{InboundACL}, @italic{OutboundACL}, @italic{LocalSwitching}, @italic{NetworkSwitching}, @italic{StaticRoute}, @italic{PolicyRoute}, and @italic{DefaultPolicyRoute}, each representing a part of the configuration. (For details, see the Margrave paper @cite{nbdfk10}.)

@multiline-racketinput["LOAD IOS <filename> WITH <prefix> <suffix>"]

also creates the above 7 policies, but renames them with the given @tt{<prefix>} and @tt{<suffix>}.
For instance, given the command:

@multiline-racketinput["LOAD IOS \"myconfig.txt\" WITH \"pre\" \"suff\""]

instead of a policy named @italic{InboundACL}, one named @italic{preInboundACLsuff} will be created. To avoid naming conflicts, use this variant command when loading multiple IOS configurations in the same Margrave session.

To load multiple router configurations at once, use

@multiline-racketinput["LOAD IOS <filename1>, <filename2>,... IN <directory>"]

or

@multiline-racketinput["LOAD IOS <filename1>, <filename2>,... IN <directory> WITH <prefix> <suffix>"]

For instance:

@multiline-racketinput["LOAD IOS \"inner-router.txt\", \"outer-router.txt\" IN \"user/tn/myconfigs\""]

will include both configurations in the same Margrave policies. (For an example of this, see Section 6.2 of the Margrave firewall paper @cite{nbdfk10}.)

@margin-note{A relative path will be interpreted relative to the location of the script file. If the file is unsaved, or if you are working at the prompt, do not use a relative path.}

@subsection{Renaming Prior Queries}

To rename a query or policy, use the @tt{RENAME} command:

@tt{RENAME <old-identifier> <new-identifier>}

For example, suppose you want to compare two different versions of the same policy, which have the same name by default. You can load the first version, then

@tt{RENAME policy policy_orig}

then load the new version and

@tt{RENAME policy policy_new}

Now you can write a single query that refers to both policies, such as this partial change-impact query:

@tt{EXPLORE policy_old:deny(x) AND policy_new:permit(x)}

You can also use the @tt{RENAME} command to save the @tt{last} query created under a unique name:

@tt{RENAME last otherquery}

@subsection{Getting INFO}

To get general information about the Margrave engine, including memory use and other statistics, use the @tt{INFO} command:

@tt{INFO}

To get information about a specific policy, vocabulary, or saved query, append the policy, vocabulary, or query identifier:

@tt{INFO mypolicy}

@subsection{Comments}

Lines beginning with @tt{//} are comments and will be ignored by Margrave.


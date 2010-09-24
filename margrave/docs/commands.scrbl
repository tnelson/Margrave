#lang scribble/manual

@title{Margrave's Query and Command Language}

@;The Margrave command language is case-insensitive. 

@section[#:tag "query-language"]{The Query Sub-Language}

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




A @deftech{vocabulary predicate} is the name of either a @tech{type} or
@tech{other predicate} in the query's @tech{vocabulary context}.
Every @tech{vocabulary predicate} has an arity no less than 1.
If a @tech{vocabulary predicate} is the name of a @tech{type}, 
its arity is 1, and it is called a @deftech{unary} predicate.

@; --------------------------------------------------------------
@; Variable vectors: explicit of <pol:req> style.

A k-ary @deftech{variable vector} is one of:
@itemlist[ @item{@bold{x@subscript{1}, ..., x@subscript{k}}, where x@subscript{1}, ..., x@subscript{k} are (not necessarily distinct) variable symbols;}
          @item{@bold{<@italic{id}:req>}, where @italic{id} is the name of a policy or saved query with a k-ary @tech{request vector};}
          @item{@bold{X, Y}, where @italic{X} and @italic{Y} are @tech{variable vectors} whose arities total k.}
          ]

@; !!! writing

The @tech{variable vector} of the form <id:req> is equivalent to explicitly stating the request vector of the vocabulary associated with @italic{id}.

@; --------------------------------------------------------------
A <condition> is a boolean combination (via AND, OR, IFF, IMPLIES, NOT, and parentheses) of @tech{atomic formula}s.

@; --------------------------------------------------------------

An @deftech{atomic formula} is one of:

@;
@;@itemlist[
 @;         @item{an @deftech{atomic vocabulary formula}, which is one of: 
 @;               @itemlist[
 @;                         @item{@bold{predname(x@subscript{1}, ..., x@subscript{k})}, 
 @;                                where x@subscript{1}, ..., x@subscript{k} are (not necessarily distinct) variable symbols 
 @;                                and <predname> is a @tech{vocabulary predicate} of arity k;}
 @;                          
   @;                       @item{@bold{x IN predname}, where x is a variable symbol and 
  @;                               <predname> is a unary @tech{vocabulary predicate};}
    @;                      
      @;                    @item{@bold{(x@subscript{1}, ..., x@subscript{k}) IN predname},
        @;                         where x@subscript{1}, ..., x@subscript{k} are (not necessarily distinct) variable symbols
          @;                       and <predname> is a k-ary @tech{vocabulary predicate};}                          
            @;              
              @;            @item{@bold{x = typename}, where x is a variable symbol and
                @;                 <typename> is the name of an @tech{at-most-one}
     @;                            or @tech{singleton} constrained type in the
       @;                          statement's @tech{vocabulary context}}
         @;                  ]}
  @;        @item{an @deftech{atomic IDB formula}, which is one of:
    @;            @itemlist[
     @;                     @item{@bold{policyid:idbname(x@subscript{1}, ..., x@subscript{k})}, where policyid is an identifier
       @;                         for a @tech{policy} with a k-ary @tech{request vector}, idbname is a valid @tech{IDB}
         @;                       in that policy), 
           @;                     and x@subscript{1},...,x@subscript{k} are (not necessarily distinct) variable symbols;}
             @;              
               @;           @item{@bold{savedquery(x@subscript{1}, ..., x@subscript{k})}, where savedquery is an identifier for a
                 @;               k-ary saved query, and x@subscript{1}, ..., x@subscript{k} are (not necessarily distinct) variable symbols}
                   @;        ]}
@;          @item{or a @deftech{variable equality formula}: @bold{x = y}, where x and y are variable symbols.}
  @;        ]



@itemlist[
          @item{an @deftech{atomic vocabulary formula}, which is one of: 
                @itemlist[
                          @item{@bold{predname(X)}, 
                                 where @italic{X} is a @tech{variable vector} and @italic{predname} is a @tech{vocabulary predicate} of the same arity;}
                           
                          @item{@bold{x IN predname}, where x is a variable symbol and 
                                 <predname> is a unary @tech{vocabulary predicate};}
                          
                          @item{@bold{(X) IN predname},
                                 where @italic{X} is a @tech{variable vector} and @italic{predname} is a @tech{vocabulary predicate} of the same arity;}                          
                          
                          @item{@bold{x = typename}, where x is a variable symbol and
                                 <typename> is the name of an @tech{at-most-one}
                                 or @tech{singleton} constrained type in the
                                 statement's @tech{vocabulary context}}
                           ]}
          @item{an @deftech{atomic IDB formula}, which is one of:
                @itemlist[
                          @item{@bold{policyid:idbname(X)}, where @italic{policyid} is an identifier
                                for a @tech{policy} with a k-ary @tech{request vector}, idbname is a valid @tech{IDB}
                                in that policy), 
                                and X is a @tech{variable vector} of arity k;}
                           
                          @item{@bold{savedquery(X)}, where @italic{savedquery} is an identifier for a
                                k-ary saved query, and X is a @tech{variable vector} of arity k;}
                           ]}
          @item{or a @deftech{variable equality formula}: @bold{x = y}, where x and y are variable symbols.}
          ]


@margin-note{The saved query identifier @bold{last} always refers to the last successful EXPLORE statement.}

@; --------------------------------------------------------------

Every EXPLORE statement has a @deftech{vocabulary context}: the set of
vocabularies across all policies mentioned in the query's <condition>
and its UNDER clause, if any. If the policies have incompatable vocabularies
(for example, if they both define the same predicate name, but with a
different arity) the query will return an error. 

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

The @deftech{UNDER} clause adds policies to the query's @tech{vocabulary context} that do not appear in the query <condition>.
If the <condition> does not explicitly refer to a policy, the query must include an UNDER clause.

The @deftech{PUBLISH} clause dictates which variables in the query condition are published for use in later queries. 

The @deftech{TUPLING} clause activates the optional tupling optimization. In order to qualify for tupling, 
all of the query's saved-query predicates (if any) must refer to saved queries that @tech{PUBLISH}ed all of their variables. 

The syntax of the @deftech{INCLUDE} clause differs depending on whether tupling has been enabled.



@itemlist[
          @item{If @tech{TUPLING} is not enabled, then the @tech{INCLUDE} clause contains a nonempty list of
                   policy @tech{IDB} predicates whose instantiations are to be included in the scenario
                   output.
                   
                   @bold{Example:}
                   @italic{INCLUDE mypolicy:permit} will cause each output scenario to include the requests that @italic{mypolicy} permits.
}
           
           
           @item{If @tech{TUPLING} is enabled, then the @tech{INCLUDE} clause may contain a nonempty list of IDB
                    and/or @tech{vocabulary predicate}s; however a tuple of
                    arguments must be supplied to each predicate.
                    Variable names shared between @tech{INCLUDE} and 
                    the <condition> are assumed to refer to the same variable.
                    
                    @bold{Examples:}
                    @italic{INCLUDE mypolicy:permit(r, a, s)} causes each scenario to report if the request (r, a, s) is permitted by mypol.
                    
                    @italic{INCLUDE mypolicy:permit(s, a, r), mypolicy:permit(r, a, s)} sets the watch on both (s, a, r) and (r, a, s). }
           
@;           @item{If @tech{TUPLING} is not enabled, then the @tech{INCLUDE} clause contains a list of policy @tech{IDB} names (see the Margrave firewall paper @cite{nbdfk10} for more information) that should be included explicitly in scenario output. By default, no policy @tech{IDB}s will appear in scenario output, since there is overhead involved in explicitly computing each.}

@;            @item{If @tech{TUPLING} is enabled, then the @tech{INCLUDE} clause contains a list of @tech{atomic IDB formula}s, @tech{atomic predicate formula}s, and @tech{atomic type formula}s whose truth should be included in scenario output. The atomic formulas given must only use variables that appear in the <condition>.}                                                                       
                                                                       
]




@; really is "truth" above. hundreds of INCLUDEs possible with tupling, and scenarios got very very cluttered. Consider use case: "which rule applies?" Only want to see the positives.

The @deftech{CEILING} clause provides a limit on the scenario-sizes Margrave will check. In most cases, Margrave can infer its own sufficient ceiling (see the Margrave firewall paper @cite{nbdfk10}). If not, Margrave uses the user-provided ceiling (or @italic{6}, if none is provided).




@section[#:tag "command-language"]{The Command Sub-Language}

The rest of Margrave's language consists of commands.

@subsection{Getting the Results of EXPLORE Statements}

If you just want to know if any scenarios exist to satisfy a query, follow it up with the IS POSSIBLE? command.

To get concrete scenarios, use:

@deftech{SHOW ALL}: Returns a string containing @italic{all} satisfying scenarios, pretty-printed for human consumption. @margin-note{Beware casual use of the SHOW ALL and GET ALL commands, as some queries can have enormous numbers of satisfying scenarios.} 

@deftech{SHOW ONE}: Returns a string containing a single satisfying scenario that Margrave finds, pretty-printed for human consumption.

@deftech{SHOW NEXT}: Returns a string containing a @italic{different} satisfying scenario, if one exists. Each successive use of @tech{SHOW NEXT} will produce a different scenario until all have been given. Using SHOW ONE or defining a new query with EXPLORE will cause Margrave to forget which scenarios it has already printed. @margin-note{Without a preceding SHOW ONE, the first SHOW NEXT will behave like SHOW ONE.}

@deftech{GET ONE}, @deftech{GET NEXT}, @deftech{GET ALL}: Same as SHOW, except returns an XML object that represents the scenario and can be used in programs.

If the @tech{TUPLING} optimization is enabled, the following commands also become available:




@deftech{SHOW (UN)REALIZED <atom>, ...}: Given a list ("candidates") of 
@tech{atomic IDB formula}s, @tech{atomic predicate formula}s, and
@tech{atomic type formula}s, 
returns the subset of the atomic formualas given that can be true (are never true) in a satisfying scenario.

(The Margrave paper @cite{nbdfk10} contains more information on the subtleties of the SHOW (UN)REALIZED command.)


@deftech{SHOW (UN)REALIZED <atom>, ... FOR CASES <atom>, ...}: 
Given two lists ("candidates" and "cases") of @tech{atomic IDB formula}s, 
@tech{atomic predicate formula}s, and @tech{atomic type formula}s,
returns a map taking each case to a separate @tech{SHOW (UN)REALIZED} list
where the case was included in the <condition>.


The example files contain sample uses of each of these. SHOW REALIZED is especially useful when reasoning about interactions between rules.


@subsection{Loading Policies}

To load a policy in Margrave's intermediate language (@secref["gs-existing"]) use the @italic{LOAD POLICY} command:

LOAD POLICY @italic{filename}

To load a Cisco IOS configuration (that uses the subset of IOS we support, see @secref["gs-ios"]) use the @italic{LOAD IOS} command:

LOAD IOS @italic{filename}

This command will create several new policies in Margrave: @italic{InboundACL}, @italic{OutboundACL}, @italic{LocalSwitching}, @italic{NetworkSwitching}, @italic{StaticRoute}, @italic{PolicyRoute}, and @italic{DefaultPolicyRoute}, each representing a part of the configuration. (For details, see @cite{nbdfk10}.)

LOAD IOS @italic{filename} WITH @italic{prefix} @italic{suffix}

also creates the above 7 policies, but renames them with the given prefix and suffix.
For instance, given the prefix "pre" and the suffix "suff", instead of a policy named @italic{InboundACL}, one named @italic{preInboundACLsuff} will be created. To avoid naming conflicts, use this variant command when loading multiple IOS configurations in the same Margrave session.

@subsection{Renaming Prior Queries}

To rename a query or policy, use the @deftech{RENAME} command:

@italic{@tech{RENAME} old-identifier new-identifier}

For example, suppose you want to compare two different versions of the same policy, which have the same name by default. You can load the first version, then

@italic{RENAME policy policy_orig}

then load the new version and

@italic{RENAME policy policy_new}

Now you can write a single query that refers to both policies, such as this partial change-impact query:

@italic{EXPLORE policy_old:deny(x) AND policy_new:permit(x)}

You can also use the RENAME command to save the "last" query created under a unique name:

@italic{RENAME last otherquery}

@subsection{Getting INFO}

To get general information about the Margrave engine, including memory use and other statistics, use the @deftech{INFO} command:

@italic{@tech{INFO}}

To get information about a specific policy, vocabulary, or saved query, append the policy, vocabulary, or query identifier:

@italic{@tech{INFO} mypolicy}

@subsection{Comments}

Lines beginning with @italic{//} are treated as comments in Margrave scripts.


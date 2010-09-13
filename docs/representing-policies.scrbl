#lang scribble/manual

@title{Representing Policies}

Policies map @tech{request}s to @tech{decision}s. Each @tech{policy} is associated with exactly one @tech{vocabulary}.

@margin-note{We are in the process of making the vocabulary and policy languages more intuitive, and new documentation will be forthcoming.}

In this section, we use the policy and vocabulary from the conference policy presented in @secref["gs-existing"] as examples.

@subsubsub*section{The Example Policy}

@racketblock[(Policy ConferencePolicy1 uses conferencepolicy
        (Target )
        (Rules 
  	  (PaperNoConflict = (Permit s a r) :- (!Conflicted s r) (ReadPaper a) (Paper r))
	  (PaperAssigned = (Permit s a r) :- (Assigned s r) (ReadPaper a) (Paper r))
	  (PaperConflict = (Deny s a r) :- (Conflicted s r) (ReadPaper a) (Paper r))
        )
        (RComb FAC)
        (PComb FAC)
	(Children ))
]


@subsubsub*section{The Example Vocabulary}

@racketblock[(PolicyVocab ConferencePolicy
             (Types
              (Subject : Author Reviewer)
              (Action : SubmitPaper ReadPaper SubmitReview)
              (Resource : Paper Review))
             (Decisions 
              Permit
              Deny)
             (Predicates
              (Conflicted : Reviewer Paper)
              (Assigned : Reviewer Paper))

	     (ReqVariables (s : Subject)
                           (a : Action)
                           (r : Resource))
             (OthVariables )
             (Constraints
              (disjoint-all Resource)
              (disjoint-all Action)
              (atmostone-all Action)
	      (abstract Subject)
	      (abstract Action)
              (abstract Resource)
              (nonempty Subject)
              (nonempty Resource)))
             ]

@;--------------------------------------------------------------------
@section[#:tag "policies"]{Elements of a Policy}


A @deftech{policy} in Margrave contains the following elements.

@subsection{Policy Name}

Every policy has a name. It is this name that can be referenced in Margrave queries.

@racket[Policy ConferencePolicy1]

@subsection{Vocabulary Name}

Every policy has a single @tech{vocabulary} that dictates what a @tech{request} is and which @tech{decisions} the policy renders.

@racket[uses conferencepolicy]

@subsection{Target Condition}

Similar to a @tech{rule}, a policy's @deftech{target} must be satisfied before any of the policy's rules can apply. A policy with an empty @tech{target} always applies.

@racket[(Target )]

@subsection{Rules}

A list of statements mapping requests to decisions.  Each @deftech{rule} has a name, a decision, and a set of conditions.

@racketblock[(Rules 
  	  (PaperNoConflict = (Permit s a r) :- (!Conflicted s r) (ReadPaper a) (Paper r))
	  (PaperAssigned = (Permit s a r) :- (Assigned s r) (ReadPaper a) (Paper r))
	  (PaperConflict = (Deny s a r) :- (Conflicted s r) (ReadPaper a) (Paper r))
        )]

@subsection{Children}

A policy's @deftech{children} are a list of sub-policies that together form the parent policy. A policy contains either @tech{rules} or @tech{children} but never both.

@racket[(Children )]

@subsection{Subpolicy- and Rule-Combination Algorithms}

Used to resolve rule and subpolicy conflicts. Margrave currently supports @tech{first-applicable}, @tech{overrides}, and @tech{none} combinators for both rules and policies.

@racketblock[(RComb FAC)
        (PComb FAC)]

In @deftech{first-applicable} policies, rules apply in the order in which they appear. In @deftech{overrides} policies, the vocabulary gives an ordering on the decisions, and higher-priority decisions take precedence over lower-priority decisions. 







@;--------------------------------------------------------------------
@section[#:tag "vocabularies"]{Elements of a Vocabulary}

A policy for a door lock may take keys as requests, and decide whether or not the door opens. 
A phone company policy may take source and destination phone numbers and decide whether the call is denied, toll-free, or toll. 
Margrave allows users to specify these domain-specific details about a policy using @tech{vocabularies}.

A @deftech{vocabulary} in Margrave contains the following:

@subsection{Types (also called Sorts)}

A hierarchy of @deftech{type}s (@deftech{sort}s) that elements of a request may belong to. 

@racketblock[(Types
              (Subject : Author Reviewer)
              (Action : SubmitPaper ReadPaper SubmitReview)
              (Resource : Paper Review))]

Top-level sorts (Subject, Action, Resource in the above example) are always pairwise @tech{disjoint}.

@subsection{Predicates}

@deftech{Predicates} represent relations that may hold of request
properties in a scenario. For instance, the relation Assigned from
@secref{gs-existing} contains reviewer-to-paper assignments in a 
given scenario.

@racketblock[(Predicates
              (Conflicted : Reviewer Paper)
              (Assigned : Reviewer Paper))]

@subsection{Decisions}

@deftech{Decisions} that the policy renders. The standard examples are permit and deny, but others can appear in more detailed policies.

@racketblock[(Decisions 
              Permit
              Deny)]

@subsection{Request Variables}

An ordered list of variable names, denoting the shape of a request to all policies that use this vocabulary. For instance, in the access-control domain, the request variables will often be (subj, act, res). In the firewall domain, the request variables will involve IP addresses, ports, etc.

@racketblock[(ReqVariables (s : Subject)
                           (a : Action)
                           (r : Resource))]

@subsection{Constraints}

Domain-specific assertions about how sorts and predicates can be populated. Margrave will not consider scenarios that violate vocabulary constraints, ruling out non-sensical results.

@racketblock[ (Constraints
              (disjoint-all Resource)
              (disjoint-all Action)
              (atmostone-all Action)
	      (abstract Subject)
	      (abstract Action)
              (abstract Resource)
              (nonempty Subject)
              (nonempty Resource))]

Valid @deftech{constraint} types are:

@itemlist[
           @item{@bold{At-most-one}: If a sort is @deftech{at-most-one} constrained, it can never contain more than one thing.}
           @item{@bold{Singleton}: If a sort is @deftech{singleton} constrained, it must contain exactly one thing.}
           @item{@bold{Nonempty}: A @deftech{nonempty} sort must never be empty.}
           @item{@bold{Abstract}: An @deftech{abstract} sort is fully covered by its children. 
                  For instance, consider a sort Bit with child sorts ZeroBit and OneBit.
                  If Bit is abstract, every Bit must be either a zero or a one. }
           @item{@bold{Function}: A binary predicate P: (A x B) can be constrained to behave as a total or partial function from A to B.}
           @item{@bold{Disjoint}: Two sorts that are @deftech{disjoint} must never share an atom.}
           @item{@bold{Subset}: Used to enforce subset constraints that cannot be captured in the sort hierarchy. 
                  If sort A is constrained to be a @deftech{subset} of B, every scenario will have A contained in B, even if B is not a super-sort of A.}
           ]
           

The constraints @tech{Disjoint}, @tech{at-most-one}, @tech{singleton}, and @tech{nonempty}
can be applied to @italic{all} subsorts of a sort at once by using the @racket[-all] suffix.
#lang scribble/manual

@title{Representing Policies}

Policies map @tech{request}s to @tech{decision}s. Each @tech{policy} is associated with exactly one @tech{vocabulary}.
In this section, we demonstrate the elements of policies and vocabularies using the conference policy presented in @secref["gs-existing"].

@margin-note{We are in the process of making the vocabulary and policy languages more intuitive, and new documentation will be forthcoming.}



@;--------------------------------------------------------------------
@section[#:tag "policies"]{Elements of a Policy}

@subsubsub*section{The Example Policy}

@racketblock[
(Policy ConferencePolicy1 uses conferencepolicy
        (Target )
        (Rules 
          (PaperNoConflict = (Permit s a r) :- (!Conflicted s r) (ReadPaper a) (Paper r))
          (PaperAssigned = (Permit s a r) :- (Assigned s r) (ReadPaper a) (Paper r))
          (PaperConflict = (Deny s a r) :- (Conflicted s r) (ReadPaper a) (Paper r)))
        (RComb FAC)
        (PComb FAC)
        (Children ))
]


@;A @deftech{policy} in Margrave contains the following elements.

@itemlist[
          @item{@racket[ConferencePolicy1] is the @deftech{policy name}.
                 Use the policy name to reference the policy in Margrave queries.}
           
           @item{@racket[conferencepolicy] is the policy's @deftech{vocabulary name}.
                  Every policy has a single @tech{vocabulary} that dictates what a 
                  @tech{request} is and which @tech{decisions} the policy renders. }
           
          @item{@racket[(Target )] is the policy's @deftech{target}. The @tech{target}
                 specifies a guard on the policy's applicability. In order for any rule
                 in the policy to apply, the policy's @tech{target} condition must also
                 apply. For instance, @racket[(Target (Reviewer s))] would mean that the
                 policy applies only to subjects who are reviewers.

                 An empty @tech{target}, as shown above, means that
                 the policy can always apply.}
          
          
          @item{The @racket[(Rules ...)] construct gives the policy's @deftech{rule} set.
                    Each @tech{rule} has a name, a decision, and a set of @deftech{conditions}. 
                    Each @tech{condition} must be the use of a @tech{type} or @tech{predicate} 
                    in the policy's vocabulary. A "!" before a @tech{type} or @tech{predicate}
                    name means that the type or predicate does @italic{not} apply.
                    
                    The first rule in the example policy:
                    
                    @racketblock[(PaperNoConflict = (Permit s a r) :- (!Conflicted s r) (ReadPaper a) (Paper r))]
                    
                    is named @racket[PaperNoConflict], renders the @racket[Permit] decision, and applies when
                    the subject is trying to read a paper on which he or she is not conflicted. 
                    
                    The variable names that appear after the decision name (e.g. @racket[Permit s a r]) must exactly match
                    the names of the request variables declared in the policy's @tech{vocabulary}. New variables can
                    be introduced on the right hand side of the rule, in which case they are interpreted
                    existentially. E.g., this rule would apply for any subject who owns @italic{some} car,
                    regardless of the action and resource involved:
                    
                    @racketblock[(PermitIfOwnsCar = (Permit s a r) :- (Owns s c) (Car c))]
                    }
          
          @item{@racket[(RComb ...)] denotes the policy's
                 @deftech{rule-combination algorithm}. This is used to resolve rule conflicts. 
                 Margrave currently supports @tech{first-applicable}, @tech{overrides},
                 and @tech{none} combinators:


                 @itemlist[
                           @item{@racket[(RComb FAC)]: In @deftech{first-applicable} policies, rules apply in 
                                    the order in which they appear.}
                            @item{@racket[(RComb O <decisions>)]: In @deftech{overrides} policies, 
                                   the vocabulary gives an 
                                   ordering on the decisions, and higher-priority decisions
                                   take precedence over lower-priority decisions. E.g.,
                                   @racket[(RComb O CallPolice Deny Permit)] means that the
                                   CallPolice decision overrides the Deny and Permit decisions,
                                   and the Deny decision overrides the Permit decision.}
                            @item{@racket[(RComb none)]: In @deftech{none} policies, multiple rules (indeed, multiple 
                                     decisions) can apply to a single request.}
                            ]}
          
                                                                                            
          @item{Margrave also supports hierarchical policies via @racket[(Children ...)]: A policy's @deftech{children} are a list
                 of sub-policies that together form the parent policy. A policy
                 contains either @tech{rules} (in which case it is a
                 @deftech{leaf policy}) or @tech{children} but never both. For example:
                 
                 @racketblock[(Policy Parentpolicy uses myvocab
                                      ...
                                      (Rules)
                                      (Children (Policy Child1 ...
                                                        (Rules ...)
                                                        (Children))
                                                (Policy Child2 ...
                                                        (Rules ...)
                                                        (Children))))]
                 
                 Conflicts between sub-policies are resolved via a @deftech{policy-combination algorithm}, which is set
                 with the @racket[(PComb ...)] construct. The same options are available to policy combination as to
                 rule combination.}
                    
          ]
    

@;--------------------------------------------------------------------
@section[#:tag "vocabularies"]{Elements of a Vocabulary}

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



A policy for a door lock may take keys as requests, and decide whether or not the door opens. 
A phone company policy may take source and destination phone numbers and decide whether the call is denied, toll-free, or toll. 
Margrave allows users to specify these domain-specific details about a policy using @tech{vocabularies}.

A @deftech{vocabulary} in Margrave contains the following:

@itemlist[
          @item{The @racket[(Types ...)] construct defines a hierarchy of
                    @deftech{type}s (also called @deftech{sort}s) that 
                    elements of a request may belong to. 
                    
                    Top-level types (Subject, Action, and Resource in the above example)
                    are always pairwise @tech{disjoint}.}
           
           @item{The @racket[(Predicates ...)] construct defines a set of
                     @deftech{predicate}s that may hold of request
                     properties in a scenario. For instance, the binary @tech{predicate} Assigned from
                     @secref{gs-existing} contains reviewer-to-paper assignments in a 
                     given scenario.}
           
           @item{The @racket[(Decisions ...)] construct gives a set of @deftech{Decisions}
                     that policies using this vocabulary render. The standard examples are
                     permit and deny, but others can appear in more detailed policies.}
           
           @item{The @racket[(ReqVariables ...)] construct defines the shape of a request
                     in this vocabulary. The request variables are an ordered list of variable
                     names and a type assignment for each.                      
                     For instance, in the access-control domain, the request variables will 
                     often be (subj, act, res). In the firewall domain, the request variables
                     will involve IP addresses, ports, etc.}
           @item{The @racket[(Constraints ...)] construct gives domain-specific assertions
                     about how types and predicates can be populated. Margrave will not consider
                     scenarios that violate vocabulary constraints, ruling out non-sensical results. 
                     
                     Valid @deftech{constraint} types are:

                     @itemlist[
                               @item{@bold{At-most-one}: If a type is @deftech{at-most-one} constrained, it can never contain more than one thing.}
                                @item{@bold{Singleton}: If a type is @deftech{singleton} constrained, it must contain exactly one thing.}
                                @item{@bold{Nonempty}: A @deftech{nonempty} type must never be empty.}
                                @item{@bold{Abstract}: An @deftech{abstract} type is fully covered by its children. 
                                       For instance, consider a type Bit with child types ZeroBit and OneBit.
                                       If Bit is abstract, every Bit must be either a zero or a one. }
                                @item{@bold{Function}: A binary predicate P: (A x B) can be constrained to behave as a total or partial function from A to B.}
                                @item{@bold{Disjoint}: Two types that are @deftech{disjoint} must never share an atom.}
                                @item{@bold{Subset}: Used to enforce subset constraints that cannot be captured in the type hierarchy. 
                                       If type A is constrained to be a @deftech{subset} of B, every scenario
                                       will have A contained in B, even if B is not a super-type of A.}
                                ]
           

The constraints @tech{Disjoint}, @tech{at-most-one}, @tech{singleton}, and @tech{nonempty}
can be applied to @italic{all} subtypes of a type at once by using the @racket[-all] suffix.
For instance: Declaring that the IPAddress type is @italic{disjoint-all} constrained forces all of its sub-types to be pairwise disjoint.
}
 
 
 ]



#lang scribble/manual

@title{Representing Policies}

Policies map @tech{request}s to @tech{decision}s. Each @tech{policy} is associated with exactly one @tech{vocabulary}.
In this section, we demonstrate the elements of policies and vocabularies using the conference policy presented in @secref["gs-existing"].

@;--------------------------------------------------------------------
@section[#:tag "policies"]{Elements of a Policy}

@subsubsub*section{The Example Policy}

@racketblock[
(Policy ConferencePolicy1 uses conferencepolicy
        (Target )
        (Rules 
          (PaperNoConflict = (Permit s a r) :-
                             (!Conflicted s r) (ReadPaper a) (Paper r))
          (PaperAssigned = (Permit s a r) :- 
                           (Assigned s r) (ReadPaper a) (Paper r))
          (PaperConflict = (Deny s a r) :- 
                           (Conflicted s r) (ReadPaper a) (Paper r)))
        (RComb FAC)
        (PComb FAC)
        (Children ))
]


@;A @deftech{policy} in Margrave contains the following elements.

@itemlist[
          @item{@tt{ConferencePolicy1} is the @deftech{policy name}.
                 Use the policy name to reference the policy in Margrave queries.}
           
           @item{@tt{conferencepolicy} is the policy's @deftech{vocabulary name}.
                  Every policy has a single @tech{vocabulary} that dictates what kind of  
                  @tech{request} it handles and which @tech{decision}s it renders. }
           
          @item{@tt{(Target )} is the policy's @deftech{target}. The @tech{target}
                 specifies a guard on the policy's applicability. In order for any rule
                 in the policy to apply, the policy's @tech{target} condition must also
                 apply. For instance, @tt{(Target (Reviewer s))} would mean that the
                 policy applies only to subjects who are reviewers.

                 An empty @tech{target}, as shown above, means that
                 the policy always applies.}
          
          
          @item{The @tt{(Rules ...)} construct gives the policy's @deftech{rule} set.
                    Each @tech{rule} has a name, a decision, and a set of @deftech{conditions}. 
                    Each @tech{condition} must be the use of a @tech{type} or @tech{predicate} 
                    in the policy's vocabulary. A "!" before a @tech{type} or @tech{predicate}
                    name means that the type or predicate does @italic{not} apply.
                    
                    The first rule in the example policy:
                    
                    @tt{(PaperNoConflict = (Permit s a r) :- (!Conflicted s r) (ReadPaper a) (Paper r))}
                    
                    is named @tt{PaperNoConflict}, renders the @tt{Permit} decision, and applies when
                    the subject is trying to read a paper on which he or she is not conflicted. 
                    
                    The variable names that appear after the decision name (e.g. @tt{Permit s a r}) must exactly match
                    the names of the request variables declared in the policy's @tech{vocabulary}. New variables can
                    be introduced on the right hand side of the rule, in which case they are interpreted
                    existentially. 
                    
                    For instance, in a policy for vehicle access, this rule would apply for any subject 
                    who owns @italic{some} car, regardless of the action and resource involved:
                    
                    @tt{(PermitIfOwnsCar = (Permit s a r) :- (Owns s c) (Car c))}
                    }
          
          @item{@tt{(RComb ...)} denotes the policy's
                 @deftech{rule-combination algorithm}. This is used to resolve rule conflicts. 
                 Margrave currently supports @tech{first-applicable}, @tech{overrides},
                 and @tech{none} combinators:


                 @itemlist[
                           @item{@tt{(RComb FAC)}: In @deftech{first-applicable} policies, rules apply in 
                                    the order in which they appear.}
                            @item{@tt{(RComb O <decisions>)}: In @deftech{overrides} policies, 
                                   the vocabulary gives an 
                                   ordering on the decisions, and higher-priority decisions
                                   take precedence over lower-priority decisions. E.g.,
                                   @tt{(RComb O CallPolice Deny Permit)} means that the
                                   CallPolice decision overrides the Deny and Permit decisions,
                                   and the Deny decision overrides the Permit decision.}
                            @item{@tt{(RComb none)}: In @deftech{none} policies, multiple rules (indeed, multiple 
                                     decisions) can apply to a single request.}
                            ]}
          
                                                                                            
          @item{Margrave also supports hierarchical policies via @tt{(Children ...)}: A policy's @deftech{children} are a list
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
                 with the @tt{(PComb ...)} construct. The same options are available to policy combination as to
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
          @item{The @tt{(Types ...)} construct defines a hierarchy of
                    @deftech{type}s (also called @deftech{sort}s) that 
                    elements of a request may belong to. 
                    
                    Top-level types (@tt{Subject}, @tt{Action}, and @tt{Resource} in the above example)
                    are always pairwise @tech{disjoint}.}
           
           @item{The @tt{(Predicates ...)} construct defines a set of
                     @deftech{predicate}s that may hold of request
                     properties in a scenario. For instance, the binary @tech{predicate} @tt{Assigned} 
                     contains reviewer-to-paper assignments in a given scenario.}
           
           @item{The @tt{(Decisions ...)} construct gives a set of @deftech{Decisions}
                     that policies using this vocabulary render. The usual decisions are
                     @tt{permit} and @tt{deny}, but others can appear in more detailed policies.}
           
           @item{The @tt{(ReqVariables ...)} construct defines the shape of a request
                     in this vocabulary. The request variables are an ordered list of variable
                     names and a type assignment for each.                      
                     For instance, in the access-control domain, the request variables will 
                     often be @tt{(s, a, r)}, representing a single subject, action, and resource. 
                     In the firewall domain, the request variables will involve IP addresses, ports, etc.}
           
           @item{The @tt{(Constraints ...)} construct gives domain-specific assertions
                     about how types and predicates can be populated. Margrave will not consider
                     scenarios that violate vocabulary constraints, ruling out non-sensical results. 
                     
                     Valid @deftech{constraint} types are:

                     @itemlist[
                               @item{@tt{atmostone}: If a type is @deftech{at-most-one} constrained, it can never contain more than one thing.}
                                @item{@tt{singleton}: If a type is @deftech{singleton} constrained, it must contain exactly one thing.}
                                @item{@tt{nonempty}: A @deftech{nonempty} type must never be empty.}
                                @item{@tt{abstract}: An @deftech{abstract} type is fully covered by its children. 
                                       For instance, consider a type Bit with child types ZeroBit and OneBit.
                                       If Bit is abstract, every Bit must be either a zero or a one. }
                                @item{@tt{partial-function}, @tt{total-function}: A binary predicate P: (A x B) can be constrained to behave as a total or partial function from A to B.}
                                @item{@tt{disjoint}: Two types that are @deftech{disjoint} must never share an atom.}
                                @item{@tt{subset}: Used to enforce subset constraints that cannot be captured in the type hierarchy. 
                                       If type A is constrained to be a @deftech{subset} of B, every scenario
                                       will have A contained in B, even if B is not a super-type of A.}
                                ]
           

The constraints @tech{disjoint}, @tech{at-most-one}, @tech{singleton}, and @tech{nonempty}
can be applied to @italic{all} subtypes of a type at once by using the @tt{-all} suffix.
For instance: Declaring that the @tt{IPAddress} type is @tt{disjoint-all} constrained forces all of its sub-types to be pairwise disjoint.
}
 
 
 ]



#lang scribble/manual

@title["Representing Policies"]

<<Pol + Vocabs; close with fact that the language is being changed and so new documentation will be forthcoming.>>


@;--------------------------------------------------------------------
@section[#:tag "policies"]{"Elements of a Policy"}

A @tech{policy} in Margrave contains the following elements.

@subsection["Vocabulary Name"]

Every policy has a single vocabulary that dictates what a @tech{request} is and which @tech{decisions} the policy renders.


@subsection["Target Condition"]

Similar to a @tech{rule}, a policy condition must be satisfied before any of the policy's rules can apply.

@subsection["Rules"]

A list of statements mapping requests to decisions.  <<< needs more >>> 

@subsection["Children"]

A list of sub-policies that together comprise the parent policy. A policy contains either @tech{rules} or @tech{children} but never both.

@subsection["Subpolicy- and Rule-Combination Algorithms"]

Used to resolve rule and subpolicy conflicts. Margrave currently supports @tech{first-applicable}, @tech{overrides}, and @tech{none} combinators for both rules and policies.





@;--------------------------------------------------------------------
@section[#:tag "vocabularies"]{"Elements of a Vocabulary"}

A @tech{vocabulary} in Margrave contains the following:

@subsection["Sorts"]

A hierarchy of types that elements of a request may belong to.
<<< example >>>

@subsection["Predicates"]

Relations that hold of request elements. For instance, the relation Assigned from @secref{getting-started-1} contains reviewer-to-paper assignments in a given scenario.

@subsection["Decisions"]

Decisions that the policy renders. The standard examples are permit and deny, but others can appear in more detailed policies.

@subsection["Request Variables"]

An ordered list of variable names, denoting the shape of a request to all policies that use this vocabulary. For instance, in the access-control domain, the request variables will often be (subj, act, res). In the firewall domain, the request variables will involve IP addresses, ports, etc.

@subsection["Constraints"]

Domain-specific assertions about how sorts and predicates can be populated. Margrave will not consider scenarios that violate vocabulary constraints, ruling out non-sensical results. Valid constraint types are:


@subsubsection["At-most-one"]

If a sort is at-most-one constrained, it can never contain more than one atom.

<<< what's an atom? >>>

@subsubsection["Singleton"]

If a sort is singleton constrained, it must contain exactly one atom.

@subsubsection["Nonempty"]

A nonempty sort must never be empty.

@subsubsection["Abstract"]

An abstract sort is fully covered by its children. For instance, consider a sort Bit with child sorts ZeroBit and OneBit. If Bit is abstract, every Bit must be either a zero or a one. 

@subsubsection["Function (Total, Partial)"]

A binary predicate P: (A x B) can be constrained to behave as a total or partial function from A to B.

@subsubsection["Disjoint"]

Two sorts that are disjoint must never share an atom.

@subsubsection["Subset"]

Used to enforce subset constraints that cannot be captured in the sort hierarchy. 
If sort A is constrained to be a subset of B, every scenario will have A contained in B, even if B is not a super-sort of A.

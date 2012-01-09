module models/continue
// Jay McCarthy and CS296-1
// TODO:
// - Subreviewers
// - Separation of Admin and Chair
// - Customization (i.e. controlling abstract length and dependency issues for already submitted papers)

open util/ordering[Conference] as CO

// Primitive objects
abstract sig Object {}
sig ConferenceInfo extends Object {}
sig UserInfo {}
sig PaperInfo {}
sig Password {}
sig Review {}
sig Decision {}

// More interesting objects
abstract sig Phase {}
one sig Initialization,   // Establish one conference administrator.
               PreSubmission,    // Adding reviewers
               Submission,       // Authors can submit papers.
               Bidding,          // Reviewers request papers.
               Assignment,       // Administrators assign papers to reviewers.
               Reviewing,        // Reviewers evaluate papers.
               Discussion,       // Papers are selected.
               Notification,     // Authors are told if rejected or admitted.
               Publishing        // Signalling the end
extends Phase {}

abstract sig PaperPhase {}
one sig PSubmission, PBidding, PAssignment, PReviewing, PDiscussion, PNotification
	extends PaperPhase {}
sig User extends Object {
	info: UserInfo,
	password: Password
}
sig Paper extends Object {
	info: PaperInfo,
	pphase : PaperPhase,
	decision: Decision
}

// Action
abstract sig Action {}
one sig ReadReview, WriteReview, 
	BecomeAuthor, MakeSubmission, ModifySubmission,
	AddReviewer, RemoveUser, ModifyUserJob, ModifyUserInfo, ModifyUserPassword,
	PaperBid, PaperConflict, PaperAssign, PaperDecide,
	AdvancePhase, EditConferenceInfo
	extends Action {}

// Continue Conference
sig Conference extends Object {
	// Configuration
	info: ConferenceInfo,
	undecided: Decision,
	decisions: set Decision,
	phase: Phase,
	// Users
	users: set User,
	// User Roles
	admins: set User,
	reviewers: set User,
	authors: set User,
	// Papers
	papers: set Paper,
	// Paper and User relations
	author : Paper ->one User,
	bid: User -> Paper,
	conflict: User -> Paper,
	assignment: User -> Paper,
	review: User -> Paper ->one Review
}

///////////////
// HELPERS
///////////////


pred paperModified( cc, cc' : Conference, p, p' : Paper ) {
	p in cc.papers
	p not in cc'.papers
	p' not in cc.papers
	p' in cc'.papers
        
	all u : User, r : Review {
	   ( p -> u ) in cc.author iff (p' -> u) in cc'.author
           ( u -> p ) in cc.bid iff ( u -> p' ) in cc'.bid
           ( u -> p ) in cc.conflict iff ( u -> p' ) in cc'.conflict 
           ( u -> p ) in cc.assignment iff ( u -> p' ) in cc'.assignment 
           ( u -> p -> r ) in cc.review iff ( u -> p' -> r ) in cc'.review

	   ( p -> u ) not in cc'.author
	   ( u -> p ) not in cc'.bid
	   ( u -> p ) not in cc'.conflict
	   ( u -> p ) not in cc'.assignment
	   ( u -> p -> r ) not in cc'.review 
	}
}

pred checkPaperDeleted( cc, cc' : Conference, p : Paper ) {
	p not in cc'.papers
	{ all au : User |
		(p -> au) not in cc'.author
		{ no u : User |
			( u -> p ) in cc'.bid
			|| ( u -> p ) in cc'.conflict
			|| ( u -> p ) in cc'.assignment }
		{ no u : User, r : Review |
			( u -> p -> r ) in cc'.review }
	}
}

pred checkOtherPapersUnchanged( cc, cc' : Conference, ps : set Paper ) {
    all op : Paper - ps {
        op in cc.papers iff op in cc'.papers
        { all u : User |
            ( op -> u ) in cc.author iff ( op -> u ) in cc'.author
              and ( u -> op ) in cc.bid iff ( u -> op ) in cc'.bid
              and ( u -> op ) in cc.conflict iff ( u -> op ) in cc'.conflict
              and ( u -> op ) in cc.assignment iff ( u -> op ) in cc'.assignment
              and { all r : Review |
                      ( u -> op -> r ) in cc.review iff ( u -> op -> r ) in cc'.review
              }
        }
    }
}




pred checkUserModified( cc, cc' : Conference, mu, mu' : User ) {
	mu not in cc'.users
	mu' in cc'.users
	{ all p : Paper |
		( p -> mu ) in cc.author iff ( p -> mu' ) in cc'.author 
		and ( mu -> p ) in cc.bid iff ( mu' -> p ) in cc'.bid 
		and ( mu -> p ) in cc.conflict iff ( mu' -> p ) in cc'.conflict 
		and ( mu -> p ) in cc.assignment iff ( mu' -> p ) in cc'.assignment 	
		and { all r : Review |
			( mu -> p -> r ) in cc.review iff ( mu' -> p -> r ) in cc'.review
		}
	}
}

pred checkNoOtherUserModified( cc, cc' : Conference, mu : User ) {
	{ all u : User |
		u != mu implies {
			u in cc.users iff u in cc'.users
			u in cc.admins iff u in cc'.admins
			u in cc.reviewers iff u in cc'.reviewers
			u in cc.authors iff u in cc'.authors
		} 
		and { all p : cc.papers |
			( p -> u ) in cc.author iff ( p -> u  ) in cc'.author
			and ( u -> p ) in cc.bid iff ( u -> p ) in cc'.bid 
			and ( u -> p ) in cc.conflict iff ( u -> p ) in cc'.conflict 
			and ( u -> p ) in cc.assignment iff ( u -> p ) in cc'.assignment
			and { all r : Review |
				( u -> p -> r ) in cc.review iff ( u -> p -> r ) in cc'.review }
		}
	}
}

///////////////
// OPERATIONS
///////////////

// Change the conference information
pred changeConferenceInfo( cc, cc' : Conference, cu : User ) {
   access[cc, cu, cc.info, EditConferenceInfo]
   // The ConferenceInfo is not constrained. (It could change)
   // Nothing else is changed
   some ci: ConferenceInfo | cc'.info = ci
   
   cc.undecided = cc'.undecided
   cc.decisions = cc'.decisions
   cc.phase = cc'.phase
   cc.users = cc'.users
   cc.admins = cc'.admins
   cc.reviewers = cc'.reviewers
   cc.authors = cc'.authors
   cc.papers = cc'.papers
   cc.author = cc'.author
   cc.bid = cc'.bid
   cc.conflict = cc'.conflict
   cc.assignment = cc'.assignment
   cc.review = cc'.review
}

// Add a user, as a reviewer
pred addUserAsReviewer( cc, cc' : Conference, cu : User, nu : User ) {
   access[cc, cu, nu, AddReviewer]
   // The user isn't already a user
   nu not in cc.users
   // The user is added
   cc'.users = cc.users + nu
   // The user is a reviewer
   cc'.reviewers = cc.reviewers + nu
   
   // Nothing else is changed
   cc.info = cc'.info
   cc.undecided = cc'.undecided
   cc.decisions = cc'.decisions
   cc.phase = cc'.phase
   cc.admins = cc'.admins
   cc.authors = cc'.authors
   cc.papers = cc'.papers
   cc.author = cc'.author
   cc.bid = cc'.bid
   cc.conflict = cc'.conflict
   cc.assignment = cc'.assignment
   cc.review = cc'.review
}

// Make a user a reviewer
pred changeUserJobToReviewer( cc, cc' : Conference, cu : User, mu : User ) {
   access[cc, cu, mu, ModifyUserJob]
   // The user is real
   mu in cc.users
   // The user is added to reviewer
   cc'.reviewers = cc.reviewers + mu
   
   // Nothing else is changed
   cc.info = cc'.info   
   cc.undecided = cc'.undecided
   cc.decisions = cc'.decisions
   cc.phase = cc'.phase
   cc.users = cc'.users
   cc.admins = cc'.admins
   cc.authors = cc'.authors
   cc.papers = cc'.papers
   cc.author = cc'.author
   cc.bid = cc'.bid
   cc.conflict = cc'.conflict
   cc.assignment = cc'.assignment
   cc.review = cc'.review
}

// Make a user no longer a reviewer
pred changeUserJobToNotReviewer( cc, cc' : Conference, cu : User, mu : User ) {
   access[cc, cu, mu, ModifyUserJob]
   // The user is real
   mu in cc.users
   // The user is removed from reviewer
   cc'.reviewers = cc.reviewers - mu

   // Nothing else is changed
   cc.info = cc'.info
   cc.undecided = cc'.undecided
   cc.decisions = cc'.decisions
   cc.phase = cc'.phase
   cc.users = cc'.users
   cc.admins = cc'.admins
   cc.authors = cc'.authors
   cc.papers = cc'.papers
   cc.author = cc'.author
   cc.bid = cc'.bid
   cc.conflict = cc'.conflict
   cc.assignment = cc'.assignment
   cc.review = cc'.review
}

// Make a user an admin
pred changeUserJobToAdmin( cc, cc' : Conference, cu : User, mu : User ) {
   access[cc, cu, mu, ModifyUserJob]
   // The user is real
   mu in cc.users
   // The user is added to admin
   cc'.admins = cc.admins + mu
   // Nothing else is changed
   cc.info = cc'.info   
   cc.undecided = cc'.undecided
   cc.decisions = cc'.decisions
   cc.phase = cc'.phase
   cc.users = cc'.users
   cc.reviewers = cc'.reviewers
   cc.authors = cc'.authors
   cc.papers = cc'.papers
   cc.author = cc'.author
   cc.bid = cc'.bid
   cc.conflict = cc'.conflict
   cc.assignment = cc'.assignment
   cc.review = cc'.review
}

// Make a user no longer an admin
pred changeUserJobToNotAdmin( cc, cc' : Conference, cu : User, mu : User ) {
   access[cc, cu, mu, ModifyUserJob]
   cu != mu
   // The user is real
   mu in cc.users
   // The user is removed from admin
   cc'.admins = cc.admins - mu
   // Nothing else is changed
   cc.info = cc'.info
   cc.undecided = cc'.undecided
   cc.decisions = cc'.decisions
   cc.phase = cc'.phase
   cc.users = cc'.users
   cc.reviewers = cc'.reviewers
   cc.authors = cc'.authors
   cc.papers = cc'.papers
   cc.author = cc'.author
   cc.bid = cc'.bid
   cc.conflict = cc'.conflict
   cc.assignment = cc'.assignment
   cc.review = cc'.review
}

// Delete a user
pred deleteUser( cc, cc' : Conference, cu : User, mu : User ) {
	access[cc, cu, mu, RemoveUser]
	cu != mu
	// The user is real
	mu in cc.users
	// The user is removed
	mu not in cc'.users
	// If the users is a reviewer
	mu in cc.reviewers implies {
		mu not in cc'.reviewers
		{ no p : Paper |
			(mu -> p) in cc'.bid
		 	|| (mu -> p) in cc'.conflict
		 	|| (mu -> p) in cc'.assignment }
 		{ no p : Paper, r : Review |
			(mu -> p -> r) in cc'.review }
	} 
	// If the user is an author, their paper is removed
	mu in cc.authors implies {
		mu not in cc'.authors
		{ all p : Paper |
			(p -> mu) in cc.author implies {
				(p -> mu) not in cc'.author
				checkPaperDeleted[cc, cc', p]
			}
		}
	}
	// Other things are unchanged
	{ all u : User |
		u != mu implies {
			u in cc.reviewers iff u in cc'.reviewers
			u in cc.authors iff u in cc'.authors
		}
	}
	{ all p : Paper, u : User |
		u != mu implies {
			(p -> u) in cc.author iff {
				p in cc'.papers
				(p -> u) in cc'.author
			}
			(u -> p) in cc.bid iff (u -> p) in cc'.bid
			(u -> p) in cc.conflict iff (u -> p) in cc'.conflict
			(u -> p) in cc.assignment iff (u -> p) in cc'.assignment
			{ all r : Review |
				(u -> p -> r) in cc.review iff (u -> p -> r) in cc'.review }
		}
	}
	// Nothing else is changed
	cc.undecided = cc'.undecided
	cc.decisions = cc'.decisions
	cc.phase = cc'.phase
}

// Become an author
pred becomeAuthor( cc, cc' : Conference, nu : User ) {
	access[cc, nu, cc, BecomeAuthor]
	nu in cc'.users
	nu in cc'.authors
	// Everything else unchanged
	cc'.info = cc.info
	cc'.undecided = cc.undecided
	cc'.decisions = cc.decisions
	cc'.phase = cc.phase
	{ all u : cc.users |
		u in cc'.users }
	cc'.admins = cc.admins
	cc'.reviewers = cc.reviewers
	{ all u : cc.authors |
		u in cc'.authors }
	cc'.papers = cc.papers
	cc'.author = cc.author
	cc'.bid = cc.bid
	cc'.conflict = cc.conflict
	cc'.assignment = cc.assignment
	cc'.review = cc.review
}

// Make a submission
pred makeSubmission( cc, cc' : Conference, cu : User, np : Paper ) {
	access[cc, cu, cc, MakeSubmission]
	np not in cc.papers
	np in cc'.papers
	( np -> cu ) in cc'.author
	np.decision = cc.undecided
	np.pphase in PSubmission
	cu in cc.reviewers implies {
		( cu -> np) in cc'.conflict
	}
	// Everything else unchanged
	cc'.info = cc.info
	cc'.undecided = cc.undecided
	cc'.decisions = cc.decisions
	cc'.phase = cc.phase
	cc'.users = cc.users
	cc'.admins = cc.admins
	cc'.reviewers = cc.admins
	cc'.authors = cc.authors
	{ all p : cc.papers |
		p in cc'.papers
		and { all u : User |
				( p -> u ) in cc.author iff ( p -> u ) in cc'.author }
	}
	cc'.bid = cc.bid
	{ all u : cc.users, p : cc.papers |
		( u -> p) in cc.conflict iff ( u -> p ) in cc'.conflict }
	cc'.assignment = cc.assignment
	cc'.review = cc.review
}

// Modify a submission
pred modifySubmission( cc, cc' : Conference, cu : User, p : Paper, p' : Paper ) {
	access[cc, cu, p, ModifySubmission]
	p'.decision = p.decision
	p'.pphase = p.pphase
	paperModified[cc, cc', p, p']
	// Everything else is unchanged
	cc'.info = cc.info
	cc'.undecided = cc.undecided
	cc'.decisions = cc.decisions
	cc'.phase = cc.phase
	cc'.users = cc.users
	cc'.admins = cc.admins
	cc'.reviewers = cc.reviewers
	cc'.authors = cc.authors
	checkOtherPapersUnchanged[cc, cc', p + p']
}

// Remove a submission
pred removeSubmission( cc, cc' : Conference, cu : User, p : Paper ) {
	access[cc, cu, p, ModifySubmission]
	checkPaperDeleted[cc, cc', p]
	// Nothing else is changed
	cc'.info = cc.info
	cc'.undecided = cc.undecided
	cc'.decisions = cc.decisions
	cc'.phase = cc.phase
	cc'.users = cc.users
	cc'.admins = cc.admins
	cc'.reviewers = cc.reviewers
	cc'.authors = cc.authors
	checkOtherPapersUnchanged[cc, cc', p]
}

// Advance the phase of a paper
pred advanceSubmission( cc, cc' : Conference, cu : User, p, p' : Paper ) {
   access[cc, cu, p, AdvancePhase]
   p.info = p'.info
   p.decision = p'.decision

   p.pphase != PNotification // necessary guard
   p.pphase = PSubmission implies p'.pphase = PBidding
   p.pphase = PBidding implies p'.pphase = PAssignment
   p.pphase = PAssignment implies p'.pphase = PReviewing
   p.pphase = PReviewing implies p'.pphase = PDiscussion
   p.pphase = PDiscussion implies p'.pphase = PNotification
   
   paperModified[cc, cc', p, p']
   // Everything else is unchanged
   cc'.info = cc.info
   cc'.undecided = cc.undecided
   cc'.decisions = cc.decisions
   cc'.phase = cc.phase
   cc'.users = cc.users
   cc'.admins = cc.admins
   cc'.reviewers = cc.reviewers
   cc'.authors = cc.authors
   checkOtherPapersUnchanged[cc, cc', p + p']
}

// Change user information
pred changeUserInformation( cc, cc' : Conference, cu, mu, mu' : User ) {
	access[cc, cu, mu, ModifyUserInfo]
	mu'.password = mu.password
	checkUserModified[cc, cc', mu, mu']
	// Nothing else is changed
	cc'.info = cc.info
	cc'.undecided = cc.undecided
	cc'.decisions = cc.decisions
	cc'.phase = cc.phase
	checkNoOtherUserModified[cc, cc', mu]
	cc'.papers = cc.papers	
}

// Change user password
pred changeUserPassword( cc, cc' : Conference, cu, mu, mu' : User ) {
	access[cc, cu, mu, ModifyUserPassword]
	mu'.info = mu.info
	checkUserModified[cc, cc', mu, mu']
	// Nothing else is changed
	cc'.info = cc.info
	cc'.undecided = cc.undecided
	cc'.decisions = cc.decisions
	cc'.phase = cc.phase
	checkNoOtherUserModified[cc, cc', mu]
	cc'.papers = cc.papers	
}

// Bid on a paper
pred bidPaper( cc, cc' : Conference, cu : User, bp : Paper ) {
	access[cc, cu, bp, PaperBid]
	bp in cc.papers
	(cu -> bp) in cc'.bid
	// Nothing else is changed
	cc'.info = cc.info
	cc'.undecided = cc.undecided
	cc'.decisions = cc.decisions
	cc'.phase = cc.phase
	cc'.users = cc.users
	cc'.admins = cc.admins
	cc'.reviewers = cc.reviewers
	cc'.authors = cc.authors
	cc'.papers = cc.papers
	cc'.author = cc.author
	{ all p : cc.papers, u : cc.reviewers |
		(u -> p) in cc.bid iff (u -> p) in cc'.bid }
	cc'.conflict = cc.conflict
	cc'.assignment = cc.assignment
	cc'.review = cc.review
}

// Unbid on a paper
pred unbidPaper( cc, cc' : Conference, cu : User, bp : Paper ) {
	access[cc, cu, bp, PaperBid]
	bp in cc.papers
	(cu -> bp) not in cc'.bid
	// Nothing else is changed
	cc'.info = cc.info
	cc'.undecided = cc.undecided
	cc'.decisions = cc.decisions
	cc'.phase = cc.phase
	cc'.users = cc.users
	cc'.admins = cc.admins
	cc'.reviewers = cc.reviewers
	cc'.authors = cc.authors
	cc'.papers = cc.papers
	cc'.author = cc.author
	{ all p : cc.papers, u : cc.reviewers |
		cu != u and bp != p implies {
			(u -> p) in cc.bid iff (u -> p) in cc'.bid } }
	cc'.conflict = cc.conflict
	cc'.assignment = cc.assignment
	cc'.review = cc.review
}

// Mark conflicted with a paper
pred markConflict( cc, cc' : Conference, cu : User, cp : Paper ) {
	access[cc, cu, cp, PaperConflict]
	cp in cc.papers
	(cu -> cp) in cc'.conflict
	// Nothing else is changed
	cc'.info = cc.info
	cc'.undecided = cc.undecided
	cc'.decisions = cc.decisions
	cc'.phase = cc.phase
	cc'.users = cc.users
	cc'.admins = cc.admins
	cc'.reviewers = cc.reviewers
	cc'.authors = cc.authors
	cc'.papers = cc.papers
	cc'.author = cc.author
	cc'.bid = cc.bid
	{ all p : cc.papers, u : cc.reviewers |
		(u -> p) in cc.conflict iff (u -> p) in cc'.conflict }
	cc'.assignment = cc.assignment
	cc'.review = cc.review
}

// Assign a paper to a user
pred assignPaper( cc, cc' : Conference, cu, au : User, ap : Paper ) {
	access[cc, cu, au, PaperAssign]
	ap in cc.papers
	(au -> ap) not in cc.conflict
	(au -> ap) in cc'.assignment
	// Nothing else is changed
	cc'.info = cc.info
	cc'.undecided = cc.undecided
	cc'.decisions = cc.decisions
	cc'.phase = cc.phase
	cc'.users = cc.users
	cc'.admins = cc.admins
	cc'.reviewers = cc.reviewers
	cc'.authors = cc.authors
	cc'.papers = cc.papers
	cc'.author = cc.author
	cc'.bid = cc.bid
	cc'.conflict = cc.conflict
	{ all p : cc.papers, u : cc.reviewers |
		(u -> p) in cc.assignment iff (u -> p) in cc'.assignment }
	cc'.review = cc.review
}

// Unassign a paper to a user
pred unassignPaper( cc, cc' : Conference, cu, au : User, ap : Paper ) {
	access[cc, cu, au, PaperAssign]
	ap in cc.papers
	(au -> ap) not in cc'.assignment
	// Nothing else is changed
	cc'.info = cc.info
	cc'.undecided = cc.undecided
	cc'.decisions = cc.decisions
	cc'.phase = cc.phase
	cc'.users = cc.users
	cc'.admins = cc.admins
	cc'.reviewers = cc.reviewers
	cc'.authors = cc.authors
	cc'.papers = cc.papers
	cc'.author = cc.author
	cc'.bid = cc.bid
	cc'.conflict = cc.conflict
	{ all p : cc.papers, u : cc.reviewers |
		au != u and ap != p implies {
			(u -> p) in cc.assignment iff (u -> p) in cc'.assignment } }
	cc'.review = cc.review
}

// Review a paper
pred reviewPaper( cc, cc' : Conference, cu : User, rp : Paper, rw : Review ) {
	access[cc, cu, rp, WriteReview]
	rp in cc.papers
	( cu -> rp -> rw ) in cc'.review
	// Nothing else is changed
	cc'.info = cc.info
	cc'.undecided = cc.undecided
	cc'.decisions = cc.decisions
	cc'.phase = cc.phase
	cc'.users = cc.users
	cc'.admins = cc.admins
	cc'.reviewers = cc.reviewers
	cc'.authors = cc.authors
	cc'.papers = cc.papers
	cc'.author = cc.author
	cc'.bid = cc.bid
	cc'.conflict = cc.conflict
	cc'.assignment = cc.assignment
	{ all p : cc.papers, u : cc.reviewers, r : Review |
		cu != u and p != rp implies {
			(u -> p -> r) in cc.review iff (u -> p -> r) in cc'.review } }
}

// Make a decision on a paper
pred makeDecision( cc, cc' : Conference, cu : User, dp, dp' : Paper ) {
	access[cc, cu, dp, PaperDecide]
	dp'.info = dp.info
	dp'.pphase = dp.pphase
	paperModified[cc, cc', dp, dp']
	dp'.decision in cc.decisions
	// Nothing else is changed
	cc'.info = cc.info
	cc'.undecided = cc.undecided
	cc'.decisions = cc.decisions
	cc'.phase = cc.phase
	cc'.users = cc.users
	cc'.admins = cc.admins
	cc'.reviewers = cc.reviewers
	cc'.authors = cc.authors
	checkOtherPapersUnchanged[cc, cc', dp + dp']
}

// Advance conference phase
pred advancePhase( cc, cc' : Conference, cu : User ) {
   access[cc, cu, cc, AdvancePhase]
   cc.phase != Publishing
   
   cc.phase = Initialization implies cc'.phase = PreSubmission
   cc.phase = PreSubmission implies cc'.phase = Submission
   cc.phase = Submission implies cc'.phase = Bidding
   cc.phase = Bidding implies cc'.phase = Assignment
   cc.phase = Assignment implies cc'.phase = Reviewing
   cc.phase = Reviewing implies cc'.phase = Discussion
   cc.phase = Discussion implies cc'.phase = Notification
   cc.phase = Notification implies cc'.phase = Publishing
   
   // Everything else is the same
   cc'.info = cc.info
   cc'.undecided = cc.undecided
   cc'.decisions = cc.decisions
   cc'.users = cc.users
   cc'.admins = cc.admins
   cc'.reviewers = cc.reviewers
   cc'.authors = cc.authors
   cc'.papers = cc.papers
   cc'.author = cc.author
   cc'.bid = cc.bid
   cc'.conflict = cc.conflict
   cc'.assignment = cc.assignment
   cc'.review = cc.review
}

///////////////
// ACCESS
///////////////

pred access( cc : Conference, subject : Object, resource : Object, action : Action ) {
	resource in Paper and action in AdvancePhase implies {
		subject in cc.admins
		resource.pphase in PAssignment implies {
			one u : User |
				(u -> resource) in cc.assignment
		}
		resource.pphase in PReviewing implies {
			one u : User, r : Review |
				(u -> resource -> r) in cc.review
		}
		resource.pphase in PDiscussion implies {
			one d : cc.decisions |
				d != cc.undecided
				and resource.decision = d
		}
	}
	resource in Conference and action in AdvancePhase implies {
		subject in cc.admins
		cc.phase in Submission implies {
			all p : cc.papers |
				p.pphase not in PSubmission
		}
		cc.phase in Bidding implies {
			// All papers after Bidding
			all p : cc.papers | {
				p.pphase not in PSubmission
				and p.pphase not in PBidding
			}
			// All reviewers have made bids (?)
			and all u : cc.reviewers |
				one p : cc.papers |
					( u -> p ) in cc.bid
		}
		cc.phase in Assignment implies {
			// All papers are assigned
			all p : cc.papers |
				p.pphase not in PSubmission
				and p.pphase not in PBidding
				and p.pphase not in PAssignment
		}
		cc.phase in Reviewing implies {
			// All papers are reviewed
			all p : cc.papers |
				p.pphase not in PSubmission
				and p.pphase not in PBidding
				and p.pphase not in PAssignment
				and p.pphase not in PReviewing
		}
		cc.phase in Discussion implies {
			// All papers have non-undecided decisions
			all p : cc.papers |
				p.pphase not in PSubmission
				and p.pphase not in PBidding
				and p.pphase not in PAssignment
				and p.pphase not in PReviewing
				and p.pphase not in PDiscussion
		}
	}
	resource in ConferenceInfo and action in EditConferenceInfo implies {
		subject in cc.admins
	}
	resource in User and action in AddReviewer implies {
		subject in cc.admins
	}
	resource in User and action in ModifyUserJob implies {
		subject in cc.admins
	}
	resource in User and action in RemoveUser implies {
		subject in cc.admins
	}
	resource in Conference and action in BecomeAuthor implies {
		subject not in cc.authors
		and cc.phase in Submission
	}
	resource in Conference and action in MakeSubmission implies {
		subject in cc.authors
		and cc.phase in Submission
	}
	resource in Paper and action in ModifySubmission implies {
		(subject in cc.admins)
		|| (resource.pphase in PSubmission and 
		     subject in cc.authors and 
		     resource in cc.papers and
		     (resource -> subject) in cc.author)
		|| (resource.pphase in PNotification and
			subject in cc.authors and
			resource in cc.papers and
			(resource -> subject) in cc.author )			
	}
	resource in Paper and action in WriteReview implies {
		resource.pphase in PReviewing and
		subject in cc.reviewers and
		(subject -> resource) in cc.assignment
	}
	resource in User and action in ModifyUserInfo implies {
		(subject in cc.admins)
		|| (subject = resource)
	}
	resource in User and action = ModifyUserPassword implies {
		(subject in cc.admins)
		|| 
           (subject = resource)
	}
	resource in Paper and action in PaperBid implies {
		resource.pphase in PBidding
		and subject in cc.reviewers
		and (subject -> resource) not in cc.conflict
	}
	resource in Paper and action in PaperConflict implies {
		resource.pphase in PBidding
		and subject in cc.reviewers
	}
	resource in User and action in PaperAssign implies {
		resource.pphase in PAssignment
		and subject in cc.admins
		and resource in cc.reviewers
	}
	resource in Paper and action in PaperDecide implies {
		resource.pphase in PDiscussion
		and subject in cc.admins
	}
}


///////////////
// INITIALIZATION
///////////////

pred init(cc0 : Conference) {
	cc0.phase = Initialization

	/* One user, the admin */
     one u : User {
			cc0.users = u
			cc0.admins = u
			cc0.reviewers = none
			cc0.authors = none
     }

	/* No papers */
	no p : Paper |	p in cc0.papers
}




// Simple sanity checks.

pred testAdminCanChangeOwnPassword {
    init[CO/first[]]
    all cc: Conference - CO/last[], cc': CO/next[cc] |
        some u, u', u'' : User |
            changeUserPassword[cc, cc', u, u', u'']
}
-- The scope here must be at least 5, since the size of Object
-- (which conference and user inherit from) must encompass all the
-- elements in the predicate.
run testAdminCanChangeOwnPassword for 5 but 2 Conference




pred testAdminCanChangeOwnUserInfo {
    init[CO/first[]]
    all cc: Conference - CO/last[], cc': CO/next[cc] |
        some u, u', u'' : User |
            changeUserInformation[cc, cc', u, u', u'']
}
run testAdminCanChangeOwnUserInfo for 5 but 2 Conference





pred testCanAdvancePhaseImmediately {
    init[CO/first[]]
    all cc: Conference - CO/last[], cc': CO/next[cc] |
        some u : User | advancePhase[cc, cc', u]
}
run testCanAdvancePhaseImmediately for 4 but 2 Conference
run testCanAdvancePhaseImmediately for 5 but 3 Conference
run testCanAdvancePhaseImmediately for 6 but 4 Conference



// Two states are next to one another if there is a possible action between them
pred TracesFull {
     init[CO/first[]]
	all cc : Conference - CO/last[], cc' : CO/next[cc] {
		(some cu : User {
			changeConferenceInfo[cc, cc', cu]
			|| (some nu : User {
				addUserAsReviewer[cc, cc', cu, nu] 
			})
			|| (some mu : User {
				changeUserJobToReviewer[cc, cc', cu, mu]
				|| changeUserJobToNotReviewer[cc, cc', cu, mu]
				|| changeUserJobToAdmin[cc, cc', cu, mu]
				|| changeUserJobToNotAdmin[cc, cc', cu, mu]
				|| deleteUser[cc, cc', cu, mu]
				|| (some mu' : User {
					changeUserInformation[cc, cc', cu, mu, mu']
					|| changeUserPassword[cc, cc', cu, mu, mu']
				})
			})
			|| (some np : Paper {
				makeSubmission[cc, cc', cu, np]
			})
			|| (some bp : Paper {
				bidPaper[cc, cc', cu, bp] 
				|| unbidPaper[cc, cc', cu, bp]
			})
			|| (some cp : Paper {
				markConflict[cc, cc', cu, cp]
			})
			|| (some au : User, ap : Paper {
				assignPaper[cc, cc', cu, au, ap]
				|| unassignPaper[cc, cc' , cu, au, ap]
			})
			|| (some rp : Paper, rw : Review {
				reviewPaper[cc, cc', cu, rp, rw]
			})
			|| (some dp, dp' : Paper {
				makeDecision[cc, cc', cu, dp, dp']
			})
			|| (some p : Paper {
				(some p' : Paper {
					modifySubmission[cc, cc', cu, p, p']
				})
				|| removeSubmission[cc, cc', cu, p]
			})
			|| advancePhase[cc, cc', cu]
		})
		|| (some nu : User {
			becomeAuthor[cc, cc', nu]
		})
	}
}







// ASSERTIONS


pred validConference( cc : Conference ) {
        // There is an admin
        cc.admins != none
        // Undecided is a Decision
        cc.undecided in cc.decisions
        // Users have roles
        cc.admins + cc.reviewers + cc.authors = cc.users
        // Admins are not authors
        cc.admins & cc.authors = none
        // Papers only have real decisions
        { all p : cc.papers |
                p.decision in cc.decisions }
        // All papers have an author
        { all p : cc.papers |
                one u : cc.users |
                        ( p -> u ) in cc.author }
        // Paper relations are only defined on real users and papers
        cc.author in cc.papers -> cc.authors
        cc.bid in cc.reviewers -> cc.papers
        cc.conflict in cc.reviewers -> cc.papers
        cc.assignment in cc.reviewers ->cc.papers
        cc.review in cc.reviewers -> cc.papers -> Review
        // Conflicts are respected in assignments
        { all u : cc.users, p : cc.papers |
                ( u -> p ) in cc.assignment implies ( u -> p) not in cc.conflict }
        // Reviews are only made on assigned papers
        { all u : cc.users, p : cc.papers, r : Review |
                ( u -> p -> r ) in cc.review implies ( u -> p ) in cc.assignment }
}

assert validityPreserved {
  TracesFull[] implies
  all cc : Conference, cc' : CO/next[cc] {
    validConference[cc] implies validConference[cc']
  }
}
check validityPreserved for 10


assert canPublish {
    TracesFull[] implies
	let ccn = CO/last[] {
		ccn.phase = Publishing
	}
}
check canPublish



assert canPublishCE {
      TracesFull[] implies
	let ccn = CO/last[] | {
		ccn.phase != Publishing	
	}
}
check canPublishCE

// There is no sequence of actions giving a way to read or write a review if there is a conflict.
assert noConflictingEditing {
     TracesFull[] implies
	let cc0 = CO/first[] | {
		all cci : Conference | {
			CO/lt[cc0, cci] implies { 
				conflictEnforcement[cci]
			}
		}
	}
}
check noConflictingEditing


///////////////
// PROPERTIES
///////////////

// A subject with no role may only view ConferenceInfo
// Someone who is not an admin, may never see reviews of papers with which he is conflicted, except his own
// A non-admin user conflicted with a paper may not see the identity of the reviewers of the paper
// If the subject is a PC member, and the phase is discussion, he can read all reviewers
// If the subject is a PC member, and submitted the review for a paper, then the subject can see all other reviews of the paper
// If the subject is a PC member, and it is not the discussion phase, and has not submitted the review for paper he is assigned, he may not read the reviews of others for that paper
// There is a phase at which all authors learn of their paper's decision
// A paper's author may not see the identity of his reviewers

///////////////
// ACCESS PROPERTIES
///////////////

// No one may view or write any review information for a paper with which he is conflicted
pred conflictEnforcement( cc : Conference ) {
	no cu : cc.users, p : cc.papers {
		(cu -> p) in cc.conflict
		and (access[cc, cu, p, WriteReview]
		       or access[cc, cu, p, ReadReview])
	}
}
// No one may write any review information for a paper he is the author of
// A reviewer may change his password
// A chair may change a reviewer's password
// The chair may add or remove PC members
// The chair may edit PC member info header
// The chair may edit conference info
// The chair may change the phase

// PRE-SUBMISSION
// Anyone may read conference info
// PC members may read PC member info

// SUBMISSION
// Authors may submit papers: the may create a submission or edit it
// Authors may retract their own submissions
// Chairs may edit paper submissions

// BIDDING
// Authors may no longer submit/modify papers
assert authorsDisabled {
     TracesFull[] implies
	all cc : Conference {
		cc.phase in Bidding implies {
			{ all u : User |
				 not access[cc, u, cc, BecomeAuthor] }
			{ all u : cc.authors, p : cc.papers |
				not access[cc, u, cc, MakeSubmission]
				and ( p -> u ) in cc.author implies {
					not access[cc, u, p, ModifySubmission]
				} }
		}
	}
}
check authorsDisabled for 5

// PC members may read papers
// PC members may submit paper bids
// PC members may view their own paper bids
// PC members may submit conflicts
// PC members may view their own conflicts

// ASSIGNMENT
// PC members may no longer submit bids
// Chairs may edit assginments
// Chairs may edit conflicts

// REVIEWING
// A PC member may make reviews of papers he is assigned
assert makeReviews {
     TracesFull[] implies
	all cc : Conference, u : User |
		cc.phase in Reviewing and u in cc.reviewers implies {
			all p : Paper |
				(u -> p) in cc.assignment implies {
					access[cc, u, p, WriteReview]
				}
		}
}
check makeReviews for 5

// REVIEWING (hidden)
// PC members may read their assignment
// PC member may read, write, and submit their own reviews assigned to him to review, but no one else's

// REVIEWING (viewable)
// PC member A may read the review written by another PC member B if
// 	- the PC member A has not been assigned that paper, or
//	- the PC member A submitted a review for that paper

// DISCUSSION
// A PC member may read all the reviews
assert discussionAllowed {
     TracesFull[] implies
	all cc : Conference, u : User |
		(cc.phase in Discussion and u in cc.reviewers) implies {
			all p : Paper |
				p in cc.papers implies {
					access[cc, u, p, ReadReview]
				}
		}
}
check discussionAllowed for 5

// NOTIFICATION
// PC members may not write any of the reviews
// Authors may read their paper decisions and reviews
// Accepted authors may review thier papers

// PUBLISHING
// Accepted authors may no longer revise their papers
assert noLongerRevise {
     TracesFull[] implies
	all cc : Conference, u : User |
		(cc.phase in Publishing and u in cc.authors) implies {
			all p : Paper |
				(p -> u) in cc.author implies {
					not access[cc, u, p, ModifySubmission]
				}
		}
}
check noLongerRevise for 5

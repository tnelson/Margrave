; The Alloy model does access control via a pred called access, which
; is true when the request is permitted. It uses implication, though,
; so the final catch-all policy is *permit*.  For the same reason, each
; block of rules has a final ``deny'' rule to represent that block's
; implication failing.

; Rules are named after the approximate line number of the alloy model that they match

; We have only one conference per scenario, so we don't make the 
; predicates for assignment, etc. take a conference argument.




(Policy uses continuev
        (Variables
         (s User)
         (a Action)
         (r Object))
        (Rules
         
         ;; ADVANCE PAPER PHASE
         
         ;	resource in Paper and action in AdvancePhase implies {
         ;		subject in cc.admins
         ;		resource.pphase in PAssignment implies {
         ;			one u : User |
         ;				(u -> resource) in cc.assignment
         ;		}
         (rule709 = (permit s a r) :- (Paper r) (= 'advancePhase a) (= 'pAssignment (paperPhase r))
                  (admin s) (exists u User (assignedTo u r)))
         
         ;		resource.pphase in PReviewing implies {
         ;			one u : User, r : Review |
         ;				(u -> resource -> r) in cc.review
         ;		}
         (rule713 = (permit s a r) :- (Paper r) (= 'advancePhase a) (= 'pReviewing (paperPhase r))
                  (admin s) (exists u User (exists rev Review (reviewOn u r rev))))
         
         ;		resource.pphase in PDiscussion implies {
         ;			one d : cc.decisions |
         ;				d != cc.undecided
         ;				and resource.decision = d
         ;		}
         ;	}
         (rule717 = (permit s a r) :- (Paper r) (= 'advancePhase a) (= 'pDiscussion (paperPhase r))
                  (admin s) (exists d Decision (and (not (= 'undecided d)) 
                                                    (decisionIs r d))))
         
         (rule707otherwise = (permit s a r) :- (Paper r) (= 'advancePhase a) 
                           (not (= 'pAssignment (paperPhase r))) (not (= 'pReviewing (paperPhase r))) (not (= 'pDiscussion (paperPhase r)))
                           (admin s))
         (rule707failed = (deny s a r) :- (Paper r) (= 'advancePhase a) )
         
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         
         ;; ADVANCE CONFERENCE PHASE
         
         ;	resource in Conference and action in AdvancePhase implies {
         ;		subject in cc.admins
         ;		cc.phase in Submission implies {
         ;			all p : cc.papers |
         ;				p.pphase not in PSubmission
         ;		}
         (rule725 = (permit s a r) :- (Conference r) (= 'advancePhase a) (= 'cSubmission (conferencePhase r))
                  (forall p Paper (= (paperPhase p) 'pSubmission)))   
         
         ;		cc.phase in Bidding implies {
         ;			// All papers after Bidding
         ;			all p : cc.papers | {
         ;				p.pphase not in PSubmission
         ;				and p.pphase not in PBidding
         ;			}
         ;			// All reviewers have made bids (?)
         ;			and all u : cc.reviewers |
         ;				one p : cc.papers |
         ;					( u -> p ) in cc.bid
         ;		}
         
         (rule729 = (permit s a r) :- (Conference r) (= 'advancePhase a) (= 'cBidding (conferencePhase r))
                  (forall p Paper (and (not (= (paperPhase p) 'pSubmission))
                                       (not (= (paperPhase p) 'pBidding))))
                  (forall u User (implies (User u)
                                          (exists p Paper (bid u p)))))   
         
         
         ;		cc.phase in Assignment implies {
         ;			// All papers are assigned
         ;			all p : cc.papers |
         ;				p.pphase not in PSubmission
         ;				and p.pphase not in PBidding
         ;				and p.pphase not in PAssignment
         ;		}
         
         (rule740 = (permit s a r) :- (Conference r) (= 'advancePhase a) (= 'cAssignment (conferencePhase r))
                  (forall p Paper (and (not (= (paperPhase p) 'pSubmission))
                                       (not (= (paperPhase p) 'pBidding))
                                       (not (= (paperPhase p) 'pAssignment)))))
         
         
         ;		cc.phase in Reviewing implies {
         ;			// All papers are reviewed
         ;			all p : cc.papers |
         ;				p.pphase not in PSubmission
         ;				and p.pphase not in PBidding
         ;				and p.pphase not in PAssignment
         ;				and p.pphase not in PReviewing
         ;		}
         
         
         (rule747 = (permit s a r) :- (Conference r) (= 'advancePhase a) (= 'cReviewing (conferencePhase r))
                  (forall p Paper (and (not (= (paperPhase p) 'pSubmission))
                                       (not (= (paperPhase p) 'pBidding))
                                       (not (= (paperPhase p) 'pAssignment))
                                       (not (= (paperPhase p) 'pReviewing)))))
         
         ;		cc.phase in Discussion implies {
         ;			// All papers have non-undecided decisions
         ;			all p : cc.papers |
         ;				p.pphase not in PSubmission
         ;				and p.pphase not in PBidding
         ;				and p.pphase not in PAssignment
         ;				and p.pphase not in PReviewing
         ;				and p.pphase not in PDiscussion
         ;		}
         ;	}
         
         (rule755 = (permit s a r) :- (Conference r) (= 'advancePhase a) (= 'cDiscussion (conferencePhase r))
                  (forall p Paper (and (not (= (paperPhase p) 'pSubmission))
                                       (not (= (paperPhase p) 'pBidding))
                                       (not (= (paperPhase p) 'pAssignment))
                                       (not (= (paperPhase p) 'pReviewing))
                                       (not (= (paperPhase p) 'pDiscussion)))))
         
         (rule725otherwise = (permit s a r) :- (Conference r) (= 'advancePhase a) 
                           (not (= 'pSubmission (conferencePhase r))) (not (= 'pBidding (conferencePhase r)))
                           (not (= 'pAssignment (conferencePhase r))) (not (= 'pReviewing (conferencePhase r))) (not (= 'pDiscussion (conferencePhase r)))
                           (admin s))
         (rule725failed = (deny s a r) :- (Conference r) (= 'advancePhase a) )
         
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  
         
         
         
         
         
         ;         
         ;;     resource in ConferenceInfo and action in EditConferenceInfo implies {
         ;;		subject in cc.admins
         ;;	}                                  
         (rule765 = (permit s a r) :- (ConferenceInfo r) (= 'editConferenceInfo a) (admin s))
         (rule765failed = (deny s a r) :- (ConferenceInfo r) (= 'editConferenceInfo a))
         
         ;;	resource in User and action in AddReviewer implies {
         ;;		subject in cc.admins
         ;;	}
         
         ;768
         (rule768 = (permit s a r) :- (user r) (= 'addReviewer a) (admin s))
         (rule768failed = (deny s a r) :- (user r) (= 'addReviewer a))
         
         ;;	resource in User and action in ModifyUserJob implies {
         ;;		subject in cc.admins
         ;;	}
         
         ; 771
         
         ;;	resource in User and action in RemoveUser implies {
         ;;		subject in cc.admins
         ;;	}
         
         ; 774
         ;;	resource in Conference and action in BecomeAuthor implies {
         ;;		subject not in cc.authors
         ;;		and cc.phase in Submission
         ;;	}
         
         ; 777
         ;;	resource in Conference and action in MakeSubmission implies {
         ;;		subject in cc.authors
         ;;		and cc.phase in Submission
         ;;	}
         
         
         ; 781
         ;;	resource in Paper and action in ModifySubmission implies {
         ;;		(subject in cc.admins)
         ;;		|| (resource.pphase in PSubmission and 
         ;;		     subject in cc.authors and 
         ;;		     resource in cc.papers and
         ;;		     (resource -> subject) in cc.author)
         ;;		|| (resource.pphase in PNotification and
         ;;			subject in cc.authors and
         ;;			resource in cc.papers and
         ;;			(resource -> subject) in cc.author )			
         ;;	}
         
         ; 785
         ;;	resource in Paper and action in WriteReview implies {
         ;;		resource.pphase in PReviewing and
         ;;		subject in cc.reviewers and
         ;;		(subject -> resource) in cc.assignment
         ;;	}
         
         ; 796
         
         ;;	resource in User and action in ModifyUserInfo implies {
         ;;		(subject in cc.admins)
         ;;		|| (subject = resource)
         ;;	}
         
         ; 801
         ;;	resource in User and action = ModifyUserPassword implies {
         ;;		(subject in cc.admins)
         ;;		|| 
         ;;           (subject = resource)
         ;;	}
         
         ; 805
         ;;	resource in Paper and action in PaperBid implies {
         ;;		resource.pphase in PBidding
         ;;		and subject in cc.reviewers
         ;;		and (subject -> resource) not in cc.conflict
         ;;	}
         
         ; 810
         ;;	resource in Paper and action in PaperConflict implies {
         ;;		resource.pphase in PBidding
         ;;		and subject in cc.reviewers
         ;;	}
         
         ; 815
         ;;	resource in User and action in PaperAssign implies {
         ;;		resource.pphase in PAssignment
         ;;		and subject in cc.admins
         ;;		and resource in cc.reviewers
         ;;	}
         
         ;819
         
         ;;	resource in Paper and action in PaperDecide implies {
         ;;		resource.pphase in PDiscussion
         ;;		and subject in cc.admins
         ;;	}
         ;;}
         
         ; 824
         
         
         (ruleFinal = (permit s a r) :- true))
        (RComb (fa permit deny)))
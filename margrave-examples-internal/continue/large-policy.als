module ContinuePolicy1
sig Subject{value: SubjectValue, type: SubjectAttributeType}
sig SubjectValue{}
sig SubjectAttributeType{}
part sig localUser, loginUser, reviewer, admin, thisUser, conflicted, paperReviewer, submittedReviewer, unsubmittedReviewer, reviewAuthor, currentUser extends SubjectValue{}
part sig ContinueRole extends SubjectAttributeType{}
sig Resource{value: ResourceValue, type: ResourceAttributeType}
sig ResourceValue{}
sig ResourceAttributeType{}
part sig conferenceRC, conferenceInfoRC, userRC, userJobsRC, userInfoRC, userPasswordRC, reviewerAssignmentsRC, reviewerAssignmentCountRC, reviewerConflictsRC, reviewerBidRC, paperRC, paperInfoRC, paperFileRC, paperDecisionRC, paperConflictsRC, paperAssignmentsRC, reviewRC, reviewSubmissionStatusRC, reviewRatingsRC, reviewCommentsAllRC, reviewCommentsPcRC, reviewReviewerRC, meetingRC, currentUserRC, reviewSubmissionRC, reviewerAssignmentsCountRC extends ResourceValue{}
part sig ContinueResource extends ResourceAttributeType{}
sig Action{value: ActionValue, type: ActionAttributeType}
sig ActionValue{}
sig ActionAttributeType{}
part sig read, write, create, delete extends ActionValue{}
part sig ContinueAction extends ActionAttributeType{}
sig Number{}
part sig s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16,
s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27, s28, s29, s30, s31, s32 extends Number{}
sig Rule{subjects: Subject, resources: Resource, actions: Action, number:Number, followers:set Number, effect: Effect}
sig Effect{}
part sig Permit, Deny extends Effect{}
sig Person{identities: set Subject}

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (loginUser in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (reviewerAssignmentsRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (read in act.value && ContinueAction in act.type) )) && 
                   r.number = s1 && 
		   r.followers = s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Permit}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (thisUser in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (reviewerAssignmentsRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (read in act.value && ContinueAction in act.type) )) && 
                   r.number = s2 && 
		   r.followers = s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Permit}

-- .....................................


fact {some r:Rule | 
                   (some res:Resource | res in r.resources && 
                                        ( (reviewerAssignmentsRC in res.value && ContinueResource in res.type) )) && 
                   r.number = s3 && 
		   r.followers = s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Deny}
	
-- .....................................

	   
fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (thisUser in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (userPasswordRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (write in act.value && ContinueAction in act.type) )) && 
                   r.number = s4 && 
		   r.followers = s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Permit}

-- .....................................


fact {some r:Rule | 
                   (some res:Resource | res in r.resources && 
                                        ( (userPasswordRC in res.value && ContinueResource in res.type) )) && 
                   r.number = s5 && 
		   r.followers = s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Deny}
-- .....................................


fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (thisUser in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (userInfoRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (read in act.value && ContinueAction in act.type) || (write in act.value && ContinueAction in act.type) )) && 
                   r.number = s6 && 
		   r.followers = s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Permit}
-- .....................................


fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (loginUser in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (userRC in res.value && ContinueResource in res.type) || (userJobsRC in res.value && ContinueResource in res.type) || (userInfoRC in res.value && ContinueResource in res.type) || (userPasswordRC in res.value && ContinueResource in res.type) || (reviewerAssignmentsRC in res.value && ContinueResource in res.type) || (reviewerAssignmentsCountRC in res.value && ContinueResource in res.type) || (reviewerConflictsRC in res.value && ContinueResource in res.type) || (reviewerBidRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (read in act.value && ContinueAction in act.type) )) && 
                   r.number = s7 && 
		   r.followers = s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Permit}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (admin in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (userRC in res.value && ContinueResource in res.type) || (userInfoRC in res.value && ContinueResource in res.type) || (userPasswordRC in res.value && ContinueResource in res.type) || (reviewerAssignmentsRC in res.value && ContinueResource in res.type) || (reviewerAssignmentsCountRC in res.value && ContinueResource in res.type) || (reviewerConflictsRC in res.value && ContinueResource in res.type) || (reviewerBidRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (read in act.value && ContinueAction in act.type) || (write in act.value && ContinueAction in act.type) )) && 
                   r.number = s8 && 
		   r.followers = s9+s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Permit}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (thisUser in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (userRC in res.value && ContinueResource in res.type) || (userJobsRC in res.value && ContinueResource in res.type) || (userInfoRC in res.value && ContinueResource in res.type) || (userPasswordRC in res.value && ContinueResource in res.type) || (reviewerAssignmentsRC in res.value && ContinueResource in res.type) || (reviewerAssignmentsCountRC in res.value && ContinueResource in res.type) || (reviewerConflictsRC in res.value && ContinueResource in res.type) || (reviewerBidRC in res.value && ContinueResource in res.type) )) && 
                   r.number = s9 && 
		   r.followers = s10+s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Deny}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (admin in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (userRC in res.value && ContinueResource in res.type) || (userInfoRC in res.value && ContinueResource in res.type) || (userPasswordRC in res.value && ContinueResource in res.type) || (reviewerAssignmentsRC in res.value && ContinueResource in res.type) || (reviewerAssignmentsCountRC in res.value && ContinueResource in res.type) || (reviewerConflictsRC in res.value && ContinueResource in res.type) || (reviewerBidRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (delete in act.value && ContinueAction in act.type) )) && 
                   r.number = s10 && 
		   r.followers = s11+s12+s13+s14+s15+s16+s17+s18+s19+s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Permit}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (paperReviewer in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (reviewReviewerRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (read in act.value && ContinueAction in act.type) )) && 
                   r.number = s11 && 
		   r.followers = s12+s13+s14+s15+s16+s17+s18+s19+s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Permit}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (paperReviewer in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (reviewSubmissionStatusRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (read in act.value && ContinueAction in act.type) )) && 
                   r.number = s12 && 
		   r.followers = s13+s14+s15+s16+s17+s18+s19+s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Permit}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (admin in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (reviewSubmissionStatusRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (read in act.value && ContinueAction in act.type) )) && 
                   r.number = s13 && 
		   r.followers = s14+s15+s16+s17+s18+s19+s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Permit}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (conflicted in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (reviewRC in res.value && ContinueResource in res.type) || (reviewSubmissionRC in res.value && ContinueResource in res.type) || (reviewRatingsRC in res.value && ContinueResource in res.type) || (reviewCommentsAllRC in res.value && ContinueResource in res.type) || (reviewCommentsPcRC in res.value && ContinueResource in res.type) || (reviewReviewerRC in res.value && ContinueResource in res.type) )) && 
                   r.number = s14 && 
		   r.followers = s15+s16+s17+s18+s19+s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Deny}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (reviewAuthor in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (reviewRC in res.value && ContinueResource in res.type) || (reviewSubmissionRC in res.value && ContinueResource in res.type) || (reviewRatingsRC in res.value && ContinueResource in res.type) || (reviewCommentsAllRC in res.value && ContinueResource in res.type) || (reviewCommentsPcRC in res.value && ContinueResource in res.type) || (reviewReviewerRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (read in act.value && ContinueAction in act.type) )) && 
                   r.number = s15 && 
		   r.followers = s16+s17+s18+s19+s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Permit}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (admin in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (reviewRC in res.value && ContinueResource in res.type) || (reviewSubmissionRC in res.value && ContinueResource in res.type) || (reviewRatingsRC in res.value && ContinueResource in res.type) || (reviewCommentsAllRC in res.value && ContinueResource in res.type) || (reviewCommentsPcRC in res.value && ContinueResource in res.type) || (reviewReviewerRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (read in act.value && ContinueAction in act.type) || (create in act.value && ContinueAction in act.type) || (delete in act.value && ContinueAction in act.type) )) && 
                   r.number = s16 && 
		   r.followers = s17+s18+s19+s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Permit}

-- .....................................

fact {some r:Rule | 
                   (some res:Resource | res in r.resources && 
                                        ( (reviewRC in res.value && ContinueResource in res.type) || (reviewSubmissionRC in res.value && ContinueResource in res.type) || (reviewRatingsRC in res.value && ContinueResource in res.type) || (reviewCommentsAllRC in res.value && ContinueResource in res.type) || (reviewCommentsPcRC in res.value && ContinueResource in res.type) || (reviewReviewerRC in res.value && ContinueResource in res.type) )) && 
                   r.number = s17 && 
		   r.followers = s18+s19+s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Deny}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (conflicted in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (paperAssignmentsRC in res.value && ContinueResource in res.type) )) && 
                   r.number = s18 && 
		   r.followers = s19+s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Deny}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (admin in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (paperAssignmentsRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (read in act.value && ContinueAction in act.type) || (write in act.value && ContinueAction in act.type) )) && 
                   r.number = s19 && 
		   r.followers = s20+s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Permit}

-- .....................................

fact {some r:Rule | 
                   (some res:Resource | res in r.resources && 
                                        ( (paperAssignmentsRC in res.value && ContinueResource in res.type) )) && 
                   r.number = s20 && 
		   r.followers = s21+s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Deny}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (conflicted in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (paperConflictsRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (read in act.value && ContinueAction in act.type) )) && 
                   r.number = s21 && 
		   r.followers = s22+s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Permit}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (conflicted in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (paperConflictsRC in res.value && ContinueResource in res.type) )) && 
                   r.number = s22 && 
		   r.followers = s23+s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Deny}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (admin in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (paperRC in res.value && ContinueResource in res.type) || (paperInfoRC in res.value && ContinueResource in res.type) || (paperFileRC in res.value && ContinueResource in res.type) || (paperDecisionRC in res.value && ContinueResource in res.type) || (paperConflictsRC in res.value && ContinueResource in res.type) || (paperAssignmentsRC in res.value && ContinueResource in res.type) || (reviewRC in res.value && ContinueResource in res.type) || (reviewSubmissionStatusRC in res.value && ContinueResource in res.type) || (reviewRatingsRC in res.value && ContinueResource in res.type) || (reviewCommentsAllRC in res.value && ContinueResource in res.type) || (reviewCommentsPcRC in res.value && ContinueResource in res.type) || (reviewReviewerRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (delete in act.value && ContinueAction in act.type) )) && 
                   r.number = s23 && 
		   r.followers = s24+s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Permit}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (admin in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (currentUserRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (write in act.value && ContinueAction in act.type) )) && 
                   r.number = s24 && 
		   r.followers = s25+s26+s27+s28+s29+s30+s31 && 
                   r.effect = Permit}

-- .....................................

fact {some r:Rule | 
                   (some res:Resource | res in r.resources && 
                                        ( (currentUserRC in res.value && ContinueResource in res.type) )) && 
                   r.number = s25 && 
		   r.followers = s26+s27+s28+s29+s30+s31 && 
                   r.effect = Deny}

-- .....................................

fact {some r:Rule | 
                   (some res:Resource | res in r.resources && 
                                        ( (conferenceInfoRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (read in act.value && ContinueAction in act.type) )) && 
                   r.number = s26 && 
		   r.followers = s27+s28+s29+s30+s31 && 
                   r.effect = Permit}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (admin in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (conferenceInfoRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (write in act.value && ContinueAction in act.type) )) && 
                   r.number = s27 && 
		   r.followers = s28+s29+s30+s31 && 
                   r.effect = Permit}

-- .....................................

fact {some r:Rule | 
                   (some res:Resource | res in r.resources && 
                                        ( (conferenceInfoRC in res.value && ContinueResource in res.type) )) && 
                   r.number = s28 && 
		   r.followers = s29+s30+s31 && 
                   r.effect = Deny}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (loginUser in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (conferenceRC in res.value && ContinueResource in res.type) || (conferenceInfoRC in res.value && ContinueResource in res.type) || (currentUserRC in res.value && ContinueResource in res.type) || (meetingRC in res.value && ContinueResource in res.type) || (paperRC in res.value && ContinueResource in res.type) || (paperInfoRC in res.value && ContinueResource in res.type) || (paperFileRC in res.value && ContinueResource in res.type) || (paperDecisionRC in res.value && ContinueResource in res.type) || (paperConflictsRC in res.value && ContinueResource in res.type) || (paperAssignmentsRC in res.value && ContinueResource in res.type) || (reviewRC in res.value && ContinueResource in res.type) || (reviewSubmissionStatusRC in res.value && ContinueResource in res.type) || (reviewRatingsRC in res.value && ContinueResource in res.type) || (reviewCommentsAllRC in res.value && ContinueResource in res.type) || (reviewCommentsPcRC in res.value && ContinueResource in res.type) || (reviewReviewerRC in res.value && ContinueResource in res.type) || (userRC in res.value && ContinueResource in res.type) || (userJobsRC in res.value && ContinueResource in res.type) || (userInfoRC in res.value && ContinueResource in res.type) || (userPasswordRC in res.value && ContinueResource in res.type) || (reviewerAssignmentsRC in res.value && ContinueResource in res.type) || (reviewerAssignmentCountRC in res.value && ContinueResource in res.type) || (reviewerConflictsRC in res.value && ContinueResource in res.type) || (reviewerBidRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (read in act.value && ContinueAction in act.type) )) && 
                   r.number = s29 && 
		   r.followers = s30+s31 && 
                   r.effect = Permit}

-- .....................................

fact {some r:Rule | 
                   (some sub:Subject | sub in r.subjects && 
                                      ( (admin in sub.value && ContinueRole in sub.type) )) && 
                   (some res:Resource | res in r.resources && 
                                        ( (conferenceRC in res.value && ContinueResource in res.type) || (conferenceInfoRC in res.value && ContinueResource in res.type) || (currentUserRC in res.value && ContinueResource in res.type) || (meetingRC in res.value && ContinueResource in res.type) || (paperRC in res.value && ContinueResource in res.type) || (paperInfoRC in res.value && ContinueResource in res.type) || (paperFileRC in res.value && ContinueResource in res.type) || (paperDecisionRC in res.value && ContinueResource in res.type) || (paperConflictsRC in res.value && ContinueResource in res.type) || (paperAssignmentsRC in res.value && ContinueResource in res.type) || (reviewRC in res.value && ContinueResource in res.type) || (reviewSubmissionStatusRC in res.value && ContinueResource in res.type) || (reviewRatingsRC in res.value && ContinueResource in res.type) || (reviewCommentsAllRC in res.value && ContinueResource in res.type) || (reviewCommentsPcRC in res.value && ContinueResource in res.type) || (reviewReviewerRC in res.value && ContinueResource in res.type) || (userRC in res.value && ContinueResource in res.type) || (userJobsRC in res.value && ContinueResource in res.type) || (userInfoRC in res.value && ContinueResource in res.type) || (userPasswordRC in res.value && ContinueResource in res.type) || (reviewerAssignmentsRC in res.value && ContinueResource in res.type) || (reviewerAssignmentCountRC in res.value && ContinueResource in res.type) || (reviewerConflictsRC in res.value && ContinueResource in res.type) || (reviewerBidRC in res.value && ContinueResource in res.type) )) && 
                   (some act:Action | act in r.actions && 
                                      ( (read in act.value && ContinueAction in act.type) || (write in act.value && ContinueAction in act.type) )) && 
                   r.number = s30 && 
		   r.followers = s31 && 
                   r.effect = Permit}

-- .....................................

fact {some r:Rule | 
                   (some res:Resource | res in r.resources && 
                                        ( (conferenceRC in res.value && ContinueResource in res.type) || (conferenceInfoRC in res.value && ContinueResource in res.type) || (currentUserRC in res.value && ContinueResource in res.type) || (meetingRC in res.value && ContinueResource in res.type) || (paperRC in res.value && ContinueResource in res.type) || (paperInfoRC in res.value && ContinueResource in res.type) || (paperFileRC in res.value && ContinueResource in res.type) || (paperDecisionRC in res.value && ContinueResource in res.type) || (paperConflictsRC in res.value && ContinueResource in res.type) || (paperAssignmentsRC in res.value && ContinueResource in res.type) || (reviewRC in res.value && ContinueResource in res.type) || (reviewSubmissionStatusRC in res.value && ContinueResource in res.type) || (reviewRatingsRC in res.value && ContinueResource in res.type) || (reviewCommentsAllRC in res.value && ContinueResource in res.type) || (reviewCommentsPcRC in res.value && ContinueResource in res.type) || (reviewReviewerRC in res.value && ContinueResource in res.type) || (userRC in res.value && ContinueResource in res.type) || (userJobsRC in res.value && ContinueResource in res.type) || (userInfoRC in res.value && ContinueResource in res.type) || (userPasswordRC in res.value && ContinueResource in res.type) || (reviewerAssignmentsRC in res.value && ContinueResource in res.type) || (reviewerAssignmentCountRC in res.value && ContinueResource in res.type) || (reviewerConflictsRC in res.value && ContinueResource in res.type) || (reviewerBidRC in res.value && ContinueResource in res.type) )) && 
                   r.number = s31 && 
		   r.followers = s32 && 
                   r.effect = Deny}

fun before(r1:Rule, r2:Rule){r2.number in r1.followers}

fun Performable(p:Person, res:Resource, act:Action)
    {some r:Rule | some i:Subject | i in p.identities && i in r.subjects &&
				    act in r.actions &&
				    res in r.resources &&
				    r.effect = Permit &&
				    (all other:Rule - r |
				                        (i in other.subjects && res in other.resources && act in other.actions) =>
							before(r, other))}
							

assert loginUserCanReadConference{ all p:Person| all res:Resource | all act:Action |
                                   (loginUser in p.identities.value && ContinueRole in p.identities.type &&
                                    conferenceRC = res.value && ContinueResource = res.type &&
                                    read = act.value && ContinueAction = act.type) => 
                                    Performable(p, res, act)}

-- I got a java.lang.OutOfMemoryError checking for 20
--check loginUserCanReadConference for 10 but 2 Action, 3 Subject, 5 Resource

assert adminCanReadConference{ all p:Person| all res:Resource | all act:Action |
				(admin in p.identities.value && ContinueRole in p.identities.type &&
				conferenceRC = res.value && ContinueResource = res.type &&
				read = act.value && ContinueAction = act.type) =>
				Performable(p, res, act)}
--check adminCanReadConference for 10 but 2 Action, 3 Subject, 5 Resource

assert adminCanWriteConference{ all p:Person | all res:Resource | all act:Action |
				(admin in p.identities.value && ContinueRole in p.identities.type &&
				conferenceRC = res.value && ContinueResource = res.type &&
				write = act.value && ContinueAction = act.type) =>
				Performable(p, res, act)}

--check adminCanWriteConference for 10 but 2 Action, 3 Subject, 5 Resource

-- leaving out the ContinueRole,Resource,Action from now on since not informative
assert noOtherConferenceAccess { all p:Person | all res:Resource | all act:Action|
				  (conferenceRC = res.value &&
				   !( (admin in p.identities.value && (write = act.value || read = act.value)) ||
				      (loginUser in p.identities.value && read = act.value))) =>
			           !Performable(p, res, act)}

--check noOtherConferenceAccess for 10 but 2 Action, 3 Subject, 5 Resource

assert AnyRoleCanReadConferenceInfo { all p:Person | all res:Resource | all act:Action |
				    (conferenceInfoRC = res.value && read = act.value) =>
			             Performable(p, res, act)}

--check AnyRoleCanReadConferenceInfo for 10 but 2 Action, 3 Subject, 5 Resource

assert adminCanWriteConferenceInfo{ all p:Person | all res:Resource | all act:Action |
				    (admin in p.identities.value && conferenceInfoRC = res.value &&
				     write = act.value ) => Performable(p, res, act)}

--check adminCanWriteConferenceInfo for 10 but 2 Action, 3 Subject, 5 Resource

assert noOtherConferenceInfoWriteAccess{ all p:Person | all res:Resource | all act:Action |
					 (conferenceInfoRC = res.value && write = act.value &&
					  !(admin in p.identities.value)) => !Performable(p, res, act)}

--check noOtherConferenceInfoWriteAccess for 10 but 2 Action, 3 Subject, 5 Resource

assert AdminCanDeleteUserIFAdminNotThisUser{ all p:Person | all res:Resource | all act:Action |
					     (userRC = res.value && delete = act.value &&
					      admin in p.identities.value && !thisUser in p.identities.value) =>
					     Performable(p, res, act)}

--check AdminCanDeleteUserIFAdminNotThisUser for 10 but 2 Action, 3 Subject, 5 Resource

assert AdminCantDeleteUserIFAdminThisUser{ all p:Person | all res:Resource | all act:Action |
					   (userRC = res.value && delete = act.value &&
					   admin in p.identities.value && thisUser in p.identities.value) =>
					   !Performable(p, res, act)}

--check AdminCantDeleteUserIFAdminThisUser for 10 but 2 Action, 3 Subject, 5 Resource

assert someoneCanDeleteUser{ some p:Person | all res:Resource | all act:Action |
			     (userRC = res.value && delete = act.value) =>
			     Performable(p, res, act)}

--java.lang.OutOfMemoryError
check someoneCanDeleteUser for 15 











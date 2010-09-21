(Policy HospitalDenyPayrollMedRecs uses HospitalPolicy
	(Target )
	(Rules

		(DocRevResults = (Permit s a r p) :- (Doctor s) (Review a) (MedTestResults r) (TreatmentPlanning p))

		(SchedRevTelData = (Permit s a r p) :- (Scheduler s) (Review a) (HomeTelData r)
				                       (Scheduling p) (DataOf r e)
						       (Consent e a r p))

		(PayrollNotify = (Permit s a r p) :- (PayrollDept s) (Collect a) (HomeInfo r)
			                             (PaymentNotification p))

		(PayrollReview = (Permit s a r p) :- (PayrollDept s) (Review a) (HomeInfo r)
			                             (AddressConfirmation p))

		(UpdateWithholding = (Permit s a r p) :- (Employee s) (Update a) (Withholding401 r)
				                         (Payment p) (WebServiceRegistered s))

		(PayrollNoRecs = (Deny s a r p) :- (PayrollDept s) (Review a) (MedTestResults r))
                )
                
	(RComb FAC)
	(PComb FAC)
	(Children ))

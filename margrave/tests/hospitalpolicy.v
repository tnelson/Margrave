(PolicyVocab HospitalPolicy
	     (Types
              (Subject : Doctor Scheduler PayrollDept Employee)
              (Action : Review Collect Update)
              (Resource : MedTestResults HomeTelData HomeInfo Withholding401)
              (Purpose : TreatmentPlanning Scheduling PaymentNotification
                       AddressConfirmation Payment))
             (Decisions 
              Permit
              Deny)
             
             (Predicates
              (DataOf : HomeTelData Employee)
              (Consent : Employee Action Resource Purpose)
              (WebServiceRegistered : Employee)
              )
             (ReqVariables (s : Subject)
                           (a : Action)
                           (r : Resource)
                           (p : Purpose))
             (OthVariables 
              (e : Employee)
              )
             
             (Constraints
              (atmostone-all Purpose) 
              (atmostone-all Action)
              
              
              (disjoint-all Purpose)
              (disjoint-all Resource)
              (disjoint-all Action)
                            
              (partial-function DataOf)
              )
)

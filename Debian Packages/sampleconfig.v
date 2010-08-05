;Packages: Net1, FS1, FS2, FS3, WWW1, Mail1, Ping1, Tomcat1, InternetKiller1
;WWW -> Net ^ FS ^ ^ (virtual)Internet ^ ! InternetKiller
;Mail -> Net ^ (FS > 1)
;Ping -> Net 
;Tomcat -> WWW v Ping
;And only ONE FS can be installed

(PolicyVocab SampleConfig
             (Types
		(Configuration : 	(Net Net1)
					(FS FS1 (FSgt1 FS2 FS3))
					(WWW WWW1)
					(Tomcat Tomcat1)
					(Mail Mail1)
					(Ping Ping1)
					(InternetKiller InternetKiller1)	
					)
	     )
             (Decisions 
              Valid
              Invalid)
             (Predicates)
	     (ReqVariables (c : Configuration))
             (OthVariables )
             (Constraints
		(subset WWW Net)
		(subset WWW FS)
		(disjoint-all FS)
		(disjoint-all FSgt1)
		(disjoint WWW InternetKiller)
		(subset Mail Net)
		(subset Mail FSgt1)
		(disjoint Mail FS1)
		(subset Ping Net)

		(abstract WWW)
		(abstract Net)
		(abstract FS)
		(abstract Mail)
		(abstract Ping)
		(abstract Tomcat)
		(abstract InternetKiller)
		(abstract FSgt1)
		)) 

(PolicyVocab generatedConfig
(Types
(Configuration : (InternetKiller InternetKiller~1 )(WWW WWW~1 )(FS FS~5 FS~4 FS~3 FS~2 FS~1 )(Tomcat Tomcat~1 )(Mail Mail~1 )(Ping Ping~1 )(Net Net~1 )(Connection Connection~2 Connection~1 ))
)
(Decisions
Valid
Invalid)
(Predicates)(ReqVariables (c : Configuration))
(OthVariables )
(Constraints
(disjoint FS~1 FS~2)
(disjoint FS~1 FS~3)
(disjoint FS~1 FS~4)
(disjoint FS~1 FS~5)
(abstract Net)
(abstract WWW)
(abstract Mail)
(abstract Ping)
(abstract Tomcat)
(abstract InternetKiller)
(abstract FS)
(abstract Connection)
))
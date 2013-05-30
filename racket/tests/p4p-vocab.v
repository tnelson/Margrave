Theory(myvoc,
       Vocab(myvoc, 
             Types(A, B, C(D, E), E(F), Subject, Action),
             Predicates( r(Subject, Action), q(Subject), q2(Subject)),
             Constants( $c(C), $d(D) ), 
             Functions( f(A, B),
                        g(B, C))
            ),
       Axioms( disjoint(q, q2),
               ;disjoint(A,B),
               singleton(A),
               abstract(C),
               formula(true),
               formula(=($c, $d))))
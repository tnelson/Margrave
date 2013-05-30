Policy( uses(myvoc),
        Variables(s(Subject), a(Action)),        
        Rules( rule1(log(s, a), true),
               rule2(deny(s, a), r(s, a), not(q(s))) 
             ),
        RComb(fa(permit, deny )))
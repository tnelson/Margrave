(Vocab TMDemo
       (Types
        (Type Entity > Person Organization)              
        (Type Door > GradCtrDoor))              
       (Predicates
        ; gradStudent is now an IDB
        ;(Predicate gradStudent Entity Person)
        (Predicate goodPerformance Entity Person)
        (Predicate advisorOf Person Person))
       (Constants (Constant 'wpi Organization)
                  (Constant 'wpics Organization)))
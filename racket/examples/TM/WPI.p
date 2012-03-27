(Policy uses TMDemo
        (Variables 
         (Variable p Person)
         (Variable d Door))
        (Rules 
          
          ;(GradStudentDeleg = (gradStudent xwpi p) :- (= xwpi 'wpi) (CSWPI.gradStudent 'cswpi p))
          

          ;(GraduateIf = (canGraduate p) :- (gradStudent 'wpi p) (exists x Entity (and (goodPerformance x p) (advisorOf p x))))
                  
         ;(Prof1 = (isProfessor p) :- (= p 'dan))
         ;(Prof2 = (isProfessor p) :- (= p 'joshua))
         ;(Prof3 = (isProfessor p) :- (= p 'kathi))
         ; Collapsed into one rule:
         
         (IsProf = (isProfessor p) :- (or (= p 'dan)
                                          (= p 'joshua)
                                          (= p 'kathi)))
         
         (GradCtr = (canAccess p d) :- (gradStudent p) (GradCtrDoor d))          
         (WPITimIsGrad = (gradStudent p) :- (= 'wpi-tim p)))
        (RComb (over gradStudent badGradStudent)))


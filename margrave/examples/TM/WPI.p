(Policy uses TMDemo
        (Variables 
         (Variable p Person)
         (Variable d Door)
         (Variable xwpi Entity))
        (Rules 
          
          ;(GradStudentDeleg = (gradStudent xwpi p) :- (= xwpi 'wpi) (CSWPI.gradStudent 'cswpi p))
          

          ;(GraduateIf = (canGraduate p) :- (gradStudent 'wpi p) (exists x Entity (and (goodPerformance x p) (advisorOf p x))))
                  
          (GradCtr = (canAccess p d) :- (gradStudent p) (GradCtrDoor d))
          
          (WPITimIsGrad = (gradStudent p) :- (= 'wpi-tim p)))
        (RComb (over gradStudent badGradStudent)))


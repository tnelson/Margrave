(Theory manyconstraints
        (Vocab manyconstraints
               (Types
                (Object > Car Computer Potato Emu))
               (Predicates                
                (pred1 Object)
                (pred2 Object)
                
                (binarypred1 Object Object)
                (coco3 Computer)      
                )
               
               (Constants ('obj1 Object)
                          ('obj2 Object))
               (Functions )
               )
        (Axioms          
         (total-relation binarypred1)
         (disjoint pred1 pred2)
         (singleton Potato)
         (nonempty Car)
         (atmostone Emu)
         (abstract Object)
         
         (constants-neq-all Object)
         ; There are no color computer 3s:
         (formula (forall comp Computer (implies (coco3 comp)
                                                 (not (= comp comp)))))
         
         )) 	     



////////////////////////////////////////
// From MTotalInstanceIterator
////////////////////////////////////////

/*private String varToRelName(List<Relation> relList, HashMap<Relation, List<Tuple>>tupMap, int iVar)
	{
		for(Relation r : relList)
		{
			int relWidth = tupMap.get(r).size();
			
			if(iVar < relWidth)
				return r.name();
			iVar -= relWidth;
		}		
		return "";
	}/*
	
	/*private String varToTupleName(List<Relation> relList, HashMap<Relation, List<Tuple>>tupMap, int iVar)
	{
		for(Relation r : relList)
		{
			int relWidth = tupMap.get(r).size();
			
			if(iVar < relWidth && r.arity() == 1)				
				return tupMap.get(r).get(iVar).atom(0).toString();
			else if (iVar < relWidth)
				return tupMap.get(r).get(iVar).toString();
			iVar -= relWidth;
		}		
		return "";
	}*/
	

	
	/*public static void bddGCCallback(int n)
	{
		//forQuery.errStream.println("GC callback reached. n = "+n);
	}*/

////////////////////////////////////////
////////////////////////////////////////
////////////////////////////////////////


/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////
// The code below is non-functional, experimental, and not updated.
// Keeping it for my own reference. - TN
/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////



/*class MPartialInstanceIterator extends MInstanceIterator
{
	List<Integer> trivialTrue = new ArrayList<Integer>();
	List<Integer> trivialFalse = new ArrayList<Integer>();
	HashMap<Integer, Translation> nonTrivialTranslations = new HashMap<Integer, Translation>();
	HashMap<Integer, Bounds> nonTrivialBounds = new HashMap<Integer, Bounds>();
	int currentSize = 1;	
	
	CNFSpyFactory spyFactory;
	
	MPartialInstanceIterator(MQueryResult qr) 
			throws MGEManagerException, MGEUnknownIdentifier, MGEBadIdentifierName
	{
		super(qr);
		
		// Prepare a CNF for each model size to be tested.

		
		for(int iSize=1;iSize<=fromContext.maxSize;iSize++)
		{
			try
			{
				createCNFFor(qr.qryFormulaWithAxioms, iSize);
			}
			catch(TrivialFormulaException e)
			{				
	        	if(e.value().booleanValue() == false)
	        		trivialFalse.add(iSize);
	        	else
	        		trivialTrue.add(iSize);
	        	
	        	nonTrivialTranslations.put(iSize, null);
	        	nonTrivialBounds.put(iSize, null);
			}
			
		}
		
		//MEnvironment.errorStream.println(nonTrivialClauses);
		//MEnvironment.errorStream.println(nonTrivialBounds);
		
		if(qr.forQuery.debug_verbosity > 1)
			MEnvironment.writeOutLine("DEBUG: Translation to CNF complete. Time: "+msKodkodTransTime + " ms.");
		
	}
	
	MSolutionInstance constructPartial(int atSize)
	{		
		
		// *********************************************
		// Step 1: Get FULL prop. model from satsolver.
        
		if(nonTrivialTranslations.get(atSize) == null || nonTrivialBounds.get(atSize) == null)
			return null;
		
		Translation theTranslation = nonTrivialTranslations.get(atSize); 
		CNFSpy theSolver = (CNFSpy) theTranslation.cnf();
		Bounds theBounds = nonTrivialBounds.get(atSize);
		final int numPrimaryVariables = theTranslation.numPrimaryVariables();
	
		
		final long startSolve = System.currentTimeMillis();
        boolean issat = theSolver.solve();
        final long endSolve = System.currentTimeMillis();
        
        msKodkodSolveTime += (endSolve - startSolve);
        
        if(!issat)
        	return null; // no solutions at this size left 

        
        // valueOf(varn) will return varn's boolean value in the model found
        
		
		// *********************************************		
		// Step 2: Perform Fontaine's alg. to get a minimal partial model
        // http://www.montefiore.ulg.ac.be/~pfontain/
        // From his PhD Thesis, pg. 89-90.

		final boolean[] dontCares = new boolean[numPrimaryVariables + 1]; // 1-based
        final int[] counts = new int[theSolver.numberOfClauses()]; // 0-based
        HashMap<Integer, Set<Integer>> literalToClauses = new HashMap<Integer, Set<Integer>>();
        
        
		//theSolver.clauses; list of array wrappers
		
        // ++++
		// Build a counter for each clause. Starting value for a clause c = 
        // number of literals the clause shares with our total model.                
        
        // Loop for each clause c
        int clausenum = 0;
		for(MIntArrayWrapper clause : theSolver.clauses) // order preserved
		{
			// Loop for each literal in c 
			for(int iIndex = 0;iIndex < clause.size();iIndex++)
			{	
				int literal = clause.get(iIndex);								
				int varnum = Math.abs(literal);
				
				// We MUST count even non-primaries for this phase. 
				// If not, could end up with "1" that should be "2" (which would allow a primary var to be freed)
				
				// Does this literal agree with the total model?
				if((theSolver.valueOf(varnum) && literal > 0) ||
						(!theSolver.valueOf(varnum) && literal < 0))
					counts[clausenum]++;	
				
				// Store the clauses that involve each literal we see
				if(!literalToClauses.containsKey(literal))
					literalToClauses.put(literal, new HashSet<Integer>());
				literalToClauses.get(literal).add(clausenum);
			}
		
			//if(counts[clausenum] > 1)
			MEnvironment.writeOutLine("Clause "+clausenum+" was "+ clause+" and had count "+counts[clausenum]);
			
			clausenum++;
		}

		// ++++		
		
		// Notes to self:
		
		// M'is partial so far
		// M is left to do
		
		// each step: remove l_i from M
		//            is (M union M') / l_i a partial model?
		//            if so, can remove l_i from M'.

		// at each step i, 
		// n_{c,i} = |c intersection (M union M') |
		
		// so the counter for a clause is: how many places does this clause agree with our model?
		// agree = literal match (atom AND sign)
		
		// ++++
		
		// Now, for each variable, try to remove it.
		// Ordering DOES matter. For now, use the default ordering provided by Kodkod. May not be optimal.		
		// TODO ? ^^^ Yes, in fact it varies randomly. Can have a size 2 class one run and a size 4 class the next.
		
		for(int iVar = 1; iVar <= numPrimaryVariables; iVar++)
		{
			// Try to remove iVar.
			// We can do so if all the clauses involving the literal for iVar have counts > 1.

			int literal = iVar;
			if(!theSolver.valueOf(iVar))
				literal = literal * (-1);
			
			MEnvironment.writeOutLine("Literal: "+literal+" was in clauses: "+literalToClauses.get(literal));
			
			boolean canRemove = true;
			for(int c: literalToClauses.get(literal))
			{
				if(counts[c] <= 1)
					canRemove = false;
			}
			if(canRemove)
			{
				for(int c: literalToClauses.get(literal))
					counts[c] --;					
				dontCares[iVar] = true;				
			}
				
			
		}
		
		MEnvironment.writeOutLine("@@@@@ Don't cares were: ");
		for(int iVar=1;iVar<=numPrimaryVariables;iVar++)
			if(dontCares[iVar])
				MEnvironment.outWriter.print(iVar + " ");
		MEnvironment.writeOutLine("");
		
		// OPT to optimize speed, could store some of these values we are calculating (like vars -> clauses map) and expand each call
		
        
		
		// *********************************************
		// Step 3: Construct instance for the partial model
		// Much of this code is taken from interpret() in Translation.class in Kodkod
		
		final TupleFactory f = theBounds.universe().factory();

		final Instance resultInstance = new Instance(theBounds.universe());
		final Instance resultDontCares = new Instance(theBounds.universe());

		
		// Should have captured the skolem bounds when we created the translation object...
		
		for(Relation r : theBounds.relations())
		{
			TupleSet lower = theBounds.lowerBound(r);
			IntSet indeces = Ints.bestSet(lower.capacity());
			indeces.addAll(lower.indexView());
			
			IntSet dcIndices = Ints.bestSet(lower.capacity());
			
			IntSet vars = theTranslation.primaryVariables(r);
			
			MEnvironment.writeOutLine("Relation "+r+" had primary vars: "+vars);
			
			if (vars!=null)
			{
				int lit = vars.min();
				for(IntIterator iter = theBounds.upperBound(r).indexView().iterator(); iter.hasNext();  ) // note last part empty
				{
					final int index = iter.next();
					
					if(dontCares[lit])
					{
						lit++;
						dcIndices.add(index);
						continue;
					}
					
					if (!indeces.contains(index) && theSolver.valueOf(lit++))
						indeces.add(index);
				}
			}
			resultInstance.add(r, f.setOf(r.arity(), indeces));
			resultDontCares.add(r, f.setOf(r.arity(), dcIndices));
		}
		
		MEnvironment.writeOutLine("@@@@@@@");
		MEnvironment.writeOutLine(resultInstance.toString());
		MEnvironment.writeOutLine(resultDontCares.toString());
		
		
		// *********************************************
		// Step 4: FINALLY, augment the CNF at this model size with the partial's negation
        // (Kodkod does much the same, except with full models, to iterate through all possible models.)
		
		// This section must occur after creating instances for returning.
		// Otherwise adding clauses will invalidate prior results.
                
        // Must use PRIMARY variables only.
        // This is because non-primary vars are the ones used for conversion to CNF.
        // (So in a way, Kodkod is ALREADY giving partial models)
		
		List<Integer> notModel = new ArrayList<Integer>(numPrimaryVariables);		
		for(int i = 1; i <= numPrimaryVariables; i++)
		{
			// If this primary var is a don't care, ignore it.
			if(dontCares[i])
				continue;
			
			notModel.add(theSolver.valueOf(i) ? -i : i);			
		}
		
		// Would be nice to use toArray here.
		final int[] notModelArray = new int[notModel.size()];
		for(int iIndex=0;iIndex<notModel.size();iIndex++)
			notModelArray[iIndex] = notModel.get(iIndex);
		theSolver.addClause(notModelArray);
		
		
		//MEnvironment.errorStream.println();
		//MEnvironment.errorStream.println(theSolver.numberOfClauses());

		
		// *********************************************
		return new MSolutionInstance(resultInstance, resultDontCares);		
	}
	
	protected void prepareNext()
	{
		
		if(the_next != null)
			return;
			
		while(currentSize <= fromContext.maxSize)
		{
			// Try to construct a new partial model at current size 
						
			the_next = constructPartial(currentSize);						
			if(the_next != null)
				break;						
			
			// Failed at this size, move on to the next one.
			currentSize++;
		}			
	}
	
	void createCNFFor(Formula f, int iSize)
	throws MGEManagerException, MGEUnknownIdentifier, MGEBadIdentifierName, TrivialFormulaException
	{
		LinkedList<String> atoms = new LinkedList<String>();
		for(int ii=0;ii<iSize;ii++)			
			atoms.add("Atom"+ii);		
		Universe u = new Universe(atoms);		
		
		Solver qrySolver = new Solver();		
		qrySolver.options().setFlatten(true);
		qrySolver.options().setSymmetryBreaking(fromContext.forQuery.mySB);		
				
		spyFactory = new CNFSpyFactory(fromContext.forQuery.mySATFactory);
		qrySolver.options().setSolver(spyFactory);
		
		Bounds qryBounds = new Bounds(u);
		f = makeBounds(u, f, qryBounds);		
		
		// Kodkod Skolemizes, which means the bounds we have at this point
		// get augmented. If we want to give variable bindings (and we do!) 
		// we need to get the NEW bounds. But they aren't a public member
		// of Translation or SATSolver. So we use the reporting interface.
		
		SkolemReporter rep = new SkolemReporter();		
		qrySolver.options().setReporter(rep);
		
        try
        {
        	// Translate via Kodkod
    		final long startTransl = System.currentTimeMillis();
            Translation trans = Translator.translate(f, qryBounds, qrySolver.options());
            CNFSpy theSpy = (CNFSpy)trans.cnf();         
    		final long endTransl = System.currentTimeMillis();
    		msKodkodTransTime += (endTransl - startTransl);
            
            // Remember the clauses.
            nonTrivialTranslations.put(iSize, trans);   
            if(rep.skolemBounds != null)
            	nonTrivialBounds.put(iSize, rep.skolemBounds);
            else
            	nonTrivialBounds.put(iSize, qryBounds); // just in case!
    		
            
                                    
            
            // Kodkod modifies the clause arrays afterward. Do not re-use!

        }
        catch (TrivialFormulaException e)
        {
        	// Either ALL solutions or NO solutions. Let the caller decide.
        	throw e;
        }        
		
		
	}

} 
*/
//end partial iterator

/*
// To print the FORMULA rather than a MODEL, need to go from literals->(relations+tuples)
HashMap<Integer, Relation> relMap = new HashMap<Integer, Relation>();
for(Relation r : qryBounds.relations())
{
    IntSet s = trans.primaryVariables(r);
    
    IntIterator it = s.iterator();
    
  //  outStream.println(r + ": "+s);
    while(it.hasNext())
    {
        Integer theInt = it.next();
        relMap.put(theInt, r);
       // outStream.println(theInt +" -> "+r);
    }
}


forQuery.outStream.println("FoL Formula (with IDB biimps) is: ");
forQuery.outStream.println(f);
forQuery.outStream.println("Universe is: "+qryBounds.universe());
forQuery.outStream.println("# Clauses: "+theSpy.numberOfClauses());
forQuery.outStream.println("# Vars: "+theSpy.numberOfVariables());

// others from CNF conversion?

forQuery.outStream.println("Prop clauses were: ");


// Construct GROUND predicate formula from propositional formula
boolean first = true;
for(MIntArrayWrapper clause : theSpy.clauses)
{
    StringBuffer clauseStr = new StringBuffer();
    
    for(int ii=0;ii<clause.size();ii++)
    {
        String atomStr = "";
        int literal = clause.get(ii);
        
        if(literal < 0)
        {
            atomStr = "~";
            literal = Math.abs(literal);
        }
        
        Relation r = relMap.get(Integer.valueOf(literal));
        if(r == null)
            atomStr += "d"+literal;
        else
        {
            atomStr += r;
            
            // Compute index: literal - min of this relation's indices. 
            int index = literal - trans.primaryVariables(r).min();
            
            
            
            atomStr += "(";                                             
            Tuple tup = factory.tuple(r.arity(), index);
            // is that true?
            // we have 2 unary relations: R1 and R2.
            // and only one atom a.
            // then R1a and R2a get prop vars
            // kodkod provides a way to separate and we arent using it.
                                    
            atomStr += tup;                    
            atomStr += ")";
        }
            

        
        if(ii > 0)
            clauseStr.append(" OR ");
        clauseStr.append(atomStr);
        
    }
    
    
    if(first)
        forQuery.outStream.println("    ("+clauseStr+")");
    else
    	forQuery.outStream.println("AND ("+clauseStr+")");
    first = false;
}
 */


/*class MBDDContext
{
	// Stores a BDD and variable interpretations for a fixed model size.
	int msize;
	
	// Don't have issues with ordering relations or tuples...
	List<Relation> relList = new ArrayList<Relation>();
	HashMap<Relation, List<Tuple>> tupMap = new HashMap<Relation, List<Tuple>>();
	
	BDD thebdd;
	BDDFactory factory;
	
	int[] constantVals = new int[1]; // placeholder
	
	MSolutionSet mySolution;

	
	public void gcCallback(int n)
	{
		//forQuery.errStream.println("GC called.");
	}
	
	MBDDContext(int size, MSolutionSet sol) 
	{
		msize = size;		
		
		mySolution = sol;
		
		// Each size needs its own factory, since factory governs the variables.
		factory = JFactory.init(3000, 10000);
		factory.setIncreaseFactor(2); // double for each 
		factory.setMaxIncrease(0); // no cap on increase (safe?)
	
		
		try
		{
			// Register our own callback so that we don't spam the user.
			factory.registerGCCallback(this, mySolution.getClass().getDeclaredMethod("bddGCCallback", int.class));
		}
		catch(NoSuchMethodException e)
		{
			MEnvironment.errorStream.println("Unable to register GC callback with JavaBDD.");
		}
		
		thebdd = factory.zero();		
	}

	void finish()
	{
		factory.done();
	}
	

} */

/*public void prettyPrintSolutionsCondensed()
{
	// Iterate through all solutions, passing them to BDD(s) for simplification.

	// Separate prop variables and BDDs for each model size we see.
	HashMap<Integer, MBDDContext> contexts = new HashMap<Integer, MBDDContext>();		
	
	while(hasNext())
	{
		try
		{			
			Solution sol = next();
			MBDDContext con; 
			
			// First time we saw this size? Then initialize BDD environment for it.
			if(!contexts.containsKey(Integer.valueOf(sol.instance().universe().size())))
			{	
				con = new MBDDContext(sol.instance().universe().size(), this);
				contexts.put(Integer.valueOf(sol.instance().universe().size()), con);
				 
				int varcount = 0;
				
				for(Relation r : sol.instance().relations())
				{
					con.relList.add(r); // establish ordering
					List<Tuple> theseTuples = new ArrayList<Tuple>();
					con.tupMap.put(r, theseTuples);
					for(Tuple t : sol.instance().universe().factory().allOf(r.arity()))
						theseTuples.add(t); // establish ordering
					
					varcount += sol.instance().universe().factory().allOf(r.arity()).size();
				}
				
				con.factory.setVarNum(varcount);
				con.constantVals = new int[varcount];
				Arrays.fill(con.constantVals, -1);					
			}
			
			con = contexts.get(Integer.valueOf(sol.instance().universe().size()));
			BDD solbdd = con.factory.one(); // bdd for THIS solution only
			
			
			// s -> Prop. So Naive it doesn't even get little dots over the i.
			int ii = 0;
			for(Relation r : con.relList)
			{
			
				for(Tuple t : con.tupMap.get(r))
				{		
					int iVal;
					if(sol.instance().relationTuples().get(r).contains(t)) 						
						iVal = 1;						
					else
						iVal = 0;						
					
					// Add to BDD
					if(iVal == 1)
						solbdd = solbdd.andWith(con.factory.ithVar(ii));
					else
						solbdd = solbdd.andWith(con.factory.nithVar(ii));
					
					// Is this variable still unchanging?
					if(con.constantVals[ii] < 0)
						con.constantVals[ii] = iVal; // first solution
					// changed!
					else if((con.constantVals[ii] == 0 || con.constantVals[ii] == 1) && con.constantVals[ii] != iVal)						
						con.constantVals[ii] = 2; // flag it						
					
					ii++;
				}
			}

			con.thebdd = con.thebdd.orWith(solbdd); // destructive and
			
		}
		catch(MGENoMoreSolutions e)
		{
			break;
			// should never reach here.
		}
		
	}		
	
	// Now for each of the BDDs...
	for(Integer iSize : contexts.keySet())
	{
		MBDDContext con = contexts.get(iSize);
	
		// Weed any constant variables out of the BDD. A constant variable x will have survived the process having
		// constantVals[x] of 0 or 1.
		for(int iVar = 0; iVar < con.factory.varNum(); iVar++)
			if(con.constantVals[iVar] == 0)
				con.thebdd = con.thebdd.restrict(con.factory.nithVar(iVar));
			else if(con.constantVals[iVar] == 1)
				con.thebdd = con.thebdd.restrict(con.factory.ithVar(iVar));
	
		
		relOutput(con);
		
		con.finish();
	}		
	
}

private void relOutput(MBDDContext con) 
{
	// Which variables were not pruned out of the BDD as constants?
	int[] usedVars = con.thebdd.varProfile();
	
	// Easy index for which value to use
	int[] iValues = new int[con.constantVals.length];

	// To enable cleaner output. Takes TUPLE NAME (e.g., "[Atom1]") to a string 
	// listing its most specific types.
	HashMap<String, String> mostSpecificTypesForAtoms = new HashMap<String, String>();
	
	// For each solution
	for(byte[] binding : (List<byte[]>)con.thebdd.allsat())
	{
		forQuery.outStream.println("*** SOLUTION CLASS at model size "+con.msize);
		
		// Re-initialize MGType output array
		mostSpecificTypesForAtoms.clear();
		
		// Which vars were don't-care?
		Set<Integer> regardlessOf = new HashSet<Integer>();
		
		// Facilitates renaming: "Atom0" --> "$s".
		HashMap<String, String> replaceWith = new HashMap<String, String>();
		
		// Figure out true values (if not used, must be a constant, etc.)
		//  and Handle renaming ($kolemized constants)
		for(int iVar = 0; iVar < binding.length ; iVar++)
		{
			// What value to use?
			// Constant value 0/1 always overrides the BDD. 
			if(con.constantVals[iVar] == 0 || con.constantVals[iVar] == 1)
				iValues[iVar] = con.constantVals[iVar];
			// BDD solution may say "I never use this variable" -- or "I don't care about this variable."
			else if(usedVars[iVar] < 1 || binding[iVar] == -1)
				iValues[iVar] = -1;
			// BDD solution uses and has a value
			else if(usedVars[iVar] > 0 && binding[iVar] > -1)
				iValues[iVar] = binding[iVar];
			else
				iValues[iVar] = 2; // error state
			
			String relname = varToRelName(con.relList, con.tupMap, iVar);
			String tuplename = varToTupleName(con.relList, con.tupMap, iVar);
			if(relname.startsWith("$") && iValues[iVar] == 1)
			{
				if(replaceWith.containsKey(tuplename))
					replaceWith.put(tuplename, replaceWith.get(tuplename) + " = " + relname);
				else
					replaceWith.put(tuplename, relname);
			}
			
			// It's a type, so add it to the list for this atom
			try
			{
				MSort t = myVocab.getSort(relname);
				
				if(iValues[iVar] == 1 && t.subsorts.size() == 0 && !mostSpecificTypesForAtoms.containsKey(tuplename))
					mostSpecificTypesForAtoms.put(tuplename, relname);
				else if(iValues[iVar] == 1 && t.subsorts.size() == 0)
					mostSpecificTypesForAtoms.put(tuplename, mostSpecificTypesForAtoms.get(tuplename)+" "+relname);
				// else not a most-specific type, do nothing
			}
			catch(MGEUnknownIdentifier e)
			{
				// not a type, do nothing
			}
			catch(MGEBadIdentifierName e)
			{
				// same
			}
		}			
		
		// For each ATOM, print out its most specific type(s).
		for(int iAtom = 0; iAtom < con.msize; iAtom++)
		{
			if(mostSpecificTypesForAtoms.containsKey("Atom"+iAtom))
			{
				String out = "Atom"+iAtom;
				for(String key : replaceWith.keySet())
					out = out.replace(key, replaceWith.get(key));
				forQuery.outStream.println(out+": "+mostSpecificTypesForAtoms.get("Atom"+iAtom));
			}
			
		}
		
		// For each variable
		for(int iVar = 0; iVar < binding.length ; iVar++)
		{				
			// -1 goes on "Regardless of" list.
			// 0/1 get printed in a style depending on their nature
			// $kolemized bindings govern renaming for easy reading (above)
			// MGType relations do most general only (but include nots)
			// state relations print all tuples that are IN them.
	
			String rname = varToRelName(con.relList, con.tupMap, iVar);
			
			MSort t = null;				
			try
			{
				t = myVocab.getSort(rname);
			}
			catch(MGEUnknownIdentifier e)
			{ }
			catch(MGEBadIdentifierName e)
			{
				// fall through with t still equal to null
			}

			
			
			// Do something for this variable.
			if(rname.startsWith("$"))
				; // do nothing (renaming dealt with above)

			else if(iValues[iVar] == -1)
				regardlessOf.add(Integer.valueOf(iVar));				
			else if(t != null)
				; // do nothing (types dealt with above)				
			
			// Print out *positive* literals for this relation.
			else if(iValues[iVar] == 1)
				forQuery.outStream.println(varToRelTupleName(con.relList, con.tupMap, iVar, replaceWith));
			
			// (positive ONLY -- or it gets spammy.)
			//else if(iValues[iVar] == 0)
			//	forQuery.outStream.println("not " +varToRelTupleName(con.relList, con.tupMap, iVar, replaceWith));
			
			else if(iValues[iVar] > 1)
				forQuery.outStream.println("ERROR: " +varToRelTupleName(con.relList, con.tupMap, iVar, replaceWith));
			
		}
		
		// print regardless-of list.
		if(regardlessOf.size() > 0)
			forQuery.outStream.println("\n... Regardless of whether or not:");		
		for(Integer iObj : regardlessOf)
		{
			// formatting later
			forQuery.outStream.println(varToRelTupleName(con.relList, con.tupMap, iObj.intValue(), replaceWith));
		}			
		forQuery.outStream.println("(Solution class contained "+Math.round(Math.pow(2, regardlessOf.size()))+" models.)");

		forQuery.outStream.println("********************\n");
	} // end for each solution
			
	//MGJavaTests.cuddStyleOutput(con.thebdd);
	//forQuery.outStream.println("^^^^^");

	//for(int iVar = 0 ; iVar < con.factory.varNum(); iVar++)
	//	forQuery.outStream.print(con.constantVals[iVar]);
	//forQuery.outStream.print("\n");
	//for(int iVar = 0 ; iVar < con.factory.varNum(); iVar++)
//	{
	//	forQuery.outStream.println(iVar+": "+varToRelTupleName(con.relList, con.tupMap, iVar, new HashMap<String, String>()));
	//}		

} */

/*	private String varToRelTupleName(List<Relation> relList, HashMap<Relation, List<Tuple>>tupMap, int iVar, HashMap<String, String> replaceWith)
{
	String tup = varToTupleName(relList, tupMap, iVar);
	
	for(String key : replaceWith.keySet())
		tup = tup.replace(key, replaceWith.get(key));
	
	return varToRelName(relList, tupMap, iVar) + "("+tup+")"; 
}*/


////////////////////////////////////////














////////////////////////////////////////
// From MQuery
////////////////////////////////////////

	/*
	static void handleSortAssertions(MVocab voc, MExploreCondition mpc,
			Map<Variable, Expression> freeVars) throws MGEBadIdentifierName,
			MGEUnknownIdentifier {
		// MREPL.outStream.println(mpc.assertPossible);
		// MEnvironment.writeErrLine(mpc.assertNecessary);
		// MEnvironment.writeErrLine(mpc.assertAtomicNecessary);

		Set<List<Variable>> toDo = new HashSet<List<Variable>>(
				mpc.assertNecessary.keySet());
		toDo.addAll(mpc.assertAtomicNecessary.keySet());
		toDo.addAll(mpc.inferredSorts.keySet());

		for (List<Variable> varvector : toDo)
		{

			MCommunicator.writeToLog("\nProducing sorts for variable vector: "+varvector);

			Set<MVariableVectorAssertion> assertsN = mpc.assertNecessary
					.get(varvector);
			Set<MVariableVectorAssertion> assertsA = mpc.assertAtomicNecessary
					.get(varvector);
			Set<MVariableVectorAssertion> assertsP = mpc.assertSufficient
					.get(varvector);

			Set<MVariableVectorAssertion> allNecessary = new HashSet<MVariableVectorAssertion>(
					assertsN);
			allNecessary.addAll(assertsA);
			
			Set<MVariableVectorAssertion> inferredforthis = new HashSet<MVariableVectorAssertion>(mpc.inferredSorts.get(varvector));
			
			for (Variable v : varvector) 
			{
				// If this is a placeholder variable, ignore it
				if(mpc.knownPlaceholders.contains(v))
					continue;
				
				MSort runningSort;
				if(!freeVars.containsKey(v) || freeVars.get(v).equals(Expression.UNIV))
				{
					// Default to no info
					freeVars.put(v, Expression.UNIV);
					runningSort = null;
				}
				else
					runningSort = voc.getSortForExpression(freeVars.get(v));			
				
				MCommunicator.writeToLog("\n  Producing sort for variable: "+v);
				MCommunicator.writeToLog("\n  (Starting at: "+freeVars.get(v)+")");

				//MEnvironment.writeErrLine("var: "+v);
				
				// Start off with our well-formed formula inferences:
				for(MVariableVectorAssertion a : inferredforthis)
				{
					// If this isn't a sort, skip for now (test arity separately since
					// getSortForExpression tests NAMES. May have a sort and a >1-ary pred.
					if(a.sortExpression.arity() > 1)
						continue;
					
					// a.positive
					//a.sortExpression
					MSort theSort = voc.getSortForExpression(a.sortExpression);
					
					// if this isn't a sort, skip for now
					if(theSort == null)
						continue;
					
					if(runningSort == null)
					{					
						runningSort = theSort;
						freeVars.put(v, a.sortExpression);
						MCommunicator.writeToLog("\n    Taking inference (was null): "+a.sortExpression);
					}
					else
					{
						
						// TODO: we don't have intersection types...
						// So juse follow the first assertion we see
						// Only replace if we have a more specific assertion
						if(voc.isSubOrSubOf(theSort, runningSort))
						{
							runningSort = theSort;
							freeVars.put(v, a.sortExpression);
							MCommunicator.writeToLog("\n    Taking inference (was "+runningSort.name+"): "+a.sortExpression);
						}
						else
							MCommunicator.writeToLog("\n    Ignoring inference: "+a.sortExpression);
					}
				} // end for each inferredforthis				

				for (MVariableVectorAssertion a : allNecessary)
				{
					
					
					//MEnvironment.writeErrLine("assertion: "+a);

					if (!a.positive)
						continue;

					// If it is NECESSARY for x to have sort A
					// Then x must have sort A (or a subsort thereof)

					// If rel is a sort (which means this is a singleton vector)
					// just handle that assertion.
					if (voc.isSort(a.sortExpression.toString()))
					{
						MSort theSort = voc.getSortForExpression(a.sortExpression);
						if (runningSort == null)
						{
							runningSort = theSort;
							freeVars.put(v, a.sortExpression);
							MCommunicator.writeToLog("\n    Taking necessary assertion (was null): "+a.sortExpression);
						}
						else
						{
							// is theSort a subsort of the current running sort?
							// (need to check grandchildren etc. Also use subset constraints.)
							// If not, ignore

							//MEnvironment.writeErrLine("SUB? --- "+runningSort.subsorts);
							//MEnvironment.writeErrLine(theSort);
							if(voc.isSubOrSubOf(theSort, runningSort))
							{
								runningSort = theSort;
								freeVars.put(v, a.sortExpression);
								MCommunicator.writeToLog("\n    Taking necessary assertion (was "+runningSort.name+"): "+a.sortExpression);
							}
							else
								MCommunicator.writeToLog("\n    Ignoring necessary assertion: "+a.sortExpression);

							// else do nothing. (we COULD see which option had
							// the most children? TODO maybe

						} 
					} // end is sort

				} // end for each allnecessary

			} // end for each v in varvector

		}
		
		MCommunicator.writeToLog("\nDone. Assertions are: "+freeVars+")");
	}*/


/////////////////////////
// MQuery: adding included IDBs at EXPLORE level:

		//}
		

		//MEnvironment.outStream.println("Query created. Fmla = ");
		//qryFormula.accept(new FormulaIndentPrintV());
		
		// **********************************
		// Handle INCLUDE parameters
		// TODO remove this. include now at SHOW rather than EXPLORE level
		/*for(String dbname : includeMap.keySet())
		{
			// Indexings if any
			Set<List<String>> indexingsToAdd = new HashSet<List<String>>();
			
			try
			{				
			
				for(List<MTerm> indexing : includeMap.get(dbname))
				{
					// Can't add directly: user has provided a vector of identifiers.
					// addIDBOutputIndexing expects an _indexing_ into the tupled ordering.
					// Need to use our knowledge (from above) about ordering of the prefix.

					// prefixVarOrder

					List<String> nIndexing = new ArrayList<String>(indexing.size());
					for(String varname : indexing)
					{
						// Look for this var
						int ii = prefixVarOrder.indexOf(varname);
						// if not found, error.
						if(ii < 0)
						{
							throw new MGEArityMismatch("Unrecognized identifier in INCLUDE indexing: "+varname);
						}
						nIndexing.add(String.valueOf(ii+1));
					}

					indexingsToAdd.add(nIndexing);
				}							
			
				// Is this an edb or an idb?
				// EDB?
				if(uber.isSort(dbname) || uber.predicates.containsKey(dbname))
				{
					result.addEDBIncludes(dbname);
					for(List<String> nIndexing : indexingsToAdd)
						result.addEDBIncludesIndexing(dbname, nIndexing);
				}
				else
				{			
					// IDB
					String idbname = dbname;
					
					// add to general list
					result.addIDBOutputs(idbname);
					for(List<String> nIndexing : indexingsToAdd)
						result.addIDBOutputIndexing(dbname, nIndexing);
				} // end of IDB case
			
			}
			catch(MGEArityMismatch e)
			{
				MEnvironment.writeErrLine(e.getLocalizedMessage());
				return null;
			}
			
		}
*/


///////////////////////////////////////////////////

	/*
	 * private boolean funcIsCoveredByOther(FuncStruct f, HashMap<Expression,
	 * Set<FuncStruct>> coveredby) throws MGEUnknownIdentifier,
	 * MGEBadIdentifierName { // We know that that f covers its own type, so
	 * check to see if there's some other in the list.
	 * if(coveredby.get(f.resulttype).size() > 1) return true;
	 *
	 * // Assert that this is a simple MgType, not an intersection (or etc.)
	 * thereof. // Search for covered subtypes of f's type. List<MGSort> tocheck
	 * = new LinkedList<MGSort>(); for(MGSort c :
	 * myVocab.getSortForExpression(f.resulttype).subsorts) tocheck.add(c);
	 *
	 * while(tocheck.size() > 0) { MGSort current = tocheck.get(0);
	 * if(coveredby.containsKey(current.rel))
	 * if(coveredby.get(current.rel).size() > 0) return true;
	 *
	 * // continue to check deeper in the hierarchy for(MGSort c :
	 * current.subsorts) tocheck.add(c); tocheck.remove(0); }
	 *
	 * if(debug_verbosity >= 2) MREPL.outStream.println("DEBUG: "+f +
	 * " was not multi-covered by "+coveredby); return false; }
	 */

	/*
	 * private static Set<FuncStruct> walkASTForFunctions(Formula origfmla,
	 * MGVocab context) throws MGEUnknownIdentifier { // Convert to NNF (nearly)
	 * and return the result of walking the AST. WalkASTForFunctionsV walker =
	 * new WalkASTForFunctionsV(context);
	 *
	 * Formula nnf_formula = origfmla.accept(new NNFConverterV());
	 *
	 *
	 * //MREPL.outStream.println("Traversing both formulas and measuring their length..."
	 * );
	 * //MREPL.outStream.println("Length of pre-nnf: "+measureFormulaSize(origfmla)+
	 * ". Length of post-nnf: "+ // measureFormulaSize(nnf_formula));
	 *
	 * Set<FuncStruct> results = nnf_formula.accept(walker);
	 *
	 * if(walker.error) throw new
	 * MGEUnknownIdentifier("AST Walker returned error: "
	 * +walker.error_condition); return results; }
	 */

/////////////////////////////////////////////////////

	/*public void addIDBOutputIndexing(String idbname, List<String> indexing)
			throws MGEArityMismatch, MGEUnknownIdentifier
	{
		// CHECK: the indexing Strings must actually be integers
		for (String inlist : indexing) {
			try {
				Integer.parseInt(inlist);
			} catch (NumberFormatException e) {
				throw new MGEArityMismatch(e.toString());
			}
		}

		// CHECK: Valid idb name

		String[] split = idbname.split(MEnvironment.sIDBSeparatorRegExp);
		String polName = split[0].toLowerCase();

		if (split.length < 2)
			throw new MGEUnknownIdentifier(
					"IDB names for output must be of the form policyname:idbname.");

		String internal_idbname = idbname.substring(polName.length() + 1)
				.toLowerCase(); // include :

		if (!myIDBCollections.containsKey(polName))
			throw new MGEUnknownIdentifier("IDB collection appeared in IDBOUTPUT that was not used in condition or UNDER clause: "
					+ polName + MEnvironment.eol + " Collections declared were: "+myIDBCollections.keySet());


		MIDBCollection coll = myIDBCollections.get(polName);

		if (!coll.containsIDB(internal_idbname))
			throw new MGEUnknownIdentifier("Unknown IDB: " + internal_idbname
					+ " in collection: " + polName);

		List<Variable> idbArity = coll.varOrderings.get(internal_idbname);
		
		// CHECK: Arity matches between the indexing given and the idb.
		if (idbArity.size() != indexing.size())
			throw new MGEArityMismatch(
					"Given indexing did not match IDB arity: " + idbname
					+ ", " + indexing + "." + " Arity of IDB was: "
					+ idbArity.size());

		// Add to indexing map
		if(!idbsToAddInFormula.containsKey(idbname))
			idbsToAddInFormula.put(idbname, new HashSet<List<String>>());
		idbsToAddInFormula.get(idbname).add(indexing);
	}
*/
	
/*	public boolean isQuerySatisfiable() throws MGException
	{
		// don't bother saving the result
		return runQuery().getTotalIterator().hasNext();
	}
*/


////////////////////////////////
// MVocabulary

	/*protected void makeThisTheTopLevelSort(String name)
			throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// Creates a new sort and makes it the parent of all parentless types
		// used in tupling

		clearCache();
		
		if (!isSort(name))
			sorts.put(name, new MSort(name));
		MSort t = getSort(name);

		if(isSubtype(t))
			throw new MGEBadIdentifierName("Sort "+name+" had supersorts; could not make it top level.");
		
		for (MSort candidate : sorts.values())
		{
			if (!isSubtype(candidate) && !(candidate.name.equals(name)))
			{
				t.subsorts.add(candidate);
				candidate.parents.add(t);
			}
		}
	}*/


	/**
	 * Does not call validateIdentifier before checking, but much faster.
	 * Should be used internally or when the parameter has already been
	 * validated.
	 * @param n
	 * @return
	 */
/*	boolean fastIsSort(String n)
	{
		return fastGetSort(n) != null;
	}*/
	
	/**
	 * See fastIsSort doc
	 * @param n
	 * @return
	 */
/*	MSort fastGetSort(String n)
	{
		return sorts.get(n);
	}
*/

/*	Set<MSortPair> getConciseDisjointSortsAsymm()
	{
		// Gives the result of calling getConciseDisjointSorts on
		// each sort, *but*, will only assert the disjointness between
		// A and B in one direction. Saves work. 
		
		Set<MSortPair> results = new HashSet<MSortPair>();
		
		for(MSort aSort : sorts.values())
		{
			Set<MSort> theseDisjs = getConciseDisjointSorts(aSort);
			
			// Set will use equality and not double-populate.
			for(MSort othSort : theseDisjs)
			{
				MSortPair aPair = new MSortPair(aSort, othSort);
				results.add(aPair);
			}
			
		}
		
		return results;
	}
*/

	// Called by MatrixTuplingV
	/*public static String constructIndexing(Expression be,
			HashMap<Variable, Integer> indexing) {
		List<String> lst = inorderTraversalOfVariableProduct(be, indexing, null);
		if (lst.size() < 1)
			return "";

		StringBuffer result = new StringBuffer(lst.get(0));
		for (int ii = 1; ii < lst.size(); ii++)
			result.append("," + lst.get(ii));
		return result.toString();
	}
	
	public static String constructIndexing(List<String> vars,
			HashMap<Variable, Integer> indexing)
	{		
		if (vars.size() < 1)
			return "";

		Variable theVar = MFormulaManager.makeVariable(vars.get(0));
		StringBuffer result = new StringBuffer(String.valueOf(indexing.get(theVar)));
		for (int ii = 1; ii < vars.size(); ii++)
		{
			theVar = MFormulaManager.makeVariable(vars.get(ii));
			result.append("," + String.valueOf(indexing.get(theVar)));
		}
		return result.toString();
	}*/	

	/*
	private static List<String> inorderTraversalOfVariableProduct(
			Expression e, HashMap<Variable, Integer> indexing,
			HashMap<Variable, String> sortenv)
	{
		// DFS this expression. Assume either a BinaryExpression node or a
		// Variable. Otherwise yell.
		List<String> sort_result = new ArrayList<String>();
		List<String> index_result = new ArrayList<String>();
		List<String> varname_result = new ArrayList<String>();

		/////////////////////////////////////////////////////////////
		// Setup for DFS
		List<Expression> dfslist = new LinkedList<Expression>();
		if(e instanceof BinaryExpression)
		{			
			BinaryExpression be = (BinaryExpression) e;
			if(!be.op().equals(ExprOperator.PRODUCT))
				throw new MUserException("inorderTraversalOfVariableProduct: Not a variable: "+be);
			
			dfslist.add(be.left());
			dfslist.add(be.right());
		}
		
		// This method is NOT meant to handle more than a variable product. The above "term" traversal method does that.
		// This method is called from tupling, which requires only variables!
		
		else if (e instanceof NaryExpression)
		{
			NaryExpression ne = (NaryExpression) e;
			if(!ne.op().equals(ExprOperator.PRODUCT))
				throw new MUserException("inorderTraversalOfVariableProduct: Not a variable: "+ne);

			for(int ii=0;ii<ne.size();ii++)
			{
				dfslist.add(ne.child(ii));
			}			
		}

		/////////////////////////////////////////////////////////////
		// DFS
		while (dfslist.size() > 0)
		{
			Expression next = dfslist.get(0);
			dfslist.remove(0);

			// What does sortenv say the sort of this variable is?
			if (next instanceof Variable)
			{
				if (sortenv != null)
					sort_result.add(sortenv.get(next));
				if (indexing != null)
					index_result.add(indexing.get(next).toString());
				varname_result.add(next.toString());

			}
			else if (next instanceof BinaryExpression)
			{
				BinaryExpression benext = (BinaryExpression) next;
				// Don't .add to the END. Need to put at the beginning,
				// or else this will not be in-order.
				//dfslist.add(benext.left()); <--- NO
				//dfslist.add(benext.right()); <--- NO
				// Add right first (so left ends up in its proper place)
				dfslist.add(0, benext.right());
				dfslist.add(0, benext.left());
			}
			else if(next instanceof NaryExpression)
			{
				NaryExpression nenext = (NaryExpression) next;
				
				// In reverse order
				for(int ii=nenext.size()-1;ii>=0;ii--)
				{
					Expression childexpr = nenext.child(ii);
					dfslist.add(0, childexpr);
				}
			}

			else
				return new ArrayList<String>(); // will warn user
		} // while there remain DFS nodes to explore

		if (sortenv != null)
			return sort_result;
		if (indexing != null)
			return index_result;
		return varname_result;

	}*/

/*	protected boolean possibleOverlap(String st1, String st2)
			throws MGEUnknownIdentifier, MGEBadIdentifierName {
		return possibleOverlap(getSort(st1), getSort(st2));
	}

	protected boolean possibleOverlap(Expression et1, Expression et2)
			throws MGEUnknownIdentifier, MGEBadIdentifierName {
		return possibleOverlap(getSortForExpression(et1),
				getSortForExpression(et2));
	}

	protected boolean possibleOverlap(MSort t1, MSort t2)
			throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// TODO use cache (and construct it!)
		
		// Since we require top-level sorts to be disjoint 
		if (t1.parents.size() == 0 && t2.parents.size() == 0 && t1 != t2)
			return false;
		
		Set<MSort> subs1 = buildSubSortSet(t1);
		Set<MSort> subs2 = buildSubSortSet(t2);
		
		// <-related?
		if(subs1 == subs2 || subs1.contains(t2) || subs2.contains(t1))
			return true;
		
		// common lower bound?
		
		if(!Collections.disjoint(subs1, subs2))
			return true;
		
		// unrelated sorts, no common lower-bound
		return false; 
	}*/
	





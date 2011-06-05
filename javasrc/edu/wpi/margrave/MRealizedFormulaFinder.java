package edu.wpi.margrave;

import java.io.PrintWriter;
import java.util.*;

import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Solver;
import kodkod.engine.fol2sat.Translation;
import kodkod.engine.fol2sat.Translator;
import kodkod.engine.fol2sat.TrivialFormulaException;
import kodkod.instance.Bounds;
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;

import org.sat4j.core.VecInt;
import org.sat4j.specs.*;
import org.w3c.dom.Document;



public class MRealizedFormulaFinder extends MCNFSpyQueryResult
{			
				
	MRealizedFormulaFinder(MPreparedQueryContext qr)
			throws MUserException
	{
		super(qr);			
  	}
		
	
	public List<String> applyIndexing(Map<String, Set<List<MTerm>>> candidates, Map<String, String> originalPreds, Map<String, List<MTerm>> originalIndexing)
	{
		// Translate to tupled form
		// e.g.
		// R(x, y, z) -> R_1,2,3
		// Note we aren't adding on the trivial (z) to the end yet. Just pred names.
		
		List<String> indexedCandidates;
		
		//MCommunicator.writeToLog("\napplyIndexing: "+candidates +";   "+originalPreds+";   "+originalIndexing);
		
		if(!fromContext.forQuery.tupled)
		{
			// Not tupled, ignore indexing
			indexedCandidates = new ArrayList<String>(candidates.keySet());			
		}
		else
		{
			// create indexed list
			indexedCandidates = new ArrayList<String>();	
		
			for(String predname : candidates.keySet())
			{
				//MCommunicator.writeToLog("\n  applyIndexing loop for: "+predname);
				
				//MEnvironment.errorStream.println("predname: "+predname);
				//MEnvironment.errorStream.println(candidates.get(predname));
				for(List<MTerm> anIndexing : candidates.get(predname))
				{
					//MCommunicator.writeToLog("\n    applyIndexing inner loop for: "+predname);
					String newName = predname;
				
//					MEnvironment.errorStream.println("indexing: "+anIndexing);
					
					// Query should have saved the visitor (and hence the ordering) in internalTuplingVisitor

					Map<Variable, Integer> theTupling = fromContext.forQuery.internalTuplingVisitor.pv.indexing;
					
					for(MTerm idxTerm : anIndexing)
					{
						if(!(idxTerm instanceof MVariableTerm))
						{
							// Forbidden in tupled query
							throw new MUserException("Query was tupled, but given candidate formula containing term "+idxTerm+
									". Only variables are allowed when tupling.");
						}
						
						String idx = idxTerm.expr.toString();
						
						// idx is a variable name, want to get its index in the tupling
						// The manager enforces only one Variable with that name.
						Variable theVar = MFormulaManager.makeVariable(idx);
						
						if(newName.equals(predname))
							newName += "_"+theTupling.get(theVar);
						else
							newName += ","+theTupling.get(theVar);
						
					}					
					
					//MEnvironment.errorStream.println(newName);
					indexedCandidates.add(newName);
					originalPreds.put(newName, predname);
					originalIndexing.put(newName, anIndexing);
				}
			}			
		}
		
		return indexedCandidates;
	}
	
	Set<String> undoIndexing(Set<String> results, Map<String, String> originalPreds, Map<String, List<MTerm>> originalIndexing)
	{
		Set<String> convertedResults = new HashSet<String>();
		for(String r : results)
		{			
			convertedResults.add(undoIndexing(r, originalPreds, originalIndexing));
		}
		return convertedResults;
	}
	
	String undoIndexing(String singleString, Map<String, String> originalPreds, Map<String, List<MTerm>> originalIndexing)
	{
		// If we changed nothing
		if(!originalPreds.containsKey(singleString))
			return singleString;
		else
		{
			return originalPreds.get(singleString)+originalIndexing.get(singleString);
		}	
	}
	
	
	
	public Map<String, Set<String>> getRealizedFormulas(Map<String, Set<List<MTerm>>> candidates, Map<String, Set<List<MTerm>>> cases)
	throws MUserException
	{
								
		Map<String, String> originalPreds = new HashMap<String, String>();
		Map<String, List<MTerm>> originalIndexing = new HashMap<String, List<MTerm>>();
		
		Map<String, Set<String>> results = new HashMap<String, Set<String>>();
		
		/////////////////////////////////////////////////////////////
		if(fromContext.forQuery.tupled)
		{
			// *** (1) Convert to indexed form
			List<String> indexedCandidates = applyIndexing(candidates, originalPreds, originalIndexing);
			List<String> indexedCases = applyIndexing(cases, originalPreds, originalIndexing);
			
			// *********************************************************************************
			// Make certain that the IDB names and indexings are declared in the parent query via IDBOUTPUT.		
			// But allow for indexed EDB names, too. Instead of checking idbOutputIndexing, check for whether 
			// the indexed name is in the vocab.
			for(String predname : indexedCandidates)
			{			
				if(!fromContext.forQuery.vocab.isSort(predname) && 
						!fromContext.forQuery.vocab.predicates.containsKey(predname) && 
						!fromContext.forQuery.idbOutputIndexing.keySet().contains(predname))
					throw new MUserException("Candidate in SHOW REALIZED: "+predname+
							" was not valid. If it is an EDB, it may be mis-spelled. If an IDB, it was not declared in the INCLUDE clause. Declared: "+fromContext.forQuery.idbOutputIndexing.keySet());
			}
			for(String predname : indexedCases)
			{
				if(!fromContext.forQuery.vocab.isSort(predname) && 
						!fromContext.forQuery.vocab.predicates.containsKey(predname) && 
						!fromContext.forQuery.idbOutputIndexing.keySet().contains(predname))
					throw new MUserException("Case in SHOW REALIZED: "+predname+
							" was not valid. If it is an EDB, it may be mis-spelled. If an IDB, it was not declared in the INCLUDE clause. Declared: "+fromContext.forQuery.idbOutputIndexing.keySet());
		
			} 	
			// TODO de-index the error messages
						
		} // end tupled PRE PROCESSING
						
		
		/////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////				
		// Check at all sizes.
		for(int iSize=1;iSize<=fromContext.maxSize;iSize++)
		{
			// Call first to add "" if needed
			Map<String, Set<String>> thisResult = getRealizedFormulasAtSize(candidates, cases, iSize);		
			addToMap(results, thisResult);
		}
		/////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////
			
		
		if(fromContext.forQuery.tupled)
		{
			// *** (3) Convert BACK to the original notation
			Map<String, Set<String>> indexedResults = new HashMap<String, Set<String>>();
			
			for(String aCase : results.keySet())
			{
				if(aCase.equals(""))
				{
					indexedResults.put("", undoIndexing(results.get(""), originalPreds, originalIndexing));
				}
				else
				{
					
					indexedResults.put(undoIndexing(aCase, originalPreds, originalIndexing),
					                   undoIndexing(results.get(aCase), originalPreds, originalIndexing));
				}
			}
			
			results = indexedResults;
		}
		
		
		return results;
		
	}
	
	public Set<String> getRealizedFormulas(Map<String, Set<List<MTerm>>> candidates) throws MUserException
	{
		Map<String, Set<List<MTerm>>> cases = new HashMap<String, Set<List<MTerm>>>();
		return getRealizedFormulas(candidates, cases).get("");
	}
		
	
	void addToMap(Map<String, Set<String>> dest, Map<String, Set<String>> src)
	{
		for(String key : src.keySet())
		{
			if(dest.containsKey(key))
				dest.get(key).addAll(src.get(key));
			else
				dest.put(key, src.get(key));
		}
	}
		
	public String caseToString(String predname, List<MTerm> args)	
	{
		if(predname.length() < 1)
			return "";
		
		String result = predname + "(";
		
		boolean first = true;
		for(MTerm t : args)
		{
			if(first)
			{
				result += t.toString();
				first = false;
			}
			else
			{
				result += ", "+t.toString();
			}
		}
		
		return result + ")";
	}
	
	public Tuple getTupleForPropVariable(Bounds theBounds, Translation theTranslation, int arity, int minVar, int theVar)
	{		
		// Compute index: literal - min of this relation's indices. 
        int index = theVar - minVar;                
        TupleFactory factory = theBounds.universe().factory();
        Tuple tup = factory.tuple(arity, index);      
        return tup;
	}
	
	Relation getRelationFromBounds(Bounds theBounds, String name)
	{
		for(Relation r : theBounds.relations())
			if(r.name().equals(name))
				return r;
		return null;
	}
	
	int getPropVariableForVariable(Bounds theBounds, Translation theTranslation, MTerm termi, Object theAtom)
	{		
		if(!(termi instanceof MVariableTerm))
			throw new MUserException("REALIZED candidates must only involve variables, not constants or functions. Got: "+termi);
		MVariableTerm vti = (MVariableTerm) termi;
		String varname = termi.toString();
		
		// Much of this code is taken from interpret() in Translation.class in Kodkod
		
		// What is the prop variable for the fact varname(theAtom)? 
		
		// First, the skolem relation.
		// This will NOT be available via MFormulaManager, since Kodkod adds it internally. 
		Relation skRel = getRelationFromBounds(theBounds, "$"+varname);
		IntSet propVars = theTranslation.primaryVariables(skRel);
		int theVar = propVars.min();
		
		TupleFactory f = theBounds.universe().factory();
			
		// Inefficient, but should work for now: loop for each tuple in the upper bound of this rel.
		// OPT: don't loop every time
		for(IntIterator iter = theBounds.upperBound(skRel).indexView().iterator(); iter.hasNext();  )
		{
			int tupleIndex = iter.next();
			Tuple theTuple = f.tuple(skRel.arity(), tupleIndex);
		
			Object val = theTuple.atom(0);
			if(val.equals(theAtom))
				return theVar;
			theVar++;
		}
		
		
		throw new MUserException("getPropVariableForVariable: Looked for "+theAtom+" and did not find it.");		
	}
	
	
	public Set<int[]> makeClauseSetFor(Bounds theBounds, Translation theTranslation, 
			String predname, List<MTerm> args, List<Integer> startHereWrapper,
			Map<Integer, String> intermVarToPred, Map<Integer, List<MTerm>> intermVarToArgs,
			Set<Integer> newQVarsIntroduced)
	{
		Set<int[]> clauseSet = new HashSet<int[]>();

		MCommunicator.writeToLog("\n  makeClauseSetFor: "+predname+" "+args);
		
		// Produce a set of clauses that says R(t_1, ..., t_n) holds. 
		// Must cover each potential binding for the interior terms, though!
		
		// Many valid p_i's means potentially intractable number of clauses.
		        
		// Can't trust manager here, since Kodkod adds relations
        Relation r = getRelationFromBounds(theBounds, predname);
        IntSet s = theTranslation.primaryVariables(r);
    	int minVarForR = s.min();    	
    	
    	// Start with which integer as q_1?
        int q = startHereWrapper.get(0);
        int firstVar = q;
        
        // CLAUSES:
        // for each p_i meaning R(a_1, ..., a_n)
		// q_i <-> t_1(a_1) and ... and t_n(a_n) and p_i 
        IntIterator it = s.iterator();
        while(it.hasNext())
        {
        	int aVar = it.next();        	
        	
        	// This is a primary variable signifying R(a_1, ..., a_n)
        	// Issue a set of clauses that signify R(t_1, ..., t_n). The t's are the hard part.

        	// (a_1, ..., a_n)
        	Tuple thisTuple = getTupleForPropVariable(theBounds, theTranslation,r.arity(), minVarForR, aVar);        	               
        	
    		/////////////////////////////////////////////////////////       	        	
        	// * For each i <=n, issue (!q or t_i(a_i))
        	for(int ii=0;ii<thisTuple.arity();ii++)
        	{        		
        		// proposition for t_i(a_i)
        		int tiai = getPropVariableForVariable(theBounds, theTranslation, args.get(ii), thisTuple.atom(ii));        		        		        		
        		
        		int[] thisClause = new int[2];
        		thisClause[0] = -1 * q;
        		thisClause[1] = tiai;
        		clauseSet.add(thisClause);
        	}
        	
    		/////////////////////////////////////////////////////////        	
        	// * issue (!q or aVar) where aVar is R(...)
        	int[] thisClause = new int[2];
    		thisClause[0] = -1 * q;
    		thisClause[1] = aVar;
    		clauseSet.add(thisClause);

    		/////////////////////////////////////////////////////////
        	// * issue (!t_1(a_1) or ... or !t_n(a_n) or !aVar or q)
    		thisClause = new int[thisTuple.arity()+2];
    		for(int ii=0;ii<thisTuple.arity();ii++)
        	{
    			int tiai = getPropVariableForVariable(theBounds, theTranslation, args.get(ii), thisTuple.atom(ii)); 
    			thisClause[ii] = -1 * tiai;
        	}
    		thisClause[thisTuple.arity()] = -1 * aVar;
    		thisClause[thisTuple.arity()+1] = q;
    		clauseSet.add(thisClause);
    		
        
        	intermVarToPred.put(q, predname);
        	intermVarToArgs.put(q, args);   
        	newQVarsIntroduced.add(q);
        	q++;        
        }
        
        startHereWrapper.set(0, q);
                
		MCommunicator.writeToLog("\n  makeClauseSetFor returning: "+stringifyArrays(clauseSet));
		
        return Collections.unmodifiableSet(clauseSet);        
	}

	String stringifyArrays(Set<int[]> arrs)
	{
		String result = "< ";
		for(int[] arr : arrs)
			result += " " + Arrays.toString(arr);
			
		return result + ">";
	}
	
	
	public Map<String, Set<String>> getRealizedFormulasAtSize(Map<String, Set<List<MTerm>>> candidates, 
			Map<String, Set<List<MTerm>>> cases, int atSize)
	{			
		Map<String, Set<String>> result = new HashMap<String, Set<String>>();

		//MEnvironment.errorStream.println("~~~~ Getting realized fmlas at size "+atSize);
		
		// For each case,
		// which candidates can be populated?
		// If cases is empty, add a single trivial case:
		if(cases.size() < 1)
		{
			cases.put("", new HashSet<List<MTerm>>());
			cases.get("").add(new ArrayList<MTerm>());
		}
				
		MCommunicator.writeToLog("\nIn getPopulatedRelationsAtSize: "+candidates + " at size: "+atSize);
		MCommunicator.writeToLog("\n-------------------------------------------------\n");
		/////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////
		// Is there any solving to do at all? Maybe not.
		// If the query is trivially false, impossible. If trivially true, 
		// may still have a contradiction between cases and candidates!
		
		if(nonTrivialTranslations.get(atSize) == null || nonTrivialBounds.get(atSize) == null)
		{			
			/*if(trivialTrue.contains(atSize))
			{
				// Trivially true --> all candidates can be realized at this size		
				for(String c : cases)
					result.put(c, new HashSet<String>(candidates));
			}
			else*/
			// TODO above not safe [tn: why???]
			if(trivialFalse.contains(atSize))
			{
				// Trivially false --> no candidates can be realized at this size,
				// regardless of case.
				for(String c : cases.keySet())
					for(List<MTerm> args : cases.get(c))
						result.put(caseToString(c, args), new HashSet<String>());
			}
			return result;			
		}
		/////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////

		//MCommunicator.writeToLog("\ngetPopulatedRelationsAtSize. Not trivial. Continuing. ");
						
		// Get the base problem.
		Translation theTranslation = nonTrivialTranslations.get(atSize); 
		CNFSpy theSolver = (CNFSpy) theTranslation.cnf();
		Bounds theBounds = nonTrivialBounds.get(atSize);
		final int numPrimaryVariables = theTranslation.numPrimaryVariables();
	
		if(fromContext.forQuery.debug_verbosity > 1)
    		MEnvironment.writeOutLine("Stats: "+theTranslation.numPrimaryVariables() +" primary vars. " + theSolver.numberOfClauses() +" clauses." );
		
        // Populate the result with empty sets
        result.clear();
		for(String c : cases.keySet())
		{
			if(c.length() < 1)
				result.put(c, new HashSet<String>());
			for(List<MTerm> args : cases.get(c))
				result.put(caseToString(c, args), new HashSet<String>());
		}
        
        List<Integer> startHereWrapper = new ArrayList<Integer>(1);
        startHereWrapper.add(theTranslation.cnf().numberOfVariables()+1);
		Map<Integer, String> intermVarToPred = new HashMap<Integer, String>();
		Map<Integer, List<MTerm>> intermVarToArgs = new HashMap<Integer, List<MTerm>>();
        Set<Integer> candidateGoalSet = new HashSet<Integer>();
        
        try
        {        	
        	ISolver realSolver = theSolver.getEquivalentSAT4j();
                	
        	
        	//////////////////////////////////////////////////////////
        	// Map the intermediate variables that indicate this candidate ---> the clauses used by them
        	// This way we know the candidate -> clauses mapping.
        	Map<Set<Integer>, Set<int[]>> candidateClauseSets = new HashMap<Set<Integer>, Set<int[]>>();        	
        	for(String aCandidateRel : candidates.keySet())        	
        	{
        		for(List<MTerm> candidateargs : candidates.get(aCandidateRel))
        		{   
        			Set<Integer> newQVarsIntroduced = new HashSet<Integer>();
        			
        			Set<int[]> candidateClauseSet = makeClauseSetFor(theBounds, theTranslation, aCandidateRel, candidateargs, 
            				startHereWrapper, intermVarToPred, intermVarToArgs, newQVarsIntroduced);
                	candidateClauseSets.put(Collections.unmodifiableSet(newQVarsIntroduced), candidateClauseSet);          
                	candidateGoalSet.addAll(newQVarsIntroduced);
        		}
        	}
            
        	//////////////////////////////////////////////////////////        	        	        	
        	for(String aCaseRel : cases.keySet())        	
        	{
        		for(List<MTerm> caseargs : cases.get(aCaseRel))
        		{        		
        			String aCase = caseToString(aCaseRel, caseargs);

        			MCommunicator.writeToLog("\n  Handling case: "+aCase+ ". Was: "+aCaseRel+" of "+caseargs);
        			
                	// Fresh todo list for each case.
        			// TODO confirm this is a deep enough copy
        			Map<Set<Integer>, Set<int[]>> freshCandidateClauseSets = new HashMap<Set<Integer>, Set<int[]>>(candidateClauseSets);        	
        			Set<Integer> freshCandidateGoals = new HashSet<Integer>(candidateGoalSet);
        			
            		Set<int[]> caseClauseSet = new HashSet<int[]>();
            		
            		Set<Integer> caseGoals;
            		if(aCaseRel.length() > 0)        
            		{
            			Set<Integer> newQVarsIntroduced = new HashSet<Integer>();
            			caseClauseSet = makeClauseSetFor(theBounds, theTranslation, aCaseRel, caseargs, 
            					startHereWrapper, intermVarToPred, intermVarToArgs, newQVarsIntroduced);      
            			caseGoals = newQVarsIntroduced;
            		}
            		else
            		{
            			caseGoals = null;
            		}
            		            	
            		
            		if(fromContext.forQuery.debug_verbosity > 1)
            			MEnvironment.writeOutLine("DEBUG: POPULATED case "+aCase+" with clause: "+caseClauseSet+
            					". SAT4j Constraint count = "+realSolver.nConstraints());
            	        	            		
            		result.put(aCase, 
            				internalRealized(realSolver, 
            						candidates, caseClauseSet, freshCandidateClauseSets, 
            						freshCandidateGoals, caseGoals,
            						theTranslation, numPrimaryVariables, intermVarToPred, intermVarToArgs));
            		

        		}
        		
        	} // end for each case

		}
		catch(ContradictionException e)
		{
			// Trivially false --> no (more) candidates can be realized at this size
			return result;
		}

        
        return result;
	}

	void handleClause (ISolver solver, int[] aClause, Set<IConstr> toRemove, List<Integer> toAssume)
	throws ContradictionException
	{
		Map<int[], IConstr> tempMap = new HashMap<int[], IConstr>();
		handleClause(solver, aClause, tempMap, toAssume);
		
		// May result in a null
		if(tempMap.size() > 0)
			toRemove.add(tempMap.get(aClause));		
	}
	
	
	void handleClause(ISolver solver, int[] aClause, Map<int[], IConstr> toRemove, List<Integer> toAssume)
	throws ContradictionException
	{
		MCommunicator.writeToLog("\nin handleClause:");
		MCommunicator.writeToLog("\n BEFORE solver.nConstraints() = "+solver.nConstraints()+"\n");
		if(aClause.length > 1)
		{			
			IConstr aConstraint = solver.addClause(new VecInt(aClause));
			
			// May return null for non-unit clauses if the clause is already satisfied
			// If a non-unit clause returns null, can ignore it.
			if(aConstraint != null)
				toRemove.put(aClause, aConstraint);
			
			MCommunicator.writeToLog("\n  Added non-unit clause: "+Arrays.toString(aClause)+" with constr result: "+aConstraint);
			
		}
		else
		{
			MCommunicator.writeToLog("\n  Will assume unit clause: "+Arrays.toString(aClause));
			toAssume.add(aClause[0]);
		}
		MCommunicator.writeToLog("\n AFTER solver.nConstraints() = "+solver.nConstraints()+"\n");

	}
	
	Set<String> internalRealized(ISolver solver, Map<String, Set<List<MTerm>>> candidates,
			Set<int[]> caseClauseSet, Map<Set<Integer>, Set<int[]>> candidateClauseSets,
			Set<Integer> candidateGoals, Set<Integer> caseGoals,
			Translation theTranslation, int numPrimaryVariables, 
			Map<Integer, String> intermVarToPred, Map<Integer, List<MTerm>> intermVarToArgs)
	{
		Set<String> result = new HashSet<String>();
		boolean issat = false;
		
		List<Integer> unitClausesToAssumeCases = new ArrayList<Integer>();	
		
		MCommunicator.writeToLog("\ninternalRealized. Case clauses: "+stringifyArrays(caseClauseSet));
		
		int firstQ = Collections.min(intermVarToArgs.keySet());
		int lastQ = Collections.max(intermVarToArgs.keySet());
		
		// Make sure the solver has space for our new variables.
		solver.newVar(lastQ);		
		
		///////////////////////////////////////////
		// Add the CASE. 
		Set<IConstr> toRemoveCase = new HashSet<IConstr>();
		try
		{
			for(int[] aClause : caseClauseSet)
			{
				handleClause(solver, aClause, toRemoveCase, unitClausesToAssumeCases);		
			}
		}
		catch(ContradictionException e)
		{
			// No more
			return result;
		}
		
		
		///////////////////////////////////////////
		//Set<int[]> clausesToAdd = new HashSet<int[]>();
		//for(Set<int[]> aSet : candidateClauseSets.values())
	//	{
	//		clausesToAdd.addAll(aSet);
	//	}	
			
		// as of 2.3.0, default sat4j was
		// return newMiniLearningHeapRsatExpSimpBiere();	
		
		// We can remove non-unit clauses.
		Map<int[], IConstr> toRemoveCandidate = new HashMap<int[], IConstr>();
		List<Integer> unitClausesToAssumeCandidates = new ArrayList<Integer>();
		
		
		// this will propagate, e.g. if we have 1 [F] 2 [F] 3 [?] will make 3 [T]
		// don't call it! just noting that the function is separate + accessible 
		// theSolver.propagate();									
		
		try
		{
			for(Set<int[]> aSet : candidateClauseSets.values())
			{		
				for(int[] aClause : aSet)
				{
					handleClause(solver, aClause, toRemoveCandidate, unitClausesToAssumeCandidates);
				}
			}
		}
		catch(ContradictionException e)
		{
			// No more
			for(IConstr rem : toRemoveCandidate.values())
				solver.removeConstr(rem);
			for(IConstr rem : toRemoveCase)
				solver.removeConstr(rem);	
			return result;
		}	
		
				
		MCommunicator.writeToLog("\n  firstQ: "+firstQ+"; lastQ: "+lastQ+"; num of vars is now: "+solver.nVars()+"; class was "+solver.getClass().toString());
		
		// Loop while there are still goals
		do
		{				
			MCommunicator.writeToLog("\n  internalPopulated core loop. these clauses remain: "+candidateClauseSets);
			MCommunicator.writeToLog("\n  before calling sat-solver, result was: "+result);
					
				
			//MEnvironment.errorStream.println("~~~~ Calling SAT Solver ");
			final long startSolve = System.currentTimeMillis();
			try
			{			
				int[] unitClausesToAssumeArr = new int[unitClausesToAssumeCandidates.size() + unitClausesToAssumeCases.size()];
				int ii = 0;
				for(int lit : unitClausesToAssumeCandidates)
				{
					unitClausesToAssumeArr[ii] = lit;
					ii++;
				}	
				for(int lit : unitClausesToAssumeCases)
				{
					unitClausesToAssumeArr[ii] = lit;
					ii++;
				}					
				
												
				//assumps - a set of literals (represented by usual non null integers in Dimacs format). 								
				
				MCommunicator.writeToLog("Calling sat-solver with assumptions: "+Arrays.toString(unitClausesToAssumeArr));
				
				debugPrintClauses(solver);
				//solver.printInfos(new PrintWriter(System.err), ">>>");
				
				issat = solver.isSatisfiable(new VecInt(unitClausesToAssumeArr));
				MCommunicator.writeToLog("\nResult was: "+issat);
			}
			catch(TimeoutException e)
			{
				// for now 
				throw new MUserException(e.toString());
			}
			
			final long endSolve = System.currentTimeMillis();
        
			msKodkodSolveTime += (endSolve - startSolve);
        
			
			//if(fromContext.forQuery.debug_verbosity > 1)
			//	MEnvironment.writeOutLine("DEBUG: Satisfiable. Adding to result.");
			
			// return not just those that are true, but all for nonempty relations
			if(issat)
				addRealizedToListAndTrimGoals(solver, theTranslation, candidates, 
						firstQ, lastQ, 
						intermVarToPred, intermVarToArgs, result, candidateClauseSets, candidateGoals,
						unitClausesToAssumeCandidates, toRemoveCandidate);
									
		} while(issat && candidateGoals.size() > 0);
        
		// Fall through should mean that we found everything.
		
		// preserve state for next case
		// required: addRealizedToListAndTrimGoals removed from toRemoveCandidate map
		for(IConstr rem : toRemoveCandidate.values())
			solver.removeConstr(rem);
		
		// remove current case
		for(IConstr rem : toRemoveCase)
			solver.removeConstr(rem);
		
        return result;
	}
	
	
	private void debugPrintClauses(ISolver solver)
	{
		// DO NOT TRUST vocab.valueToString. uses "head" from the clause, not same as var #.
		// And getIthConstr is returning ArrayIndexOutOfBoundsException even if we use nConstraints as a limiter...
		
		// debug
		org.sat4j.minisat.core.Solver theSolver = (org.sat4j.minisat.core.Solver) solver;

		MCommunicator.writeToLog("\n  There are "+theSolver.nConstraints()+" clauses. They are:");
		
		try
		{
			for(int ii=0;ii<theSolver.nConstraints();ii++)
				MCommunicator.writeToLog("\n clause "+ii+": "+theSolver.getIthConstr(ii));
		}
		catch(ArrayIndexOutOfBoundsException e)
		{
			// Why in the world is this being thrown? It's off by a lot more than 1.
			MCommunicator.writeToLog("\n clause ... (stopped here; array index went out of bounds.)");			
		}
		

	}


	protected void addRealizedToListAndTrimGoals(ISolver theSolver, Translation trans, 
			Map<String, Set<List<MTerm>>> candidates, int firstQ, int lastQ, 
			Map<Integer, String> intermVarToPred, Map<Integer, List<MTerm>> intermVarToArgs, 
			Set<String> result, 
			Map<Set<Integer>, Set<int[]>> candidateClauseSets, 
			Set<Integer> candidateGoals,
			List<Integer> unitClausesToAssumeCandidates,
			Map<int[], IConstr> toRemoveCandidates)
	{
		// This method needs to do 2 things:
		// (1) Add caseToString(String predname, List<MTerm> args) to result
		// (2) Remove clauses
		
		// For (1) need to go backwards from var to its relation.
		for(int iVar = firstQ;iVar <= lastQ;iVar++)
		{						
			//if(theSolver.valueOf(iVar)) // true
			if(theSolver.model(iVar))
			{
			//	MCommunicator.writeToLog("aptlatg. var true in the model: "+iVar);
				
				String predname = intermVarToPred.get(iVar);
				List<MTerm> args = intermVarToArgs.get(iVar);
				String fmla = caseToString(predname, args);
				
				if(candidates.containsKey(predname) && 
				   candidates.get(predname).contains(args) && 
				   !result.contains(fmla))
				{
					MEnvironment.writeToLog("\n--- New Realized found: "+fmla+"\n");
					result.add(fmla);
				
					// We can remove all clause sets and goals for this fmla
					// Each fmla has a unique set of q's used as interm. vars
					// And this map takes the set of q's to the clauses for it.
					for(Map.Entry<Set<Integer>, Set<int[]>> entry : candidateClauseSets.entrySet())
					{
						Set<Integer> qVars = entry.getKey();						
						if(qVars.contains(iVar)) // is this for q?
						{
							Set<int[]> clausesForQ = entry.getValue();
							
							MEnvironment.writeToLog("\n--- Removing clauses: "+clausesForQ+"\n");
							MEnvironment.writeToLog("\n--- Removing goals: "+qVars+"\n");
							
							// remove clause sets (vals)
							for(int[] aClause : clausesForQ)
							{
								if(toRemoveCandidates.containsKey(aClause))
								{
									theSolver.removeConstr(toRemoveCandidates.get(aClause));
									toRemoveCandidates.remove(aClause); // no longer needs removal at end
								}
								else
									unitClausesToAssumeCandidates.remove(aClause);
							}
							
							// remove goals (keys)
							candidateGoals.removeAll(qVars);
						}
					}									
					
				} // end if new result
				
			} // end if model sets Q to true
		} // end for each Q variable		
		
	}
	
	
	/////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	
	static void tests_createvp1()
	{
		List<String> creationCommands = new ArrayList<String>();
		// U >= {A, B, C}
		// Now lower bound, so a, b, c all pairwise disjoint
		creationCommands.add("<MARGRAVE-COMMAND type=\"ADD\"><VOCAB-IDENTIFIER vname=\"SRTest1\" /><SORT-WITH-CHILDREN name=\"U\"><SORT name=\"A\" /><SORT name=\"B\" /><SORT name=\"C\" /></SORT-WITH-CHILDREN></MARGRAVE-COMMAND> ");
		// R: A 	
		creationCommands.add("<MARGRAVE-COMMAND type=\"ADD\"><VOCAB-IDENTIFIER vname=\"SRTest1\" /><PREDICATE name=\"r\" /><RELATIONS><RELATION name=\"A\"/></RELATIONS></MARGRAVE-COMMAND> ");
		// P: A
		creationCommands.add("<MARGRAVE-COMMAND type=\"ADD\"><VOCAB-IDENTIFIER vname=\"SRTest1\" /><PREDICATE name=\"p\" /><RELATIONS><RELATION name=\"A\"/></RELATIONS></MARGRAVE-COMMAND> ");
		// c: -> C
		creationCommands.add("<MARGRAVE-COMMAND type=\"ADD\"><VOCAB-IDENTIFIER vname=\"SRTest1\" /><CONSTANT name=\"c\" type=\"C\" /></MARGRAVE-COMMAND>");
		// f: C -> A
		creationCommands.add("<MARGRAVE-COMMAND type=\"ADD\"><VOCAB-IDENTIFIER vname=\"SRTest1\" /><FUNCTION name=\"f\"><RELATIONS><RELATION name=\"C\" /><RELATION name=\"A\" /></RELATIONS></FUNCTION></MARGRAVE-COMMAND> ");
		
		creationCommands.add("<MARGRAVE-COMMAND type=\"CREATE POLICY LEAF\"><POLICY-IDENTIFIER pname=\"SRP1\" /><VOCAB-IDENTIFIER vname=\"SRTest1\" /></MARGRAVE-COMMAND> ");
		creationCommands.add("<MARGRAVE-COMMAND type=\"ADD\"><POLICY-IDENTIFIER pname=\"P\" /><VARIABLE-DECLARATION sort=\"A\" varname=\"x\" /></MARGRAVE-COMMAND>");
		creationCommands.add("<MARGRAVE-COMMAND type=\"ADD\"><POLICY-IDENTIFIER pname=\"P\" /><VARIABLE-DECLARATION sort=\"A\" varname=\"y\" /></MARGRAVE-COMMAND>");
		
		// r(x)
		// any y
		
		// TODO: error if bad arity in rule
		
		// TODO: error if re-defining relation in same vocab
		
		// TODO: allow x, y in var list if y not used after substitution
		
		creationCommands.add("<MARGRAVE-COMMAND type=\"ADD\"><POLICY-IDENTIFIER pname=\"SRP1\" /><RULE name=\"Rule1\"><DECISION-TYPE type=\"permit\"><ID id=\"x\" /><ID id=\"y\" /></DECISION-TYPE>" +
				"<TARGET><AND><ATOMIC-FORMULA><RELATION-NAME><ID id=\"r\" /></RELATION-NAME><TERMS><VARIABLE-TERM id=\"x\" /></TERMS></ATOMIC-FORMULA>" +
				"<ATOMIC-FORMULA><RELATION-NAME><ID id=\"r\" /></RELATION-NAME><TERMS><VARIABLE-TERM id=\"y\" /></TERMS></ATOMIC-FORMULA></AND>" +				
				"</TARGET></RULE></MARGRAVE-COMMAND>");
		
		creationCommands.add("<MARGRAVE-COMMAND type=\"SET RCOMBINE FOR POLICY\"><POLICY-IDENTIFIER pname=\"SRP1\" /><COMB-LIST>" +
				"<FA><ID id=\"permit\" /><ID id=\"deny\" /></FA>" +
				"<OVERRIDES decision=\"permit\"><ID id=\"callpolice\" /></OVERRIDES>" +
				"<OVERRIDES decision=\"deny\"><ID id=\"callpolice\" /></OVERRIDES></COMB-LIST></MARGRAVE-COMMAND>"); 
		
		creationCommands.add("<MARGRAVE-COMMAND type=\"PREPARE\"><POLICY-IDENTIFIER pname=\"SRP1\" /></MARGRAVE-COMMAND>"); 
		 
		
		//creationCommands.add("");
		
		for(String cmd : creationCommands)
		{
			MCommunicator.handleXMLCommand(cmd);
		}
	}
	
	static void tests_createqry1()
	{
		// P.permit(x, y)
		String aQuery = 
			"<MARGRAVE-COMMAND type=\"EXPLORE\"><EXPLORE id=\"SRQry1\"><CONDITION>" +
			"<ATOMIC-FORMULA><RELATION-NAME><ID id=\"SRP1\"/><ID id=\"permit\"/></RELATION-NAME><TERMS><VARIABLE-TERM id=\"x\" /><VARIABLE-TERM id=\"y\" /></TERMS></ATOMIC-FORMULA>" +
			"</CONDITION>" +
			"<PUBLISH><VARIABLE-DECLARATION sort=\"A\" varname=\"y\" /><VARIABLE-DECLARATION sort=\"A\" varname=\"x\" /></PUBLISH></EXPLORE></MARGRAVE-COMMAND> ";
		MCommunicator.handleXMLCommand(aQuery);
	}
	
	public static void unitTests()
	{
		MEnvironment.writeErrLine("----- Begin MRealizedFormulaFinder Tests (No messages is good.) -----");
		
		tests_createvp1();
		
		System.err.println(MCommunicator.transformXMLToString(MEnvironment.printInfo("SRP1")));
		
		tests_createqry1();
		
		
		// !!!!!!!!!!!!
		// TODO need to force inclusion of IDBs!
		
		// !!!!!!!!!!!!
		// TODO terms (similar to forcing inclusion of idbs? )

		// !!!!!!!!!!!!
		// TODO confirm deep enough copy (above)

		
		// !!!!!!!!!!!!
		// TODO clause set. versus goals

		// !!!!!!!!!!!!
		// TODO test CASES

		
		
		// simple no cases
		Document result;
		Map<String, Set<List<MTerm>>> mapAx = new HashMap<String, Set<List<MTerm>>>();
		Map<String, Set<List<MTerm>>> mapBx = new HashMap<String, Set<List<MTerm>>>();
		Map<String, Set<List<MTerm>>> mapPx = new HashMap<String, Set<List<MTerm>>>();
		/*map1.put("Permit", new HashSet<List<MTerm>>());
		List<MTerm> tlist1 = new ArrayList<MTerm>();
		tlist1.add(new MVariableTerm("x"));
		tlist1.add(new MVariableTerm("y"));
		map1.get("Permit").add(tlist1);*/
		mapAx.put("A", new HashSet<List<MTerm>>());
		List<MTerm> tlist_x = new ArrayList<MTerm>();
		tlist_x.add(new MVariableTerm("x"));
		mapAx.get("A").add(tlist_x);
		
				
		result = MEnvironment.showRealized("SRQry1", mapAx);								
		System.err.println(MCommunicator.transformXMLToString(result));
				
		mapBx.put("B", new HashSet<List<MTerm>>());
		mapBx.get("B").add(tlist_x);
		
		result = MEnvironment.showRealized("SRQry1", mapAx, mapBx);								
		System.err.println(MCommunicator.transformXMLToString(result));
		
		mapPx.put("p", new HashSet<List<MTerm>>());
		mapPx.get("p").add(tlist_x);
		
		result = MEnvironment.showRealized("SRQry1", mapPx, mapAx);								
		System.err.println(MCommunicator.transformXMLToString(result));
		
		MEnvironment.writeErrLine("----- End MRealizedFormulaFinder Tests -----");	
	}

	
}

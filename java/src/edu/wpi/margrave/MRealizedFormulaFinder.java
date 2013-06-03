package edu.wpi.margrave;

//import java.io.PrintWriter;
import java.util.*;

//import kodkod.ast.Expression;
//import kodkod.ast.Formula;
import kodkod.ast.Relation;
//import kodkod.engine.Solver;
import kodkod.engine.fol2sat.Translation;
//import kodkod.engine.fol2sat.Translator;
//import kodkod.engine.fol2sat.TrivialFormulaException;
import kodkod.instance.Bounds;
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
//import kodkod.instance.TupleSet;
//import kodkod.instance.Universe;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;
import org.sat4j.core.VecInt;
import org.sat4j.specs.*;
//import org.w3c.dom.Document;



public class MRealizedFormulaFinder extends MCNFSpyQueryResult
{			
				
	MRealizedFormulaFinder(MPreparedQueryContext qr)
			throws MUserException
	{
		super(qr);			
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
	
	private Set<String> findMissingRelations(
			Map<String, Set<List<MTerm>>> idbsNeeded)
	{ 
		Set<String> missingRels = new HashSet<String>();
				
		for(String key : idbsNeeded.keySet())
		{					
			// If this is an EDB, pass on
			if(fromContext.forQuery.vocab.predicates.containsKey(key) ||
			   fromContext.forQuery.vocab.isSort(key) ||
			   fromContext.forQuery.vocab.constants.containsKey(key) ||
			   fromContext.forQuery.vocab.functions.containsKey(key))
				continue;
			
			// If this isn't an IDB in the query, error
			// Note that containsIDB is not the correct method to call, here. 
			//if(!fromContext.forQuery.containsIDB(key)) <--- not this.
			if(!fromContext.forQuery.myIDBCollectionsContainWithDot(key))
				throw new MUserException("Could not show realized formula involving relation "+key+" since the query had no knowledge of that relation.\n"+
						"The query knew about the following IDB relations: "+fromContext.forQuery.getmyIDBCollectionsIDBs()+
						"\nand the following EDB relations+sorts+constants:\n"+fromContext.forQuery.vocab.predicates.keySet()+","+
						fromContext.forQuery.vocab.sorts.keySet()+","+fromContext.forQuery.vocab.constants.keySet()+
						"\nComplex terms (i.e., terms using functions) and equality between variables are not supported.");
			
			// Validate arities in which we are using the relation.
			int arity = fromContext.forQuery.myIDBCollectionsHaveArityForWithDot(key);
			
			for(List<MTerm> args : idbsNeeded.get(key))
			{
				if(arity != args.size())
					throw new MUserException("Could not show realized formula "+key+args+" since the that relation has different arity="+arity);							
				
			}
								
			// Force axiomatization of this relation. Since the check of myIDBCollectionsContainWithDot passed above,
			// this IDB _does_ exist and the query can axiomatize it. But only add if the query doesn't know it needs
			// to axiomatize! Otherwise we will loop forever.
			if(!fromContext.forQuery.idbsToAddInFormula.contains(key))
				missingRels.add(key);
		}
		
		return missingRels;
	}
	

	public Set<String> getRealizedFormulas(Map<String, Set<List<MTerm>>> candidates) throws MUserException
	{
		Map<String, Set<List<MTerm>>> cases = new HashMap<String, Set<List<MTerm>>>();
		return getRealizedFormulas(candidates, cases).get("");
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
	
	public Tuple getTupleForPropVariable(Bounds theBounds, Translation theTranslation, IntSet s, Relation r, int theVar)
	throws MInternalNoBoundsException
	{		        
		// The relation's upper bound has a list of tuple indices. The "index" variable below is an index
		// into that list of indices. (If our upper bound was _everything_, would be no need to de-reference.)
		
        int minVarForR = s.min();    	
		
		// Compute index: (variable - minvariable) of this relation's variables 
        int index = theVar - minVarForR;                            
                                
        // OPT: How slow is this? May want to cache...
        int[] arr = theBounds.upperBound(r).indexView().toArray();
        
        TupleFactory factory = theBounds.universe().factory();   
        Tuple tup = factory.tuple(r.arity(), arr[index]);  

        //MCommunicator.writeToLog("\ngetTupleForPropVariable: thisTuple="+tup+" for "+theVar+". Relation had vars: "+s+" and the offset was "+minVarForR+
        //		"leaving index="+index+". Upper bound indexview: "+Arrays.toString(arr)+
        //		"\nThe de-referenced tuple index is: "+arr[index]);

        
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
		//MVariableTerm vti = (MVariableTerm) termi;
		String varname = termi.toString();
		
		// Much of this code is taken from interpret() in Translation.class in Kodkod		
		// What is the prop variable for the fact varname(theAtom)? 
		
		// First, the skolem relation.
		// This will NOT be available via MFormulaManager, since Kodkod adds it internally. 
		Relation skRel = getRelationFromBounds(theBounds, "$"+varname);
		IntSet propVars = theTranslation.primaryVariables(skRel);
		int theVar = propVars.min();
		
		//MCommunicator.writeToLog("\ngetPropVariableForVariable: skRel="+skRel);	
		
		TupleFactory f = theBounds.universe().factory();
			
		// Inefficient, but should work for now: loop for each tuple in the upper bound of this rel.
		// OPT: don't loop every time
		for(IntIterator iter = theBounds.upperBound(skRel).indexView().iterator(); iter.hasNext();  )
		{
			int tupleIndex = iter.next();
			Tuple theTuple = f.tuple(skRel.arity(), tupleIndex);
		
			Object val = theTuple.atom(0);
			
			//MCommunicator.writeToLog("\ngetPropVariableForVariable: val="+val+"; theAtom="+theAtom);	
			
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
	throws MInternalNoBoundsException
	{
		Set<int[]> clauseSet = new HashSet<int[]>();

		MCommunicator.writeToLog("\n  makeClauseSetFor: "+predname+" "+args);
		MCommunicator.writeToLog("\n  PRE startherewrapper="+startHereWrapper+" newQVarsIntroduced="+newQVarsIntroduced);
		
		// Produce a set of clauses that says R(t_1, ..., t_n) holds. 
		// Must cover each potential binding for the interior terms, though!
		
		// Many valid p_i's means potentially intractable number of clauses.
		        
		// Can't trust MFormulaManager here, since Kodkod adds relations itself:
        Relation r = getRelationFromBounds(theBounds, predname);        
        if(fromContext.forQuery.debug_verbosity > 2)
        {
        	MCommunicator.writeToLog("\nRelation="+r+". Bounds covered relations: "+theBounds.relations()); // theBounds.toString() doesn't always work here.
        }
        
    	
		IntSet s = theTranslation.primaryVariables(r);
        		
        if(s == null)
        {
        	// Will have null if no tuples allocated to the relation. (If the relation's upper bound is empty.)
        	// If the tuple is empty, this candidate or case is impossible.
        	throw new MInternalNoBoundsException();
        }
        
		MCommunicator.writeToLog("\n\nRelation "+r+" has "+s.size()+" primary variables.");
		if(fromContext.forQuery.debug_verbosity > 2)
			MCommunicator.writeToLog("\n--> "+s);
        
    	// Start with which integer as q_1?
        int q = startHereWrapper.get(0);
        
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
        	Tuple thisTuple = getTupleForPropVariable(theBounds, theTranslation, s, r, aVar);        	                       	        
        	
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
         
		MCommunicator.writeToLog("\n  POST startherewrapper="+startHereWrapper+" newQVarsIntroduced="+newQVarsIntroduced);

        
        if(fromContext.forQuery.debug_verbosity > 2)
        {
        	MCommunicator.writeToLog("\n\nmakeClauseSetFor returning "+clauseSet.size()+" clauses: "+stringifyArrays(clauseSet));
        }
        return Collections.unmodifiableSet(clauseSet);        
	}

	String stringifyArrays(Set<int[]> arrs)
	{
		StringBuffer result = new StringBuffer("< ");
		for(int[] arr : arrs)
			result.append(" " + Arrays.toString(arr));
			
		return result.toString() + ">";
	}
	
	public Map<String, Set<String>> getUnrealizedFormulas(Map<String, Set<List<MTerm>>> candidates, 
			Map<String, Set<List<MTerm>>> cases)
	{			
		Map<String, Set<String>> invresult = new HashMap<String, Set<String>>();
		Map<String, Set<String>> result = new HashMap<String, Set<String>>();
		
		invresult = getRealizedFormulas(candidates, cases);
		
		MCommunicator.writeToLog("Entered getUnrealizedFormulas with args:\n"+candidates+"\n"+cases);
		MCommunicator.writeToLog("Realized returned: "+invresult);
		
		// The ones that don't appear in invresult need to appear in result.
		
		if(cases.keySet().isEmpty())
		{
			handleUnrealizedCase("", candidates, invresult, result);
		}
		
		for(String caseRel : cases.keySet())
		{
			for(List<MTerm> caseArgs : cases.get(caseRel))
			{
				String caseString = caseToString(caseRel, caseArgs);
				handleUnrealizedCase(caseString, candidates, invresult, result);														
			}			
		}
		
		return result;
	}
	
	
	private void handleUnrealizedCase(String caseString, Map<String, Set<List<MTerm>>> candidates,
			Map<String, Set<String>> invresult, Map<String, Set<String>> result) 
	{
		result.put(caseString, new HashSet<String>());
		
		MCommunicator.writeToLog("Handling unrealized case:"+caseString);
		
		for(String candRel : candidates.keySet())
		{
			for(List<MTerm> candArgs : candidates.get(candRel))
			{
				String candString = caseToString(candRel, candArgs);
				if(!invresult.containsKey(caseString) || !invresult.get(caseString).contains(candString))
				{
					result.get(caseString).add(candString);
					MCommunicator.writeToLog("Added: "+candString);
				}
				
			}		
		}
		
	}

	public Map<String, Set<String>> getRealizedFormulas(Map<String, Set<List<MTerm>>> candidates, 
			Map<String, Set<List<MTerm>>> cases)
	{			
		Map<String, Set<String>> result = new HashMap<String, Set<String>>();
		
		/////////////////////////////////////////////////////////////
		// First re-write equalities that can be written. E.g.
		// $c=x becomes $c(x).
		rewriteEqualities(candidates);
		rewriteEqualities(cases);
		
		/////////////////////////////////////////////////////////////
		// Are any relations mentioned in candidates/cases missing from the bounds?
		// (This occurs for IDB relations.)
		Set<String> missingRels = findMissingRelations(candidates); 
		missingRels.addAll(findMissingRelations(cases));

		if(missingRels.size() > 0)
		{
			MCommunicator.writeToLog("\nSR: adding missing IDBs and re-running the query before finding realized fmlas...");
			MCommunicator.writeToLog("\nMissing: "+missingRels);
			MQuery newQuery = new MQuery(fromContext.forQuery);			
			
			newQuery.addIDBsToForceAxiomatization(missingRels);			
			MCommunicator.writeToLog("\nNew query will axiomatize: "+newQuery.idbsToAddInFormula);

			// If the axiomatization fails, this will loop forever...
			MRealizedFormulaFinder newFinder = newQuery.runQuery().getRealizedFormulaFinder(); 						

			return newFinder.getRealizedFormulas(candidates, cases);
		}		
		// otherwise, continue as normal: we have all the relations we need										
		/////////////////////////////////////////////////////////////
		
				
		// For each case,
		// which candidates can be populated?
		// If cases is empty, add a single trivial case:
		if(cases.size() < 1)
		{
			cases.put("", new HashSet<List<MTerm>>());
			cases.get("").add(new ArrayList<MTerm>());
		}
		
		MCommunicator.writeToLog("\n\n-------------------------------------------------\n");
		MCommunicator.writeToLog("\n-------------------------------------------------\n");				
		MCommunicator.writeToLog("\nIn getRealizedFormulas. Candidates="+candidates+"\nCases:"+cases);
		MCommunicator.writeToLog("\ntrivialFalse="+trivialFalse+"; trivialTrue="+trivialTrue+"; nonTrivialTranslation="+nonTrivialTranslation);
		MCommunicator.writeToLog("\n-------------------------------------------------\n");
		MCommunicator.writeToLog("\n-------------------------------------------------\n\n");
		
		/////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////
		// Is there any solving to do at all? Maybe not.
		// If the query is trivially false, impossible. If trivially true, 
		// may still have a contradiction between cases and candidates!
		
		if(nonTrivialTranslation == null || nonTrivialBounds == null)
		{			
			/*if(trivialTrue.contains(atSize))
			{
				// Trivially true --> all candidates can be realized at this size		
				for(String c : cases)
					result.put(c, new HashSet<String>(candidates));
			}
			else*/
			// TODO above not safe [tn: why??? answer: because of cases. candidates by themselves would be fine, but may have contradiction
			// between case and candidate]
			if(trivialFalse)
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
		Translation theTranslation = nonTrivialTranslation; 
		CNFSpy theSolver = (CNFSpy) theTranslation.cnf();
		Bounds theBounds = nonTrivialBounds;
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
        			
        			try
        			{
        				Set<int[]> candidateClauseSet = makeClauseSetFor(theBounds, theTranslation, aCandidateRel, candidateargs, 
        						startHereWrapper, intermVarToPred, intermVarToArgs, newQVarsIntroduced);
        				candidateClauseSets.put(Collections.unmodifiableSet(newQVarsIntroduced), candidateClauseSet);  
        				
        				candidateGoalSet.addAll(newQVarsIntroduced);
        			}
        			catch(MInternalNoBoundsException e)
        			{
        				// aCandidateRel has no tuples allocated to it (empty upper bound). So this
        				// To reflect this, the candidate's clause set must contain the empty clause.
        				MCommunicator.writeToLog("\nMInternalNoBoundsException caught. Not registering a clause set for this candidate.");  
        			}
        		}
        	}
            
        	//////////////////////////////////////////////////////////        	        	        	
        	for(String aCaseRel : cases.keySet())        	
        	{
        		for(List<MTerm> caseargs : cases.get(aCaseRel))
        		{        		
        			String aCase = caseToString(aCaseRel, caseargs);

        			MCommunicator.writeToLog("\n-------------------------------------------------\n");
        			MCommunicator.writeToLog("\n\n\n  Handling aCase="+aCase+ ". aCaseRel="+aCaseRel+" caseargs="+caseargs);
        			MCommunicator.writeToLog("\n-------------------------------------------------\n");
        			
                	// Fresh todo list for each case.
        			// Deep enough copy
        			Map<Set<Integer>, Set<int[]>> freshCandidateClauseSets = new HashMap<Set<Integer>, Set<int[]>>();
        			for(Map.Entry<Set<Integer>, Set<int[]>> e : candidateClauseSets.entrySet())
        				freshCandidateClauseSets.put(e.getKey(), new HashSet<int[]>(e.getValue()));
        			
        			// This is deep enough since Integers are immutable
        			Set<Integer> freshCandidateGoals = new HashSet<Integer>(candidateGoalSet);
        			
            		Set<int[]> caseClauseSet = new HashSet<int[]>();            		
            		Set<Integer> caseGoals;
            		
            		// We use "" as the null case. This occurs when the user asks for realized formulas
            		// _without_ listing any cases.
            		try
            		{
            			if(aCaseRel.length() > 0)        
            			{
            				Set<Integer> newQVarsIntroduced = new HashSet<Integer>();
            				caseClauseSet = makeClauseSetFor(theBounds, theTranslation, aCaseRel, caseargs, 
            						startHereWrapper, intermVarToPred, intermVarToArgs, newQVarsIntroduced);      
            				caseGoals = newQVarsIntroduced;
            			}
            			else
            			{
            			//  	So only one case to check, with no clauses.
            				MCommunicator.writeToLog("\nThere were no cases given, so preparing the empty case.");
            				caseGoals = null;
            			}
            			
            			if(fromContext.forQuery.debug_verbosity > 2)
            			{
            				MCommunicator.writeToLog("\n-------------------------------------------------\n");            		            	
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
            		catch(MInternalNoBoundsException e)
            		{
            			MCommunicator.writeToLog("\nMInternalNoBoundsException caught. Ignoring this case.");      
            		}
        		} // end for each tuple
        		
        	} // end for each case rel

		}
		catch(ContradictionException e)
		{
			// Trivially false --> no (more) candidates can be realized at this size
			return result;
		}

        
        return result;
	}

	private void rewriteEqualities(Map<String, Set<List<MTerm>>> things) {
		if(!things.containsKey("="))
			return;		
				
		for(List<MTerm> aVector : things.get("="))
		{
			 assert(aVector.size() == 2);
			 MTerm a = aVector.get(0);
			 MTerm b = aVector.get(1);
			 if(a instanceof MFunctionTerm || a instanceof MFunctionTerm)
				 throw new MUserException("Unsupported show-realized formula; cannot use complex terms: "+a.toString()+"="+b.toString());
			 
			 if(b instanceof MVariableTerm)
			 {
				 if(!things.containsKey(a.toString()))
					 things.put(a.toString(), new HashSet<List<MTerm>>());
				 
				 List<MTerm> newList = new ArrayList<MTerm>(1);
				 newList.add(b);
				 things.get(a.toString()).add(newList);
				 MCommunicator.writeToLog("Converting to "+a.toString()+" -- "+newList);
			 }
			 else if(a instanceof MVariableTerm)
			 {
				 if(!things.containsKey(b.toString()))
					 things.put(b.toString(), new HashSet<List<MTerm>>());
				 
				 List<MTerm> newList = new ArrayList<MTerm>(1);
				 newList.add(a);
				 things.get(b.toString()).add(newList);
				 MCommunicator.writeToLog("Converting to "+a.toString()+" -- "+newList);
			 }			 
		}
		
		things.remove("=");
	}

	void handleClause (ISolver solver, int[] aClause, Set<IConstr> toRemove, Set<Integer> toAssume)
	throws ContradictionException
	{
		Map<int[], IConstr> tempMap = new HashMap<int[], IConstr>();
		handleClause(solver, aClause, tempMap, toAssume);
		
		// May result in a null
		if(tempMap.size() > 0)
			toRemove.add(tempMap.get(aClause));		
	}
	
	
	void handleClause(ISolver solver, int[] aClause, Map<int[], IConstr> toRemove, Set<Integer> toAssume)
	throws ContradictionException
	{
		if(fromContext.forQuery.debug_verbosity > 2)
		{
			MCommunicator.writeToLog("\nin handleClause:");
			MCommunicator.writeToLog("\n BEFORE solver.nConstraints() = "+solver.nConstraints()+"\n");
		}
		
		if(aClause.length > 1)
		{			
			IConstr aConstraint = solver.addClause(new VecInt(aClause));
			
			// May return null for non-unit clauses if the clause is already satisfied
			// If a non-unit clause returns null, can ignore it.
			if(aConstraint != null)
				toRemove.put(aClause, aConstraint);
			
			if(fromContext.forQuery.debug_verbosity > 2)
			{
				MCommunicator.writeToLog("\n  Added non-unit clause: "+Arrays.toString(aClause)+" with constr result: "+aConstraint);
			}
			
		}
		else
		{
			if(fromContext.forQuery.debug_verbosity > 2)
			{
				MCommunicator.writeToLog("\n  Will assume unit clause: "+aClause[0]);
			}
			
			toAssume.add(aClause[0]);
		}
		if(fromContext.forQuery.debug_verbosity > 2)
		{
			MCommunicator.writeToLog("\n AFTER solver.nConstraints() = "+solver.nConstraints()+"\n");
		}

	}
	
	Set<String> internalRealized(ISolver solver, Map<String, Set<List<MTerm>>> candidates,
			Set<int[]> caseClauseSet, Map<Set<Integer>, Set<int[]>> candidateClauseSets,
			Set<Integer> candidateGoals, Set<Integer> caseGoals,
			Translation theTranslation, int numPrimaryVariables, 
			Map<Integer, String> intermVarToPred, Map<Integer, List<MTerm>> intermVarToArgs)
	{
		Set<String> result = new HashSet<String>();
		
		Set<Integer> unitClausesToAssumeCases = new HashSet<Integer>();	
		
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
			// Case clause sets
			for(int[] aClause : caseClauseSet)
			{
				handleClause(solver, aClause, toRemoveCase, unitClausesToAssumeCases);		
			}
			
			// case goals (if any case)
			if(caseGoals != null)
			{
				int[] caseGoalClause = new int[caseGoals.size()];
				int ii =0;
				for(int lit : caseGoals)
				{
					caseGoalClause[ii] = lit;;
					ii++;
				}
				handleClause(solver, caseGoalClause, toRemoveCase, unitClausesToAssumeCases);
			}

		}
		catch(ContradictionException e)
		{
			// No more
			return result;
		}
		
					
		// as of 2.3.0, default sat4j was
		// return newMiniLearningHeapRsatExpSimpBiere();	
		
		// We can remove non-unit clauses.
		Map<int[], IConstr> toRemoveCandidate = new HashMap<int[], IConstr>();
		Set<Integer> unitClausesToAssumeCandidates = new HashSet<Integer>();
		
		
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
		
		boolean issat = false;
		
		// Loop while there are still goals
		do
		{	
			if(fromContext.forQuery.debug_verbosity > 2)
			{
				MCommunicator.writeToLog("\n  internalPopulated core loop. these clauses remain: ");
				for(Set<Integer> key : candidateClauseSets.keySet())
				{
					MCommunicator.writeToLog("\n\n      "+key+" -> ");
					for(int[] val : candidateClauseSets.get(key))
					{
						MCommunicator.writeToLog(Arrays.toString(val));
					}
				}
				MCommunicator.writeToLog("\n  before calling sat-solver, result was: "+result);
			}
					
				
			List<Integer> potentialGoalUnit = new ArrayList<Integer>();
						
			
			//MEnvironment.errorStream.println("~~~~ Calling SAT Solver ");
			final long startSolve = System.currentTimeMillis();
			IConstr remGoals = null;
			try
			{			
				//////////////////////////////////
				// New Goals clause each iteration
				int[] goalClause = new int[candidateGoals.size()];
				int ii = 0;
				for(int lit : candidateGoals)
				{
					goalClause[ii] = lit;
					ii++;
				}
				
				try
				{
					// If the goal is size=1, don't add to the const set					
					if(candidateGoals.size() == 1)
					{
						for(int lit : candidateGoals)
							potentialGoalUnit.add(lit);
					}
					else
					{
						remGoals = solver.addClause(new VecInt(goalClause));
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
								
				
				//////////////////////////////////
				// Re-construct unit clause set each iteration
				int[] unitClausesToAssumeArr = new int[unitClausesToAssumeCandidates.size() + 
				                                       unitClausesToAssumeCases.size() +
				                                       potentialGoalUnit.size()];
				ii = 0;
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
				if(potentialGoalUnit.size() > 0)
					unitClausesToAssumeArr[ii] = potentialGoalUnit.get(0);
				
											
				//assumps - a set of literals (represented by usual non null integers in Dimacs format). 								
				
				if(fromContext.forQuery.debug_verbosity > 2)
				{
					MCommunicator.writeToLog("Calling sat-solver with assumptions: "+Arrays.toString(unitClausesToAssumeArr));
				}
				
				// This is extremely verbose...
				if(fromContext.forQuery.debug_verbosity > 2)
					debugPrintClauses(solver);
								
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
        
			// Satisfiable? Then we have goals to remove and results to add.
			if(issat)
				addRealizedToListAndTrimGoals(solver, theTranslation, candidates, 
						firstQ, lastQ, 
						intermVarToPred, intermVarToArgs, result, candidateClauseSets, candidateGoals,
						unitClausesToAssumeCandidates, toRemoveCandidate);
			
			// Remove the "used" goals disjunction in preparation for the new one.
			if(remGoals != null)
				solver.removeConstr(remGoals);
									
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
		//org.sat4j.minisat.core.Solver<?> theSolver = (org.sat4j.minisat.core.Solver<?>) solver;
		org.sat4j.minisat.core.Solver theSolver = (org.sat4j.minisat.core.Solver) solver;

		MCommunicator.writeToLog("\n  There are "+theSolver.nConstraints()+"/"+solver.nConstraints()+" clauses. They are:");
		
		
		
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

	String intArrayCollectionToString(Collection<int[]> coll)
	{
		StringBuffer buff = new StringBuffer();
		
		buff.append("[ ");
		for(int[] arr : coll)
		{
			buff.append(Arrays.toString(arr));
			buff.append(" ");
		}
		buff.append(" ]");
		
		return buff.toString();
	}
	

	protected void addRealizedToListAndTrimGoals(ISolver theSolver, Translation trans, 
			Map<String, Set<List<MTerm>>> candidates, int firstQ, int lastQ, 
			Map<Integer, String> intermVarToPred, Map<Integer, List<MTerm>> intermVarToArgs, 
			Set<String> result, 
			Map<Set<Integer>, Set<int[]>> candidateClauseSets, 
			Set<Integer> candidateGoals,
			Set<Integer> unitClausesToAssumeCandidates,
			Map<int[], IConstr> toRemoveCandidates)
	{
		// This method needs to do 2 things:
		// (1) Add caseToString(String predname, List<MTerm> args) to result
		// (2) Remove clauses
		
		if(fromContext.forQuery.debug_verbosity > 2)
		{
			MCommunicator.writeToLog("\nartlatg: goals were "+candidateGoals);
		}
		
		// For (1) need to go backwards from var to its relation.
		for(int iVar = firstQ;iVar <= lastQ;iVar++)
		{						
			if(fromContext.forQuery.debug_verbosity > 2)
			{
				MCommunicator.writeToLog("\n  checking variable "+iVar+": "+theSolver.model(iVar));
			}
			
			// theSolver.model: Provide the truth value of a specific variable in the model.
			if(theSolver.model(iVar))
			{
				if(fromContext.forQuery.debug_verbosity > 2)
				{
					MCommunicator.writeToLog("\n  var true in the model: "+iVar);
					
					for(int iitemp=1;iitemp<iVar;iitemp++)
					{
						MCommunicator.writeToLog("\n "+iitemp+" -> "+theSolver.model(iitemp));
					}
				}
				
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
					
							if(fromContext.forQuery.debug_verbosity > 2)
							{
								MEnvironment.writeToLog("\n--- Removing clauses for var="+iVar+"\n: "+intArrayCollectionToString(clausesForQ)+"\n");
								MEnvironment.writeToLog("\n--- Removing goal variables: "+qVars+"\n");
							}
							
							// remove clause sets (vals)
							for(int[] aClause : clausesForQ)
							{
								if(toRemoveCandidates.containsKey(aClause))
								{
									// remove nothing until the end of this case.
									//boolean x = theSolver.removeConstr(toRemoveCandidates.get(aClause));
									//MEnvironment.writeToLog("\nRemoving: "+Arrays.toString(aClause)+" --> "+toRemoveCandidates.get(aClause)+"; "+x);
									//toRemoveCandidates.remove(aClause); // no longer needs removal at end
									
								}
								else
								{
									// aClause should be unit; remove the first literal
									assert(aClause.length == 1);
									//unitClausesToAssumeCandidates.remove(aClause[0]);																											
								}
							}
							
							// remove goals (keys)
							candidateGoals.removeAll(qVars);
						}
					}									
					
				} // end if new result
				
			} // end if model sets Q to true
		} // end for each Q variable		
	
		if(fromContext.forQuery.debug_verbosity > 2)
		{
			MCommunicator.writeToLog("\n  artlatg: goals are "+candidateGoals);
		}
		
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
		creationCommands.add("<MARGRAVE-COMMAND type=\"ADD\"><POLICY-IDENTIFIER pname=\"SRP1\" /><VARIABLE-DECLARATION sort=\"A\" varname=\"x\" /></MARGRAVE-COMMAND>");
		creationCommands.add("<MARGRAVE-COMMAND type=\"ADD\"><POLICY-IDENTIFIER pname=\"SRP1\" /><VARIABLE-DECLARATION sort=\"A\" varname=\"y\" /></MARGRAVE-COMMAND>");
		
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
		
		//System.err.println(MCommunicator.transformXMLToString(MEnvironment.printInfo("SRP1")));
		
		tests_createqry1();
		
		
		// !!!!!!!!!!!!
		// TODO terms (similar to forcing inclusion of idbs? )

	
		
		
		// simple no cases
		
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
		
		
						
		//result = MEnvironment.showRealized("SRQry1", mapAx);								
		//System.err.println(MCommunicator.transformXMLToString(result));
		testCase("1", "SRQry1", mapAx, new HashMap<String, Set<List<MTerm>>>(), "{=[A(x)]}");
		//////////////////////////////
		
		mapBx.put("B", new HashSet<List<MTerm>>());
		mapBx.get("B").add(tlist_x);
		
		// B is disjoint from A, so expect unrealized
		testCase("2", "SRQry1", mapAx, mapBx, "{B(x)=[]}");
		//////////////////////////////
		
		// Test reverse: no tuples in CANDIDATE. Above, was no tuples in CASE:		
		testCase("2a", "SRQry1", mapBx, mapAx, "{A(x)=[]}");
		
		//////////////////////////////
		
		mapPx.put("p", new HashSet<List<MTerm>>());
		mapPx.get("p").add(tlist_x);
		
		testCase("3", "SRQry1", mapPx, mapAx, "{A(x)=[p(x)]}");
		
		//////////////////////////////
		Map<String, Set<List<MTerm>>> mapPxRx = new HashMap<String, Set<List<MTerm>>>(mapPx);
		mapPxRx.put("r", new HashSet<List<MTerm>>());
		mapPxRx.get("r").add(tlist_x);
		
		testCase("4", "SRQry1", mapPxRx, mapAx, "{A(x)=[p(x), r(x)]}");
						
		//////////////////////////////
		Map<String, Set<List<MTerm>>> mapPermitx = new HashMap<String, Set<List<MTerm>>>();
				
		List<MTerm> tlist_xy = new ArrayList<MTerm>(tlist_x);
		tlist_xy.add(new MVariableTerm("y"));
		
		mapPermitx.put("SRP1.permit", new HashSet<List<MTerm>>());
		mapPermitx.get("SRP1.permit").add(tlist_xy);
				
		//result = MEnvironment.showRealized("SRQry1", mapPermitx, mapAx);								
		//System.err.println(MCommunicator.transformXMLToString(result));
		
		testCase("5", "SRQry1", mapPermitx, mapAx, "{A(x)=[SRP1.permit(x, y)]}");
				
		//////////////////////////////
		
		// Test multiple cases, addition of IDBs
		Map<String, Set<List<MTerm>>> mapAxBx = new HashMap<String, Set<List<MTerm>>>(mapAx);
		mapAxBx.put("B", new HashSet<List<MTerm>>());
		mapAxBx.get("B").add(tlist_x);
		
		testCase("6", "SRQry1", mapPermitx, mapAxBx, "{A(x)=[SRP1.permit(x, y)], B(x)=[]}");
		
		//////////////////////////////
		//////////////////////////////
		MEnvironment.writeErrLine("----- End MRealizedFormulaFinder Tests -----");	
	}

	
	static void testCase(String testId, String qId, Map<String, Set<List<MTerm>>> candidates, Map<String, Set<List<MTerm>>> cases, String expected)
	{
		MPreparedQueryContext aResult = MEnvironment.getQueryResult(qId);
		Map<String, Set<String>> result = aResult.getRealizedFormulaFinder().getRealizedFormulas(candidates, cases);
		if(!result.toString().equals(expected))
		{
			MEnvironment.writeErrLine("  MRealizedFormulaFinder test "+testId+" failed!");
			MEnvironment.writeErrLine("  Expected: "+expected);
			MEnvironment.writeErrLine("  Got: "+result);
		}

	}
	
}

package edu.wpi.margrave;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import kodkod.ast.Relation;
import kodkod.engine.fol2sat.Translation;
import kodkod.instance.Bounds;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;

import org.sat4j.core.VecInt;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IConstr;
import org.sat4j.specs.IProblem;
import org.sat4j.specs.ISolver;
import org.sat4j.specs.TimeoutException;

public class MUnrealizedFormulaFinder extends MCNFSpyQueryResult
{
	MUnrealizedFormulaFinder(MPreparedQueryContext qr)
	throws MUserException
	{
		super(qr);			
	}

	public Set<String> getUnrealizedFormulas(Map<String, Set<List<MTerm>>> candidates) throws MUserException
	{
		Map<String, Set<List<MTerm>>> cases = new HashMap<String, Set<List<MTerm>>>();
		return getUnrealizedFormulas(candidates, cases).get("");
	}
	
	public Set<String> getUnrealizedFormulas(List<MTerm> candidates)
	{
		List<String> cases = new ArrayList<String>();
		return getUnrealizedFormulas(candidates, cases).get("");		
	}


	
	public Map<String, Set<String>> getUnrealizedFormulas(List<String> candidates, List<String> cases)
	{
		// Return a list of relations in candidates that are NEVER populated
		// in any satisfying model UP TO THE QUERY'S CEILING. No guarantees at
		// higher model sizes.
		
		
		
		// No indexings in this verison. Names are raw; will not work with tupling unaided.		
		
		Map<String, Set<String>> result = new HashMap<String, Set<String>>();
		
		// Check at all sizes.
		for(int iSize=1;iSize<=fromContext.maxSize;iSize++)
		{
			// <--- diff. for unpop
			// Call first to add "" if needed
			Map<String, Set<String>> thisResult = getUnrealizedFormulasAtSize(candidates, cases, iSize);		
			
			// Don't addToMap. Want to intersect.
			intersectToMap(result, thisResult);
		}
		
		return result;				
	}
	
	void intersectToMap(Map<String, Set<String>> dest, Map<String, Set<String>> src)
	{
		for(String key : src.keySet())
		{
			if(dest.containsKey(key))
				dest.get(key).retainAll(src.get(key));
			else
				dest.put(key, src.get(key));
		}
	}

	
	public Map<String, Set<String>> getUnrealizedFormulas(Map<String, Set<List<MTerm>>> candidates, Map<String, Set<List<MTerm>>> cases ) throws MUserException
	{
		Map<String, String> originalPreds = new HashMap<String, String>();
		Map<String, List<String>> originalIndexing = new HashMap<String, List<String>>();
		
		// *** (1) Convert to indexed form
		if(fromContext.forQuery.tupled)
		{
			List<String> indexedCandidates = applyIndexing(candidates, originalPreds, originalIndexing);
			List<String> indexedCases = applyIndexing(cases, originalPreds, originalIndexing);
		}
		
		// *********************************************************************************
		// Make certain that the IDB names and indexings are declared in the parent query via IDBOUTPUT.					
		for(String predname : indexedCandidates)
		{
			if(!fromContext.forQuery.vocab.isSort(predname) && 
					!fromContext.forQuery.vocab.predicates.containsKey(predname) && 
					!fromContext.forQuery.idbOutputIndexing.keySet().contains(predname))
				throw new MUserException("Candidate in SHOW UNREALIZED: "+predname+
						" was not valid. If it is an EDB, it may be mis-spelled. If an IDB, it was not declared in the INCLUDE clause. Declared: "+fromContext.forQuery.idbOutputIndexing.keySet());		
		}
		for(String predname : indexedCases)
		{
			if(!fromContext.forQuery.vocab.isSort(predname) && 
					!fromContext.forQuery.vocab.predicates.containsKey(predname) && 
					!fromContext.forQuery.idbOutputIndexing.keySet().contains(predname))
				throw new MUserException("Case in SHOW UNREALIZED: "+predname+
						" was not valid. If it is an EDB, it may be mis-spelled. If an IDB, it was not declared in the INCLUDE clause. Declared: "+fromContext.forQuery.idbOutputIndexing.keySet());		
		} 	
		// TODO de-index the error messages
		// *********************************************************************************
		
		
		
		// *** (2) Invoke the sat solver
		// UN-populated
		Map<String, Set<String>> results = getUnrealizedFormulas(indexedCandidates, indexedCases); 
				
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
		
		return indexedResults;

	}
	
	public Map<String, Set<String>> getUnrealizedFormulasAtSize(List<String> candidates, List<String> cases, int atSize)
	{
		// There appears to be a lot of duplicated code between pop and unpop, but in reality
		// the two approaches differ in subtle ways. Leaving the "duplicate" code for now. - TN
		
		if(fromContext.forQuery.debug_verbosity > 1)
    		MEnvironment.writeOutLine("Getting UN-populated relations at size "+atSize);
				
		if(cases.size() == 0)
			cases.add("");
		
		// Is there any solving to do?
		if(nonTrivialTranslations.get(atSize) == null || nonTrivialBounds.get(atSize) == null)
		{
			Map<String, Set<String>> result = new HashMap<String, Set<String>> ();
			if(trivialTrue.contains(atSize))
			{
				// Trivially true --> all relations can be populated at this size
				for(String c : cases)
					result.put(c, new HashSet<String>());
			}
			else
			{
				// Trivially false --> no relations can be populated at this size
				for(String c : cases)
					result.put(c, new HashSet<String>(candidates));
			}
			return result;			
		}
		
		
		Map<String, Set<String>> result = new HashMap<String, Set<String>>();
		
		// Get the base problem.
		Translation theTranslation = nonTrivialTranslations.get(atSize); 
		CNFSpy theSolver = (CNFSpy) theTranslation.cnf();
		Bounds theBounds = nonTrivialBounds.get(atSize);
		final int numPrimaryVariables = theTranslation.numPrimaryVariables();
			
		
		if(fromContext.forQuery.debug_verbosity > 1)
    		MEnvironment.writeOutLine("Stats: "+theTranslation.numPrimaryVariables() +" primary vars. " + theSolver.numberOfClauses() +" clauses." );
		
		// Which variables mean a populated relation?
		Set<Integer> lookForTheseVars = new HashSet<Integer>();
		
		HashMap<Integer, Relation> mapCandidateRels = new HashMap<Integer, Relation>();

		// <--- !!! we care about the same variables for unpopulated, so no change here
        for(Relation r : theBounds.relations())
        {
            IntSet s = theTranslation.primaryVariables(r);            

            // only remember candidate rels (dont care about the rest)
            if(candidates.contains(r.name()))
            {
                IntIterator it = s.iterator();
            	while(it.hasNext())
            	{
            		Integer theInt = it.next();
            		mapCandidateRels.put(theInt, r);
            		//MEnvironment.errorStream.println(theInt +" -> "+r);
                
            		lookForTheseVars.add(theInt); // related to candidate, watch it
            	}
            }
        }        
		
        // If nothing to find, don't look.
        if(lookForTheseVars.size() == 0)
        {
        	result.clear();
        	for(String aCase : cases)
        		result.put(aCase, new HashSet<String>());
        	return result;
        }
        
		try
		{
			ISolver realSolver = theSolver.getEquivalentSAT4j();

        
			for(String aCase : cases)
			{
				Set<Integer> currentLookFor = new HashSet<Integer>(lookForTheseVars);
        	
				// Require case:
				Set<Integer> caseSet = new HashSet<Integer>();
				for(Relation r : theBounds.relations())
					if(r.name().equals(aCase))
					{
						IntSet caseVars = theTranslation.primaryVariables(r);
						IntIterator cvIt = caseVars.iterator();
						while(cvIt.hasNext())
							caseSet.add(cvIt.next());
						break;
					}
        	
				// Don't want to add a case for the null case
				// But DO want to include an empty clause if the case can never happen!
				int[] caseClause = constructLookForClause(caseSet);
				
				IConstr toRemove = null;
				if(!aCase.equals(""))
				{	
	        		if(caseClause.length == 0)
	        		{        	
	        			if(fromContext.forQuery.debug_verbosity > 1)
	            			MEnvironment.writeOutLine("DEBUG: case clause was empty. Moving on.");
	            					
	    				result.put(aCase, new HashSet<String>(candidates));
	    				continue; // next case        			
	        		}
					
					try
					{
						// SAT4j doesn't let you remove unit clauses. Instead, can pass as an ASSUMPTION
						if(caseClause.length > 1)
							toRemove = realSolver.addClause(new VecInt(caseClause));
       				}
    				catch(ContradictionException e)
    				{
    					// If adding that clause caused a contradiction, the case is impossible. Nothing can get populated.
    					if(fromContext.forQuery.debug_verbosity > 1)
                			MEnvironment.writeOutLine("DEBUG: case led to a contradiction.");

    					result.put(aCase, new HashSet<String>(candidates));
    					continue; // next case
    				}
				}
				else
				{
					// "" means no cases at all. warn the internal method
					caseClause = null;
				}
        	        		        	
        	if(fromContext.forQuery.debug_verbosity > 1)
        		MEnvironment.writeOutLine("DEBUG: UNPOPULATED case "+aCase+" with clause: "+Arrays.toString(caseClause)+
        				". SAT4j constraint count = "+realSolver.nConstraints());        
        	
        	result.put(aCase, internalUnrealized(candidates, caseClause, realSolver, currentLookFor, theTranslation, numPrimaryVariables, mapCandidateRels));
        	
        	if(caseClause != null && caseClause.length > 1)
        		realSolver.removeConstr(toRemove);
        }
        
             
		}
		catch(ContradictionException e)
		{
			// Trivially false --> no relations can be populated at this size
			for(String c : cases)
				result.put(c, new HashSet<String>(candidates));
			return result;
		}
        
        return result;
	}
	

	Set<String> internalUnrealized(Collection<String> startWith, int[] caseClause, ISolver solver, Set<Integer> lookForTheseVars, Translation theTranslation, int numPrimaryVariables, Map<Integer, Relation> mapCandidateRels)
	{
		Set<String> result = new HashSet<String>(startWith);
		
		IProblem problem = solver;
			
		// check EACH separately. no need to construct clauses
        while(lookForTheseVars.size() > 0)
        {             	
        	// Get one
        	int theVar = lookForTheseVars.iterator().next();
        	lookForTheseVars.remove(theVar);
        	        	
        	// Ask: can the original query be satisfied
        	//      AND
        	//      this variable be true? (i.e., R populated by tuple t)
        	        	
			if(fromContext.forQuery.debug_verbosity > 1)
        		MEnvironment.writeOutLine("DEBUG: Trying unit clause for var: "+theVar);
     		
			//MEnvironment.errorStream.println("~~~~ Calling SAT Solver ");
			final long startSolve = System.currentTimeMillis();
			boolean issat = false;
			try
			{
				//PrintWriter pw = new PrintWriter(MEnvironment.errorStream);
				//solver.printInfos(pw, ":::: ");			
				//solver.printStat(pw, ";;;; ");
				//pw.flush();
				
				// null clause means no cases. NOT THE SAME as empty clause.
				if(caseClause == null || caseClause.length > 1)
					issat = problem.isSatisfiable(new VecInt(new int[] {theVar}));
				else if(caseClause.length == 1)
					issat = problem.isSatisfiable(new VecInt(new int[] {theVar, caseClause[0]}));
				else
					issat = false; // empty clause								
			}
			catch(TimeoutException e)
			{
				// ...
				MEnvironment.errorWriter.println(e.getMessage());
			}
			catch(IllegalArgumentException e)
			{
				MEnvironment.errorWriter.println("ILLEGAL ARG (inner)");
				MEnvironment.errorWriter.println(e.getMessage());
				issat = false;
			}
			
			final long endSolve = System.currentTimeMillis();        
			msKodkodSolveTime += (endSolve - startSolve);
						
			if(issat)
			{				
				Relation r = mapCandidateRels.get(theVar);
				IntSet toRemove = theTranslation.primaryVariables(r);
				
				if(fromContext.forQuery.debug_verbosity > 1)
					MEnvironment.writeOutLine("Found "+theVar+" true; rel was: "+r.name()+" and all vars for it were: "+toRemove);
				
				// REMOVE 
				// (1) The relation for this var from results
				//    actually, remove the NAME; result is a set of strings, not relations
				result.remove(r.name());
				
				// (2) Remove all vars for this relation from lookForTheseVars
				// Returns an IntSet, so can't just removeAll
				IntIterator it = toRemove.iterator();
				while(it.hasNext())
					lookForTheseVars.remove(it.next());					
				
			}
			else if(fromContext.forQuery.debug_verbosity > 1)
				MEnvironment.errorWriter.println("Found "+theVar+" FALSE");
			
			
		} // end loop while still vars to look for

		
		return result;
	}
	
	
}

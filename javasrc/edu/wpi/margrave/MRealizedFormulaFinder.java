package edu.wpi.margrave;

import java.util.*;

import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Solver;
import kodkod.engine.fol2sat.Translation;
import kodkod.engine.fol2sat.Translator;
import kodkod.engine.fol2sat.TrivialFormulaException;
import kodkod.instance.Bounds;
import kodkod.instance.Universe;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;

import org.sat4j.core.VecInt;
import org.sat4j.specs.*;



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
	
	Set<String> undoIndexing(Set<String> results, Map<String, String> originalPreds, Map<String, List<String>> originalIndexing)
	{
		Set<String> convertedResults = new HashSet<String>();
		for(String r : results)
		{			
			convertedResults.add(undoIndexing(r, originalPreds, originalIndexing));
		}
		return convertedResults;
	}
	
	String undoIndexing (String singleString, Map<String, String> originalPreds, Map<String, List<String>> originalIndexing)
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
	
	/*public Set<String> getRealizedFormulas(List<MTerm> candidates)
	{
		List<String> cases = new ArrayList<String>();
		return getRealizedFormulas(candidates, cases).get("");		
	}*/
	
	
	
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
		
	public Map<String, Set<String>> getRealizedFormulasAtSize(List<String> candidates, List<String> cases, int atSize)
	{			
		//MEnvironment.errorStream.println("~~~~ Getting populated relations at size "+atSize);
		
		// For each case,
		// which candidates can be populated?
		// If cases is empty, add a single trivial case:
		if(cases.size() < 1)
			cases.add("");
				
		//MCommunicator.writeToLog("getPopulatedRelationsAtSize: "+candidates);
		
		// Is there any solving to do?
		if(nonTrivialTranslations.get(atSize) == null || nonTrivialBounds.get(atSize) == null)
		{
			
			Map<String, Set<String>> result = new HashMap<String, Set<String>> ();
			if(trivialTrue.contains(atSize))
			{
				// Trivially true --> all relations can be populated at this size
				for(String c : cases)
					result.put(c, new HashSet<String>(candidates));
			}
			else
			{
				// Trivially false --> no relations can be populated at this size
				for(String c : cases)
					result.put(c, new HashSet<String>());
			}
			return result;			
		}

		//MCommunicator.writeToLog("getPopulatedRelationsAtSize. Not trivial. Continuing. ");
		
		Map<String, Set<String>> result = new HashMap<String, Set<String>>();
				
		// Get the base problem.
		Translation theTranslation = nonTrivialTranslations.get(atSize); 
		CNFSpy theSolver = (CNFSpy) theTranslation.cnf();
		Bounds theBounds = nonTrivialBounds.get(atSize);
		final int numPrimaryVariables = theTranslation.numPrimaryVariables();
	
		if(fromContext.forQuery.debug_verbosity > 1)
    		MEnvironment.writeOutLine("Stats: "+theTranslation.numPrimaryVariables() +" primary vars. " + theSolver.numberOfClauses() +" clauses." );

		
		// Which variables mean a populated relation?
		// Which variables map to which relation?
		Set<Integer> lookForTheseVars = new HashSet<Integer>();			
		HashMap<Integer, Relation> mapCandidateRels = new HashMap<Integer, Relation>();
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
		
		//MCommunicator.writeToLog("\ngetPopulatedRelationsAtSize. lookForTheseVars: "+lookForTheseVars);
		//MCommunicator.writeToLog("\ngetPopulatedRelationsAtSize. mapCandidateRels: "+mapCandidateRels);
        
        // If nothing to find, don't bother looking.
        result.clear();
        if(lookForTheseVars.size() == 0)
        {        	        	
			for(String c : cases)
				result.put(c, new HashSet<String>());
        	return result;
        }
        
        try
        {        	
        	ISolver realSolver = theSolver.getEquivalentSAT4j();
        
        	// Fresh todo list for each case.
        	for(String aCase : cases)
        	{
        		Set<Integer> currentLookFor = new HashSet<Integer>(lookForTheseVars);        	
        	
        		// Assert that this case must be satisfied.
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
        	
        		// We DO want an empty clause if the case makes no sense. But don't add if the case is ""
        		int[] caseClause = constructLookForClause(caseSet);
        		IConstr toRemove = null;
        		
          		if(!("".equals(aCase)))
        		{
            		if(caseClause.length == 0)
            		{        	
            			if(fromContext.forQuery.debug_verbosity > 1)
                			MEnvironment.writeOutLine("DEBUG: case clause was empty. Moving on.");
                					
        				result.put(aCase, new HashSet<String>());
        				continue; // next case        			
            		}
            		
        			// SAT4j doesn't let you remove unit clauses. Instead, can pass as an ASSUMPTION
        			try
        			{
        				if(caseClause.length > 1)
        					toRemove = realSolver.addClause(new VecInt(caseClause));
        			}
        			catch(ContradictionException e)
        			{
        				// If adding that clause caused a contradiction, the case is impossible. Nothing can get populated.
        				if(fromContext.forQuery.debug_verbosity > 1)
                			MEnvironment.writeOutLine("DEBUG: case led to a contradiction.");
                					
        				result.put(aCase, new HashSet<String>());
        				continue; // next case
        			}
					
        		}
        		else
        		{
        			// Not empty clause; NO clause.
        			caseClause = null;
        		}
        		
        		if(fromContext.forQuery.debug_verbosity > 1)
        			MEnvironment.writeOutLine("DEBUG: POPULATED case "+aCase+" with clause: "+Arrays.toString(caseClause)+
        					". SAT4j Constraint count = "+realSolver.nConstraints());
        	        	
        		result.put(aCase, internalRealized(realSolver, caseClause, currentLookFor, theTranslation, numPrimaryVariables, mapCandidateRels));
        		if(toRemove != null)
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

	
	Set<String> internalRealized(ISolver solver, int[] caseClause, Set<Integer> lookForTheseVars, Translation theTranslation, int numPrimaryVariables, Map<Integer, Relation> mapCandidateRels)
	{
		Set<String> result = new HashSet<String>();
		boolean issat = false;
		
		do
		{				
			//MCommunicator.writeToLog("internalPopulated core loop. these vars remain: "+lookForTheseVars);
			
			// Create clause to direct the search
			//MEnvironment.errorStream.println("result is now: "+result);
			int[] newClause = constructLookForClause(lookForTheseVars);			
			//MEnvironment.errorStream.println("New clause: "+Arrays.toString(newClause));

			if(fromContext.forQuery.debug_verbosity > 1)
        		MEnvironment.writeOutLine("DEBUG: Checking sat with clause "+Arrays.toString(newClause));
        	
			IConstr toRemove = null;
			
			// Keep looking (or start looking) for these things. If there's >1 of them, we can remove. IF not, need to make an assumption.
			try
			{
				if(newClause.length > 1)
					toRemove = solver.addClause(new VecInt(newClause));							
			}
			catch(ContradictionException e)
			{
				// No more
				return result;
			}
				
			//MEnvironment.errorStream.println("~~~~ Calling SAT Solver ");
			final long startSolve = System.currentTimeMillis();
			try
			{
				if(newClause.length == 1)
				{
					// neither clause can be safely removed (possibly can't remove caseClause b/c it doesn't exist)
					if(caseClause != null && caseClause.length == 1)
						issat = solver.isSatisfiable(new VecInt(new int[] {caseClause[0], newClause[0]}));
				
					// caseClause can be safely removed, but newClause cannot
					else
						issat = solver.isSatisfiable(new VecInt(new int[] {newClause[0]}));
				
				}
				else
				{
					// newClause can be safely removed, but caseClause cannot (or doesn't exist)
					if(caseClause != null && caseClause.length == 1)
						issat = solver.isSatisfiable(new VecInt(new int[] {caseClause[0]}));
				
					// both can be safely removed
					else
						issat = solver.isSatisfiable();
				}
			}
			catch(TimeoutException e)
			{
				// for now 
				throw new RuntimeException(e);
			}
			if(toRemove != null)
				solver.removeConstr(toRemove);				
			final long endSolve = System.currentTimeMillis();
        
			msKodkodSolveTime += (endSolve - startSolve);
        
			if(!issat)
			{
				//MEnvironment.errorStream.println("unsat!");
				return result; // no more solutions  
			}
			
			if(fromContext.forQuery.debug_verbosity > 1)
				MEnvironment.writeOutLine("DEBUG: Satisfiable. Adding to result.");
			
			// return not just those that are true, but all for nonempty relations
			addRealizedToListAndTrimGoals(solver, theTranslation, numPrimaryVariables, mapCandidateRels, result, lookForTheseVars);
									
		} while(issat && lookForTheseVars.size() > 0);
        
		// Fall through should mean that we found everything.
		
        return result;
	}
	
	
	protected void addRealizedToListAndTrimGoals(ISolver theSolver, Translation trans, int numPrimary, 
			Map<Integer, Relation> relMap, Set<String> result, Set<Integer> lookForTheseVars)
	{
		// This method needs to do 2 things:
		// (1) Add nonempty relations to result
		// Compute the boolean variables that mean "some tuple t is in R" and
		// (2) remove them all from lookForTheseVars
		
		// For (1) need to go backwards from var to its relation.
		for(int iVar = 1;iVar <= numPrimary;iVar++)
		{
			if(!relMap.containsKey(iVar))
				continue; // not for a candidate rel, ignore
			
			//if(theSolver.valueOf(iVar)) // true
			if(theSolver.model(iVar))
			{
			//	MCommunicator.writeToLog("aptlatg. var true in the model: "+iVar);
				
				Relation r = relMap.get(iVar);
				if(!result.contains(r.name()))
				{
					//MEnvironment.errorStream.println("--- New Rel found: "+r.name());
					result.add(r.name());
					
					IntSet forR = trans.primaryVariables(r);
					IntIterator it = forR.iterator();
					while(it.hasNext())
					{
						int iRelVar = it.next();
						//MEnvironment.errorStream.println("--- Removing: "+iRelVar);
						lookForTheseVars.remove(iRelVar);
					}
				}
				
			}
		}		
		
	}
	
}

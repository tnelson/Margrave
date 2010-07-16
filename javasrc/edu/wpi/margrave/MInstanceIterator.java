/*
  	Copyright 2009 Brown University and Worcester Polytechnic Institute.
    
    This file is part of Margrave.

    Margrave is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Margrave is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Margrave.  If not, see <http://www.gnu.org/licenses/>.
*/

// tn

package a;

import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.util.*;

import org.sat4j.core.VecInt;
import org.sat4j.minisat.SolverFactory;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IConstr;
import org.sat4j.specs.IProblem;
import org.sat4j.specs.ISolver;
import org.sat4j.specs.TimeoutException;


import kodkod.ast.Decl;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.bool.BooleanFormula;
import kodkod.engine.config.Reporter;
import kodkod.engine.fol2sat.Translation;
import kodkod.engine.fol2sat.Translator;
import kodkod.engine.fol2sat.TrivialFormulaException;
import kodkod.engine.satlab.SATAbortedException;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.satlab.SATSolver;
import kodkod.instance.*;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;

//import net.sf.javabdd.*;


// Acts as an Iterator over solutions to a query. 
// Also provides routines to print all and print one, etc.

// BDD required public
// public 

class MSolutionInstance
{
	private Instance instance;
	private Instance dontcare;
	private List<String> annotations;
	
	MSolutionInstance(Instance instance, Instance dontcare, List<String> annotations)
	{
		this.instance = instance;
		this.dontcare = dontcare;
		this.annotations = annotations;
	}
	
	MSolutionInstance(Instance instance, Instance dontcare)
	{
		this.instance = instance;
		this.dontcare = dontcare;
		this.annotations = new ArrayList<String>(); 
	}
	
	Instance getFacts()
	{
		return instance;
	}
	
	Instance getDontCares()
	{
		return dontcare;				
	}
	
	List<String> getAnnotations()
	{
		// still passing back the actual objects instead of clones or nonces
		return annotations;
	}
}

class MTotalInstanceIterator extends MInstanceIterator
{
	private List<Iterator<Solution>> iteratorlist = new LinkedList<Iterator<Solution>>();
	boolean newKodkodIterator = true;
		
	MTotalInstanceIterator(MQueryResult qr) 
	throws MGEUnknownIdentifier, MGEUnsortedVariable, MGEManagerException, MGEBadIdentifierName	
	{
		super(qr);
		
		// TS Iterator just trusts Kodkod, builds a list of iterators for each size.
			

		// Search linearly from model size 1 up to maxsize. 
		LinkedList<String> atoms = new LinkedList<String>();
		for(int modelsize=1;modelsize<=qr.maxSize;modelsize++)
		{
			atoms.clear();
			for(int ii=0;ii<modelsize;ii++)			
				atoms.add("Atom"+ii);			

			iteratorlist.add(doBoundsAndKodKod_All(new Universe(atoms), qr.qryFormulaWithAxioms));
		}
	}
	
	protected void prepareNext()
	{
		
		// "peek" -- is the next model an unsat "filler"? If so, done with that model size.
		Solution sol;
		
		// If we have a real model waiting already from a prior call, don't condense.
		if(the_next != null)
			return;
		
		// If we have nothing waiting, and no iterators left, don't try to condense.
		if(iteratorlist.size() < 1)
			return;		
		
		do
		{				
			sol = iteratorlist.get(0).next(); // here

			// Count the time reported in the FIRST reply from each size
			if(newKodkodIterator)
			{
				// Can't trust these values, Kodkod is not reporting accurately.
				msKodkodSolveTime += sol.stats().solvingTime();
				msKodkodTransTime += sol.stats().translationTime();
				
				if(fromResult.forQuery.debug_verbosity > 1)
				{
					System.out.println("DEBUG: Beginning a new Kodkod solution iterator. Translation time for this iterator was: " + sol.stats().translationTime());
					System.out.println("       TOTAL translation time so far for this query: ");
				}
				newKodkodIterator = false;
			}
			
			if(unsatSol(sol))
			{
				iteratorlist.remove(0);
				newKodkodIterator = true;
			}
		}
		while(iteratorlist.size() > 0 && unsatSol(sol));
		
		// we got out of the loop by finding a model. Don't forget about it!
		if(!unsatSol(sol))
			the_next = new MSolutionInstance(sol.instance(), null);				
	}

	protected static boolean unsatSol(Solution sol)
	{
		if(sol.outcome().equals(Solution.Outcome.TRIVIALLY_UNSATISFIABLE) ||
		   sol.outcome().equals(Solution.Outcome.UNSATISFIABLE))
		   return true;
		return false;
	}
	
	private Iterator<Solution> doBoundsAndKodKod_All(Universe u, Formula f)
	throws MGEUnknownIdentifier, MGEUnsortedVariable, MGEManagerException, MGEBadIdentifierName
	{				
		ThreadMXBean mxBean = ManagementFactory.getThreadMXBean();
		long start = mxBean.getCurrentThreadCpuTime();	
		
		// Pass to KodKod for satsolving
		Solver qrySolver = new Solver();		
		qrySolver.options().setFlatten(true);
		qrySolver.options().setSolver(fromResult.forQuery.mySATFactory);
		qrySolver.options().setSymmetryBreaking(fromResult.forQuery.mySB);
					
		Bounds qryBounds = new Bounds(u);
		f = makeBounds(u, f, qryBounds);
				
		if(fromResult.forQuery.debug_verbosity >= 2)
			MEnvironment.outStream.println("DEBUG: Time (ms) to create bounds and finalize IDB collections: " + (mxBean.getCurrentThreadCpuTime()-start)/1000000);
		
		Iterator<Solution> sols = qrySolver.solveAll(f, qryBounds);
		
		return sols; 
		
	}

	
}

class MPopulatedRelationFinder extends MPartialInstanceIterator
{
	//  TODO Some duplicated code between this class and the partial iterator.
	// For now, extend
			
	MPopulatedRelationFinder(MQueryResult qr)
			throws MGEManagerException, MGEUnknownIdentifier, MGEBadIdentifierName
	{
		super(qr);
  	}
	
	public List<String> applyIndexing(Map<String, Set<List<String>>> candidates, Map<String, String> originalPreds, Map<String, List<String>> originalIndexing)
	{
		// Translate to tupled form
		List<String> indexedCandidates;
		
		if(!fromResult.forQuery.tupled)
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
				//System.err.println("predname: "+predname);
				//System.err.println(candidates.get(predname));
				for(List<String> anIndexing : candidates.get(predname))
				{
					String newName = predname;
				
//					System.err.println("indexing: "+anIndexing);
					
					// Query should have saved the visitor (and hence the ordering) in internalTuplingVisitor

					Map<Variable, Integer> theTupling = fromResult.forQuery.internalTuplingVisitor.pv.indexing;
					
					for(String idx : anIndexing)
					{
						// idx is a variable name, want to get its index in the tupling
						// The manager enforces only one Variable with that name.
						Variable theVar = MFormulaManager.makeVariable(idx);
						
						if(newName.equals(predname))
							newName += "_"+theTupling.get(theVar);
						else
							newName += ","+theTupling.get(theVar);
						
					}					
					
					//System.err.println(newName);
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
	
	
	
	public Map<String, Set<String>> getPopulatedRelations(Map<String, Set<List<String>>> candidates, Map<String, Set<List<String>>> cases)
	throws MSemanticException
	{
								
		// This version contains indexings for tupling
		// TODO strings everywhere; should be more structured?

		Map<String, String> originalPreds = new HashMap<String, String>();
		Map<String, List<String>> originalIndexing = new HashMap<String, List<String>>();
		
		// *** (1) Convert to indexed form
		List<String> indexedCandidates = applyIndexing(candidates, originalPreds, originalIndexing);
		List<String> indexedCases = applyIndexing(cases, originalPreds, originalIndexing);
						
		
		
		
		// *********************************************************************************
		// Make certain that the IDB names and indexings are declared in the parent query via IDBOUTPUT.					
		for(String predname : indexedCandidates)
		{
			if(!fromResult.forQuery.idbOutputIndexing.containsKey(predname))
				throw new MSemanticException("Candidate in SHOW POPULATED: "+predname+" was not declared as an IDB to output in the query. Declared: "+fromResult.forQuery.idbOutputIndexing.keySet());
		}
		for(String predname : indexedCases)
		{
			if(!fromResult.forQuery.idbOutputIndexing.containsKey(predname))
				throw new MSemanticException("Case in SHOW POPULATED: "+predname+" was not declared as an IDB to output in the query. Declared: "+fromResult.forQuery.idbOutputIndexing.keySet());	
		} 	
		// TODO de-index the error messages
		// *********************************************************************************
		
		
		
		
		
		// *** (2) Invoke the sat solver
		Map<String, Set<String>> results = getPopulatedRelations(indexedCandidates, indexedCases); 
				
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
	
	public Set<String> getPopulatedRelations(Map<String, Set<List<String>>> candidates) throws MSemanticException
	{
		Map<String, Set<List<String>>> cases = new HashMap<String, Set<List<String>>>();
		return getPopulatedRelations(candidates, cases).get("");
	}
	
	public Set<String> getPopulatedRelations(List<String> candidates)
	{
		List<String> cases = new ArrayList<String>();
		return getPopulatedRelations(candidates, cases).get("");		
	}

	public Set<String> getUnpopulatedRelations(Map<String, Set<List<String>>> candidates) throws MSemanticException
	{
		Map<String, Set<List<String>>> cases = new HashMap<String, Set<List<String>>>();
		return getUnpopulatedRelations(candidates, cases).get("");
	}
	
	public Set<String> getUnpopulatedRelations(List<String> candidates)
	{
		List<String> cases = new ArrayList<String>();
		return getUnpopulatedRelations(candidates, cases).get("");		
	}

	
	public Map<String, Set<String>> getPopulatedRelations(List<String> candidates, List<String> cases)
	{
		// No indexings in this verison. Names are raw; will not work with tupling unaided.
		
		Map<String, Set<String>> result = new HashMap<String, Set<String>>();
				
		// Check at all sizes.
		for(int iSize=1;iSize<=fromResult.maxSize;iSize++)
		{
			// Call first to add "" if needed
			Map<String, Set<String>> thisResult = getPopulatedRelationsAtSize(candidates, cases, iSize);		
			addToMap(result, thisResult);
		}
		
		return result;
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
	
	public Map<String, Set<String>> getUnpopulatedRelations(List<String> candidates, List<String> cases)
	{
		// Return a list of relations in candidates that are NEVER populated
		// in any satisfying model UP TO THE QUERY'S CEILING. No guarantees at
		// higher model sizes.
		// No indexings in this verison. Names are raw; will not work with tupling unaided.		
		
		Map<String, Set<String>> result = new HashMap<String, Set<String>>();
		
		// Check at all sizes.
		for(int iSize=1;iSize<=fromResult.maxSize;iSize++)
		{
			// <--- diff. for unpop
			// Call first to add "" if needed
			Map<String, Set<String>> thisResult = getUnpopulatedRelationsAtSize(candidates, cases, iSize);		
			
			// Don't addToMap. Want to intersect.
			intersectToMap(result, thisResult);
		}
		
		return result;				
	}
	
	public Map<String, Set<String>> getUnpopulatedRelations(Map<String, Set<List<String>>> candidates, Map<String, Set<List<String>>> cases ) throws MSemanticException
	{
		Map<String, String> originalPreds = new HashMap<String, String>();
		Map<String, List<String>> originalIndexing = new HashMap<String, List<String>>();
		
		// *** (1) Convert to indexed form
		List<String> indexedCandidates = applyIndexing(candidates, originalPreds, originalIndexing);
		List<String> indexedCases = applyIndexing(cases, originalPreds, originalIndexing);
		
		// *********************************************************************************
		// Make certain that the IDB names and indexings are declared in the parent query via IDBOUTPUT.					
		for(String predname : indexedCandidates)
		{
			if(!fromResult.forQuery.idbOutputIndexing.containsKey(predname))
				throw new MSemanticException("Candidate in SHOW UNPOPULATED: "+predname+" was not declared as an IDB to output in the query. Declared: "+fromResult.forQuery.idbOutputIndexing.keySet());
		}
		for(String predname : indexedCases)
		{
			if(!fromResult.forQuery.idbOutputIndexing.containsKey(predname))
				throw new MSemanticException("Case in SHOW UNPOPULATED: "+predname+" was not declared as an IDB to output in the query. Declared: "+fromResult.forQuery.idbOutputIndexing.keySet());	
		} 	
		// TODO de-index the error messages
		// *********************************************************************************
		
		
		
		// *** (2) Invoke the sat solver
		// UN-populated
		Map<String, Set<String>> results = getUnpopulatedRelations(indexedCandidates, indexedCases); 
				
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
	
	public Map<String, Set<String>> getUnpopulatedRelationsAtSize(List<String> candidates, List<String> cases, int atSize)
	{
		// There appears to be a lot of duplicated code between pop and unpop, but in reality
		// the two approaches differ in subtle ways. Leaving the "duplicate" code for now. - TN
		
		if(fromResult.forQuery.debug_verbosity > 1)
    		MEnvironment.outStream.println("Getting UN-populated relations at size "+atSize);
				
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
			
		
		if(fromResult.forQuery.debug_verbosity > 1)
    		MEnvironment.outStream.println("Stats: "+theTranslation.numPrimaryVariables() +" primary vars. " + theSolver.numberOfClauses() +" clauses." );
		
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
            		//System.out.println(theInt +" -> "+r);
                
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
	        			if(fromResult.forQuery.debug_verbosity > 1)
	            			MEnvironment.outStream.println("DEBUG: case clause was empty. Moving on.");
	            					
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
    					if(fromResult.forQuery.debug_verbosity > 1)
                			MEnvironment.outStream.println("DEBUG: case led to a contradiction.");

    					result.put(aCase, new HashSet<String>(candidates));
    					continue; // next case
    				}
				}
				else
				{
					// "" means no cases at all. warn the internal method
					caseClause = null;
				}
        	        		        	
        	if(fromResult.forQuery.debug_verbosity > 1)
        		MEnvironment.outStream.println("DEBUG: UNPOPULATED case "+aCase+" with clause: "+Arrays.toString(caseClause)+
        				". SAT4j constraint count = "+realSolver.nConstraints());        
        	
        	result.put(aCase, internalUnpopulated(candidates, caseClause, realSolver, currentLookFor, theTranslation, numPrimaryVariables, mapCandidateRels));
        	
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
	

	Set<String> internalUnpopulated(Collection<String> startWith, int[] caseClause, ISolver solver, Set<Integer> lookForTheseVars, Translation theTranslation, int numPrimaryVariables, Map<Integer, Relation> mapCandidateRels)
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
        	        	
			if(fromResult.forQuery.debug_verbosity > 1)
        		MEnvironment.outStream.println("DEBUG: Trying unit clause for var: "+theVar);
     		
			//System.out.println("~~~~ Calling SAT Solver ");
			final long startSolve = System.currentTimeMillis();
			boolean issat = false;
			try
			{
				//PrintWriter pw = new PrintWriter(System.out);
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
				System.err.println(e.getMessage());
			}
			catch(IllegalArgumentException e)
			{
				System.out.println("ILLEGAL ARG (inner)");
				System.err.println(e.getMessage());
				issat = false;
			}
			
			final long endSolve = System.currentTimeMillis();        
			msKodkodSolveTime += (endSolve - startSolve);
						
			if(issat)
			{				
				Relation r = mapCandidateRels.get(theVar);
				IntSet toRemove = theTranslation.primaryVariables(r);
				
				if(fromResult.forQuery.debug_verbosity > 1)
					System.out.println("Found "+theVar+" true; rel was: "+r.name()+" and all vars for it were: "+toRemove);
				
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
			else if(fromResult.forQuery.debug_verbosity > 1)
				System.out.println("Found "+theVar+" FALSE");
			
			
		} // end loop while still vars to look for

		
		return result;
	}
	
	public Map<String, Set<String>> getPopulatedRelationsAtSize(List<String> candidates, List<String> cases, int atSize)
	{			
		//System.out.println("~~~~ Getting populated relations at size "+atSize);
		
		// For each case,
		// which candidates can be populated?
		// If cases is empty, add a single trivial case:
		if(cases.size() < 1)
			cases.add("");
				
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

		Map<String, Set<String>> result = new HashMap<String, Set<String>>();
				
		// Get the base problem.
		Translation theTranslation = nonTrivialTranslations.get(atSize); 
		CNFSpy theSolver = (CNFSpy) theTranslation.cnf();
		Bounds theBounds = nonTrivialBounds.get(atSize);
		final int numPrimaryVariables = theTranslation.numPrimaryVariables();
	
		if(fromResult.forQuery.debug_verbosity > 1)
    		MEnvironment.outStream.println("Stats: "+theTranslation.numPrimaryVariables() +" primary vars. " + theSolver.numberOfClauses() +" clauses." );

		
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
            		//System.out.println(theInt +" -> "+r);
                
            		lookForTheseVars.add(theInt); // related to candidate, watch it
            	}
            }
        }        
		
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
            			if(fromResult.forQuery.debug_verbosity > 1)
                			MEnvironment.outStream.println("DEBUG: case clause was empty. Moving on.");
                					
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
        				if(fromResult.forQuery.debug_verbosity > 1)
                			MEnvironment.outStream.println("DEBUG: case led to a contradiction.");
                					
        				result.put(aCase, new HashSet<String>());
        				continue; // next case
        			}
					
        		}
        		else
        		{
        			// Not empty clause; NO clause.
        			caseClause = null;
        		}
        		
        		if(fromResult.forQuery.debug_verbosity > 1)
        			MEnvironment.outStream.println("DEBUG: POPULATED case "+aCase+" with clause: "+Arrays.toString(caseClause)+
        					". SAT4j Constraint count = "+realSolver.nConstraints());
        	        	
        		result.put(aCase, internalPopulated(realSolver, caseClause, currentLookFor, theTranslation, numPrimaryVariables, mapCandidateRels));
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

	
	Set<String> internalPopulated(ISolver solver, int[] caseClause, Set<Integer> lookForTheseVars, Translation theTranslation, int numPrimaryVariables, Map<Integer, Relation> mapCandidateRels)
	{
		Set<String> result = new HashSet<String>();
		boolean issat = false;
		
		do
		{				
			// Create clause to direct the search
			//System.out.println("result is now: "+result);
			int[] newClause = constructLookForClause(lookForTheseVars);			
			//System.out.println("New clause: "+Arrays.toString(newClause));

			if(fromResult.forQuery.debug_verbosity > 1)
        		MEnvironment.outStream.println("DEBUG: Checking sat with clause "+Arrays.toString(newClause));
        	
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
				
			//System.out.println("~~~~ Calling SAT Solver ");
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
				// for now TODO
				throw new RuntimeException(e);
			}
			if(toRemove != null)
				solver.removeConstr(toRemove);				
			final long endSolve = System.currentTimeMillis();
        
			msKodkodSolveTime += (endSolve - startSolve);
        
			if(!issat)
			{
				//System.out.println("unsat!");
				return result; // no more solutions  
			}
			
			if(fromResult.forQuery.debug_verbosity > 1)
				MEnvironment.outStream.println("DEBUG: Satisfiable. Adding to result.");
			
			// return not just those that are true, but all for nonempty relations
			addPopulatedToListAndTrimGoals(solver, theTranslation, numPrimaryVariables, mapCandidateRels, result, lookForTheseVars);
									
		} while(issat && lookForTheseVars.size() > 0);
        
		// Fall through should mean that we found everything.
		
        return result;
	}
	
	
	protected void addPopulatedToListAndTrimGoals(ISolver theSolver, Translation trans, int numPrimary, 
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
				Relation r = relMap.get(iVar);
				if(!result.contains(r.name()))
				{
					//System.out.println("--- New Rel found: "+r.name());
					result.add(r.name());
					
					IntSet forR = trans.primaryVariables(r);
					IntIterator it = forR.iterator();
					while(it.hasNext())
					{
						int iRelVar = it.next();
						//System.out.println("--- Removing: "+iRelVar);
						lookForTheseVars.remove(iRelVar);
					}
				}
				
			}
		}		
		
	}
	
	protected int[] constructLookForClause(Set<Integer> lookForTheseVars)
	{
		// initialized to 0
		int[] result = new int[lookForTheseVars.size()];
		
		// We are looking for THIS var or THIS var or...
		int iIndex = 0;
		for(Integer iVar : lookForTheseVars)
		{
			result[iIndex] = iVar;
			iIndex++;
		}
		
		return result;
	}
}




class MPartialInstanceIterator extends MInstanceIterator
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

		
		for(int iSize=1;iSize<=fromResult.maxSize;iSize++)
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
		
		//System.out.println(nonTrivialClauses);
		//System.out.println(nonTrivialBounds);
		
		if(qr.forQuery.debug_verbosity > 1)
			MEnvironment.outStream.println("DEBUG: Translation to CNF complete. Time: "+msKodkodTransTime + " ms.");
		
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
				System.out.println("Clause "+clausenum+" was "+ clause+" and had count "+counts[clausenum]);
			
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
			
			System.out.println("Literal: "+literal+" was in clauses: "+literalToClauses.get(literal));
			
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
		
		System.out.println("@@@@@ Don't cares were: ");
		for(int iVar=1;iVar<=numPrimaryVariables;iVar++)
			if(dontCares[iVar])
				System.out.print(iVar + " ");
		System.out.println();
		
		// TODO to optimize speed, could store some of these values we are calculating (like vars -> clauses map) and expand each call
		
        
		
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
			
			System.out.println("Relation "+r+" had primary vars: "+vars);
			
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
		
		System.out.println("@@@@@@@");
		System.out.println(resultInstance);
		System.out.println(resultDontCares);
		
		
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
		
		
		//System.out.println();
		//System.out.println(theSolver.numberOfClauses());

		
		// *********************************************
		return new MSolutionInstance(resultInstance, resultDontCares);		
	}
	
	protected void prepareNext()
	{
		
		if(the_next != null)
			return;
			
		while(currentSize <= fromResult.maxSize)
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
		qrySolver.options().setSymmetryBreaking(fromResult.forQuery.mySB);		
				
		spyFactory = new CNFSpyFactory(fromResult.forQuery.mySATFactory);
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
            
            // Kodkod modifies the clause arrays afterward. Do not re-use!

        }
        catch (TrivialFormulaException e)
        {
        	// Either ALL solutions or NO solutions. Let the caller decide.
        	throw e;
        }        
		
		
	}

}


class MIntArrayWrapper
{
    private int hashCode;
    private int[] theArray;
    private int length;
    
    MIntArrayWrapper(int[] theArray)
    {        
    	// Caller is obligated to make a COPY of the clause array passed by Kodkod.
        this.theArray = theArray; 
        hashCode = Arrays.hashCode(theArray);
        this.length = theArray.length;       
    }
    
    public boolean equals(Object obj)
    {
        if(obj instanceof MIntArrayWrapper)
            return Arrays.equals(theArray, ((MIntArrayWrapper)obj).theArray);
        return false;
    }

    public int hashCode()
    {
        return hashCode;
    }
    
    public int size()
    {
        return length;
    }
    
    public int get(int index)
    {
        return theArray[index]; // no out of bounds protection
    }
    
    public int[] getArray()
    {
    	return theArray;
    }
    
    public String toString()
    {
        StringBuffer result = new StringBuffer();
        for(int ii=0;ii<size();ii++)
        {
            result.append(get(ii));
            result.append(" ");
        }
        
        return result.toString();
    }
        
}


class CNFSpy implements SATSolver
{
    List<MIntArrayWrapper> clauses = new ArrayList<MIntArrayWrapper>();
    SATSolver internalSolver;
    SATFactory mySATFactory;
    
    CNFSpy(SATFactory mySATFactory)
    {
        // Spy on Kodkod -- remember the clauses given.
        internalSolver = mySATFactory.instance();
        this.mySATFactory = mySATFactory;
    }    
    
	@Override
    public void free()
    {
        internalSolver.free();
        clauses.clear();
    }

        
    @Override
    public boolean addClause(int[] lits)
    {
        // "No reference to the specified array is kept, so it can be reused."
        //  (From Kodkod Java doc)
        // Kodkod _does_ re-use, so we can't wrap lits directly; we need to copy first.
        
        int[] litsCopy = Arrays.copyOf(lits, lits.length);
        MIntArrayWrapper wrapper = new MIntArrayWrapper(litsCopy);

       // System.out.println("ADDED CLAUSE after "+clauses.size()+" others. It was: "+Arrays.toString(litsCopy));
        
        clauses.add(wrapper);
        return internalSolver.addClause(lits);
    }
    
    public void printClauses()
    {
    	int iClause = 0;
    	for(MIntArrayWrapper aClause : clauses)
    	{
    		System.out.println("Clause "+iClause+": "+Arrays.toString(aClause.getArray()));
    		iClause++;    			
    	}
    }

    public ISolver getEquivalentSAT4j()
    throws ContradictionException
    {
    	// Do not have access to the internal ISolver object that Kodkod keeps. But we can re-create.
    	
    	ISolver result = SolverFactory.newDefault();
    	result.newVar(internalSolver.numberOfVariables());
    	// no!
    	//result.setExpectedNumberOfClauses(internalSolver.numberOfClauses());
    	result.setTimeout(36000); // 10 hrs (just in case)
    	
   		for(MIntArrayWrapper aClause : clauses)
   		{
   			IConstr x = result.addClause(new VecInt(aClause.getArray()));
			
   			//PrintWriter pw = new PrintWriter(System.out);
			//result.printInfos(pw, ":::: ");
			//pw.flush();
   			
			//if(x == null)
   			//System.err.println("conv: "+x + " : "+aClause.toString());
   			// sometimes null, but not always
   			// taut, cont, or... unit?
   		}
    	    	
    	return result;
    }
    
    public CNFSpy makeCopy()
    {
    	// Copy this solver.
    	// Yes, that means internalSolver.addClause calls. :(
    	
    	CNFSpy newSolver = new CNFSpy(mySATFactory);
    	newSolver.addVariables(internalSolver.numberOfVariables());
    	// Can't use the same clauses set. But we can use the same arrays (no need to *copy* each clause)
    
    	for(MIntArrayWrapper wrapper : clauses)
    	{
    		newSolver.clauses.add(wrapper);
    		newSolver.internalSolver.addClause(wrapper.getArray());
    		
    		// DEBUG
    		// Is this lovely clause tautologous?
    		// slow for now
    		/*for(int ii=0;ii<wrapper.size();ii++)
    			for(int jj=ii+1;jj<wrapper.size();jj++)
    			{
    				if(Math.abs(wrapper.get(ii)) == Math.abs(wrapper.get(jj)))
    					System.err.println("TAUT");
    			}*/
    	}
    	
    	
    	return newSolver;
    }
    
    @Override
    public void addVariables(int numVars) {
        internalSolver.addVariables(numVars);
        
    }

    @Override
    public int numberOfClauses() {
        return internalSolver.numberOfClauses();
    }

    @Override
    public int numberOfVariables() {
        return internalSolver.numberOfVariables();
    }

    @Override
    public boolean solve() throws SATAbortedException
    {
        return internalSolver.solve();
    }

    @Override
    public boolean valueOf(int variable)
    {
        return internalSolver.valueOf(variable);
    }

}

class CNFSpyFactory extends SATFactory
{
    private SATFactory mySATFactory;
    
    CNFSpyFactory(SATFactory mySATFactory)
    {
        this.mySATFactory = mySATFactory;    
    }
    
    @Override
    public SATSolver instance()
    {
        return new CNFSpy(mySATFactory);                      
    }
    
}

class MQueryResult
{
	protected int maxSize;
	private int hbu_ceiling;	
	
	public long msQueryCreationTime;
	public long msQueryRunTime;
	public long msQueryTuplingTime;
	
	protected Formula qryFormulaWithAxioms;
	
	MQuery forQuery;
	
	protected MQueryResult(MQuery q, Formula qfwa, int maxsize, int hbmax, long timeCreateObject, long timeRunQuery, long timeTupling)
	{
		// Used to print intelligently		
		forQuery = q;
	
		// How big a universe did we check up to?
		maxSize = maxsize;
	
		// How big did our analysis say the HB term universe could be?
		hbu_ceiling = hbmax;

		qryFormulaWithAxioms = qfwa;
	
		// How long did Margrave take to create the query object?
		msQueryCreationTime = timeCreateObject;
		msQueryRunTime = timeRunQuery;
		msQueryTuplingTime = timeTupling;
	}
	
	public int get_hu_ceiling()
	{
		return hbu_ceiling;
	}
	
	public int get_universe_max()
	{
		return maxSize;
	}
	
	public MTotalInstanceIterator getTotalIterator() 
	throws MGEUnknownIdentifier, MGEUnsortedVariable, MGEManagerException, MGEBadIdentifierName
	{
		return new MTotalInstanceIterator(this);
	}

	public MPartialInstanceIterator getPartialIterator()
	throws MGEManagerException, MGEUnknownIdentifier, MGEBadIdentifierName
	{
		return new MPartialInstanceIterator(this);
	}

	public MPopulatedRelationFinder getPopulatedRelationFinder() 
	throws MGEManagerException, MGEUnknownIdentifier, MGEBadIdentifierName
	{
		return new MPopulatedRelationFinder(this);
	}

	public int countModels() 
	{
		return countModelsAtSize(0);
	}
	
	
	public int countModelsAtSize(Integer n)
	{	
		try
		{
			MInstanceIterator it = getTotalIterator();
			
			int count = 0; 
			while(it.hasNext()) 
			{ 
				try 
				{ 
					 Instance sol = it.next().getFacts();
					 if(n< 1 || sol.universe().size() == n)
						 count++;
				}
				catch(MGENoMoreSolutions e)
				{} 
			
			}
			
			return count;
		}
		catch(MGException e)
		{
			return -1; // error
		}		
	}

	public boolean isSatisfiable()
	throws MGException
	{
		MInstanceIterator it = getTotalIterator();
						 
		if(it.hasNext())
			return true;
		return false;
		
		// return exception if the iterator has problems; distinguish from true/false.
	}

	public String getPrettySolution(MSolutionInstance sol, MInstanceIterator fromIterator)
	{
		// TODO Extremely tangled. Fix the design of MQuery vs. MQueryResult vs. MInstanceIterator
		
		if(forQuery.tupled)
		{
			// This solution is tupled, and forQuery is the inner query
			MSolutionInstance unTupledSol = forQuery.internalTupledQuery.processTupledSolutionForThis(sol);
			return forQuery.getPrettyPrintForSolution(forQuery.internalTupledQuery.vocab, unTupledSol, fromIterator);
		}
		else if(forQuery.internalTupledQuery != null)
		{
			// This solution is tupled, and somehow forQuery is the outer query
			MSolutionInstance unTupledSol = forQuery.processTupledSolutionForThis(sol);
			return forQuery.getPrettyPrintForSolution(forQuery.vocab, unTupledSol, fromIterator);
		}
		else
		{									
			return forQuery.getPrettyPrintForSolution(forQuery.vocab, sol, fromIterator);
		}
		
	}
	
}

public abstract class MInstanceIterator
{
	protected MSolutionInstance the_next;

	// Calculated internally!
	public long msKodkodSolveTime = 0;
	public long msKodkodTransTime = 0;
	
	// TODO check these calculations
	
	// Used in output to help display the vectors for idbs
	Map<String, String> idbToTup = new HashMap<String, String>();
	
	protected MQueryResult fromResult;
	
	protected MInstanceIterator(MQueryResult fromResult)
	{		
		this.fromResult = fromResult; 
				
		// Next solution (if any)
		the_next = null;
	}
	
	public boolean hasNext()
	{			
		// Kodkod returns "unsatisfiable" instances... for purposes of grabbing a real SOLUTION,
		// those instances are useless. Ignore them.
		prepareNext();
			
		// If no more sizes to enumerate
		if(the_next == null)
			return false;
		return true;
	}
	
	// Descendants will dictate how models are constructed and requested.
	abstract protected void prepareNext();
	
	
	public MSolutionInstance next() throws MGENoMoreSolutions
	{
		// hasNext prepares the next solution (if any exists)
		if(!hasNext())
			throw new MGENoMoreSolutions("No more solutions exist for the query.");
						
		// Make room for the next solution to come.
		MSolutionInstance result = the_next;
		the_next = null;
		return result;
	}
	
	public boolean warn_user()
	{
		// Warn the user if there is an inf. herbrand univ, OR if the user has overridden a finite one.
		if(fromResult.get_hu_ceiling() < 0 || fromResult.get_hu_ceiling() > fromResult.get_universe_max())
			return true;
		
		return false;
	}	

	
	public long getQueryTuplingTime()
	{
		return fromResult.msQueryTuplingTime;		
	}
	
	
	private String varToRelName(List<Relation> relList, HashMap<Relation, List<Tuple>>tupMap, int iVar)
	{
		for(Relation r : relList)
		{
			int relWidth = tupMap.get(r).size();
			
			if(iVar < relWidth)
				return r.name();
			iVar -= relWidth;
		}		
		return "";
	}
	
	private String varToTupleName(List<Relation> relList, HashMap<Relation, List<Tuple>>tupMap, int iVar)
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
	}
	
	protected Formula makeBounds(Universe u, Formula f, Bounds qryBounds)
	throws MGEManagerException, MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// Create bounds on relations 
		TupleFactory factory = u.factory();
				
		// Type predicates (EDB) 
		for(MSort t : fromResult.forQuery.vocab.sorts.values())
		{						
			qryBounds.bound(t.rel, factory.allOf(t.rel.arity()));					
		}
		
		// State predicates (EDB)
		for(Relation r : fromResult.forQuery.vocab.predicates.values())
			qryBounds.bound(r, factory.allOf(r.arity()));				
		
		
		// ****************************************
		// Bound IDBs that we want to output
		// ****************************************
		
		// Are we including non-decision IDBs in the calculation? If not, and this isn't one, ignore.		
		if(fromResult.forQuery.getIDBNamesToOutput().size() > 0)
		{						
			// Re-use cached work whenever possible.
			HashMap<MIDBCollection, RelationAndVariableReplacementV> initialVisitors = 
				new HashMap<MIDBCollection, RelationAndVariableReplacementV>();
			FreeVariableCollectionV freeVarCollector = new FreeVariableCollectionV();
			Set<Formula> impSet = new HashSet<Formula>();
			
			//System.err.println(fromResult.forQuery.idbNamesToOutput);
			
			// Decisions, Rule Applicability, etc. (IDBs at policy level)
			for(MIDBCollection idbs : fromResult.forQuery.myIDBCollections.values())
			{			
				// What is this policy publishing?
			 
				for(String idbname : idbs.idbs.keySet())
				{
					//System.err.println(idbs.name+":"+idbname);							
					
					// Is this an idb to be published? If not, skip it.
					if(!fromResult.forQuery.getIDBNamesToOutput().contains(idbs.name+":"+idbname))
					{						
						continue;
					}
															
					Formula idbFormula = idbs.idbs.get(idbname);
					
					//System.err.println("++ " +idbs.name+":"+idbname);
					//System.err.println(idbFormula.hashCode());
					
					// At this point, idbFormula is using object references from the Policy instead
					// of the proper vocabulary, which would lead to unbound relation exceptions if 
					// we did not do this:
					
					RelationAndVariableReplacementV vis;
					if(initialVisitors.containsKey(idbs))
						vis = initialVisitors.get(idbs);
					else
					{
						vis = MIDBCollection.getReplacementVisitor(idbs.vocab, fromResult.forQuery.vocab);
						initialVisitors.put(idbs, vis);
					}
					
					idbFormula = idbFormula.accept(vis);
									
					// What temporary variables did the policy refer to?
					// (Order doesn't really matter here, since it's all flat universal quantifiers)
					// Some confusion in naming if someone sees the formula, but not really a difference.
					Set<Variable> tempvars = idbFormula.accept(freeVarCollector);
									
					// What is the arity of the relation?
					 int idbArity = tempvars.size();					
					
					// Zero arity means either always true or always false... 
					// Would be nice to support but KodKod doesn't allow nullary relations
					if(idbArity < 1)
						continue;
									
					// Create a temporary relation 
					// (If tupled, attach the indexing -- uglier but desirable for correctness checking.)
					Relation therel;
					if(fromResult.forQuery.tupled)
					{
						//outStream.println(idbOutputIndexing);
						//outStream.println(idbs.name+":"+idbname);
						
						// Debug only (messes up listing possibly nonempty relations, etc, etc.)
						//List<String> indexing = idbOutputIndexing.get(idbs.name+":"+idbname);
						//therel = MGFormulaManager.makeRelation(idbs.name+":"+idbname+indexing, idbArity);
						therel = MFormulaManager.makeRelation(idbs.name+":"+idbname, idbArity); // same as non-tupled
					}
					else
						therel = MFormulaManager.makeRelation(idbs.name+":"+idbname, idbArity);
					
					// And "bound" it.
					qryBounds.bound(therel, factory.allOf(idbArity));			
				
					// Get proper ordering of tempvars
												
					// Kludge?
					// IDB set is Policy: Just assume that this IDB has the same order of variable as a decision must.
					// IDB set is CustomIDB set: we get an ordering from the custom idb set
					
					// TODO This isn't needed anymore, is it? Can just use varOrdering? Double-check...
					
					Expression tup;
					if(idbs instanceof MPolicy)
					{
						// We assume the request vector ordering from the vocab, since this is a Policy, not a custom IDB.
						// But any given request variable need not appear. The order, however, is preserved.
						
						// Convert from a list of vars to a list of strings; keep only vars that actually appear.
						List<String> varnameorder = new ArrayList<String>(idbArity);
						for(Variable v : idbs.vocab.requestVectorOrder)
							if(tempvars.contains(v))
								varnameorder.add(v.name());
						
						if(idbArity != varnameorder.size())
							throw new MGEUnknownIdentifier("Arity of IDB formula did not match expected: "+idbs.name + ": "+idbname);
																
						//ordered_tempvars = idbs.vocab.requestVectorOrder;						
						tup = MFormulaManager.makeVarTuple(varnameorder);
					}
					else if(idbs instanceof MQuery) // used to be MCustomIDB
					{
						if(idbArity != idbs.varOrdering.size())
							throw new MGEUnknownIdentifier("Arity of IDB formula did not match expected: "+idbs.name + ": "+idbname);
						
						// Use internal ordering
						tup = MFormulaManager.makeVarTupleV( idbs.varOrdering );
					}
					else if(idbs instanceof MInternalIDBCollection)
					{
						// Tupled query's IDB. This was a policy before.
						
						// Use the first (and only) variable in tempvars. We've tupled the formula, so it 
						// does indeed have only one free variable now.
						Iterator<Variable> itv = tempvars.iterator();
						if(!itv.hasNext())
							throw new MGEBadIdentifierName("No free variables detected in IDB flagged for tupled output: "+idbs.name);
						tup = itv.next();
					}
					else
						throw new MGEUnknownIdentifier("Bad IDB collection type: "+idbs.getClass().getName());
					
					 
					
					// Don't forget to add bi-implication (the relation holds iff the policy says it does.)									

					// Restrict the IDB Formulas to use the proper types for their variables.
					// (Note that rule-scope existentials are already typed in their quantifier.)
					Set<Formula> properTypes = new HashSet<Formula>();					
					for(Variable var : tempvars)
					{ 	
						if(fromResult.forQuery.vocab.requestVarDomains.containsKey(var.name()))
						{
							Formula atom = MFormulaManager.makeAtom(var, fromResult.forQuery.vocab.requestVarDomains.get(var.name()));
							properTypes.add(atom);
						}
					}
				

					Formula atom2 = MFormulaManager.makeAtom(tup, therel);
					Formula atom2a = MFormulaManager.makeAnd(idbFormula, MFormulaManager.makeConjunction(properTypes));					
					Formula imp = MFormulaManager.makeIFF(atom2, atom2a);
									
					// Order doesn't matter
					String prettyVarTuple = "";
					for(Variable var : tempvars)
					{
						if(prettyVarTuple.length() > 0 )
							prettyVarTuple = var.name()+" "+prettyVarTuple;
						else
							prettyVarTuple = var.name();
						
						Decl theDecl = MFormulaManager.makeOneOfDecl(var, Expression.UNIV);
						imp = MFormulaManager.makeForAll(imp, theDecl);
					}							
					
					// May have many idbs, so don't just do .makeAnd over and over.
					// Save in a set and make one Conjunction NaryFormula.
					
					impSet.add(imp);
					//System.err.println(imp);
					
					// ****
					// We only quantify vars that actually matter, EVEN IF the idb definition itself
					// includes more. This means that later on we need to label the output properly.
					idbToTup.put(therel.name(), prettyVarTuple);
					
				} // end for each idb in idbset
			} // end for each idbset						
			
			if(fromResult.forQuery.debug_verbosity > 1)
			{
				MEnvironment.outStream.println("DEBUG: IDB output bi-implications created. There were "+impSet.size()+" of them.");
			}
			
			impSet.add(f); // f and imp1 and imp2 and ...
			f = MFormulaManager.makeConjunction(impSet);
		} // end of if including idbs in output								
		// ****************************************

		
		
		return f;
	}



	

		

	
	/*public static void bddGCCallback(int n)
	{
		//forQuery.errStream.println("GC callback reached. n = "+n);
	}*/
}














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
			System.err.println("Unable to register GC callback with JavaBDD.");
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

class SkolemReporter implements Reporter
{
	Bounds skolemBounds = null; 
	
	@Override
	public void detectedSymmetries(Set<IntSet> parts) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void detectingSymmetries(Bounds bounds) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void flattening(BooleanFormula circuit) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void generatingSBP() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void optimizingBoundsAndFormula() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void skolemizing(Decl decl, Relation skolem, List<Decl> context) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void solvingCNF(int primaryVars, int vars, int clauses) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void translatingToBoolean(Formula formula, Bounds bounds)
	{		
		skolemBounds = bounds;		
	}

	@Override
	public void translatingToCNF(BooleanFormula circuit) {
		// TODO Auto-generated method stub
		
	}
	
}

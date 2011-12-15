/*
  	Copyright 2009-2010 Brown University and Worcester Polytechnic Institute.
    
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

package edu.wpi.margrave;

import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.util.*;

import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.instance.*;

class KodkodContext
{
	Formula fmla;	
	Bounds bounds;
	
	KodkodContext(Formula fmla, Bounds bounds)
	{
		this.fmla = fmla;
		this.bounds = bounds;
	}
			
	public String toString()
	{
		return "KodkodContext: "+fmla.toString()+"\n"+bounds.toString();
	}
}


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

	public Set<Object> getUsedAtoms() 
	{
		Set<Object> result = new HashSet<Object>();
		
		// Check each relation
		for(Relation r : instance.relations())
		{
			TupleSet theTuples = instance.tuples(r);
			for(Tuple aTuple : theTuples)
			{
				for(int iIndex = 0;iIndex < aTuple.arity(); iIndex++)
				{					
					result.add(aTuple.atom(iIndex));										
				}
			}
		}
		
		return result;
	}

}

public class MTotalInstanceIterator extends MInstanceIterator
{
	Iterator<Solution> kodkodIterator;
	boolean firstSolution = true;
		
	MTotalInstanceIterator(MPreparedQueryContext qr) 
	throws MGEUnknownIdentifier, MGEManagerException, MGEBadIdentifierName	
	{
		super(qr);
		
		// Wrap Kodkod's iterator.			 		
		kodkodIterator = doBoundsAndKodKod(qr.qryFormulaWithAxioms);
	}
	
	protected void prepareNext()
	{		
		// We have a real model waiting already from a prior call
		if(the_next != null)
			return;
		
		// Are we out of models? (iterator contains one "unsat" instance; but calls afterward will
		// raise an exception if we do not check for this.)
		if(!kodkodIterator.hasNext())
			return;
		
		Solution sol = kodkodIterator.next(); 

		// Count the time reported in the FIRST reply from each size
		if(firstSolution)
		{
			// Can't trust these values, Kodkod is not reporting accurately.
			msKodkodSolveTime += sol.stats().solvingTime();
			msKodkodTransTime += sol.stats().translationTime();
			
			if(fromContext.forQuery.debug_verbosity > 1)
			{
				MEnvironment.writeOutLine("DEBUG: Beginning a new Kodkod solution iterator. Translation time for this iterator was: " + sol.stats().translationTime());
				MEnvironment.writeOutLine("       TOTAL translation time so far for this query: ");
			}
			
			firstSolution = false;
		}

		
		// we got out of the loop by finding a model. Don't forget about it!
		if(!unsatSol(sol))
		{
			the_next = new MSolutionInstance(sol.instance(), null);
		}
	}

	protected static boolean unsatSol(Solution sol)
	{
		if(sol.outcome().equals(Solution.Outcome.TRIVIALLY_UNSATISFIABLE) ||
		   sol.outcome().equals(Solution.Outcome.UNSATISFIABLE))
		   return true;
		return false;
	}
		
	private Iterator<Solution> doBoundsAndKodKod(Formula f)
	throws MGEUnknownIdentifier, MGEManagerException, MGEBadIdentifierName
	{				
		ThreadMXBean mxBean = ManagementFactory.getThreadMXBean();
		long start = mxBean.getCurrentThreadCpuTime();			
		
		// Pass to KodKod for satsolving
		Solver qrySolver = new Solver();	
		
		qrySolver.options().setFlatten(true);

		qrySolver.options().setSolver(fromContext.forQuery.mySATFactory);
		qrySolver.options().setSymmetryBreaking(fromContext.forQuery.mySB);
							
		//KodkodContext context = makeConservativeBounds(f);
		KodkodContext context = makeBounds(f);
						
		if(fromContext.forQuery.debug_verbosity >= 2)
			MEnvironment.writeOutLine("DEBUG: Time (ms) to create bounds and finalize IDB collections: " + (mxBean.getCurrentThreadCpuTime()-start)/1000000);
		
		Iterator<Solution> sols = qrySolver.solveAll(context.fmla, context.bounds);		
		return sols; 		
	}
	
}


abstract class MInstanceIterator extends MQueryResult
{
	protected MSolutionInstance the_next;
			
	protected MInstanceIterator(MPreparedQueryContext fromContext)
	{		
		super(fromContext);
				
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
	
	/*public boolean warn_user()
	{
		// Warn the user if there is an inf. herbrand univ, OR if the user has overridden a finite one.
		if(fromContext.getCeilingUsed() < 0 || fromContext.getCeilingUsed() > fromContext.get_universe_max())
			return true;
		
		return false;
	}	*/

	
	public long getQueryTuplingTime()
	{
		return fromContext.msQueryTuplingTime;		
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
	

	
	/*public static void bddGCCallback(int n)
	{
		//forQuery.errStream.println("GC callback reached. n = "+n);
	}*/
}

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

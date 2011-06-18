package edu.wpi.margrave;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sat4j.core.VecInt;
import org.sat4j.minisat.SolverFactory;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.ISolver;

import kodkod.ast.Decl;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Solver;
import kodkod.engine.bool.BooleanFormula;
import kodkod.engine.config.Reporter;
import kodkod.engine.fol2sat.Translation;
import kodkod.engine.fol2sat.Translator;
import kodkod.engine.fol2sat.TrivialFormulaException;
import kodkod.engine.satlab.SATAbortedException;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.satlab.SATSolver;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.instance.TupleFactory;
import kodkod.instance.Universe;
import kodkod.util.ints.IntSet;

class MCNFSpyQueryResult extends MQueryResult
{
	List<Integer> trivialTrue = new ArrayList<Integer>();
	List<Integer> trivialFalse = new ArrayList<Integer>();
	HashMap<Integer, Translation> nonTrivialTranslations = new HashMap<Integer, Translation>();
	HashMap<Integer, Bounds> nonTrivialBounds = new HashMap<Integer, Bounds>();

	CNFSpyFactory spyFactory;
	
	MCNFSpyQueryResult(MPreparedQueryContext qr)
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
				
		if(qr.forQuery.debug_verbosity > 1)
			MEnvironment.writeOutLine("DEBUG: Translation to CNF complete. Time: "+msKodkodTransTime + " ms.");
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
    		
    		//MCommunicator.writeToLog("\nBounds: \n"+qryBounds.toString());
    		//MCommunicator.writeToLog("\nFormula: "+f.toString());
            Translation trans = Translator.translate(f, qryBounds, qrySolver.options());
           // CNFSpy theSpy = (CNFSpy)trans.cnf();         
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


public abstract class MQueryResult
{
	// Calculated internally!
	public long msKodkodSolveTime = 0;
	public long msKodkodTransTime = 0;	
	
	protected MPreparedQueryContext fromContext;
	
	// Used in output to help display the vectors for idbs
	Map<String, String> idbToTup = new HashMap<String, String>();
	
	MQueryResult(MPreparedQueryContext fromContext)
	{
		this.fromContext = fromContext;		
	}
	
	protected Formula makeBounds(Universe u, Formula f, Bounds qryBounds)
	throws MGEManagerException, MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// Create bounds on relations 
		TupleFactory factory = u.factory();
				
		MCommunicator.writeToLog("\nCreating bounds for universe "+u+"...");
		
		// Type predicates (EDB) 
		for(MSort t : fromContext.forQuery.vocab.sorts.values())
		{						
			qryBounds.bound(t.rel, factory.allOf(t.rel.arity()));					
		}
		
		// State predicates (EDB)
		for(MPredicate p : fromContext.forQuery.vocab.predicates.values())
			qryBounds.bound(p.rel, factory.allOf(p.rel.arity()));				
		// constants
		for(MConstant c : fromContext.forQuery.vocab.constants.values())
			qryBounds.bound(c.rel, factory.allOf(c.rel.arity()));
		// functions
		for(MFunction fn : fromContext.forQuery.vocab.functions.values())
			qryBounds.bound(fn.rel, factory.allOf(fn.rel.arity()));				

		
		// ****************************************
		// Bound IDBs that we want to output
		// ****************************************
		
		// Are we including non-decision IDBs in the calculation? If not, and this isn't one, ignore.		
		if(fromContext.forQuery.getIDBNamesToOutput().size() > 0)
		{						
			// Re-use cached work whenever possible.
			HashMap<MIDBCollection, RelationAndTermReplacementV> initialVisitors = 
				new HashMap<MIDBCollection, RelationAndTermReplacementV>();
			FreeVariableCollectionV freeVarCollector = new FreeVariableCollectionV();
			Set<Formula> impSet = new HashSet<Formula>();
			
			//MEnvironment.errorStream.println(fromContext.forQuery.idbNamesToOutput);
			MCommunicator.writeToLog("\nThere are IDBs to axiomatize.");
			
			// Decisions, Rule Applicability, etc. (IDBs at policy level)
			for(MIDBCollection idbs : fromContext.forQuery.myIDBCollections.values())
			{			
				// What is this policy publishing?
			 
				for(String idbname : idbs.idbKeys())
				{						
					//MEnvironment.errorStream.println(idbs.name+MEnvironment.sIDBSeparator+idbname);							
					
					// Is this an idb to be published? If not, skip it.
					if(!fromContext.forQuery.getIDBNamesToOutput().contains(idbs.name+MEnvironment.sIDBSeparator+idbname))
					{						
						continue;
					}
					
					MCommunicator.writeToLog("\nAxiomatizing IDB: "+idbname);													
					
					Formula idbFormula = idbs.getIDB(idbname);
					
					//MEnvironment.errorStream.println("++ " +idbs.name+MEnvironment.sIDBSeparator+idbname);
					//MEnvironment.errorStream.println(idbFormula.hashCode());
					
					// At this point, idbFormula is using object references from the Policy instead
					// of the proper vocabulary, which would lead to unbound relation exceptions if 
					// we did not do this:
					
					/// !!! TODO is this even needed anymore?
					
					RelationAndTermReplacementV vis;
					if(initialVisitors.containsKey(idbs))
						vis = initialVisitors.get(idbs);
					else
					{
						vis = MIDBCollection.getReplacementVisitor(idbs.vocab, fromContext.forQuery.vocab);
						initialVisitors.put(idbs, vis);
					}
					
					idbFormula = idbFormula.accept(vis);
									
					// What temporary variables did the policy refer to?
					// (Order doesn't really matter here, since it's all flat universal quantifiers)
					// Some confusion in naming if someone sees the formula, but not really a difference.
					Set<Variable> freeVars = idbFormula.accept(freeVarCollector);
									
					// What is the arity of the relation?
					int idbArity = freeVars.size();					
					
					// Zero arity means either always true or always false... 
					// Would be nice to support but KodKod doesn't allow nullary relations
					if(idbArity < 1)
					{
						// Get around it by using the full arity of the LHS relation:
						idbArity = idbs.varOrderings.get(idbname).size();
						freeVars = new HashSet<Variable>(); // adding is unsupported in prior instance
						for(Variable v : idbs.varOrderings.get(idbname))
							freeVars.add(v);
					}				
					// Create a temporary relation 
					// (If tupled, attach the indexing -- uglier but desirable for correctness checking.)
					Relation therel =  MFormulaManager.makeRelation(idbs.name+MEnvironment.sIDBSeparator+idbname, idbArity);		
					
					// And bound it.
					qryBounds.bound(therel, factory.allOf(idbArity));			
				
					///////////////////////////////////////////////////////
					// Get proper ordering of freeVars
					
					List<String> actualVarNameOrder = new ArrayList<String>(idbArity);												
					List<Variable> expectedVars = idbs.varOrderings.get(idbname);
					for(Variable v : expectedVars)
					{
						// Does this expected variable actually appear, free, in the IDB?
						if(freeVars.contains(v))
							actualVarNameOrder.add(v.name());
					}
					if(idbArity != actualVarNameOrder.size())
						throw new MGEUnknownIdentifier("Arity of IDB formula did not match expected: "+idbs.name + ": "+idbname);
					Expression tup = MFormulaManager.makeExprTuple(actualVarNameOrder);										
					
					////////////////////////////////////////////////////////////
					// The relation holds IF AND ONLY IF its idb formula is true									
					////////////////////////////////////////////////////////////
					
					// Restrict the IDB Formulas to use the proper types for their free variables.
					// TODO: when we fix bounds, this will no longer be necessary
					Set<Formula> properTypes = new HashSet<Formula>();					
					for(Variable var : freeVars)
					{ 	
						// Do I know a sort for this variable?						
						
						if(idbs.varSorts.containsKey(var))
						{
							Formula atom = MFormulaManager.makeAtom(var, idbs.varSorts.get(var));
							properTypes.add(atom);
						}
					}
				

					Formula atom2 = MFormulaManager.makeAtom(tup, therel);
					Formula atom2a = MFormulaManager.makeAnd(idbFormula, MFormulaManager.makeConjunction(properTypes));					
					Formula imp = MFormulaManager.makeIFF(atom2, atom2a);
									
					// Order doesn't matter
					String prettyVarTuple = "";
					for(Variable var : freeVars)
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
					//MEnvironment.errorStream.println(imp);
					
					// ****
					// We only quantify vars that actually matter, EVEN IF the idb definition itself
					// includes more. This means that later on we need to label the output properly.
					idbToTup.put(therel.name(), prettyVarTuple);
					
				} // end for each idb in idbset
			} // end for each idbset						
			
			if(fromContext.forQuery.debug_verbosity > 1)
			{
				MEnvironment.writeOutLine("DEBUG: IDB output bi-implications created. There were "+impSet.size()+" of them.");
			}
			
			impSet.add(f); // f and imp1 and imp2 and ...
			f = MFormulaManager.makeConjunction(impSet);
		} // end of if including idbs in output								
		// ****************************************

		
		
		return f;
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

       // MEnvironment.errorStream.println("ADDED CLAUSE after "+clauses.size()+" others. It was: "+Arrays.toString(litsCopy));
        
        clauses.add(wrapper);
        return internalSolver.addClause(lits);
    }
    
    public void printClauses()
    {
    	int iClause = 0;
    	for(MIntArrayWrapper aClause : clauses)
    	{
    		MEnvironment.writeOutLine("Clause "+iClause+": "+Arrays.toString(aClause.getArray()));
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
   			// swap if need the IConstr
   			//IConstr x = result.addClause(new VecInt(aClause.getArray()));
   			result.addClause(new VecInt(aClause.getArray()));
			
   			//PrintWriter pw = new PrintWriter(MEnvironment.errorStream);
			//result.printInfos(pw, ":::: ");
			//pw.flush();
   			
			//if(x == null)
   			//MEnvironment.errorStream.println("conv: "+x + " : "+aClause.toString());
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
    					MEnvironment.errorStream.println("TAUT");
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
class SkolemReporter implements Reporter
{
	Bounds skolemBounds = null; 
	
	@Override
	public void detectedSymmetries(Set<IntSet> parts) {
	}

	@Override
	public void detectingSymmetries(Bounds bounds) {
	}

	@Override
	public void flattening(BooleanFormula circuit) {
	}

	@Override
	public void generatingSBP() {
	}

	@Override
	public void optimizingBoundsAndFormula() {
	}

	@Override
	public void skolemizing(Decl decl, Relation skolem, List<Decl> context) {
	}

	@Override
	public void solvingCNF(int primaryVars, int vars, int clauses) {
	}

	@Override
	public void translatingToBoolean(Formula formula, Bounds bounds)
	{		
		skolemBounds = bounds;		
	}

	@Override
	public void translatingToCNF(BooleanFormula circuit) {				
	}
	
}

class MPreparedQueryContext
{
	protected int maxSize;	
	protected int sufficientMaxSize;	
	
	public long msQueryCreationTime;
	public long msQueryRunTime;
	public long msQueryTuplingTime;
	
	Formula qryFormulaWithAxioms;
	
	MQuery forQuery;
	
	Map<String, Set<List<MTerm>>> includeMap = new HashMap<String, Set<List<MTerm>>>();
	
	protected MPreparedQueryContext(MQuery q, Formula qfwa, int maxsize, int hbmax, long timeCreateObject, long timeRunQuery, long timeTupling)
	{
		// Used to print intelligently		
		forQuery = q;
	
		// How big a universe did we check up to?
		maxSize = maxsize;
	
		// How big did our analysis say the HB term universe could be?
		sufficientMaxSize = hbmax;

		qryFormulaWithAxioms = qfwa;
	
		// How long did Margrave take to create the query object?
		msQueryCreationTime = timeCreateObject;
		msQueryRunTime = timeRunQuery;
		msQueryTuplingTime = timeTupling;
	}
	
	public int get_hu_ceiling()
	{
		return sufficientMaxSize;
	}
	
	public int get_universe_max()
	{
		return maxSize;
	}
	
	public MTotalInstanceIterator getTotalIterator() 
	throws MGEUnknownIdentifier, MGEManagerException, MGEBadIdentifierName
	{
		return new MTotalInstanceIterator(this);
	}

	/*public MPartialInstanceIterator getPartialIterator()
	throws MGEManagerException, MGEUnknownIdentifier, MGEBadIdentifierName
	{
		return new MPartialInstanceIterator(this);
	}*/

	public MRealizedFormulaFinder getRealizedFormulaFinder() 
	throws MUserException
	{
		return new MRealizedFormulaFinder(this);
	}

	/*public MUnrealizedFormulaFinder getUnrealizedFormulaFinder() 
	throws MUserException
	{
		return new MUnrealizedFormulaFinder(this);
	}*/

	
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
		catch(MBaseException e)
		{
			return -1; // error
		}		
	}

	public boolean isSatisfiable()
	throws MBaseException
	{
		MInstanceIterator it = getTotalIterator();
						 
		if(it.hasNext())
			return true;
		return false;
		
		// return exception if the iterator has problems; distinguish from true/false.
	}

	public void setInclude(HashMap<String, Set<List<MTerm>>> includeMap)
	{
		this.includeMap = includeMap; 
		
	}
}


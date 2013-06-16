package edu.wpi.margrave;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
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
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;
import kodkod.util.ints.IntSet;

class MCNFSpyQueryResult extends MQueryResult
{
	Translation nonTrivialTranslation;
	Bounds nonTrivialBounds;
	boolean trivialFalse;
	boolean trivialTrue;
	
	CNFSpyFactory spyFactory;
	
	MCNFSpyQueryResult(MPreparedQueryContext qr)
	{
		super(qr);				
			
		try
		{
			// Prepare a CNF to watch:
			createCNFFor(qr.qryFormulaWithAxioms);
		}
		catch(TrivialFormulaException e)
		{				
			// Either all models or no models. Set the appropriate flags.
	       	if(e.value().booleanValue() == false)
	       		trivialFalse = true;
	       	else
	       		trivialTrue = true;
	       	
	       	// Clear these out (previously set by createCNFFor)
	       	nonTrivialTranslation = null;
	       	nonTrivialBounds = null;
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
	
	void createCNFFor(Formula f)
	throws MGEManagerException, MGEUnknownIdentifier, MGEBadIdentifierName, TrivialFormulaException
	{						
		Solver qrySolver = new Solver();		
		qrySolver.options().setFlatten(true);
		qrySolver.options().setSymmetryBreaking(fromContext.forQuery.mySB);		
				
		spyFactory = new CNFSpyFactory(fromContext.forQuery.mySATFactory);
		qrySolver.options().setSolver(spyFactory);
		
		KodkodContext ctxt = makeBounds(f);				
		
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
            Translation trans = Translator.translate(ctxt.fmla, ctxt.bounds, qrySolver.options());
            
           // CNFSpy theSpy = (CNFSpy)trans.cnf();         
    		final long endTransl = System.currentTimeMillis();
    		msKodkodTransTime += (endTransl - startTransl);
            
            // Remember the clauses.
            nonTrivialTranslation = trans;  
            if(rep.skolemBounds != null)
            	nonTrivialBounds = rep.skolemBounds;
            else
            	nonTrivialBounds = ctxt.bounds;

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
	//Map<String, String> idbToTup = new HashMap<String, String>();	
	
	MQueryResult(MPreparedQueryContext fromContext)
	{
		this.fromContext = fromContext;		
	}
	
	/*protected KodkodContext makeConservativeBounds(Formula f)
	throws MGEManagerException, MGEUnknownIdentifier, MGEBadIdentifierName
	{
		
		// Create the appropriate number of atoms to use for our universe
		LinkedList<String> atoms = new LinkedList<String>();
		for(int ii=0;ii<fromContext.getCeilingUsed();ii++)
		{
			atoms.add("Atom"+ii);			
		}
		Universe u = new Universe(atoms);
		Bounds qryBounds = new Bounds(u);

		////////////////////////////////////////////////////////////
		
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

		// Testing code: to remove
		//KodkodContext foo = makeBounds(f);		
		//if(fromContext.forQuery.debug_verbosity > 1)			
		//	MEnvironment.writeOutLine("Kodkod Context Produced:\n"+foo);
		
		return new KodkodContext(f, qryBounds);
	} // end makeConservativeBounds
	*/
	
	/////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////
	protected void handleExtraDueToUNIV(Map<Relation, Set<String>> sortUpperBounds,  Set<String> atomSet)
	{
		// What if we have |UNIV| = 3, but everything else was -1? Happens when we have to
		// defer to defaults because we can't generate bounds for the query.
		
		int haveCurrently = atomSet.size();
		int numNeeded = fromContext.ceilingsToUse.get(MEnvironment.sUnivSortName).intValue();		
		for(int ii=haveCurrently+1;ii<=numNeeded;ii++)
		{
			String theAtom = "UNIV#"+ii;			
			atomSet.add(theAtom);		
			MCommunicator.writeToLog("\n!!! UNIV required an extra element "+theAtom);
			MCommunicator.writeToLog("\n !!! Propagating the new atom to ALL SORTS.");
			
			// Propagate to ALL SORTS.			
			for(MSort dt : fromContext.forQuery.vocab.sorts.values())
			{
				sortUpperBounds.get(dt.rel).add(theAtom);
			}						
		}
	}
	
	protected TupleSet wrapAsTuples(TupleFactory factory, Set<String> atoms)
	{
		if(atoms.size() == 0)
			return factory.noneOf(1);
		
		Set<Tuple> result = new HashSet<Tuple>();
		for(String s : atoms)
			result.add(factory.tuple(s));
		return factory.setOf(result);
	}
	
	protected void makeSortLowerBound(MSort t, Map<Relation, Set<String>> lowerBounds)
	{					
		// TODO
		// We do not have exact sig boundries, so no lower-bounds for now.
		// (We say |A| <= k, not |A| == k. If we were exact, could follow Alloy's example for them.)
		
	}
	protected void makeSortUpperBound(MSort t, Map<Relation, Set<String>> upperBounds, Set<String> atomSet)
	{
		// fromContext.ceilingsToUse contains UPPER BOUND sizes for each sort.			
				
		int numNeeded = fromContext.ceilingsToUse.get(t.name).intValue();
		
		if(t.subsorts.size() <= 0)
		{
			// Base case. Build atoms.
						
			for(int ii=1;ii<=numNeeded;ii++)
			{
				String theAtom = t.name+"#"+ii;
				upperBounds.get(t.rel).add(theAtom);
				atomSet.add(theAtom);
				MCommunicator.writeToLog("\n!!! Leaf sort "+t+" gets new element "+theAtom);
				propagateNewAtomInUpperBounds(upperBounds, t, theAtom);
			}
									
		}
		else
		{
			// Recursive case. Call for children. 
			for(MSort childt : t.subsorts)
			{
				makeSortUpperBound(childt, upperBounds, atomSet);
				upperBounds.get(t.rel).addAll(upperBounds.get(childt.rel));
			}

			// Do we need to add more atoms that are NOT in the children?
			int haveCurrently = upperBounds.get(t.rel).size();
			for(int ii=haveCurrently+1;ii<=numNeeded;ii++)
			{
				String theAtom = t.name+"#"+ii;
				upperBounds.get(t.rel).add(theAtom);
				atomSet.add(theAtom);				
				MCommunicator.writeToLog("\n!!! Sort "+t+" gets new element "+theAtom+" to meet ceiling="+numNeeded+". Currently had: "+haveCurrently);
				propagateNewAtomInUpperBounds(upperBounds, t, theAtom);				
			}						
			
			// If any child* has size -1, its upper bound must contain the upper bound of this type
			// (Since we've already resolved user bounds + computed bounds, -1 means don't know what to do.
			//  Use defaults, etc)
			for(MSort childt : fromContext.forQuery.vocab.buildSubSortSet(t))
			{
				int childnum = fromContext.ceilingsToUse.get(childt.name);
				if(childnum == -1)
				{
					MCommunicator.writeToLog("\nChild "+childt+" had -1 as ceiling to use. Propagating upper bounds from "+t);
					upperBounds.get(childt.rel).addAll(upperBounds.get(t.rel));
				}
			}
			
		}	// end recursive case		
	} // end method
	
	
	protected void propagateNewAtomInUpperBounds(Map<Relation, Set<String>> upperBounds, MSort t, String theAtom)
	{
		// Step 1: Propagate to subsorts (if any). Since this atom was
		// created locally for sort t, we need to allow it to inhabit t's children.
		Set<MSort> propagateTo = fromContext.forQuery.vocab.buildSubSortSet(t);
		for(MSort dt : propagateTo)
		{
			MCommunicator.writeToLog("\n !!! Bounds for "+t.rel+" propagating "+theAtom+" to subsort: "+dt);
			upperBounds.get(dt.rel).add(theAtom);
		}		
		
		// This step is un-necessary if incomparable sorts are disjoint.
		// If the coercion goes UP the hierarchy, it is covered by the recursion in makeSortUpperBound.
		// If the coercion goes DOWN the hierarchy, the block above covers it.
		// If it goes ACROSS the hierarchy, it can't actually be used, since the two sorts are disjoint.
		
		// Step 2: Propagate the atom... 
		// Ask FormulaSigInfo what sorts these atoms may get propagated to.
		// If A can propagate to B via Skolem coercions, make sure our 
		// UPPER bounds respect that.
//		for(LeafExpression r : fromContext.herbrandBounds.sortsThisCanBeCoercedTo(t.rel))
//		{
//			if(!upperBounds.containsKey(r))
//			{
//				throw new MNotASortException("MQueryResult: LeafExpression "+r+" was not a sort relation.");
//			}
//			
//			MCommunicator.writeToLog("\n!!! Bounds for "+t.rel+" appending to "+r);
//			
//			upperBounds.get(r).addAll(upperBounds.get(t.rel));
//		}

	}
	
	protected TupleSet makePredicateUpperBound(TupleFactory factory, Bounds qryBounds, MPredicate p)
	{
		//System.err.println(p);		
		boolean first = true;
		TupleSet myBounds = factory.allOf(p.type.size()); // dummy value
		for(MSort t : p.type)
		{			
			if(first)
			{
				myBounds = qryBounds.upperBound(t.rel);
				first = false;
			}
			else
			{
				myBounds = myBounds.product(qryBounds.upperBound(t.rel));
			}
		}		
		
		return myBounds;		
	}


	
	protected KodkodContext makeBounds(Formula f)
	throws MGEManagerException, MGEUnknownIdentifier, MGEBadIdentifierName
	{		
		// Create bounds on relations 						
		MCommunicator.writeToLog("\nCreating bounds ...");		
		
		Map<Relation, Set<String>> sortUpperBounds = new HashMap<Relation, Set<String>>();
		Map<Relation, Set<String>> sortLowerBounds = new HashMap<Relation, Set<String>>();
		Set<String> atomSet = new HashSet<String>();
		
		// Initialize
		for(MSort t : fromContext.forQuery.vocab.sorts.values())
		{
			sortLowerBounds.put(t.rel, new HashSet<String>());
			sortUpperBounds.put(t.rel, new HashSet<String>());
		}
		
		// Bound the type predicates
		// makeSortLowerBound and makeSortUpperBound will walk the sort tree and
		// automatically assign bounds for the subsorts! But if we want to name
		// atoms the way Alloy does, we have to create sets of atoms BEFORE the universe
		// and hence before the Bounds object. So build some maps.
		for(MSort t : fromContext.forQuery.vocab.getTopLevelSorts())
		{					
			makeSortUpperBound(t, sortUpperBounds, atomSet); 
			makeSortLowerBound(t, sortLowerBounds);						
		}
				
		// Now suppose UNIV has more than the sum of its children.
		// This happens if, e.g., all sorts are infinitary and we just use default.
		handleExtraDueToUNIV(sortUpperBounds, atomSet);
		
		// the special SRhelper atom:
		String srhelper = "SRHELPER"; 
		if(fromContext.forQuery.realizedIndexing.size() > 0)
		{
			// this atom isn't in any "real" upper bounds.
			atomSet.add(srhelper);
		}
		
		// Create the universe!
		Universe u = new Universe(atomSet);
		TupleFactory factory = u.factory();
		Bounds qryBounds = new Bounds(u);
		
		// Declare our pre-created sort bounds to Kodkod 
		for(MSort t : fromContext.forQuery.vocab.sorts.values())
		{					
			// Takes collection<Tuple>, not collection<String>
			TupleSet upperBound = wrapAsTuples(factory, sortUpperBounds.get(t.rel));
			TupleSet lowerBound = wrapAsTuples(factory, sortLowerBounds.get(t.rel));
			qryBounds.bound(t.rel, lowerBound, upperBound);
		}
		
		// Non-sort predicates
		for(MPredicate p : fromContext.forQuery.vocab.predicates.values())
			qryBounds.bound(p.rel, makePredicateUpperBound(factory, qryBounds, p));				

		// constants
		for(MConstant c : fromContext.forQuery.vocab.constants.values())
		{
			MEnvironment.writeToLog("\n  Bounding constant: "+c);
			qryBounds.bound(c.rel, makePredicateUpperBound(factory, qryBounds, c));
		}

		// functions
		for(MFunction fn : fromContext.forQuery.vocab.functions.values())
			qryBounds.bound(fn.rel, makePredicateUpperBound(factory, qryBounds, fn));				
				
		
		
		// ****************************************
		// Show realized works differently now: don't blindly axiomatize
		// the idb. Instead, construct a special unary, lone relation for
		// each candidate and each goal.
		// ****************************************
				
		if(fromContext.forQuery.realizedIndexing.size() > 0)
		{						
			Set<Formula> impSet = new HashSet<Formula>();
			
			//MEnvironment.errorStream.println(fromContext.forQuery.idbNamesToOutput);
			MCommunicator.writeToLog("\nThere are SR indexings to apply.");
			
			// Decisions, Rule Applicability, etc. (IDBs at policy level)
			// run through each, checking for indexing for SRs
			for(MIDBCollection idbs : fromContext.forQuery.myIDBCollections.values())
			{							
				// What is this policy publishing?
			 
				for(String idbname : idbs.idbKeys())
				{																						
					
					// Is this an idb to be published? If not, skip it.
					if(!fromContext.forQuery.realizedIndexing.keySet().contains(idbs.name+MEnvironment.sIDBSeparator+idbname))
					{						
						continue;
					}
					
					MCommunicator.writeToLog("\nHandling SR for IDB: "+idbname);	
					Formula idbFormula = idbs.getIDB(idbname);
				
					//int idbDeclaredArity = idbs.varOrderings.get(idbname).size();
					//List<Variable> idbVars = idbs.varOrderings.get(idbname);
					
					for(List<MTerm> args : fromContext.forQuery.realizedIndexing.get(idbs.name+MEnvironment.sIDBSeparator+idbname))
					{

						// Create a temporary relation
						// for EACH SR indexing.		
						Relation therel =  MFormulaManager.makeRelation(MQuery.makeSRHelper(idbs.name, idbname,args), 1);
						TupleSet helperset = factory.setOf(srhelper);																			
						
						// And bound it.
						MCommunicator.writeToLog("\nBounding "+therel+" above by: "+factory.setOf(helperset));
						qryBounds.bound(therel, factory.setOf(helperset));										
					
						////////////////////////////////////////////////////////////
						// The relation holds IF AND ONLY IF its idb formula is true									
						////////////////////////////////////////////////////////////

						Formula atom2 = therel.some(); // the only "quantification" needed here
						Formula atom2a = idbFormula;					
						Formula imp = MFormulaManager.makeIFF(atom2, atom2a);
									
						//idbs.varSorts.get(var)					
					
						// May have many idbs, so don't just do .makeAnd over and over.
						// Save in a set and make one Conjunction NaryFormula.
					
						impSet.add(imp);
						
					} // end for each indexing
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
				
	
		if(fromContext.forQuery.debug_verbosity > 1)			
			MEnvironment.writeOutLine("Bounds Produced:\n"+qryBounds);
		
		////////
		// Finally: quantify the query variables before passing to kodkod context:
		for(Decl d : fromContext.forQuery.qDecls)
		{
			f = MFormulaManager.makeExists(f, d);
		}
		
		KodkodContext result = new KodkodContext(f, qryBounds); 
		MCommunicator.writeToLog("\nBounds produced. Returning new context: "+result);
		
		return result;
	} // end makeBounds()

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
	// Sort Name ---> Ceiling
	Map<String, Integer> ceilingsToUse;
	Map<String, Integer> ceilingsSufficient;
	FormulaSigInfo herbrandBounds;
		
	public long msQueryCreationTime;
	public long msQueryKodkodTime;
	
	Formula qryFormulaWithAxioms;
	
	MQuery forQuery;
	
	public Set<String> warnings = new HashSet<String>();
	
	Map<String, Set<List<MTerm>>> includeMap = new HashMap<String, Set<List<MTerm>>>();
	
	public int getCeilingUsed()
	{
		if(ceilingsToUse.containsKey(MEnvironment.sUnivSortName))
			return ceilingsToUse.get(MEnvironment.sUnivSortName);
		return -1;
	}
	public int getCeilingComputed()
	{		
		if(ceilingsSufficient.containsKey(MEnvironment.sUnivSortName))
			return ceilingsSufficient.get(MEnvironment.sUnivSortName);
		return -1;
	}
	
	protected MPreparedQueryContext(MQuery q, Formula qfwa,
			FormulaSigInfo herbrandBounds,
			long timePreprocessing)
	{
		// Used to print intelligently		
		forQuery = q;

		this.herbrandBounds = herbrandBounds;
		
		// How big did our analysis say the HB term universe could be?	
		ceilingsSufficient = herbrandBounds.getCountMapping();			
				
		// How big a universe did we check up to?
		ceilingsToUse = resolveSortCeilings(ceilingsSufficient);
				
		// The query formula. STILL UNQUANTIFIED
		qryFormulaWithAxioms = qfwa;
	
		// How long did Margrave take to create the query object?
		msQueryCreationTime = timePreprocessing;
		
		// Time to run so far. (Will add to this when invoking Kodkod)
		msQueryKodkodTime = 0;
	}
	
	/**
	 * Our term-counting algorithm has given us ceilings on all sorts (-1 for infinity).
	 * The user has also given us ceilings. We need to reconcile our bounds with the user's.
	 * 
	 * We may also have to sanity-check the user-provided bounds. For instance, if B < A
	 * and user(A) = 2 and user(B) = 3, we need to resolve the mis-match.
	 * 
	 * @param computedSortCeilings
	 * @return Ceilings, filtered to respect what the user wants.
	 */
	private Map<String, Integer> resolveSortCeilings(Map<String, Integer> computedSortCeilings)
	{		
		Map<String, Integer> userSortCeilings = new HashMap<String, Integer>(MEnvironment.globalUserSortCeilings);
		for(String sname : forQuery.localCeilings.keySet())
		{
			// local ceilings over-rule global ceilings
			userSortCeilings.put(sname, forQuery.localCeilings.get(sname));
		}		
		
		MCommunicator.writeToLog("\nIn resolveSortCeilings. userSortCeilings="+userSortCeilings+" and computedSortCeilings="+computedSortCeilings);
		
		// ALLOY (p128): "You can give bounds on... ... so long as whenever a signature
		//                has been given a bound, the bounds of its parent and of any other
		//                extensions of the same parent can be determined."
				
		// User-provided bounds are in:
		// MEnvironment.sortCeilings 
		// not guaranteed to be complete (may not contain a bound for many sorts)
	
		// Start w/ computed values
		Map<String, Integer> result = new HashMap<String, Integer>(computedSortCeilings);			
				
		// If we have an empty universe, invalidate the results and use 
		// user-supplied ceiling, just as if there was an infinite universe.
		if(!result.containsKey(MEnvironment.sUnivSortName) ||
				result.get(MEnvironment.sUnivSortName).intValue() <= 0)
		{
			for(String sortName : result.keySet())
			{
				result.put(sortName, -1);
			}
		}
		
		// -1 means "no bound given by anybody; propagate from supersorts."
		
		// Reconcile w/ user:
		// User given, Alg > -1
		//   If User => Alg, just set it. Will create extra in the appropriate sort.
		//   If User < Alg, set it. Set all this sort's children to -1. ACTIVATE WARNING. 
		// User given, Alg = -1
		//   Use user's bound. For all children, if user didn't give a bound, set to -1
		//   and will propagate down. ACTIVATE WARNING.
		// User NOT given, Alg > -1
		//   Use generated bound.
		// User NOT given, Alg = -1
		//   Leave as -1, and bounds creation will propagate down. ACTIVATE WARNING.

		List<MSort> todo = new ArrayList<MSort>(forQuery.vocab.sorts.values());
		
		// Fake "sort" for UNIV. We MUST resolve user and computed bounds for overall universe.
		MSort univ = new MSort("univ");
		univ.subsorts.addAll(forQuery.vocab.sorts.values());
		todo.add(univ);
		
		while(todo.size() > 0)
		{
			MSort s = todo.get(0);
			todo.remove(0);
			MCommunicator.writeToLog("\nResolving for sort="+s);
			
			if(userSortCeilings.containsKey(s.name)) // User given!
			{
				if(computedSortCeilings.get(s.name) == -1) // alg = -1
				{
					result.put(s.name, userSortCeilings.get(s.name));					
					warnings.add("Margrave could not compute sufficient bounds for type "+s.name+
							". Using user-provided bound of "+userSortCeilings.get(s.name)+".");
					for(MSort child : forQuery.vocab.buildSubSortSet(s))
					{
						// set-all-descends-to-neg1-if-user-didnt-give.
						todo.remove(child);
						if(userSortCeilings.containsKey(child.name))
							result.put(child.name, userSortCeilings.get(child.name));
						else
							result.put(child.name, -1);
					}
				}
				else if(computedSortCeilings.get(s.name) <= userSortCeilings.get(s.name))
				{
					result.put(s.name, userSortCeilings.get(s.name));
				}
				else // computed > userceiling
				{
					result.put(s.name, userSortCeilings.get(s.name));
					warnings.add("Margrave guarantees that "+computedSortCeilings.get(s.name)+
							" is a suffient bound for type "+s.name+
							". Instead it is using the user-provided bound of "+
							userSortCeilings.get(s.name)+", which is smaller.");
					
					//for(MSort child : forQuery.vocab.buildSubSortSet(s))
					//{
					//	// set-all-descends-to-neg1-if-user-didnt-give.
					//	todo.remove(child);
					//	if(userSortCeilings.containsKey(child.name))
					//		result.put(child.name, userSortCeilings.get(child.name));
					//	else
					//		result.put(child.name, -1);
					//}
				}
			}
			else // user not given!
			{
				if(computedSortCeilings.get(s.name) == -1)
				{
					// Leave as -1.
					warnings.add("Margrave could not compute sufficient bounds for type "+s.name+" and none were provided.");
				}
				else
				{
					// use computed bound
				}
			}
		}
		
		// Apply defaults if needed
		if(result.get(MEnvironment.sUnivSortName).intValue() < 1)
			result.put(MEnvironment.sUnivSortName, MEnvironment.topSortCeilingOfLastResort);		
					
		MCommunicator.writeToLog("\nresolveSortCeilings done. Returning "+result);
		return result;
		
		
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
		try
		{
			MInstanceIterator it = getTotalIterator();
			
			int count = 0; 
			while(it.hasNext()) 
			{ 
				try 
				{ 
					 @SuppressWarnings("unused")
					 Instance sol = it.next().getFacts();
					 count++;
				}
				catch(MGENoMoreSolutions e)
				{} 
			
				// There may be billions of scenarios.
				// Don't make Margrave freeze by trying to count them all.
				if(count > MEnvironment.scenarioCountingMax)
					return -1;				
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


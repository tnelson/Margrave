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

import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Evaluator;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.bool.BooleanFormula;
import kodkod.engine.config.Reporter;
import kodkod.engine.fol2sat.UnboundLeafException;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.*;
import kodkod.util.ints.IntSet;

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
	
		Solution sol;
		try
		{
			sol = kodkodIterator.next();
		}
		catch(UnboundLeafException e)
		{
			throw new MUserException("Unknown relation, type, or constant.\n"+e.getMessage());
		}

		// Count the time reported in the FIRST reply (may be unsatisfiable)
		if(firstSolution)
		{
			msKodkodSolveTime += sol.stats().solvingTime();
			msKodkodTransTime += sol.stats().translationTime();
			fromContext.msQueryKodkodTime = msKodkodSolveTime + msKodkodTransTime;
			firstSolution = false;
			
			if(fromContext.forQuery.debug_verbosity > 1)
			{
				MEnvironment.writeOutLine("DEBUG: Beginning a new Kodkod solution iterator.\nTranslation time for this iterator was: " + 
						sol.stats().translationTime()+"\nSolving time for this iterator was: "+sol.stats().solvingTime());
				MCommunicator.writeToLog("\nprepareNext asked Kodkod for a solution. Kodkod time was: "+fromContext.msQueryKodkodTime);
			}			
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

		if(MCommunicator.bMinimalModels)
		{							
			MyReporter myReporter = new MyReporter();
			qrySolver.options().setSolver(new MinimalSolverFactory(myReporter));
			qrySolver.options().setReporter(myReporter);
		}
		else
		{
			qrySolver.options().setSolver(SATFactory.DefaultSAT4J);			
		}	
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
		// NOTE:
		// For #:include, evaluation here suffices. No need to include
		// the IDB in the query. But for show-realized, we need to include
		// the IDB. Hence the apparently duplicate code.
		
		// hasNext prepares the next solution (if any exists)
		if(!hasNext())
			throw new MGENoMoreSolutions("No more solutions exist for the query.");
										
		// Make room for the next solution to come.
		MSolutionInstance result = the_next;				
		the_next = null;
				
		// Add INCLUDE facts:
		for(String relName : fromContext.includeMap.keySet())
		{
			// Check NOW, because we will be adding the relation later on! Need to do this
			// before we pass into inner loop.
			if(fromContext.includeMap.get(relName).size() < 1)
				continue;
			Relation theRelation = null;
			Relation theNotRelation = null;
			boolean isEDB = false;
			for(List<MTerm> args : fromContext.includeMap.get(relName))
			{
				theRelation = MFormulaManager.makeRelation(relName, args.size());
				isEDB = result.getFacts().relations().contains(theRelation);	
				theNotRelation = MFormulaManager.makeRelation("NOT_"+relName, args.size());
				break;
			}
						
			for(List<MTerm> args : fromContext.includeMap.get(relName))
			{
				// This instance should be modifiable. So pass back INCLUDE facts as facts in relations.
				// (This way is far more structured than, say, a string annotation.) 
				MCommunicator.writeToLog("\n\n Checking INCLUDE for "+relName+args);						
								
				// What does this tuple map to in the model?
				TupleFactory factory = result.getFacts().universe().factory();
				Tuple theTuple = factory.tuple(termsToAtoms(args, result.getFacts()));		
				
				// Running set of tuples (ONLY TUPLES OF INTEREST) in the IDB
				TupleSet tuplesInIDB = factory.noneOf(args.size());
				
				// Running set of tuples (ONLY TUPLES OF INTEREST) not in the IDB
				TupleSet tuplesNotInIDB = factory.noneOf(args.size());
				
				// Is it in the relation? First check to see if this is an EDB relation:
				boolean bIsInRelation = false;
				if(isEDB)
				{				
					// Seems silly to support EDBs at all... they will be included anyway. Ignore...
					//bIsInRelation = result.getFacts().relationTuples().get(theRelation).contains(theTuple);
					continue;
				}
				else
				{
					// Need to get the fmla for that relation and evaluate it in the model.
					Evaluator theEvaluator = new Evaluator(result.getFacts());
					
					// Is this a saved query or a policy IDB? (For now... dot.)
					String[] theSplit = relName.split(MEnvironment.sIDBSeparatorRegExp);	
					
					MCommunicator.writeToLog("\nTrying to INCLUDE; relation was not an EDB. Split was: "+Arrays.toString(theSplit));
					
					if(theSplit.length == 1)
					{
						MIDBCollection pol = MEnvironment.getPolicyOrView(relName);
						Formula idbf = MEnvironment.getOnlyIDB(relName);
						if(pol == null || idbf == null)
							throw new MUserException("Margrave could not find a saved query named: "+relName);
						idbf = MCommunicator.performSubstitution(relName, pol, idbf, args);
						idbf = addFreeVarsForInclude(idbf, result.getFacts());
						bIsInRelation = theEvaluator.evaluate(idbf);
					}
					else
					{
						// not saved query, must be a policy idb
						String collName = theSplit[0];
						String relationName = theSplit[1];
						MIDBCollection pol = MEnvironment.getPolicyOrView(collName);
						if(pol == null)
							throw new MUserException("Unknown policy: "+collName);
																	
						// throws exception rather than returning null						
						Formula idbf = MCommunicator.validateDBIdentifier(collName, relationName);						
						idbf = MCommunicator.performSubstitution(relationName, pol, idbf, args);
						idbf = addFreeVarsForInclude(idbf, result.getFacts());	
						//MCommunicator.writeToLog("\nEvaluating: "+idbf+" in model=\n"+result.getFacts());
						bIsInRelation = theEvaluator.evaluate(idbf);
					}
															
				}
				
				if(bIsInRelation)
				{
					tuplesInIDB.add(theTuple);
					// Instance .add: "Maps the given relation to the given tuple set."
					// So .add is really .set. Need to include what's already there.
					TupleSet alreadyIn = result.getFacts().tuples(theRelation);
					if(alreadyIn != null)
					{
						// Check above because if this is the first tuple to add,
						// we won't have added the IDB relation to the instance yet.
						tuplesInIDB.addAll(alreadyIn);
					}
					result.getFacts().add(theRelation, tuplesInIDB);	
					MCommunicator.writeToLog("\n The atomic formula was TRUE. Adding to relation.");
					MCommunicator.writeToLog("\n There are now "+result.getFacts().relationTuples().get(theRelation).size()+" tuples in the relation.");
				}
				else
				{
					tuplesNotInIDB.add(theTuple);
					TupleSet alreadyInNot = result.getFacts().tuples(theNotRelation);
					if(alreadyInNot != null)
					{
						// Check above because if this is the first tuple to add,
						// we won't have added the IDB relation to the instance yet.
						tuplesNotInIDB.addAll(alreadyInNot);
					}
					result.getFacts().add(theNotRelation, tuplesNotInIDB);
					MCommunicator.writeToLog("\n NOT true. Adding to not-relation.");
					MCommunicator.writeToLog("\n There are now "+result.getFacts().relationTuples().get(theNotRelation).size()+" tuples in the NOT relation.");
				}						
			}
		}
		
		return result;
	}
	
	Relation getVariableRelationFromModel(Instance model, String name)
	{
		String lookFor = "$"+name;
		for(Relation r : model.relations())
		{
			if(r.name().equals(lookFor))
				return r;			
		}
		
		throw new MUserException("Margrave tried and failed to find the Skolem relation for variable "+name+
					" denoted in instance: "+model+".");

	}
	
	Formula addFreeVarsForInclude(Formula fmla, Instance model)
	{
		final Set<Variable> freeVars = fmla.accept(new FreeVariableCollectionV());
		Formula newFmla = fmla;
		
		for(Variable v : freeVars)
		{
			Relation skolemVRelation = getVariableRelationFromModel(model, v.name());
			
			Decls theDecl = MFormulaManager.makeOneOfDecl(v, skolemVRelation);
			newFmla = MFormulaManager.makeExists(newFmla, theDecl);
		}
		
		return newFmla;
	}
	
	Object termToAtom(MTerm aterm, Instance model)
	{
		// What atom does <aterm> denote in <model>?
		// Recur for nested terms.
		
		Evaluator theEvaluator = new Evaluator(model);
		
		if(aterm instanceof MConstantTerm)
		{
			MConstantTerm aconstantterm = (MConstantTerm) aterm;			
			Relation r = (Relation) aconstantterm.expr;
			if(!model.contains(r))
				throw new MUserException("Margrave tried and failed to find what the term "+aterm+
						" denoted in instance: "+model+".\nModel did not contain a relation for the outermost part of the term.");
			
			TupleSet theTuples = model.relationTuples().get(r);
			if(theTuples.isEmpty())
				throw new MUserException("Margrave tried and failed to find what the term "+aterm+
						" denoted in instance: "+model+".\nModel did not contain an atom for the term.");
			
			Tuple theTuple = theTuples.iterator().next();
			return theTuple.atom(0);
		}
		if(aterm instanceof MVariableTerm)
		{
			MVariableTerm avariableterm = (MVariableTerm) aterm;	
			// Do NOT do 
			// Relation r = MFormulaManager.makeRelation("$"+avariableterm.variableName, 1); // assume skolemized
			// because **Kodkod** produces the Skolem relation, and does not go through MFormulaManager. Instead:
			
			// Also cannot do
			//TupleSet theTuples = theEvaluator.evaluate(avariableterm.expr);
			// because Skolemization replaces the original relation...

			
			Relation r = getVariableRelationFromModel(model, avariableterm.variableName);
			
			
			TupleSet theTuples = model.relationTuples().get(r);
			if(theTuples.isEmpty())
				throw new MUserException("Margrave tried and failed to find what the term "+aterm+
						" denoted in instance: "+model+".\nModel did not contain an atom for the term.");
			
			Tuple theTuple = theTuples.iterator().next();
			return theTuple.atom(0);
		}
		else
		{
//			throw new MUserException("INCLUDE is not yet supported for formulas that contain complex terms.");
			
			MFunctionTerm afunctionterm = (MFunctionTerm)aterm;
			TupleSet theTuples = theEvaluator.evaluate(afunctionterm.expr);
					
			Tuple theTuple = theTuples.iterator().next();
			return theTuple.atom(0);			
		}
	}
	
	List<Object> termsToAtoms(List<MTerm> terms, Instance model)
	{
		List<Object> atoms = new ArrayList<Object>(terms.size());		
		for(MTerm aterm : terms)
			atoms.add(termToAtom(aterm, model));
		return atoms;
	}

}
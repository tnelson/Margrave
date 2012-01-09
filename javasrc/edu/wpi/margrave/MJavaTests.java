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

//import java.lang.management.ManagementFactory;
//import java.lang.management.ThreadMXBean;
import java.util.*;

import org.sat4j.core.VecInt;
import org.sat4j.maxsat.WeightedMaxSatDecorator;
//import org.sat4j.minisat.SolverFactory;
//import org.sat4j.opt.MaxSatDecorator;
import org.sat4j.pb.IPBSolver;
import org.sat4j.pb.OptToPBSATAdapter;
//import org.sat4j.pb.tools.XplainPB;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IConstr;
//import org.sat4j.specs.IProblem;
import org.sat4j.specs.ISolver;
import org.sat4j.specs.TimeoutException;
import org.sat4j.tools.ModelIterator;
//import org.sat4j.tools.xplain.Explainer;
import org.sat4j.tools.xplain.Xplain;

import kodkod.engine.*;
import kodkod.instance.*;
import kodkod.ast.*;
import kodkod.engine.Solution.Outcome;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;


public class MJavaTests
{

	public static boolean testIsSatisfiable(Formula fmla, MVocab voc, int size, Map<Variable, Expression> varSorts)
	{
		// Used for internal test cases where there may not be an XML query or MQuery object.
		// Runs Kodkod to see if fmla is satisfiable under voc.

		
		/////////////////////////////////////////////////////////////				
		// First, close fmla existentially over its free variables.
		Set<Variable> freeVars = fmla.accept(new FreeVariableCollectionV());
		for(Variable v : freeVars)
		{
			Expression theRel;
			if(!varSorts.containsKey(v))
				theRel = Expression.UNIV;
			else
				theRel = varSorts.get(v);
			Decl d = MFormulaManager.makeOneOfDecl(v, theRel);
			fmla = MFormulaManager.makeExists(fmla, d);
		}
				
		// And make sure the vocabulary's constraints are respected!
		fmla = MFormulaManager.makeAnd(fmla, voc.getAxiomFormulaNoEffectOnSize());
		fmla = MFormulaManager.makeAnd(fmla, MFormulaManager.makeConjunction(voc.getAxiomFormulasThatMayAffectSize()));	
		
		/////////////////////////////////////////////////////////////
		// Now apply Kodkod
		
		Solver qrySolver = new Solver();			
		//qrySolver.options().setFlatten(true);
		qrySolver.options().setSolver(SATFactory.DefaultSAT4J);
		//qrySolver.options().setSymmetryBreaking(fromResult.forQuery.mySB);
	
		Set<String> theUniv = new HashSet<String>();
		for(int ii=0;ii<size;ii++)
			theUniv.add("Atom"+ii);
		Universe u = new Universe(theUniv);
		
		Bounds qryBounds = new Bounds(u);
		TupleFactory factory = u.factory();			

		for(MSort t : voc.sorts.values())
		{
			qryBounds.bound(t.rel, factory.allOf(t.rel.arity()));					
		}
		for(MPredicate p : voc.predicates.values())
		{
			qryBounds.bound(p.rel, factory.allOf(p.rel.arity()));	
		}		
		
		Solution sol = qrySolver.solve(fmla, qryBounds);
		
		return (sol.outcome().equals(Outcome.SATISFIABLE) || 
				sol.outcome().equals(Outcome.TRIVIALLY_SATISFIABLE));
	}
	
	
	public static void exitJavaTests(int arg)
	{
		MEnvironment.flushBuffers("Java Tests");
		System.exit(arg);
	}

	
	public static void main(String[] args)
	throws MBaseException
	{
		//testSAT();
		//System.exit(1);
		
		MEnvironment.writeOutLine("Starting java-based tests.");			
		
		MCommunicator.bDoLogging = true;
		MCommunicator.initializeLog();
		MCommunicator.writeToLog("\n\n");
		
		/////////////////////////////////////////////////////////////
		RelationAndTermReplacementV.unitTests();
		/////////////////////////////////////////////////////////////
		MQuery.unitTest();
		/////////////////////////////////////////////////////////////
		MVocab.unitTest();
		/////////////////////////////////////////////////////////////		
		MPolicyLeaf.unitTests();
		/////////////////////////////////////////////////////////////		
		MPolicySet.unitTests();
		/////////////////////////////////////////////////////////////
		MCommunicator.unitTests();
		/////////////////////////////////////////////////////////////
		//MRealizedFormulaFinder.unitTests();
				
		/////////////////////////////////////////////////////////////
		try 
		{
			FormulaSigInfo.unitTests();
		} catch (MNotASortException e) {
			
			MEnvironment.writeOutLine(">>> FormulaSigInfo tests FAILED: exception thrown!\n"+e.getLocalizedMessage());
		}		
		/////////////////////////////////////////////////////////////	
		
		
		
		
		// Do not put anything after this point except the call to exitJavaTests.
		/////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////
		exitJavaTests(1);
	
		// do_time_tupling_new();
		// 		exitJavaTests(1);

		// do_time_tupling();
		// 		exitJavaTests(1);

	//	benchmarkXACML();
	//			exitJavaTests(1);
						
/*
		
		
				
		
		// Main test blocks		
		runTests();		
		
		// 		exitJavaTexts(1);

		
		// tupling benchmarks
		MEnvironment.writeOutLine("\n\nTUPLING BENCHMARKS\n\n");
		// Not _new; the new timer uses unary preds for address bits
		do_time_tupling();					
				

	
		
		
		// Everything from tests should be out of scope now.
		// Test that the weak references in the manager are doing their job.
		//System.gc();
		//MEnvironment.writeOutLine("State of Formula Manager after forced gc call: ");
		//MFormulaManager.printStatistics();

		//System.gc();
		//MFormulaManager.ping();
		//System.gc();
		//MFormulaManager.printStatistics();

*/
		/*
		 * Leave everything after the above printStatistics() call empty.
		 */
	}
	
/*	public static void testSAT()
	{
		// "Hello World" for MaxSAT
		// 1 or (2 and !3)
		// == 1 or 2; 1 or !3
		
		// 5 models overall. Two minimal ones:
		// [-1, 2, -3] 
		// and  [1, -2, -3]
		
		try
		{	
			ISolver solver = org.sat4j.minisat.SolverFactory.newDefault();
			Xplain<ISolver> xpl1 = new Xplain<ISolver>(solver);
			xpl1.newVar(3);		
			xpl1.addClause(new VecInt(new int[] {1, 2}));
			xpl1.addClause(new VecInt(new int[] {1, -3}));
			xpl1.addClause(new VecInt(new int[] {-1}));
			//xpl1.addClause(new VecInt(new int[] {2}));xpl1.addClause(new VecInt(new int[] {3}));
		
			//////////////////////////////////////////////
			if(xpl1.isSatisfiable())
			{
				// >> iterator must decorate the Xplainer.
				ModelIterator mi = new ModelIterator(xpl1);
				System.err.println("Satisfiable. Models:");
				
				while(mi.isSatisfiable())
				{
					System.err.println(Arrays.toString(mi.model()));
					System.err.println(Arrays.toString(mi.primeImplicant()));
					System.err.println();
				}
				
				// Returns null if no assumptions used
				//System.err.println(mi.unsatExplanation());
							
				System.err.println("Out of models. Explanation:");
				System.err.println(xpl1.explain());	
				// This works ^. 
				
			}
			else
			{
				System.err.println("Unsatisfiable.");
				System.err.println(xpl1.explain());	
			}
			
			//////////////////////////////////////////////
			// Requires org.sat4j.maxsat
			//////////////////////////////////////////////
			
			IPBSolver pbsolver = org.sat4j.pb.SolverFactory.newDefault();
			//XplainPB x = new XplainPB(pbsolver);
			
			WeightedMaxSatDecorator d = new WeightedMaxSatDecorator(pbsolver);
			d.newVar(3);		
			d.addHardClause(new VecInt(new int[] {1, 2}));
			d.addHardClause(new VecInt(new int[] {1, -3}));
			
			//d.addLiteralsToMinimize(new VecInt(new int [] {1, 2, 3} ));
			d.addSoftClause(1, new VecInt(new int[] {-1}));
			d.addSoftClause(1, new VecInt(new int[] {-2}));
			d.addSoftClause(1, new VecInt(new int[] {-3}));
						
			if(d.admitABetterSolution())
			{
			
				
				// prime impl not OK given cnf conversion
				
				//ModelIterator mi = new ModelIterator(d);
				System.err.println("MaxSAT was satisfiable. Objective function value was: "+d.calculateObjective());
				System.err.println("Models at that objective function value are:");
				
				int funcValue = d.getObjectiveValue().intValue();
							
				while(d.admitABetterSolution() && d.getObjectiveValue().intValue() == funcValue)
				{
					System.err.println(Arrays.toString(d.model()));
					System.err.println(Arrays.toString(d.primeImplicant()));
					System.err.println(d.isOptimal());
					//System.err.println(Arrays.toString(mi.model()));					
					
					int[] negationOfThisModel = new int [d.nVars()];
					for(int iVar=1;iVar<=d.nVars();iVar++)
					{
						if(d.model(iVar)) { negationOfThisModel[iVar-1] = -iVar; }
						else { negationOfThisModel[iVar-1] = iVar; };						
					}
					
					// This stops us at 1 solution. (Not sure why).
					// Ahh, is it used to abandon a potentially sub-optimal soln?
					//d.discardCurrentSolution();
					
					// Note: this might be a very inefficient way of iterating through models. ModelIterator doesn't seem to work with maxsat...
					// If it slows down in practice, ask. 														
					d.addHardClause(new VecInt(negationOfThisModel));
				}
								
			}
			else
			{
				System.err.println("MaxSAT was unsatisfiable.");
			}
			
			
			
			//////////////////////////////////////////////
			// Requires org.sat4j.maxsat
			//////////////////////////////////////////////
			
			pbsolver = org.sat4j.pb.SolverFactory.newDefault();		
			 d = new WeightedMaxSatDecorator(pbsolver);		

			d.newVar(3);		
			d.addHardClause(new VecInt(new int[] {1, 2}));
			d.addHardClause(new VecInt(new int[] {1, -3}));
			
			//d.addLiteralsToMinimize(new VecInt(new int [] {1, 2, 3} ));
			d.addSoftClause(1, new VecInt(new int[] {-1}));
			d.addSoftClause(1, new VecInt(new int[] {-2}));
			d.addSoftClause(1, new VecInt(new int[] {-3}));
					
			IPBSolver d2 = new OptToPBSATAdapter(d);
			if(d2.isSatisfiable())
			{
			
				
				// prime impl not OK given cnf conversion
				
				//ModelIterator mi = new ModelIterator(d);
				System.err.println("Models are:");
										
				
				while(d2.isSatisfiable())
				{
					System.err.println(d.calculateObjective());
					System.err.println(d.isOptimal());
					System.err.println(Arrays.toString(d2.model()));
					System.err.println(Arrays.toString(d2.primeImplicant()));
					//System.err.println(Arrays.toString(mi.model()));					
					
					int[] negationOfThisModel = new int [d.nVars()];
					for(int iVar=1;iVar<=d.nVars();iVar++)
					{
						if(d.model(iVar)) { negationOfThisModel[iVar-1] = -iVar; }
						else { negationOfThisModel[iVar-1] = iVar; };						
					}
					
					// This stops us at 1 solution. (Not sure why).
					// Ahh, is it used to abandon a potentially sub-optimal soln?
					//d.discardCurrentSolution();
					
					// Note: this might be a very inefficient way of iterating through models. ModelIterator doesn't seem to work with maxsat...
					// If it slows down in practice, ask. 														
					d.addHardClause(new VecInt(negationOfThisModel));
				}
								
			}
			else
			{
				System.err.println("MaxSAT was unsatisfiable.");
			}

			
			
			
			
			
			
			
			
			
			
			
			//////////////////////////////////////////////
			// Requires org.sat4j.maxsat
			//////////////////////////////////////////////
			
			pbsolver = org.sat4j.pb.SolverFactory.newDefault();			
			
			d = new WeightedMaxSatDecorator(pbsolver);
			d.newVar(1000);					
			// Test w/ at most one 
			int[] fooArray = new int[10000];
			for(int i=0;i<10000;i++)
				fooArray[i] = i+1;
			
			// option 1: at most range-size of range(a1), range(a2). a1 != a2 since they are ELEMENTS not terms
			// 
			//d.addAtMost(new VecInt(fooArray), 5001);
			// (not strictly required...)
			d.addAtLeast(new VecInt(fooArray), 4999);
					
			
			
			// Why is atmost so much faster than atleast? (yes, checked them separately)
			// is it the solver trying to give max negatives?
			// is it fewer clauses?
			// ...?
			// check UNSAT time...
			
			//for(int i=0;i<5000;i++)
			//	d.addHardClause(new VecInt(new int[] { i+1 }));

			for(int i=0;i<5000;i++)
				d.addHardClause(new VecInt(new int[] { (-1) *(i+1) }));

			
			if(d.admitABetterSolution())
			{
			
				
				// prime impl not OK given cnf conversion
				
				//ModelIterator mi = new ModelIterator(d);
				System.err.println("[atleast/most] MaxSAT was satisfiable. Objective function value was: "+d.calculateObjective());
				
				int funcValue = d.getObjectiveValue().intValue();
							
				while(d.admitABetterSolution())
				{
					System.err.println(Arrays.toString(d.model()));
					//System.err.println(Arrays.toString(d.primeImplicant()));
					//System.err.println(d.isOptimal());
					//System.err.println(Arrays.toString(mi.model()));					
					
					int[] negationOfThisModel = new int [d.nVars()];
					for(int iVar=1;iVar<=d.nVars();iVar++)
					{
						if(d.model(iVar)) { negationOfThisModel[iVar-1] = -iVar; }
						else { negationOfThisModel[iVar-1] = iVar; };						
					}
					
					// This stops us at 1 solution. (Not sure why).
					// Ahh, is it used to abandon a potentially sub-optimal soln?
					//d.discardCurrentSolution();
					
					// Note: this might be a very inefficient way of iterating through models. ModelIterator doesn't seem to work with maxsat...
					// If it slows down in practice, ask. 														
					d.addHardClause(new VecInt(negationOfThisModel));
				}
								
			}
			else
			{
				System.err.println("MaxSAT was unsatisfiable.");
			}

			
			
		}
		catch(ContradictionException e)
		{
			System.err.println("Unsatisfiable due to contradiction detected when adding a clause.");
		}
		catch(TimeoutException e)
		{
			System.err.println("SAT Solver ran out of time.");
		}
		
				
		
		
		
		
		
		//
		 // The proposal is: add extra props for each potential variable equality. Put a high cost on primary vars; low cost on var equality.
		  
		  
		 		
		// TODO: test Opt adapter instead of the above.
		
		
		
		
		// test cardinality constraints
		
	}*/

}


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
		fmla = MFormulaManager.makeAnd(fmla, voc.getFixedAxiomFormula());
		fmla = MFormulaManager.makeAnd(fmla, MFormulaManager.makeConjunction(voc.getUserAxiomFormulas()));	
		
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
		MEnvironment.writeOutLine("Starting java-based tests.");
		
		/////////////////////////////////////////////////////////////
		RelationAndVariableReplacementV.unitTests();
		/////////////////////////////////////////////////////////////
		MQuery.unitTest();
		/////////////////////////////////////////////////////////////
		MVocab.unitTest();
		
		MPolicyLeaf.unitTests();
		MPolicySet.unitTests();
		
/////////////////////////////////////////////////////////////
		try {
			FormulaSigInfo.unitTests();
		} catch (MNotASortException e) {
			
			MEnvironment.writeOutLine(">>> FormulaSigInfo tests FAILED: exception thrown!\n"+e.getLocalizedMessage());
		}
		
		
		
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
	

}


/* 
 * Kodkod -- Copyright (c) 2005-2007, Emina Torlak
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.wpi.margrave;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;

import kodkod.ast.Decl;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.engine.bool.BooleanFormula;
import kodkod.engine.config.Reporter;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.satlab.SATSolver;
import kodkod.instance.Bounds;
import kodkod.util.ints.IntSet;

import org.sat4j.core.VecInt;
import org.sat4j.minisat.SolverFactory;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IConstr;
import org.sat4j.specs.ISolver;
import org.sat4j.specs.IVecInt;
import org.sat4j.specs.IteratorInt;
import org.sat4j.specs.TimeoutException;

class MyReporter implements Reporter
{
	Bounds skolemBounds = null; 
	private int primaryVars;
	
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
		//System.out.println("Reporter: skolemizing: "+ decl +"; "+skolem+";"+context);
	}

	@Override
	public void solvingCNF(int primaryVars, int vars, int clauses) {
		// Make Kodkod tell us how many primary Vars...
		//System.out.println("REPORTER: "+primaryVars);
		this.primaryVars = primaryVars; 
	}

	@Override
	public void translatingToBoolean(Formula formula, Bounds bounds)
	{		
		// Make kodkod tell us what it's skolemized so we're working with the proper signature/bounds.
		//System.out.println("Reporter: translatingToBoolean: "+ bounds+"==========\n");
		skolemBounds = bounds;		
	}

	@Override
	public void translatingToCNF(BooleanFormula circuit) {				
	}
	
	int getNumPrimaryVariables()
	{			
		return primaryVars;
	}
	
}



class MinimalSolverFactory extends SATFactory
{   
	private MyReporter theReporter; 
	
	public MinimalSolverFactory(MyReporter theReporter)
	{
		super();
		this.theReporter = theReporter;		
	}
	
    @Override
    public SATSolver instance()
    {
    	// Registering a reporter gives us two pieces of information:
    	// (1) Which variables are primary (and thus subject to tampering to get minimal models);
    	// (2) Which relations are added for Skolemization.
    	// ... can also get at more information by modifying MyReporter.
    	
    	MinimalSolver result = new MinimalSolver(SolverFactory.instance().defaultSolver()); 
    	result.registerReporter(theReporter);
        return result;
    }
    
}

/**
 * A wrapper class that provides
 * access to the basic funcionality of the MiniSAT solvers
 * (org.sat4j.specs.ISolver) from CRIL. 
 * 
 * @author Emina Torlak
 */
class MinimalSolver implements SATSolver {
	private ISolver solver;
	private final ReadOnlyIVecInt wrapper;
	private Boolean sat; 
	private int vars, clauses;
	private MyReporter theReporter;
	
	//MinimalSolver fields:
	private  int[] minimalModel;
	private boolean foundMinimal = false;
	
	/**
	 * Constructs a wrapper for the given instance
	 * of ISolver.
	 * @throws NullPointerException - solver = null
	 */
	MinimalSolver(ISolver solver) {
		if (solver==null)
			throw new NullPointerException("solver");
		this.solver = solver;
		this.wrapper = new ReadOnlyIVecInt();
		this.sat = null;
		this.vars = this.clauses = 0;
	}

	public void registerReporter(MyReporter rep)
	{
		theReporter = rep;
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATSolver#numberOfVariables()
	 */
	public int numberOfVariables() {
		return vars; 
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATSolver#numberOfClauses()
	 */
	public int numberOfClauses() {
		return clauses; 
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATSolver#addVariables(int)
	 */
	public void addVariables(int numVars) {
		if (numVars < 0)
			throw new IllegalArgumentException("numVars < 0: " + numVars);
		else if (numVars > 0) {
			vars += numVars;
			solver.newVar(vars);
		}
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATSolver#addClause(int[])
	 */
	public boolean addClause(int[] lits) {
		if(foundMinimal)
		{
			System.out.println("* Kodkod tried to add a clause, but rejected: "+Arrays.toString(lits));
			return false;
		}
		try {
			if (!Boolean.FALSE.equals(sat)) {
				clauses++;
				solver.addClause(wrapper.wrap(lits));
//				for(int lit : lits) {
//					System.out.print(lit + " ");
//				}
//				System.out.println(0);
				return true;
			}
			
		} catch (ContradictionException e) {
			sat = Boolean.FALSE;
		}
		return false;
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATSolver#solve()
	 */
	public boolean solve() {
		try {
			if (!Boolean.FALSE.equals(sat))
				sat = Boolean.valueOf(solver.isSatisfiable());
			
			//MinimalSolver:
			if(sat)
			{	
				try{					
					nextMinimal();
					foundMinimal = true;
				}
				catch(ContradictionException e)
				{System.err.println(e.getMessage());}
			}
			
			return sat;
		} catch (org.sat4j.specs.TimeoutException e) {
			throw new RuntimeException("timed out");
		} 
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATSolver#valueOf(int)
	 */
	public final boolean valueOf(int variable) {
		if (!Boolean.TRUE.equals(sat)) 
			throw new IllegalStateException();
		if (variable < 1 || variable > vars)
			throw new IllegalArgumentException(variable + " !in [1.." + vars+"]");
		
		
		//Salman: It may or may not work!
		//if(minimalModel != null)
			return minimalModel[variable - 1] > 0 ? true: false;
		//	else
		//		return false;
		//return solver.model(variable);
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATSolver#free()
	 */
	public synchronized final void free() {
		solver = null;
	}
	
	/**
	 * A wrapper for an int array that provides
	 * read-only access to the array via the IVecInt interface. 
	 * 
	 * @author Emina Torlak
	 */
	private static final class ReadOnlyIVecInt implements IVecInt {
		private int[] vec;
		
		/**
		 * Sets this.vec to the given vector
		 * and returns this.
		 */
		IVecInt wrap(int[] vec) {
			this.vec = vec;
			return this;
		}
		
		public int size() {
			return vec.length;
		}

		public boolean isEmpty() {
			return size() == 0;
	    }
		
		public void shrink(int arg0) {
			throw new UnsupportedOperationException();
		}

		public void shrinkTo(int arg0) {
			throw new UnsupportedOperationException();
		}

		public IVecInt pop() {
			throw new UnsupportedOperationException();
		}

		public void growTo(int arg0, int arg1) {
			throw new UnsupportedOperationException();
		}

		public void ensure(int arg0) {
			throw new UnsupportedOperationException();
		}

		public IVecInt push(int arg0) {
			throw new UnsupportedOperationException();
		}

		public void unsafePush(int arg0) {
			throw new UnsupportedOperationException();
		}

		public int unsafeGet(int arg0) {
			return vec[arg0];
		}

		public void clear() {
			throw new UnsupportedOperationException();
		}

		public int last() {
			return vec[vec.length - 1];
		}

		public int get(int arg0) {
			if (arg0 < 0 || arg0 >= vec.length)
				throw new IndexOutOfBoundsException("arg0: " + arg0);
			return vec[arg0];
		}

		public void set(int arg0, int arg1) {
			throw new UnsupportedOperationException();		
		}

		public boolean contains(int arg0) {
			for(int i : vec) {
				if (i==arg0) return true;
			}
			return false;
		}

		public void copyTo(IVecInt arg0) {
			int argLength = arg0.size();
			arg0.ensure(argLength + vec.length);
			for(int i : vec) {
				arg0.set(argLength++, i);
			}
		}

		public void copyTo(int[] arg0) {
			assert arg0.length >= vec.length;
			System.arraycopy(vec,0, arg0, 0, vec.length);
		}

		public void moveTo(IVecInt arg0) {
			throw new UnsupportedOperationException();	
		}

		public void moveTo2(IVecInt arg0) {
			throw new UnsupportedOperationException();	
		}

		public void moveTo(int[] arg0) {
			throw new UnsupportedOperationException();	
		}

		public void moveTo(int arg0, int arg1) {
			throw new UnsupportedOperationException();	
		}

		public void insertFirst(int arg0) {
			throw new UnsupportedOperationException();
		}

		public void remove(int arg0) {
			throw new UnsupportedOperationException();
		}

		public int delete(int arg0) {
			throw new UnsupportedOperationException();
		}

		public void sort() {
			throw new UnsupportedOperationException();
		}

		public void sortUnique() {
			throw new UnsupportedOperationException();
		}

		public IteratorInt iterator() {
			return new IteratorInt() {
				int cursor = 0;
				public boolean hasNext() {
					return cursor < vec.length;
				}
				public int next() {
					if (!hasNext()) 
						throw new NoSuchElementException();
					return vec[cursor++];
				}
			};
		}

		public int containsAt(int e) {
			for(int n=vec.length, i=0; i<n; i++) if (vec[i]==e) return i;
			return -1;
		}

		public int containsAt(int e, int from) {
			if (from<vec.length) for(int n=vec.length, i=from+1; i<n; i++) if (vec[i]==e) return i;
			return -1;
		}

		@Override
		public int[] toArray() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public int indexOf(int arg0) {
			// TODO Auto-generated method stub
			return 0;
		}

		@Override
		public void moveTo(int arg0, int[] arg1) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public IVecInt[] subset(int arg0) {
			// TODO Auto-generated method stub
			return null;
		}
		
	}
	
	//Salman: Finds the next minimal model
	private void nextMinimal() throws TimeoutException, ContradictionException{
		//This keeps constraints to be removed from the solver
		//after finding the next model.
		ArrayList<IConstr> constraints = new ArrayList<IConstr>();
		//All the unit clauses being passed to the solver as assumptions.
		ArrayList<Integer> unitClauses = new ArrayList<Integer>();
		
		
		do
		{			
			minimalModel = solver.model();
			
			//System.out.println("Candidate: "+Arrays.toString(minimalModel));
			//System.out.println("numprimary: "+theReporter.getNumPrimaryVariables());
			
			// Given that candidate for minimal-model, try to make something smaller.
			// add: disjunction of negations of all positive literals in M (constraint)
			// add: all negative literals as unit clauses
			
			// An array of the next constraint being added.
			ArrayList<Integer> constraint = new ArrayList<Integer>();
			for(int i = 0; i < theReporter.getNumPrimaryVariables(); i++){
				if(minimalModel[i] > 0)
					constraint.add(-minimalModel[i]);
				else
					unitClauses.add(minimalModel[i]);
			}
			
			//System.out.println("constraint: "+constraint);
			constraints.add(solver.addClause(toVecInt(constraint)));
		}
		while(Boolean.valueOf(solver.isSatisfiable(toVecInt(unitClauses))));
		
		// Remove all the cone-specific constraints we just added from the solver:
		Iterator<IConstr> it = constraints.iterator();
		while(it.hasNext()){
			solver.removeConstr(it.next());
		}
		
		// Add a constraint saying that the solver should never give us a model in this cone again.
		// Don't bother capturing a way to remove it...
		ArrayList<Integer> noRepeat = new ArrayList<Integer>();
		for(int i = 0; i < theReporter.getNumPrimaryVariables(); i++){
			if(minimalModel[i] > 0)
				noRepeat.add(-minimalModel[i]);
		}
		System.out.println("* Adding clause to exclude cone: "+noRepeat);
		solver.addClause(toVecInt(noRepeat));
	}
	
	//Helpers
	private VecInt toVecInt(ArrayList<Integer> integers)
	{
	    int[] ret = new int[integers.size()];
	    for (int i=0; i < ret.length; i++)
	    {
	        ret[i] = integers.get(i).intValue();
	    }
	    return new VecInt(ret);
	}	
	
	
/*	public static void main(String[] args) {
//		final SAT4J z = (SAT4J)SATFactory.DefaultSAT4J.instance();
//		z.addVariables(3);
//		int[] clause = {1,2,3};
//		z.addClause(clause);
//		int[] clause1 = {-3};
//		z.addClause(clause1);
//		System.out.println(z.solver.nVars());
//		z.addVariables(4);
//		System.out.println(z.solver.nVars());
//		clause1[0] = 7;
//		z.addClause(clause1);
		z.addVariables(1);
		int[] clause1 = {1};
		z.addClause(clause1);
		clause1[0] = -1;
		z.addClause(clause1);
		
			System.out.println(z.solve());
			//System.out.println(z.variablesThatAre(true, 1, 1));
		
	}*/
}

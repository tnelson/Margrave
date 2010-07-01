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

import java.lang.management.*;

import kodkod.ast.*;
import kodkod.ast.operator.*;
import kodkod.ast.visitor.*;
import kodkod.engine.Statistics;

import kodkod.engine.satlab.SATFactory;


import kodkod.instance.*;


import java.util.*;

// Kodkod's AbstractReplacer doesn't cache ANYTHING by default, and we would rather it cached everything.
// I have not added all possible node types yet, but the big ones are here...

abstract class AbstractCacheAllReplacer extends AbstractReplacer {

	AbstractCacheAllReplacer(HashSet<Node> beginCached) {
		super(beginCached);
	}

	public Formula visit(ComparisonFormula comp) {
		// Default: no change
		// *************************
		// NOTE! Beware extending this class to BinaryExpressions, etc.
		// will bypass the Formula Manager.

		if (cache.containsKey(comp))
			return lookup(comp);

		cached.add(comp);
		return cache(comp, comp);
	}

	public Formula visit(BinaryFormula binFormula) {
		if (cache.containsKey(binFormula))
			return lookup(binFormula);

		// We want to cache this formula
		cached.add(binFormula);

		try {
			return cache(binFormula, MFormulaManager.makeComposition(binFormula
					.op(), binFormula.left().accept(this), binFormula.right()
					.accept(this)));
		} catch (MGEManagerException e) {
			System.err.println(e);
			return cache(binFormula, binFormula.left().accept(this).compose(
					binFormula.op(), binFormula.right().accept(this)));
		}
	}

	public Formula visit(NotFormula not) {
		if (cache.containsKey(not))
			return lookup(not);

		// We want to cache this formula
		cached.add(not);

		Formula replacement = not.formula().accept(this);
		return cache(not, MFormulaManager.makeNegation(replacement));
		
	}

	public Formula visit(QuantifiedFormula quantFormula) {
		if (cache.containsKey(quantFormula))
			return lookup(quantFormula);

		// We want to cache this formula
		cached.add(quantFormula);

		Formula newf = quantFormula.formula().accept(this);
		Decls newdecls = quantFormula.decls().accept(this);

		try {
			if (quantFormula.quantifier().equals(Quantifier.SOME))
				return cache(quantFormula, MFormulaManager.makeExists(newf,
						newdecls));
			else
				return cache(quantFormula, MFormulaManager.makeForAll(newf,
						newdecls));
		} catch (MGEManagerException e) {
			return cache(quantFormula, newf.quantify(quantFormula.quantifier(),
					newdecls));
		}
	}

	public Formula visit(NaryFormula nFormula) {
		if (cache.containsKey(nFormula))
			return lookup(nFormula);

		// We want to cache this formula
		cached.add(nFormula);

		Set<Formula> newformulas = new HashSet<Formula>();
		for (Formula f : nFormula)
			newformulas.add(f.accept(this));

		try {
			return cache(nFormula, MFormulaManager.makeComposition(nFormula
					.op(), newformulas));
		} catch (MGEManagerException e) {
			System.err.println(e);
			return cache(nFormula, Formula.compose(nFormula.op(), newformulas));
		}
	}
}

abstract class AbstractCacheAllDetector extends AbstractDetector {
	AbstractCacheAllDetector(HashSet<Node> beginCache) {
		super(beginCache);
	}

	public Boolean visit(BinaryFormula binFormula) {
		if (cache.containsKey(binFormula))
			return lookup(binFormula);

		// We want to cache this formula
		cached.add(binFormula);
		return super.visit(binFormula);
	}

	public Boolean visit(NotFormula not) {
		if (cache.containsKey(not))
			return lookup(not);

		// We want to cache this formula
		cached.add(not);
		return super.visit(not);
	}

	public Boolean visit(QuantifiedFormula quantFormula) {
		if (cache.containsKey(quantFormula))
			return lookup(quantFormula);

		// We want to cache this formula
		cached.add(quantFormula);
		return super.visit(quantFormula);
	}

	public Boolean visit(NaryFormula nFormula) {
		if (cache.containsKey(nFormula))
			return lookup(nFormula);

		// We want to cache this formula
		cached.add(nFormula);
		return super.visit(nFormula);
	}

}

abstract class AbstractCacheAllCollector<T> extends AbstractCollector<T> {
	AbstractCacheAllCollector(HashSet<Node> beginCache) {
		super(beginCache);
	}

	public Set<T> visit(BinaryFormula binFormula) {
		if (cache.containsKey(binFormula))
			return lookup(binFormula);

		// We want to cache this formula
		cached.add(binFormula);
		return super.visit(binFormula);
	}

	public Set<T> visit(NotFormula not) {
		if (cache.containsKey(not))
			return lookup(not);

		// We want to cache this formula
		cached.add(not);
		return super.visit(not);
	}

	public Set<T> visit(QuantifiedFormula quantFormula) {
		if (cache.containsKey(quantFormula))
			return lookup(quantFormula);

		// We want to cache this formula
		cached.add(quantFormula);
		return super.visit(quantFormula);
	}

	public Set<T> visit(NaryFormula nFormula) {
		if (cache.containsKey(nFormula))
			return lookup(nFormula);

		// We want to cache this formula
		cached.add(nFormula);
		return super.visit(nFormula);
	}

	public Set<T> visit(ComparisonFormula compFormula) {
		if (cache.containsKey(compFormula))
			return lookup(compFormula);

		// We want to cache this formula
		cached.add(compFormula);
		return super.visit(compFormula);
	}
}

class FormulaFullMeasurementV extends AbstractDetector {
	// NO CACHING
	// Use for debug purposes only: Will measure the entire tree, multi-counting
	// references.

	// WARNING!
	// Make certain that FormulaFullMeasurementV and FormulaMeasurementV both
	// have the SAME set of visitor
	// methods. Otherwise some node types will be counted by one but not the
	// other.

	public int counter;

	public FormulaFullMeasurementV() {
		super(new HashSet<Node>());
		counter = 0;
	}

	public Boolean visit(ComparisonFormula comp) {
		counter++;
		return super.visit(comp);
	}

	public Boolean visit(BinaryFormula binFormula) {
		counter++;
		return super.visit(binFormula);
	}

	public Boolean visit(NotFormula not) {
		counter++;
		return super.visit(not);
	}

	public Boolean visit(QuantifiedFormula quantFormula) {
		counter++;
		return super.visit(quantFormula);
	}

	public Boolean visit(NaryFormula nFormula) {
		counter++;
		return super.visit(nFormula);
	}
}

class FormulaMeasurementV extends AbstractCacheAllDetector {
	int counter = 0;

	int iBinaryFormula = 0;
	int iNotFormula = 0;
	int iQuantifiedFormula = 0;
	int iNaryFormula = 0;
	int iComparisonFormula = 0;

	public FormulaMeasurementV() {
		super(new HashSet<Node>());
	}

	public Boolean visit(ComparisonFormula comp) {
		if (!cache.containsKey(comp))
			counter++;
		return super.visit(comp);
	}

	public Boolean visit(BinaryFormula binFormula) {
		if (!cache.containsKey(binFormula)) {
			counter++;
			iBinaryFormula++;
		}
		return super.visit(binFormula);
	}

	public Boolean visit(NotFormula not) {
		if (!cache.containsKey(not)) {
			counter++;
			iNotFormula++;
		}
		return super.visit(not);
	}

	public Boolean visit(QuantifiedFormula quantFormula) {
		if (!cache.containsKey(quantFormula)) {
			counter++;
			iQuantifiedFormula++;
		}
		return super.visit(quantFormula);
	}

	public Boolean visit(NaryFormula nFormula) {
		if (!cache.containsKey(nFormula)) {
			counter++;
			iNaryFormula++;
		}
		return super.visit(nFormula);
	}

}

/**
 * Visitor which replaces instances for some Relations and Variables
 * 
 * @author tn
 * 
 */
class RelationAndVariableReplacementV extends AbstractCacheAllReplacer {
	private HashMap<Relation, Relation> relpairs;
	private HashMap<Variable, Variable> varpairs;

	private boolean no_change;

	public RelationAndVariableReplacementV(HashMap<Relation, Relation> pps,
			HashMap<Variable, Variable> vps) {
		super(new HashSet<Node>());

		// Replace P with Q in the pair.
		relpairs = pps;

		// Replace x with y in the pair.
		varpairs = vps;

		// Only visit if there is SOMETHING different
		no_change = true;
		for (Relation r : relpairs.keySet())
			if (!r.equals(relpairs.get(r))) {
				no_change = false;
				break;
			}

		for (Variable v : varpairs.keySet())
			if (!v.equals(varpairs.get(v))) {
				no_change = false;
				break;
			}
		
		// System.out.println("NEW REPLACEMENT VISITOR:");
		// System.out.println(pps);
		// System.out.println(vps);
	}

	public Expression visit(Relation therel) {
		if (no_change)
			return therel;

		if (cache.containsKey(therel))
			return lookup(therel);
		cached.add(therel); // add this BEFORE calling cache

		// Perform the replacement here, if needed.
		if (relpairs.containsKey(therel))
			return cache(therel, relpairs.get(therel));
		else
			return cache(therel, therel);
	}

	public Formula visit(ComparisonFormula comp) {
		if (no_change)
			return comp;

		// Need to make sure the replacement tuples and atomic formulas
		// go through the Formula Manager.

		if (cache.containsKey(comp))
			return lookup(comp);
		cached.add(comp);

		if (ExprCompOperator.EQUALS.equals(comp.op())) {
			// We require LEFT and RIGHT to both be Variables.
			// Therefore we directly invoke the formula manager here, and don't
			// need to do
			// anything special.

			Expression newlhs = comp.left().accept(this);
			Expression newrhs = comp.right().accept(this);

			if (!(newlhs instanceof Variable && newrhs instanceof Variable)) {
				System.err
						.println("Warning: ComparisonFormula with EQUALS operator with non-Variable children visited: "
								+ comp);
				return cache(comp, newlhs.eq(newrhs));
			}

			try {
				return cache(comp, MFormulaManager.makeEqAtom(
						(Variable) newlhs, (Variable) newrhs));
			} catch (MGEManagerException e) {
				System.err.println(e);
				return cache(comp, newlhs.eq(newrhs));
			}

		} else {
			// IN (SUBSET)

			// Left hand side (at least) may be a BinaryExpression.
			// If we just call accept(this), may end up constructing new tuples
			// that
			// the manager doesn't know about. Instead...

			Expression newlhs;
			Expression newrhs;

			try {
				if (comp.left() instanceof BinaryExpression) {
					// We have a var tuple. Replace vars AS NEEDED!

					newlhs = MFormulaManager.substituteVarTuple(
							(BinaryExpression) comp.left(), varpairs);
				} else
					newlhs = comp.left().accept(this);

				newrhs = comp.right().accept(this);

				return cache(comp, MFormulaManager.makeAtom(newlhs, newrhs));
			} catch (MGEManagerException e) {
				System.err.println(e);

				newlhs = comp.left().accept(this);
				newrhs = comp.right().accept(this);
				return cache(comp, newlhs.in(newrhs));
			}

		}
	}

	public Expression visit(Variable var) {
		if (cache.containsKey(var))
			return lookup(var);
		cached.add(var);

		// Perform the variable replacement here, if needed.
		if (varpairs.containsKey(var))
			return cache(var, varpairs.get(var));
		else
			return cache(var, var);
	}

	private static void runUnitTest(RelationAndVariableReplacementV vrepl,
			Formula pre, Formula post) {
		if (!pre.accept(vrepl).toString().equals(post.toString()))
			System.out.println("Error: Expected " + post.toString() + ", got: "
					+ pre.accept(vrepl).toString());
	}

	public static void unitTests() {
		System.out
				.println("----- Begin RelationReplacementV Tests (No messages is good.) -----");

		HashMap<Relation, Relation> rtestset = new HashMap<Relation, Relation>();
		HashMap<Variable, Variable> vtestset = new HashMap<Variable, Variable>();

		Relation R = MFormulaManager.makeRelation("R", 1);
		Relation P = MFormulaManager.makeRelation("P", 1);
		Relation Q = MFormulaManager.makeRelation("Q", 1);
		rtestset.put(R, P);

		Variable x = MFormulaManager.makeVariable("x");
		Variable y = MFormulaManager.makeVariable("y");
		vtestset.put(x, y);

		RelationAndVariableReplacementV v = new RelationAndVariableReplacementV(
				rtestset, vtestset);

		runUnitTest(v, R.no(), P.no());

		Formula oldf = R.intersection(Q).union(P).some();
		oldf.accept(v);
		if (!oldf.toString().equals(
				R.intersection(Q).union(P).some().toString()))
			System.out
					.println("Error in RelationReplacementV test case: visitor is overwriting old reference.");

		runUnitTest(v, x.eq(y), y.eq(y));

		oldf = x.eq(y);
		oldf.accept(v);
		if (!oldf.toString().equals("(x = y)"))
			System.out
					.println("Error in VariableReplacementV test case: visitor is overwriting old reference.");

		System.out.println("----- End RelationReplacementV Tests -----");
	}
}

/**
 * Visitor to simplify formula in obvious ways. For instance, (x && false) will
 * become false, etc.
 * 
 * @author tn
 * 
 */

/**
 * Visitor to detect use of transitive closure within a formula. (This is used
 * in query optimization.)
 * 
 * @author tn
 * 
 */
class FindClosureUseV extends AbstractCacheAllDetector {
	// static cache, will not interfere with garbage collection, but will try
	// to prevent recomputation. (Booleans are safe, so...)
	private static WeakHashMap<Formula, Boolean> closureUseCache = new WeakHashMap<Formula, Boolean>();

	private Boolean lookup(Formula f) {
		return closureUseCache.get(f);
	}

	private Boolean cache(Formula f, Boolean value) {

		closureUseCache.put(f, value);
		return value;
	}

	public FindClosureUseV() {
		super(new HashSet<Node>());
	}

	public Boolean visit(UnaryExpression ue) {
		Boolean cached = lookup(ue);
		if (cached != null)
			return cached;

		if (ue.op().equals(ExprOperator.CLOSURE)
				|| ue.op().equals(ExprOperator.REFLEXIVE_CLOSURE))
			return cache(ue, true);
		return cache(ue, false);
	}

	// public Boolean visit(NaryFormula nary)
	// {
	// maybe it should implement map after all, would let us abstract all this
	// out to the parent class
	// (how?)
	// abstract's constructor takes the Map<K, V>?
	// concrete's constructor passes... but then a synch problem?
	// }

}

class ContainsQuantifiersCheckV extends AbstractCacheAllDetector {
	public ContainsQuantifiersCheckV() {
		super(new HashSet<Node>());
	}

	public Boolean visit(QuantifiedFormula qf) {
		return true;
	}

}

/**
 * 
 * NOT SAFE to re-use an instance, due to the extra fields
 * 
 */
class PrenexCheckV extends AbstractCacheAllDetector {
	// Returns true if the formula suits our narrow prenexing
	// requirements for tupling.

	public Formula matrix; // pointer to where the matrix starts
	public int qCount; // number of existentials in the prefix
	public String tupleTypeConstruct; // "Subject Action Resource"

	// NO SPACES! will be translated as >1 arity. instead use underscore
	public String tupleTypeName; // "Subject@Action@Resource"
	
	public HashMap<Variable, Integer> indexing; // s -> 1
	public HashMap<String, Variable> revIndexing; // 1-> s

	private boolean inPrefix;

	// Universals are NOT SUPPORTED yet
	// Promoting existentials is NOT SUPPORTED yet

	public PrenexCheckV()
	{
		super(new HashSet<Node>());
		
		inPrefix = true;
		qCount = 0;
		tupleTypeConstruct = "";
		tupleTypeName = "";
		matrix = Formula.TRUE;
		
		indexing = new HashMap<Variable, Integer>();
		revIndexing = new HashMap<String, Variable>();
	}

	public Boolean visit(QuantifiedFormula qf) {
		// Should still be in the prefix
		if (!inPrefix)
			return false;

		// no support for universals
		if (qf.quantifier().equals(Quantifier.ALL))
			return false;

		qCount++;
		
		for (Decl d : qf.decls()) {
			if (tupleTypeConstruct.length() < 1) {
				tupleTypeConstruct = d.expression().toString();
				tupleTypeName = d.expression().toString();
			} else {
				tupleTypeConstruct += " " + d.expression().toString();
				tupleTypeName += "@" + d.expression().toString();
			}

			

			indexing.put(d.variable(), qCount); // index for this variable,
												// 	already incremented
			revIndexing.put(String.valueOf(qCount), d.variable());
				

			// System.out.println("Indexed "+String.valueOf(qCount) + " "
			// +d.variable());
		}

		// don't use the cache here
		return qf.formula().accept(this);
	}

	public Boolean visit(ComparisonFormula cf) {
		if (cache.containsKey(cf))
			return lookup(cf);

		if (inPrefix)
			matrix = cf;
		inPrefix = false;

		// hit bottom
		cached.add(cf);
		return cache(cf, true);
	}

	public Boolean visit(NotFormula nf) {
		if (cache.containsKey(nf))
			return lookup(nf);

		// If the matrix starts with negation, need to put the matrix pointer
		// HERE,
		// not on nf.formula().
		if (inPrefix)
			matrix = nf;
		inPrefix = false;

		// continue down
		cached.add(nf); // true if true lower
		return cache(nf, nf.formula().accept(this));
	}

	public Boolean visit(BinaryFormula bf) {
		if (cache.containsKey(bf))
			return lookup(bf);

		if (inPrefix)
			matrix = bf;
		inPrefix = false;

		boolean l = bf.left().accept(this);
		boolean r = bf.right().accept(this);

		cached.add(bf);
		return cache(bf, (l && r));
	}

	public Boolean visit(NaryFormula naryf) {
		if (cache.containsKey(naryf))
			return lookup(naryf);

		if (inPrefix)
			matrix = naryf;
		inPrefix = false;

		boolean result = true;
		for (Formula f : naryf)
			result = result && f.accept(this);
		cached.add(naryf);
		return cache(naryf, result);
	}

	public Boolean visit(ConstantFormula cf) {
		if (cache.containsKey(cf))
			return lookup(cf);

		if (inPrefix)
			matrix = cf;
		inPrefix = false;

		cached.add(cf);
		return cache(cf, true);
	}
}

class MatrixTuplingV extends AbstractCacheAllReplacer {
	// Translate matrix (existential case only).

	// New vocabulary. Visitor will add new sorts as needed.
	// Same sort structure as before duplicated pren.qCount times.
	// E.g., where A < B before, now A_1 < B_1, A_2 < B_2, and so on.
	public MVocab newvocab;
	public MVocab oldvocab;
	public Variable newvar;
	public Set<String> equalAxiomsNeeded;

	// Used privately to get information about this formula's prefix
	protected PrenexCheckV pv;

	// Store that P became P_1, P_2, and P_4.
	// (Applies for BOTH sorts and state preds)
	HashMap<String, Set<String>> tupledPredicates;

	// ii -> { old pred names we tupled with index ii }
	HashMap<String, Set<String>> oldPredNamesTupledWithIndex;

	MatrixTuplingV(PrenexCheckV pren, MVocab old) {
		super(new HashSet<Node>());

		oldvocab = old;
		newvocab = new MVocab("Tupled:" + old.vocab_name);
		newvar = MFormulaManager.makeVariable("z");
		equalAxiomsNeeded = new HashSet<String>();

		// All of them
		for (int ileft = 1; ileft <= pren.qCount; ileft++)
			for (int iright = ileft + 1; iright <= pren.qCount; iright++) {
				String name = "=_" + ileft + "," + iright;
				equalAxiomsNeeded.add(name);
				try {
					newvocab.addPredicate(name, pren.tupleTypeName);
				} catch (MGEBadIdentifierName e) {

				}
			}


		tupledPredicates = new HashMap<String, Set<String>>();
		oldPredNamesTupledWithIndex = new HashMap<String, Set<String>>();
		for (int index = 1; index <= pren.qCount; index++)
			oldPredNamesTupledWithIndex.put(String.valueOf(index),
					new HashSet<String>());

		pv = pren;

		// New top-level sort is created after this visitor runs.
		// Not elegant, but avoids problem of A < B < C
	}

	public Formula visit(ComparisonFormula cf) {
		if (cache.containsKey(cf))
			return lookup(cf);

		Formula newf;

		// EQUAL: var1 = var2.
		if (cf.op().equals(ExprCompOperator.EQUALS)) {
			if (!(cf.left() instanceof Variable)
					|| !(cf.right() instanceof Variable)) {
				System.err.println("Comparison: " + cf
						+ " must be over variables.");
				System.exit(1);
				return cf; // don't know what to do
			}

			int leftidx = pv.indexing.get(cf.left());
			int rightidx = pv.indexing.get(cf.right());

			// Have we already seen these, just in reverse order? (Equality is
			// symmetric; don't
			// create an =21 predicate when we've already seen and created =12.)

			String reverse_predname = "=_" + rightidx + "," + leftidx;
			if (equalAxiomsNeeded.contains(reverse_predname)) {
				// Swap!
				int temp = leftidx;
				leftidx = rightidx;
				rightidx = temp;
			}

			// Add a new =_{ij} predicate
			String predname = "=_" + leftidx + "," + rightidx;

			// Add equality axioms for this new predicate -- AFTER all
			// the new predicates are generated!
			equalAxiomsNeeded.add(predname);

			try {
				newvocab.addPredicate(predname, pv.tupleTypeName); // add new
																	// predicate

			} catch (MGEBadIdentifierName e) {
				System.err.println(e);
				System.exit(1);
				return cf; // don't know what to do
			}

			try {
				Relation newrel = newvocab.getRelation(predname);
				newf = MFormulaManager.makeAtom(newvar, newrel);

				cached.add(cf);
				return cache(cf, newf);
			} catch (Exception e) {
				System.err.println(e);
				System.exit(1);
				return cf; // don't know what to do
			}
		}

		// SUBSET: (var1 X ... X varN) in Relation
		else if (cf.op().equals(ExprCompOperator.SUBSET)) {

			if (!(cf.right() instanceof Relation)) {
				System.err.println("Comparison: " + cf
						+ " must be vs. a Relation.");
				System.exit(1);
				return cf; // fail
			}

			// DFS of left hand side (like in well-sortedness test) to get
			// indexing

			String predname;
			String suffix;
			if (cf.left() instanceof Variable)
				suffix = "_" + pv.indexing.get(cf.left());
			else if (cf.left() instanceof BinaryExpression)
				suffix = "_"
						+ MVocab.constructIndexing(
								(BinaryExpression) cf.left(), pv.indexing);
			else {
				System.err.println("Comparison: " + cf + " -- improper LHS.");
				System.exit(1);
				return cf; // don't know what to do
			}

			if (suffix.equals("_null")) {
				System.err.println("Bad indexing for " + cf.left());
				System.exit(1);
				return cf; // fail
			}

			predname = cf.right().toString() + suffix;

			try {

				// Is this a predicate or a sort? If a sort, does it have a
				// parent?
				boolean isSort = oldvocab.isSort(cf.right().toString());
				if (!isSort) {
					newvocab.addPredicate(predname, pv.tupleTypeName);
					addToMap(cf.right().toString(), suffix, predname);
				} else
					addSortWithSupers(newvocab, oldvocab,
							cf.right().toString(), suffix);
				// addToMap done in the above call

				Relation newrel = newvocab.getRelation(predname);
				newf = MFormulaManager.makeAtom(newvar, newrel);

				cached.add(cf);
				return cache(cf, newf);

			} catch (Exception e) {
				System.err.println(e);
				System.exit(1);
				return cf; // don't know what to do
			}

		}

		// for new features
		System.err.println("Comparison: " + cf + "; unrecognized operator.");
		System.exit(1);
		return cf;
	}

	private void addToMap(String oldname, String suffix, String newToAdd) {
		if (!tupledPredicates.containsKey(oldname))
			tupledPredicates.put(oldname, new HashSet<String>());
		tupledPredicates.get(oldname).add(newToAdd);

		String index = suffix.substring(1); // remove the underscore

		// we initialized in constructor
		oldPredNamesTupledWithIndex.get(index).add(oldname);
	}

	private void addSortWithSupers(MVocab newvocab, MVocab oldvocab,
			String oldpredname, String suffix) throws MGEUnknownIdentifier,
			MGEBadIdentifierName {
		// What if A < B < C, and A_1 is what we've found?
		// Create B_1 But we MUST also create C_1.
		// (IP Addr > Range > Single IP. -- play nice with tuple axioms)

		MSort oldsort = oldvocab.getSort(oldpredname);
		if (oldsort.parent != null) {
			// Deal with parent's parent (if any)
			addSortWithSupers(newvocab, oldvocab, oldsort.parent.name, suffix);

			newvocab.addSubSort(oldsort.parent.name + suffix, oldpredname
					+ suffix);
			addToMap(oldpredname, suffix, oldpredname + suffix);
		} else {
			newvocab.addSort(oldpredname + suffix);
			addToMap(oldpredname, suffix, oldpredname + suffix);
		}

		// Now, if this sort has any subSET constraints on it (distinct from our
		// subSORTing partial order on the sorts)
		// We must handle those as well. (Don't forget to propagate the
		// constraint to the new vocab.)
		if (oldvocab.axioms.setsSubset.containsKey(oldpredname))
			for (String oldparent : oldvocab.axioms.setsSubset
					.get(oldsort.name)) {
				// System.out.println("***** "+oldparent +" ] "+oldpredname);
				addSortWithSupers(newvocab, oldvocab, oldparent, suffix);
				newvocab.axioms.addConstraintSubset(oldpredname + suffix,
						oldparent + suffix);
			}

	}

}

/**
 * Visitor to collect all FREE Variables referred to by the formula
 * 
 * @author tn
 * 
 */
class FreeVariableCollectionV extends AbstractCacheAllCollector<Variable> {
	public HashSet<Variable> newSet() {
		return new HashSet<Variable>();
	}

	public FreeVariableCollectionV() {
		super(new HashSet<Node>());
	}

	public Set<Variable> visit(Variable v) {
		if (cache.containsKey(v))
			return lookup(v);
		cached.add(v);

		HashSet<Variable> tempset = new HashSet<Variable>();
		tempset.add(v);
		return cache(v, tempset);
	}

	public Set<Variable> visit(QuantifiedFormula qf) {
		if (cache.containsKey(qf))
			return lookup(qf);
		cached.add(qf);

		// What free variables appear inside this quantifier?
		Set<Variable> tempset = qf.formula().accept(this);

		// These variables are quantified in this scope.
		// (Don't worry about re-quantification later, since Kodkod won't run
		// vs. such a formula.)
		for (Decl d : qf.decls())
			tempset.remove(d.variable());

		return cache(qf, tempset);
	}
}

class RelationsUsedCollectionV extends AbstractCacheAllCollector<Relation> {
	public HashSet<Relation> newSet() {
		return new HashSet<Relation>();
	}

	public RelationsUsedCollectionV() {
		super(new HashSet<Node>());
	}

	public Set<Relation> visit(Relation r) {
		if (cache.containsKey(r))
			return lookup(r);

		cached.add(r);
		HashSet<Relation> tempset = new HashSet<Relation>();
		tempset.add(r);
		return cache(r, tempset);
	}

}

/**
 * The MGQuery class encapsulates a query against a policy or policies. It
 * contains the KodKod formula which is being tested, as well as the quantifiers
 * for that formula. The quantifiers are kept separate in order to facilitate
 * easy composition of queries.
 * 
 * @author Tim Nelson
 * @author tn@cs.wpi.edu
 */

public class MQuery extends MIDBCollection 
{
	protected boolean tupled;
	protected MQuery internalTupledQuery;
	protected MatrixTuplingV internalTuplingVisitor;

	/**
	 * KodKod Formula object which encodes the query. This formula is NOT
	 * quantifier-free. As of 8/09, this formula no longer includes axioms: just
	 * the user query.
	 */
	private Formula myQueryFormula;

	/**
	 * This map contains the policies and views this query runs with respect to.
	 * We make an assumption that all policies on this list use the same MGVocab
	 * object (or they wouldn't be on the list together.)
	 * 
	 * The string element is the policy name.
	 */
	HashMap<String, MIDBCollection> myIDBCollections;

	/**
	 * Indicates which SAT Solver application KodKod should use. Default is
	 * MiniSAT.
	 */
	public SATFactory mySATFactory;

	/**
	 * Integer value which tells KodKod how heavy-handed to be with symmetry
	 * breaking. Default is 20. Must be using a symmetric SAT Solver (Such as
	 * MiniSAT) to use this.
	 * 
	 * @see mySATFactory
	 */
	public int mySB;

	/**
	 * Integer value for the maximum universe size that will be tried. Margrave
	 * will start at size 1 and enumerate models until this size is exceeded.
	 * 
	 * Note that if a query has a finite Herbrand Universe of size k, the
	 * SMALLEST OF THE TWO will be used. If the smallest is sizeCeiling, the
	 * user will be warned.
	 */
	public int sizeCeiling;

	/**
	 * 0: No debug output 1: Display statistics 2: Display statistics and query
	 * execution stages 3: Display statistics, query execution stages, and
	 * formulas. (Warning: level 3 runs .toString() on Formula objects, and so
	 * may be unwise to use for large policies or large queries.)
	 * 
	 * Defaults to 0.
	 */
	public int debug_verbosity;
	
	
	// For tupled queries. Need to know which tuple indexing to use for a given
	// IDB.
	// Setting function will confirm that it is a valid list of numbers, and
	// that
	// the arity matches the desired IDB name.
	protected HashMap<String, Set<List<String>>> idbOutputIndexing;

	public boolean doTupling;

	// time spent before query creation
	protected long msPreprocessingTime;

	// time spent on tupling (if any)
	protected long msTuplingTime;

	// The default default is SAT4j, for compatibility.
	// For performance (or if using large queries, for which SAT4j can run out
	// of heap space)
	// switch to MiniSAT.
	static SATFactory defaultSATFactory = SATFactory.DefaultSAT4J;
	
	public void printSettings() 
	{
		// Print out the current settings for this query.

		if (mySATFactory.equals(SATFactory.MiniSat))
			MEnvironment.outStream.println("SATFactory: MiniSAT");
		else
			MEnvironment.outStream.println("SATFactory: SAT4j (Default)");

		MEnvironment.outStream.println("SB: " + mySB);
		MEnvironment.outStream.println("Size ceiling: " + sizeCeiling);
	}

	private void init(Formula nFormula) throws MGEUnsortedVariable {
		// Simplify query formula before anything else.
		// MREPL.outStream.println("Before: "+nFormula);

		// Not included in timer, for now...
		// ThreadMXBean mxBean = ManagementFactory.getThreadMXBean();
		// long start = mxBean.getCurrentThreadCpuTime();
		
		//myQueryFormula = nFormula.accept(new SimplifyFormulaV());
		myQueryFormula = nFormula;

		// System.err.println("Time (ms) to simplify formula in MGQuery.init(): "+
		// (mxBean.getCurrentThreadCpuTime() - start) / 1000000);
		// MREPL.outStream.println("After: "+myQueryFormula);
		// Initial solver is...

		mySATFactory = defaultSATFactory;
		// mySATFactory = SATFactory.DefaultSAT4J;

		// Initial symmetry-breaking level is 20:
		mySB = 20;

		// Initial model size ceiling. User can modify this.
		sizeCeiling = 6;

		debug_verbosity = 0;

		tupled = false;
		doTupling = false;

		msPreprocessingTime = 0;
		msTuplingTime = 0;
		idbOutputIndexing = new HashMap<String, Set<List<String>>>();
	}

	// constructors
	private MQuery(Formula nFormula, MPolicy initialPolicy)
			throws MGEUnsortedVariable, MGEUnknownIdentifier,
			MGEBadQueryString, MGEArityMismatch {
		vocab = initialPolicy.vocab;
		init(nFormula);

		myIDBCollections = new HashMap<String, MIDBCollection>();
		myIDBCollections.put(initialPolicy.name, initialPolicy);

		// Don't need to combine or replace -- only one source vocab.

	}

	private MQuery(MVocab uber, Formula nFormula,
			List<MIDBCollection> idbcollections) throws MGEUnsortedVariable {
		vocab = uber;
		init(nFormula);

		myIDBCollections = new HashMap<String, MIDBCollection>();
		for (MIDBCollection p : idbcollections)
			myIDBCollections.put(p.name, p);
	}

	protected MQuery(MVocab uber, Formula nFormula,
			Set<MIDBCollection> idbcollections) throws MGEUnsortedVariable {
		vocab = uber;
		init(nFormula);

		myIDBCollections = new HashMap<String, MIDBCollection>();
		for (MIDBCollection p : idbcollections)
			myIDBCollections.put(p.name, p);
	}

	private MQuery(MVocab prevVocab, Formula nFormula,
			HashMap<String, MIDBCollection> prevIDBsList)
			throws MGEUnsortedVariable, MGEUnknownIdentifier,
			MGEBadQueryString, MGEArityMismatch {
		// This constructor is used for query refinement.
		// No combination of vocabs is done.
		vocab = prevVocab;

		init(nFormula);
		myIDBCollections = prevIDBsList;

	}

	private MQuery(Formula nFormula, MVocab voc) throws MGEUnsortedVariable,
			MGEUnknownIdentifier, MGEBadQueryString, MGEArityMismatch {
		// *** This constructor exists for testing purposes only. It should
		// NEVER
		// be used outside of a unit test. (Note no IDB collections passed.)

		vocab = voc;
		init(nFormula);
		myIDBCollections = new HashMap<String, MIDBCollection>();
	}

	public Set<String> getIDBNamesToOutput()
	{
		return idbOutputIndexing.keySet();
	}
	
	public void useMiniSAT() {
		mySATFactory = SATFactory.MiniSat;
	}

	public void useSAT4j() {
		mySATFactory = SATFactory.DefaultSAT4J;
	}

	public static void useMiniSATAsDefault() {
		defaultSATFactory = SATFactory.MiniSat;
	}

	public static void useSAT4jAsDefault() {
		defaultSATFactory = SATFactory.DefaultSAT4J;
	}

	protected static int measureFullFormulaSize(Formula f) {
		FormulaFullMeasurementV fullmeasure = new FormulaFullMeasurementV();
		f.accept(fullmeasure);
		return fullmeasure.counter;
	}

	protected static int measureFormulaReferences(Formula f) {
		FormulaMeasurementV measure = new FormulaMeasurementV();
		f.accept(measure);
		// MREPL.outStream.println("bin refs: "+measure.iBinaryFormula);
		// MREPL.outStream.println("nary refs: "+measure.iNaryFormula);
		// MREPL.outStream.println("not refs: "+measure.iNotFormula);
		// MREPL.outStream.println("quant refs: "+measure.iQuantifiedFormula);
		// MREPL.outStream.println("comparison refs: "+measure.iComparisonFormula);
		return measure.counter;
	}

	/*
	 * private static Set<FuncStruct> walkASTForFunctions(Formula origfmla,
	 * MGVocab context) throws MGEUnknownIdentifier { // Convert to NNF (nearly)
	 * and return the result of walking the AST. WalkASTForFunctionsV walker =
	 * new WalkASTForFunctionsV(context);
	 * 
	 * Formula nnf_formula = origfmla.accept(new NNFConverterV());
	 * 
	 * 
	 * //MREPL.outStream.println("Traversing both formulas and measuring their length..."
	 * );
	 * //MREPL.outStream.println("Length of pre-nnf: "+measureFormulaSize(origfmla)+
	 * ". Length of post-nnf: "+ // measureFormulaSize(nnf_formula));
	 * 
	 * Set<FuncStruct> results = nnf_formula.accept(walker);
	 * 
	 * if(walker.error) throw new
	 * MGEUnknownIdentifier("AST Walker returned error: "
	 * +walker.error_condition); return results; }
	 */

	protected void print_query_formula() {
		MEnvironment.outStream.println(myQueryFormula);
	}

	/*
	 * private boolean funcIsCoveredByOther(FuncStruct f, HashMap<Expression,
	 * Set<FuncStruct>> coveredby) throws MGEUnknownIdentifier,
	 * MGEBadIdentifierName { // We know that that f covers its own type, so
	 * check to see if there's some other in the list.
	 * if(coveredby.get(f.resulttype).size() > 1) return true;
	 * 
	 * // Assert that this is a simple MgType, not an intersection (or etc.)
	 * thereof. // Search for covered subtypes of f's type. List<MGSort> tocheck
	 * = new LinkedList<MGSort>(); for(MGSort c :
	 * myVocab.getSortForExpression(f.resulttype).subsorts) tocheck.add(c);
	 * 
	 * while(tocheck.size() > 0) { MGSort current = tocheck.get(0);
	 * if(coveredby.containsKey(current.rel))
	 * if(coveredby.get(current.rel).size() > 0) return true;
	 * 
	 * // continue to check deeper in the hierarchy for(MGSort c :
	 * current.subsorts) tocheck.add(c); tocheck.remove(0); }
	 * 
	 * if(debug_verbosity >= 2) MREPL.outStream.println("DEBUG: "+f +
	 * " was not multi-covered by "+coveredby); return false; }
	 */

	protected int getHerbrandUniverseCeilingFor(Formula queryAndQueryAxioms,
			boolean prenexExistential) throws MGEUnknownIdentifier,
			MGEBadIdentifierName {
		ThreadMXBean mxBean = ManagementFactory.getThreadMXBean();
		long startTime = mxBean.getCurrentThreadCpuTime();

		// Reject transitive closure
		if (queryAndQueryAxioms.accept(new FindClosureUseV()))
			return -1;

		if (debug_verbosity >= 2)
			MEnvironment.outStream.println("DEBUG: Time (ms) to check for closure: "
					+ (mxBean.getCurrentThreadCpuTime() - startTime) / 1000000);
		startTime = mxBean.getCurrentThreadCpuTime();

		// Use LeafExpression rather than Relation, because we may need to
		// represent Expression.UNIV
		// if sort inference fails.

		// Build a FormulaSigInfo object and get its totals.
		// Now that FormulaSigInfo is separate, a better internal data structure
		// would be nice. (TODO)
		Set<LeafExpression> sorts = new HashSet<LeafExpression>();
		Map<LeafExpression, Set<LeafExpression>> supersorts = new HashMap<LeafExpression, Set<LeafExpression>>();
		Map<LeafExpression, List<LeafExpression>> predicates = new HashMap<LeafExpression, List<LeafExpression>>();

		for (MSort s : vocab.sorts.values()) {
			// s.rel is a sort.
			sorts.add(s.rel);

			// What are the supersorts of s.rel? (parents + supersort
			// constraints)
			Set<LeafExpression> supers = new HashSet<LeafExpression>();

			Set<MSort> mgsups = vocab.buildSuperSetSet(s); // this is transitive
			for (MSort sup : mgsups)
				supers.add(sup.rel);

			supersorts.put(s.rel, supers);
		}

		for (String pname : vocab.predicates.keySet()) {
			// What sort is this predicate?
			List<LeafExpression> predArity = new ArrayList<LeafExpression>();
			String sortconstruct = vocab.predtypes.get(pname);

			String[] args = sortconstruct.split("\\s");
			for (String arg : args)
				predArity.add(vocab.getRelation(arg));

			predicates.put(vocab.predicates.get(pname), predArity);
		}

		if (debug_verbosity >= 2)
			MEnvironment.outStream
					.println("DEBUG: Time (ms) to populate maps for FormulaSigInfo: "
							+ (mxBean.getCurrentThreadCpuTime() - startTime)
							/ 1000000);
		startTime = mxBean.getCurrentThreadCpuTime();

		//Formula nnf_formula = queryAndQueryAxioms.accept(new NNFConverterV());
		Formula nnf_formula = queryAndQueryAxioms; //.accept(new NNFConverterV());
		
		if (debug_verbosity >= 2)
			MEnvironment.outStream.println("DEBUG: Time (ms) to convert to NNF: "
					+ (mxBean.getCurrentThreadCpuTime() - startTime) / 1000000);
		startTime = mxBean.getCurrentThreadCpuTime();

		try {
			FormulaSigInfo.EnumSAPHandling sap = FormulaSigInfo.EnumSAPHandling.sapKeep;
			if (prenexExistential)
				sap = FormulaSigInfo.EnumSAPHandling.sapIgnore;

			FormulaSigInfo info = new FormulaSigInfo(sorts, supersorts,
					predicates, new HashSet<SigFunction>(),
					new HashSet<SigFunction>(), nnf_formula, sap);

			if (debug_verbosity >= 2) {
				MEnvironment.outStream
						.println("DEBUG: Generating a ceiling on necessary model size.");
			}

			int result = info.getTermCount(); // no params = total

			if (debug_verbosity >= 2) {
				if (result < 0)
					MEnvironment.outStream
							.println("\n DEBUG: Counting terms... infinitely many terms. Detail:");
				else
					MEnvironment.outStream.println("\n DEBUG: Counting terms... " + result
							+ " terms generated. Detail:");

				info.printInfo();
			}

			return result;
		} catch (UnsupportedFormulaException E) {
			// unsupported
			return -1;
		} catch (NotASortException E) {
			// unsupported
			return -1;
		}

	}

	protected Formula makePredsIndistinguish(MVocab old, MVocab newvocab,
			Variable newvar, String leftidx, String rightidx)
			throws MGEManagerException, MGEBadIdentifierName
	{
		// Could do better with caching, but would need to protect query
		// properties better.

		// Construct a formula saying that for each predicate, P_i IFF P_j
		// (For >1-ary predicates, this gets more complex.)
		Set<Formula> isoconj_formulas = new HashSet<Formula>();

		// Sorts first:
		// find all sorts that got tupled for both these indices, and include
		// them in the axiom
		for (String oldsort : vocab.sorts.keySet())
		{
				
			MSort newsort1 = newvocab.fastGetSort(oldsort + "_" + leftidx);
			MSort newsort2 = newvocab.fastGetSort(oldsort + "_" + rightidx);
				
			if(newsort1 != null && newsort2 != null)
			{
				
				// If we get this far, this sort was tupled for both indices.

				Formula insort1 = MFormulaManager
						.makeAtom(newvar, newsort1.rel);
				Formula insort2 = MFormulaManager
						.makeAtom(newvar, newsort2.rel);
				isoconj_formulas.add(MFormulaManager.makeIFF(insort1, insort2));
			}
		} // end for each sort

		// Now predicates:
		// This is more involved. If 1=2, then P_134 <-> P_234 for all 3,4 (if
		// we care about them)
		// But FOR NOW (see above) we have only allowed tupling where predicates
		// are all unary.
		for (String predname : vocab.predicates.keySet()) {
			// Is it used for both these indices?
			try {
				Relation newpred1 = newvocab.getRelation(predname + "_"
						+ leftidx);
				Relation newpred2 = newvocab.getRelation(predname + "_"
						+ rightidx);

				// If we get this far, this sort was tupled for both indices.
				Formula insort1 = MFormulaManager.makeAtom(newvar, newpred1);
				Formula insort2 = MFormulaManager.makeAtom(newvar, newpred2);
				isoconj_formulas.add(MFormulaManager.makeIFF(insort1, insort2));
			} catch (MGEUnknownIdentifier e) {
			} // do nothing if unused
		} // end for each state predicate

		// Done
		return MFormulaManager.makeConjunction(isoconj_formulas);
	}

	/**
	 * Runs KodKod on the query this object represents, returning a MGSolution
	 * object. Will discover whether the query has a finite Herbrand Universe
	 * and take appropriate action.
	 * 
	 * @return MGSolution for this query.
	 * 
	 * @throws MGEUnsortedVariable
	 * @throws MGEQueryUnsatisfiable
	 * @throws MGEUnknownIdentifier
	 * @throws MGEArityMismatch
	 * @throws MGEBadQueryString
	 * @throws MGEManagerException
	 * @throws MGEBadIdentifierName
	 */

	public MQueryResult runQuery() throws MGEUnsortedVariable,
			MGEUnknownIdentifier, MGEArityMismatch, MGEBadQueryString,
			MGEManagerException, MGEBadIdentifierName {
		if (debug_verbosity >= 2)
			MEnvironment.outStream.println("DEBUG: Beginning to execute query (runQuery) ");

		if(!tupled)
		{
			// TODO: Really ought to have MTupledQuery as a subclass. 
			internalTupledQuery = null; // re-init every time we run
			internalTuplingVisitor = null;
		}
		
		ThreadMXBean mxBean = ManagementFactory.getThreadMXBean();
		long start = mxBean.getCurrentThreadCpuTime();

		long startTime = mxBean.getCurrentThreadCpuTime();

		// Formulas for fixed and query axioms.
		// Query axioms affect Herbrand Universe size!
		Formula myFixedAxioms = vocab.getFixedAxiomFormula();
		Set<Formula> myQueryAxioms = vocab.getUserAxiomFormulas();
		Formula queryAxiomsConjunction = MFormulaManager
				.makeConjunction(myQueryAxioms);

		// Build the actual query formula: The user query, the user axioms, and
		// the fixed sig axioms
		Formula queryWithAxioms = MFormulaManager.makeAnd(myQueryFormula,
				queryAxiomsConjunction);
		queryWithAxioms = MFormulaManager.makeAnd(queryWithAxioms,
				myFixedAxioms);

		if (debug_verbosity >= 2)
			MEnvironment.outStream
					.println("DEBUG: Time (ms) to get and build axiom formulas: "
							+ (mxBean.getCurrentThreadCpuTime() - startTime)
							/ 1000000);
		startTime = mxBean.getCurrentThreadCpuTime();
				
		
		PrenexCheckV pren = new PrenexCheckV();
		boolean prenexExistential = myQueryFormula.accept(pren);
		
		
		// Get Herbrand Universe
		int totalHerbrandMax;

		// If this is a tupled query, we know size = 1.
		if (tupled)
			totalHerbrandMax = 1;
		else {
			if (debug_verbosity >= 2)
				MEnvironment.outStream.println("DEBUG: Getting HU Ceiling. ");
			totalHerbrandMax = getHerbrandUniverseCeilingFor(MFormulaManager
					.makeAnd(myQueryFormula, queryAxiomsConjunction),
					prenexExistential);
		}

		if (debug_verbosity >= 2)
			MEnvironment.outStream
					.println("DEBUG: Time (ms) in getHerbrandUniverseCeilingFor block: "
							+ (mxBean.getCurrentThreadCpuTime() - startTime)
							/ 1000000);
		startTime = mxBean.getCurrentThreadCpuTime();

		// TODO: Deal with binary and larger preds. (Annoying, complicated, many
		// many axioms.)
		boolean noNonUnaryPredicates = true;
		// For now, tupling is only allowed if the vocabulary involves only
		// unary predicates
		// Sorts are unary by nature. So check the state preds
		for (Relation r : vocab.predicates.values())
			if (r.arity() > 1) {
				noNonUnaryPredicates = false;

				if (debug_verbosity >= 1 && doTupling)
					MEnvironment.outStream
							.println("DEBUG: Tupling was enabled, but signature had non-unary predicates. Tupling is\n"
									+ "  not allowed (for now) when binary or larger relations are involved.");
				break;
			}

		// was: && herbrandmax > 1; for now always tuple if told to
		// (provided that we have a finite Herbrand universe)
		if (doTupling && totalHerbrandMax > 0 && noNonUnaryPredicates) {
			// Try to build a more optimal "tupled" query
			// This tupling always yields a size=1 bound, when it's possible
			// The price: More predicates.

			// DEBUG
			startTime = mxBean.getCurrentThreadCpuTime();
			if (debug_verbosity >= 2)
				MEnvironment.outStream.println("DEBUG: Pre-tupling block Time: "
						+ (startTime - start) / 1000000);

			// If the user wants IDB output with tupling, need all the IDB
			// formulas to be
			// ********** QUANTIFIER FREE ***********.

			boolean idbs_ok = true;
			ContainsQuantifiersCheckV qfreecheck = new ContainsQuantifiersCheckV();
			for (String idbname : getIDBNamesToOutput())
			{
				// Make certain that we have an indexing for this IDB.
				if (!idbOutputIndexing.containsKey(idbname))
					throw new MGEUnknownIdentifier(
							"Query with tupling enabled lacked indexing for IDB to output: "
									+ idbname);

				// Get the IDB formula
				String[] split = idbname.split(":");
				String polName = split[0].toLowerCase();

				if (split.length < 2)
					throw new MGEUnknownIdentifier(
							"IDB names for output must be of the form policyname:idbname.");

				String internal_idbname = idbname.substring(
						polName.length() + 1).toLowerCase(); // include :
				// MREPL.outStream.println(polName + " " + internal_idbname);

				if (!myIDBCollections.containsKey(polName))
					throw new MGEUnknownIdentifier("Unknown IDB collection: "
							+ polName);
				MIDBCollection coll = myIDBCollections.get(polName);

				if (!coll.idbs.containsKey(internal_idbname))
					throw new MGEUnknownIdentifier("Unknown IDB: "
							+ internal_idbname + " in collection: " + polName);

				Formula idbformula = coll.idbs.get(internal_idbname);

				// If there is a quantifier...
				if (idbformula.accept(qfreecheck)) {
					idbs_ok = false;
					break;
				}
			}

			if (debug_verbosity >= 2)
				MEnvironment.outStream.println("Checked IDB output indexing. Time: "
						+ (mxBean.getCurrentThreadCpuTime() - startTime)
						/ 1000000);
			startTime = mxBean.getCurrentThreadCpuTime();

			List<String> indexedIDBNamesToOutput = new ArrayList<String>();
			
			// First check to see if the user query is prenex existential-only
			// COULD lift existentials if not in the prefix, but we don't (for
			// now)
			// Also need to meet other requirements (certain axioms excluded,
			// for instance)

			if (idbs_ok && prenexExistential
					&& vocab.axioms.funcPartial.size() == 0
					&& vocab.axioms.funcTotal.size() == 0
					&& vocab.axioms.otherConstraintStrings.size() == 0) {
				// Construct new vocabulary and new query formula.
				// (new formula should be the query formula's translation only,
				// since the new vocab will contain new axioms over the new
				// signature.)

				// In this special case of tupling, we get only one sort that
				// represents the type of the tuple. Thus we can let UNIV
				// represent
				// that sort in the new query. First we translate the formula
				// (which tells us new predicates, etc.) and then translate the
				// vocab to include new disjointness and new preds.

				if (debug_verbosity >= 2)
					MEnvironment.outStream.println("DEBUG: Ran prenexcheck. Time: "
							+ (mxBean.getCurrentThreadCpuTime() - startTime)
							/ 1000000);
				startTime = mxBean.getCurrentThreadCpuTime();

				if (debug_verbosity >= 2)
					MEnvironment.outStream
							.println("DEBUG: Tupling is allowed and enabled. Starting matrix rewrite. ");

				MatrixTuplingV mtup = new MatrixTuplingV(pren, vocab);
				Formula tupledFormula = pren.matrix.accept(mtup);

				if (debug_verbosity >= 3) {
					// Show what the tupled sorts are in detail.
					MEnvironment.outStream
							.println("DEBUG: Tupled sorts that appeared in the query: ");
					for (MSort s : mtup.newvocab.sorts.values()) {
						String childstr = "(TOP!)";
						if (s.parent != null)
							childstr = "(PARENT: " + s.parent.name + ")";
						MEnvironment.outStream.println("  " + s.name + " " + childstr);
					}
				}

				if (debug_verbosity >= 2)
					MEnvironment.outStream.println("DEBUG: Matrix tupling complete. Time: "
							+ (mxBean.getCurrentThreadCpuTime() - startTime)
							/ 1000000);
				startTime = mxBean.getCurrentThreadCpuTime();

				// replaces myIDBCollections in new query creation
				HashMap<String, MIDBCollection> tupledIDBCollections = new HashMap<String, MIDBCollection>();

				// Re-use visitors when possible
				HashMap<MIDBCollection, RelationAndVariableReplacementV> initialVisitors = new HashMap<MIDBCollection, RelationAndVariableReplacementV>();
				// HashMap equality is by entry equality is by key/value
				// equality, so it's ok to use them as keys here.
				HashMap<HashMap<Variable, Variable>, RelationAndVariableReplacementV> indexingVisitors = new HashMap<HashMap<Variable, Variable>, RelationAndVariableReplacementV>();

				// ******************
				// IDB OUTPUT 
				// ******************
				
				if (getIDBNamesToOutput().size() > 0)
				{
					for (String idbname : getIDBNamesToOutput())
					{
						// We want to tuple this idb formula, so it can be
						// included in output.
						idbname = idbname.toLowerCase();

						// Get the IDB formula
						int firstColon = idbname.indexOf(':');
						String polName = idbname.substring(0, firstColon);
						String internal_idbname = idbname
								.substring(firstColon + 1); // include :

						// These should all be valid, would have errored out
						// beforehand otherwise.
						MIDBCollection coll = myIDBCollections.get(polName);
						Formula originalIDBFormula = coll.idbs.get(internal_idbname);

						// ***
						// Convert from the original policy/custom's vocab to
						// the ubervocab being used for this query.
						RelationAndVariableReplacementV vis;
						if (initialVisitors.containsKey(coll))
							vis = initialVisitors.get(coll);
						else {
							vis = MIDBCollection.getReplacementVisitor(
									coll.vocab, vocab);
							initialVisitors.put(coll, vis);
						}
						originalIDBFormula = originalIDBFormula.accept(vis);

						
						// for each indexing
						Set<List<String>> indexings = idbOutputIndexing.get(idbname);
						for(List<String> user_indexing : indexings)
						{
							// Fresh copy of the original formula
							Formula idbformula = originalIDBFormula;
							
							
							// Assemble suffix for this indexing
							String idbnameSuffix = "_";
							boolean first = true;
							for(String iStr : user_indexing)
							{
								if(first)
								{
									idbnameSuffix += iStr;
									first = false;
								}
								else
									idbnameSuffix += ","+iStr;
							}
						
						
							// <<<<<<< INDENTATION WRONG

							
							
						HashMap<Variable, Variable> newvars = new HashMap<Variable, Variable>();


						// MREPL.outStream.println("pren.revIndexing, then user_indexing for idb: "+idbname);
						// MREPL.outStream.println(pren.revIndexing);
						// MREPL.outStream.println(user_indexing);

							int ii = 0;
							for (Variable v : coll.varOrdering) 																
							{
								// We are at the ii-th variable in the IDB's
								// input vector.
								// If the user-supplied ordering was
								// 1,2,3,...etc, we could just:
								// Variable v2 =
								// pren.revIndexing.get(String.valueOf(ii));
								// Instead we need to use the value the user
								// gave.
								// (Note: revindexing is 1-based, which is why
								// the user-supplied ordering is.)

								int iIndex = Integer.parseInt(user_indexing
										.get(ii));
								Variable v2 = pren.revIndexing.get(String
										.valueOf(iIndex));

								// If the query didn't provide enough variables
								// to support this:
								if (v2 == null)
									throw new MGEArityMismatch(
											"Could not tuple "
													+ idbname
													+ " for output; query prefix was insufficiently wide. ");

								// Replace v with v2, if they are different
								// objects
								if (v != v2)
									newvars.put(v, v2);

								ii++;
							}
							

						// ***
						// Still placeholder vars at this point, just the
						// correct ones.
						// It still remains to put this IDB in the context of
						// the tuple vector.
						// (Placeholder reqVars need to be replaced with the
						// quantified vars from the
						// query. Note that the IDB may not use them all.)
						if (newvars.size() > 0) // Don't even bother if there's
												// no replacements to make
						{
							RelationAndVariableReplacementV renaming;
							if (indexingVisitors.containsKey(newvars))
							{
								renaming = indexingVisitors.get(newvars);
							}
							else
							{
								renaming = new RelationAndVariableReplacementV(
										new HashMap<Relation, Relation>(),
										newvars);								
								indexingVisitors.put(newvars, renaming);
							}

							idbformula = idbformula.accept(renaming);
						}
						
						// ***
						// Use same MatrixTuplingV as we used for the main
						// formula. (Re-use cached work.)
						Formula new_idbformula = idbformula.accept(mtup);
						// MREPL.outStream.println(idbformula);
						// MREPL.outStream.println(new_idbformula);

						// Store this new formula for the new query's use.
						if (!tupledIDBCollections.containsKey(polName))
							tupledIDBCollections.put(polName,
									new MInternalIDBCollection(polName,
											mtup.newvocab));

						String new_internal_name = internal_idbname + idbnameSuffix;
						
 						indexedIDBNamesToOutput.add(polName+":"+new_internal_name);											

						tupledIDBCollections.get(polName).idbs.put(new_internal_name, new_idbformula);
						
						//System.err.println("Tupling IDB: "+new_internal_name);
						//System.err.println(new_idbformula.hashCode());
						
						//System.err.println(tupledIDBCollections.keySet());
						//System.err.println(tupledIDBCollections.get(polName).idbs);
						//System.err.println(new_internal_name);						
						
						
						} // end for each indexing on this name
					} // for each idbname to output
					
				} // if we have idbs to include in output

				if (debug_verbosity >= 2)
					MEnvironment.outStream.println("DEBUG: Tupled "
							+ getIDBNamesToOutput().size() + " IDBs (possibly >1 indexing per). Time: "
							+ (mxBean.getCurrentThreadCpuTime() - startTime)
							/ 1000000);
				startTime = mxBean.getCurrentThreadCpuTime();

				// tupled sorts will be in mtup.newvocab

				// Deal with the abstract constraint
				// For everything abstract in the pre-tupling vocab
				for (String relName : vocab.axioms.setsAbstract) {
					MSort oldSort = vocab.getSort(relName);

					// Get the list of tupled extensions of relName
					Set<String> newPreds = mtup.tupledPredicates.get(relName);
					if (newPreds == null)
						continue;

					for (String newName : newPreds) {
						// Carry over the abstract constraint.
						// MSort newSort = mtup.newvocab.getSort(newName);

						String idxStr = newName
								.substring(newName.lastIndexOf("_") + 1);
						// Need to add children if any do not yet appear:
						for (MSort oldChild : oldSort.subsorts) {
							if (!mtup.newvocab
									.isSort(oldChild.name + "_" + idxStr)) {
								String newname = oldChild.name + "_" + idxStr;
								mtup.newvocab.addSubSort(newName, newname);

								// make sure caches are correct
								if (!mtup.oldPredNamesTupledWithIndex
										.containsKey(idxStr))
									mtup.oldPredNamesTupledWithIndex.put(idxStr,
											new HashSet<String>());
								if (!mtup.tupledPredicates
										.containsKey(oldChild.name))
									mtup.tupledPredicates.put(oldChild.name,
											new HashSet<String>());
								mtup.oldPredNamesTupledWithIndex.get(idxStr).add(
										oldChild.name);
								mtup.tupledPredicates.get(oldChild.name).add(
										newname);
							}

							if (debug_verbosity >= 3)
								MEnvironment.outStream.println("DEBUG: Abstract sort "
										+ relName + " at index " + idxStr
										+ ": Forced addition of tupled child "
										+ oldChild.name + "_" + idxStr);
						}

						// No more if here; Child sorts will
						// always be carried over if the abstract parent appears
						// at index i.
						// if(oldSort.subsorts.size() ==
						// newSort.subsorts.size())
						mtup.newvocab.axioms.addConstraintAbstract(newName);
					}
				}

				// 1/28/10 subsort exhaustiveness no longer required. leaving
				// code commented out!
				// Instead, do the above

				/*
				 * 
				 * 
				 * //
				 * ***********************************************************
				 * *** // Deal with potential Child-sort exhaustiveness problem
				 * which // left unfixed, causes a completeness issue. Margrave
				 * forces parent // sorts to be exhausted by their children.
				 * Tupling conveniently // (and efficiently) "forgets" about
				 * sorts that are unmentioned in the // query formula. Imagine
				 * Naturals > {Evens, Odds} over a formula // that mentions only
				 * Evens. We still may need to consider models where // Evens
				 * doesn't hold.
				 * 
				 * // For each tupled parent sort (P_i), // If all its children
				 * are included in the tupled signature (C_i), do nothing. // If
				 * some of its children are missing from the tupled signature,
				 * lump // them together into one big "otherP_i_CorDorE_only" to
				 * represent the situation // where P holds, but none of the
				 * included children FOR THIS INDEX do. // Note: =12 does NOT
				 * imply otherP_1 <--> otherP_2, since different // subs of P
				 * may occur for those indices.
				 * 
				 * // for each TUPLED sort which was included (if not included,
				 * we don't care) // Key: parent. Value: Set of child sorts not
				 * included. HashMap<String, Set<String>> noneOfAboves = new
				 * HashMap<String, Set<String>>();
				 * 
				 * for(MGSort oldparent : myVocab.sorts.values()) // P {
				 * 
				 * // How was this sort tupled (if at all)? Set<String>
				 * newparentnames = mtup.getTupledPredicatesFor(oldparent.name);
				 * 
				 * //MREPL.outStream.println("::"+oldparent.name +
				 * " --> "+newparentnames);
				 * 
				 * for(String newparentname : newparentnames) // P_i { MGSort
				 * newparent = mtup.newvocab.getSort(newparentname);
				 * 
				 * String idx =
				 * newparentname.substring(newparentname.lastIndexOf("_")+1);
				 * 
				 * // What child sorts would we EXPECT, if all were included?
				 * Set<String> expected_names = new HashSet<String>();
				 * for(MGSort oldchild : oldparent.subsorts)
				 * expected_names.add(oldchild.name + "_" + idx);
				 * 
				 * // Which of expected did not make it in? (if none, move on.
				 * If some, lump together.) Set<String> missing = new
				 * HashSet<String>(expected_names); Set<String> these_appear =
				 * new HashSet<String>();
				 * 
				 * for(MGSort newchild : newparent.subsorts)
				 * these_appear.add(newchild.name);
				 * 
				 * missing.removeAll(these_appear);
				 * 
				 * // If we're missing some, but NOT missing all (if missing
				 * all, can forget about child sorts) if(missing.size() > 0 &&
				 * these_appear.size() > 0) { // Need to create a
				 * "none of the above" sort.
				 * 
				 * try { String missingstr = ""; for(String miss : missing)
				 * missingstr += "_"+miss;
				 * 
				 * // The index is in newparentname (e.g., P_1)
				 * mtup.newvocab.addSubSort(newparentname,
				 * "other_"+newparentname+":"+missingstr); MGSort newsort =
				 * mtup.newvocab.getSort("other_"+newparentname+":"+missingstr);
				 * noneOfAboves.put(newparentname, missing);
				 * 
				 * // Since it represents
				 * "none of the appearing childsorts applied", // it must be
				 * disjoint from them. // (But remember to remove it from the
				 * list...we just added it after all.) Set<MGSort>
				 * former_children = new HashSet<MGSort>(newparent.subsorts);
				 * former_children.remove(newsort);
				 * mtup.newvocab.axioms.addConstraintDisjoint(newsort,
				 * former_children);
				 * 
				 * 
				 * if(this.debug_verbosity >= 3) {
				 * MREPL.outStream.println("DEBUG: Covering parent sorts...");
				 * MREPL.outStream
				 * .println(" "+newparent.name+" was missing: "+missing);
				 * MREPL.outStream.println("  Added new sort to tupled vocab:" +
				 * newsort);MREPL.outStream.println(
				 * "  The new sort is disjoint from everything in: "
				 * +former_children); } } catch(MGEBadIdentifierName e) { throw
				 * new MGEBadQueryString(e.toString()); }
				 * 
				 * }
				 * 
				 * }
				 * 
				 * 
				 * 
				 * }
				 * 
				 * if(debug_verbosity >= 2)MREPL.outStream.println(
				 * "DEBUG: Child-sort exhaustiveness block done. Time: " +
				 * (mxBean.getCurrentThreadCpuTime()-startTime)/1000000);
				 * startTime = mxBean.getCurrentThreadCpuTime();
				 * 
				 * 
				 * // AFTER THIS POINT, NOT EVERY NEWVOCAB SORT WILL COORISPOND
				 * TO AN OLDVOCAB ONE! // (none of the above sorts added above.)
				 */

				// ***************************
				// Sort of original predicates is respected
				// If P: AxB, P_1,3(z) ---> A_1(z) and B_3(z)
				// (This is already enforced by the sig of the new predicate in
				// the new vocab.)

				// (No code needed)

				// ***************************
				// Prefix's sort is respected
				
				// This is "tupling axioms" or \beta in the doc
				
				// If we translated forsome x:A | forsome y:B to forsome z:UNIV
				// it must still hold that A_1(z) and B_2(z)
				Set<Formula> respected_2 = new HashSet<Formula>();
				int index = 1;
				for (String sortname : pren.tupleTypeConstruct.split(" ")) 
				{
					try
					{
						//System.err.println("Must assert that "+sortname+" in index "+index);
						
						if (!mtup.newvocab.isSort(sortname + "_" + index))
						{
							mtup.newvocab.addSort(sortname + "_" + index);
							
							// must also cache this sort in mtup's 2 caches
							
							String iiStr = Integer.toString(index);
														
							// make sure caches are correct
							if (!mtup.oldPredNamesTupledWithIndex.containsKey(iiStr))
								mtup.oldPredNamesTupledWithIndex.put(iiStr, new HashSet<String>());
							if (!mtup.tupledPredicates.containsKey(sortname))
								mtup.tupledPredicates.put(sortname,	new HashSet<String>());
							
							mtup.oldPredNamesTupledWithIndex.get(iiStr).add(sortname);
							mtup.tupledPredicates.get(sortname).add(sortname + "_" + index);							
							
						}
						
						
						Relation r = mtup.newvocab.getRelation(sortname + "_"
								+ index);
						respected_2.add(MFormulaManager
								.makeAtom(mtup.newvar, r));

					} catch (MGEBadIdentifierName e) {
						throw new MGEBadQueryString("Tupling error: Relation "
								+ sortname + "_" + index
								+ " was not created properly.");
					}

					index++;
				}

				Formula respected_2_fmla = MFormulaManager
						.makeConjunction(respected_2);
				tupledFormula = MFormulaManager.makeAnd(tupledFormula,
						respected_2_fmla);

				if (debug_verbosity >= 2)
					MEnvironment.outStream
							.println("DEBUG: Size of tupling axioms to make prefix sort respected: "
									+ measureFullFormulaSize(respected_2_fmla)
									+ " nodes, "
									+ measureFormulaReferences(respected_2_fmla)
									+ " references.");

				if (debug_verbosity >= 2)
					MEnvironment.outStream.println("DEBUG: Prefix sort respected. Time: "
							+ (mxBean.getCurrentThreadCpuTime() - startTime)
							/ 1000000);
				startTime = mxBean.getCurrentThreadCpuTime();

				// ***************************************************************
				// LONE
				// For each i and j (i != j), and each lone P
				// If Pi and Pj both appear in the query, we need to conclude
				// that =ij.
				// (Even if =ij doesn't appear in the query proper, we need to
				// include it.
				// Consider this situation:
				// 
				// lone P, Q
				// P_i and P_j; Q_j and Q_k
				// =ij =jk do not appear
				// =ik appears.
				// We need to force =ik since equality is transitive.
				// Also just using =ij means we don't have to separately enforce
				// that
				// i and j must be isomorphic w/r/t predicate membership

				HashSet<Formula> lone_formulas = new HashSet<Formula>();

				Set<String> lones = new HashSet<String>();
				lones.addAll(vocab.axioms.setsAtMostOne);
				lones.addAll(vocab.axioms.setsSingleton);

				int iLoneEqCounter = 0;
				Set<Formula> ij_triggers = new HashSet<Formula>();

				// *****************************
				// Leaving out SOME for now.
				// Print warning message for "ONE" vs. "LONE": Tupling doesn't
				// treat them as different
				if (vocab.axioms.setsSingleton.size() > 0)
					MEnvironment.outStream
							.println("\nWarning: Tupling treats ONE as if it were LONE.\n");

				long triggerCount = 0;

				// For each pair of distinct indices
				for (int ii = 1; ii <= pren.qCount; ii++) {
					// MREPL.outStream.println("\n\n -> " + ii + " --> " +
					// mtup.oldPredNamesTupledWithIndex.get(String.valueOf(ii)));

					for (int jj = ii + 1; jj <= pren.qCount; jj++) {
						// What preds were shared? Much better than checking
						// each lone predicate name.
						// (caching is good. was: 984 ms now: 15ms)
						// will still take a while if there are a lot of shared
						// preds, of course.
						Set<String> sharedOldPredNames = new HashSet<String>(
								mtup.oldPredNamesTupledWithIndex.get(String
										.valueOf(ii)));
						sharedOldPredNames
								.retainAll(mtup.oldPredNamesTupledWithIndex
										.get(String.valueOf(jj)));
						sharedOldPredNames.retainAll(lones);

											
						//System.out.println(ii+","+jj);
						//System.out.println(sharedOldPredNames);
						//System.out.println(mtup.oldPredNamesTupledWithIndex.get(String.valueOf(ii)));
						//System.out.println(mtup.oldPredNamesTupledWithIndex.get(String.valueOf(jj)));
						//System.out.println();
						
						
						// ^^^ Must appear at index i, index j, and be LONE.
						
						// reset trigger list
						ij_triggers.clear();

						for (String lonesort : sharedOldPredNames)
						{
							// For this LONE P, Are both P_i and P_j involved in
							// the query?

							try {
								Relation pi = mtup.newvocab
										.getRelation(lonesort + "_" + ii);
								Relation pj = mtup.newvocab
										.getRelation(lonesort + "_" + jj);

								// If so, assert P_i and P_j --> =ij

								Formula trigger1 = MFormulaManager.makeAtom(
										mtup.newvar, pi);
								Formula trigger2 = MFormulaManager.makeAtom(
										mtup.newvar, pj);
								ij_triggers.add(MFormulaManager.makeAnd(
										trigger1, trigger2));
								triggerCount++;

							} catch (MGEUnknownIdentifier e) {
							} // fall through

						} // each lone

						// Did we get any triggers?
						if (ij_triggers.size() > 0) {
							Formula disj_of_triggers = MFormulaManager
									.makeDisjunction(ij_triggers);

							// Is this a new equality pred that hasn't appeared
							// in the query itself?
							String eqpredname = "=_" + ii + "," + jj;
							String reverseeqpredname = "=_" + jj + "," + ii;
							
							/*
							if (!mtup.newvocab.predicates
									.containsKey(eqpredname)
									&& !mtup.newvocab.predicates
											.containsKey(reverseeqpredname))
							{
								iLoneEqCounter++;
								mtup.equalAxiomsNeeded.add(eqpredname);
								try {
									mtup.newvocab.addPredicate(eqpredname,
											pren.tupleTypeName); // add new
																	// predicate
								} catch (MGEBadIdentifierName e) {
									throw new MGEBadQueryString(
											"Unable to create equality predicate: "
													+ eqpredname);
								}
							} else */
							if (mtup.newvocab.predicates
									.containsKey(reverseeqpredname))
								eqpredname = reverseeqpredname;
							// else, we're fine

							Relation eqrel = mtup.newvocab.predicates
									.get(eqpredname);
							Formula iseq = MFormulaManager.makeAtom(
									mtup.newvar, eqrel);
							lone_formulas.add(MFormulaManager.makeImplication(
									disj_of_triggers, iseq));

							if (debug_verbosity >= 2) {
								MEnvironment.outStream
										.println("DEBUG: LONE resulted in ---> ");
								MEnvironment.outStream.println(disj_of_triggers
										+ "\n  THEN " + iseq);
							}
						}

					} // jj
				} // ii

				Formula loneAxiomsFmla = MFormulaManager
						.makeConjunction(lone_formulas);
				tupledFormula = MFormulaManager.makeAnd(tupledFormula,
						loneAxiomsFmla);

				if (debug_verbosity >= 2)
					MEnvironment.outStream
							.println("DEBUG: Number of new equality predicates introduced by LONE: "
									+ iLoneEqCounter);

				if (debug_verbosity >= 2)
					MEnvironment.outStream.println("DEBUG: LONE handling. Time: "
							+ (mxBean.getCurrentThreadCpuTime() - startTime)
							/ 1000000);
				startTime = mxBean.getCurrentThreadCpuTime();

				// *******************************
				// FINALIZE *single* top-level sort
				// DO NOT ADD ANY SORTS AFTER THIS!
				// *******************************
				try 
				{
					mtup.newvocab.addSingleTopLevelSort(pren.tupleTypeName);
				} catch (MGEBadIdentifierName e) {
					throw new MGEBadQueryString(
							"Tupling error: Unable to create unified top-level sort: "
									+ pren.tupleTypeName);
				}

				if (debug_verbosity >= 2)
					MEnvironment.outStream.println("DEBUG: Finalized top level sort. Time: "
							+ (mxBean.getCurrentThreadCpuTime() - startTime)
							/ 1000000);
				startTime = mxBean.getCurrentThreadCpuTime();

				// ***************************
				// EQUALITY AXIOMS
				// If two components are "equal" (are in a =i,j predicate)
				// then they are isomorphic w/r/t predicates and sorts
				Set<Formula> equalityAxioms = new HashSet<Formula>();

				// Set --> List so we can iterate in order (double-loop inside)
				List<String> equalsNeeded = new ArrayList<String>(
						mtup.equalAxiomsNeeded);
				for(int iNeeds = 0; iNeeds < equalsNeeded.size();iNeeds++)
				{
					String needs = equalsNeeded.get(iNeeds);
					
					// Needs: "=_"+leftidx+","+rightidx
					// For a specific =_{i,j}
					int needsLastComma = needs.lastIndexOf(",");
					String rightidx = needs.substring(needsLastComma + 1);
					String leftidx = needs.substring(2, needsLastComma);

					Formula isoconj = makePredsIndistinguish(vocab,
							mtup.newvocab, mtup.newvar, leftidx, rightidx);
					Relation eqrel = mtup.newvocab.predicates.get(needs);
					Formula eqFormula = MFormulaManager.makeAtom(mtup.newvar,
							eqrel);

					if (debug_verbosity >= 2) {
						//MREPL.outStream
						//		.println("DEBUG: Equality implies predicate isomorphism: "
						//				+ leftidx + "=" + rightidx);
						//MREPL.outStream.println(isoconj);
					}

					// (1) Simulated equality leads to isomorphism
					// Don't bother stating this if leftidx = rightidx, since
					// the bi-implication is trivial then.
					if (!leftidx.equals(rightidx))
						equalityAxioms.add(MFormulaManager.makeImplication(
								eqFormula, isoconj));

					// (2) Simulated equality is an equivalence relation
					// But note: We only care about certain =_{i,j} relations.
					// SYMMETRY: The matrix rewrite handles this, since if =2,1
					// has already
					// been created, it will be used in the place of =1,2.

					// If this is =i,i for some i, it must hold. (Reflexivity)
					// TODO these should not even exist; supposed to be replaced by tautology
					if (leftidx.equals(rightidx))
						equalityAxioms.add(eqFormula);

					// *******************************
					// state transitivity: =1,2 and =3,2 imply =1,3
					for(int iNeedOther = iNeeds+1; iNeedOther < equalsNeeded.size();iNeedOther++)
					{
						String other = equalsNeeded.get(iNeedOther); 
							
						int otherLastComma = other.lastIndexOf(",");					
						String rightidx2 = other.substring(otherLastComma + 1);
						String leftidx2 = other.substring(2, otherLastComma);

						// this bit can probably go (since +1)
						/*
						if (rightidx.equals(rightidx2)
								&& leftidx.equals(leftidx2))
							continue;
						if (leftidx.equals(rightidx2)
								&& rightidx.equals(leftidx2))
							continue;
							*/

						try
						{
							String thirdpred = getThirdEq(mtup.newvocab,
									leftidx, rightidx, leftidx2, rightidx2,
									pren);

							// If this is a newly added equality pred, make sure
							// we state it implies predicate isomorphism
							/*if (thirdpred.length() > 0
									&& !equalsDone.contains(thirdpred)) {
								if (debug_verbosity >= 2)
									//MREPL.outStream.println("DEBUG: Queueing "
									//		+ thirdpred
									//		+ " for isomorphism axiom.");
								equalsNeeded.add(thirdpred);
							}*/

							if (thirdpred.length() > 0)
							{
								Relation eqRel2 = mtup.newvocab.predicates
										.get(other);
								Relation eqRel3 = mtup.newvocab.predicates
										.get(thirdpred);

								// MREPL.outStream.println("---->\n" + eqRel3.name() +
								// " of arity "+ eqRel3.arity());

								Formula eqFormula2 = MFormulaManager.makeAtom(
										mtup.newvar, eqRel2);
								Formula eqFormula3 = MFormulaManager.makeAtom(
										mtup.newvar, eqRel3);
								Formula eqTrans = MFormulaManager
										.makeImplication(
												MFormulaManager.makeAnd(
														eqFormula, eqFormula2),
												eqFormula3);
								equalityAxioms.add(eqTrans);

								if (debug_verbosity >= 2) {
									//MREPL.outStream
									//		.println("DEBUG: Transitivity of equality --->");
									//MREPL.outStream.println("  " + rightidx + ","
									//		+ leftidx + " and " + rightidx2
									//		+ "," + leftidx2);
									//MREPL.outStream.println("  --> " + thirdpred);
								}

							} // end if appropriate third pred

						} catch (MGEBadIdentifierName e) {
							throw new MGEBadQueryString(
									"Tupling error: unable to state transitivity of equality w/ indices: "
											+ leftidx + "," + rightidx
											+ " and " + leftidx2 + ","
											+ rightidx2);
						}

					} // end for each =ij (other)

				} // end for each =ij (needs)

				
				
				Formula equalityAxiomsFmla = MFormulaManager
						.makeConjunction(equalityAxioms);
				tupledFormula = MFormulaManager.makeAnd(tupledFormula,
						equalityAxiomsFmla);

				if (debug_verbosity >= 2)
					MEnvironment.outStream
							.println("DEBUG: Size of tupling axioms to deal with express equality: "
									+ measureFullFormulaSize(equalityAxiomsFmla)
									+ " nodes, "
									+ measureFormulaReferences(equalityAxiomsFmla)
									+ " references. " + equalityAxioms.size() + " equality axioms overall.");

				if (debug_verbosity >= 2)
					MEnvironment.outStream.println("DEBUG: Equality axioms done. Time: "
							+ (mxBean.getCurrentThreadCpuTime() - startTime)
							/ 1000000);
				startTime = mxBean.getCurrentThreadCpuTime();

				// ****************************
				// Vocab change
				// disjointness: same construction as sorts themselves
				// Original form: A disj (B1, B2, B3...)

				// for each index
				// for each pred tupled into this index
				// copy disjs of that pred originally (names)
				// intersect w/ preds tupled into this index

				// is it possible for an implied disjointness to be lost when
				// tupling?
				// no, because we carry over all parents (and constrained
				// supersorts).

				// Avoid reachable check in addConstraintDisjoint by calculating
				// and copying directly.
				// mtup.newvocab.axioms.axiomDisjoints = disjoints we calculate
				// in this loop

				HashMap<MSort, Set<MSort>> newDisjoints = new HashMap<MSort, Set<MSort>>();
				int iDisjCount = 0;

				// IMPORTANT:
				// This loop deals with NON TOP LEVEL SORTS.
				// Pairs of top-level sorts are disjoint by definition, and are dealt with below.
				// (next block)
				
				// axioms for disjointness pt 1.
				Set<Formula> eqDisjAxioms = new HashSet<Formula>();

				for (int iIndex = 1; iIndex <= pren.qCount; iIndex++)
				{
					String strIndex = String.valueOf(iIndex);
					String strIndexWithUnderscore = "_" + strIndex;

					Set<String> oldPredsForThisIndex = mtup.oldPredNamesTupledWithIndex
							.get(strIndex);

					for (String oldPredName : oldPredsForThisIndex)
					{

						MSort oldSort = vocab.getSort(oldPredName);

						// sorts disjoint from oldSort in old vocab
						Set<MSort> oldDisjoints = vocab.axioms.axiomDisjoints
								.get(oldSort);

						if (oldDisjoints == null)
							continue;

						MSort newSort = mtup.newvocab.getSort(oldPredName
								+ strIndexWithUnderscore);
						newDisjoints.put(newSort, new HashSet<MSort>());

						for (MSort oldDisjoint : oldDisjoints)
						{
							// *******************************************
							// DISJ AXIOM TYPE 1 (equality + disj)

							for (int iIndex2 = iIndex + 1; iIndex2 <= pren.qCount; iIndex2++) 
							{							
								String new2 = oldDisjoint.name + "_" + iIndex2;

								// second index -- is this sort tupled into it?
								// don't validate
								if (!mtup.newvocab.fastIsSort(new2))
									continue;

								
								MSort ns1 = newSort;
								MSort ns2 = mtup.newvocab.getSort(new2);
								Relation eqpred = mtup.newvocab
										.getRelation("=_" + iIndex + ","
												+ iIndex2);
								
								Formula thisAxiom = makeDisjointnessEqAxiom(mtup.newvar, ns1.rel, ns2.rel, eqpred);
								eqDisjAxioms.add(thisAxiom);

							}

							// *******************************************
							// DISJ AXIOM TYPE 2 (disj of new sorts)

							// is the other sort tupled for this index?
							if (oldPredsForThisIndex.contains(oldDisjoint.name))
							{
								MSort newDisjoint = mtup.newvocab
										.getSort(oldDisjoint.name
												+ strIndexWithUnderscore);
								// boolean added =
								// mtup.newvocab.axioms.addConstraintDisjoint(newSort,
								// newDisjoint);
								newDisjoints.get(newSort).add(newDisjoint);
								// if(added)
								iDisjCount++;
							}
						}

					}
				} // end for each index
				mtup.newvocab.axioms.axiomDisjoints = newDisjoints;

				// TODO is it safe to just call vocab.axioms.axiomsDisjoints.get
				// if we store only one way?

				Formula eqDisjAxiomsFmla = MFormulaManager
						.makeConjunction(eqDisjAxioms);
				tupledFormula = MFormulaManager.makeAnd(tupledFormula,
						eqDisjAxiomsFmla);


				int iTopLevelCount = 0;

				// *********************************
				//     TOP LEVEL SORTS DISJOINTNESS
				// If A and B are top level sorts in old vocab, A_i and B_i will
				// NOT be top-level in new vocab,
				// which has a single unified top-level sort based on the
				// prefix.
				// However, we still want to require A & B to be disjoint in all
				// components! (and we must issue other axioms)

				// Get set of old top-level sorts
				Set<String> oldtopnames = new HashSet<String>();
				for (MSort top : vocab.sorts.values()) {
					if (vocab.isSubtype(top))
						continue; // only do this for tops
					oldtopnames.add(top.name);
				}
				
				// for each index, the set of those top-sorts w/ that index that
				// are used are all pairwise disj
				// {A_1, B_1}, {C_2, D_2, E_2}, etc.)
				for (int ii = 1; ii <= pren.qCount; ii++) 
				{
					// For some reason the keys here are strings, not integers...
					String iiStr = Integer.toString(ii);
					Set<String> oldAppearingAti = mtup.oldPredNamesTupledWithIndex.get(iiStr);
					oldAppearingAti.retainAll(oldtopnames);
					
					//System.err.println(oldtopnames);
					//System.err.println(oldAppearingAti);
					//System.err.println(mtup.oldPredNamesTupledWithIndex.get(iiStr));
					//System.err.println();
					
					
					// ********************************
					// disj part 2
					// ALL top-level sorts that appear at index i are disjoint 
					// accumulate the list
					Set<String> new_i_names = new HashSet<String>();					
					for(String sname : oldAppearingAti)
					{
						new_i_names.add(sname + "_" + ii);
					}
					if (new_i_names.size() > 1)
					{	
						//System.err.println(new_i_names);
						mtup.newvocab.axioms.addConstraintDisjoint(new_i_names);
					}
					
										
					
					// ********************************
					// disj part 1				
					// For every OTHER index j
					Set<Formula> eqTopLevelDisjAxioms = new HashSet<Formula>();
					for(int jj = ii+1; jj <= pren.qCount; jj++)
					{
						String jjStr = Integer.toString(jj);
						Set<String> oldAppearingAtj = mtup.oldPredNamesTupledWithIndex.get(jjStr);
						oldAppearingAtj.retainAll(oldtopnames);
						
						// TODO really shouldn't represent these "impossible" equalities at all
						
						// For every pairing of an old at i and an old at j, if different they must imply !=
						for(String topi : oldAppearingAti)
							for(String topj : oldAppearingAtj)
							{
								if(topi.equals(topj)) continue;
								
								// Assert !=
								
								MSort ns1 = mtup.newvocab.fastGetSort(topi+"_"+ii);
								MSort ns2 = mtup.newvocab.fastGetSort(topj+"_"+jj);
								Relation eqpred = mtup.newvocab.getRelation("=_" + ii + ","+ jj);
								
								Formula thisAxiom = makeDisjointnessEqAxiom(mtup.newvar, ns1.rel, ns2.rel, eqpred);
								eqTopLevelDisjAxioms.add(thisAxiom);
								
								//System.err.println("Asserting "+thisAxiom);
								
							}
						
						//System.err.println(ii+","+jj+"; "+oldAppearingAti + " : "+oldAppearingAtj);
						
						
						
					}
					
					Formula eqTopLevelDisjAxiomsFmla = MFormulaManager.makeConjunction(eqTopLevelDisjAxioms);
					tupledFormula = MFormulaManager.makeAnd(tupledFormula,
							eqTopLevelDisjAxiomsFmla);
					iTopLevelCount += eqTopLevelDisjAxioms.size();
					

				}

				
				if (debug_verbosity >= 2) {
					MEnvironment.outStream.println("DEBUG: Disjointness. Time: "
							+ (mxBean.getCurrentThreadCpuTime() - startTime)
							/ 1000000);
					MEnvironment.outStream.println("    New disjoints: " + iDisjCount);
					MEnvironment.outStream
							.println("    Number of secondary eq/disj axioms added: "
									+ eqDisjAxioms.size());
					MEnvironment.outStream.println("    Number of top-level secondary eq/disj axioms added: "+iTopLevelCount);
				}
				startTime = mxBean.getCurrentThreadCpuTime();
				
				// *****************************
				// add NEW PREFIX to the formula
				// This must be done LAST!
				// *****************************
				Relation tupletyperel = mtup.newvocab
						.getSort(pren.tupleTypeName).rel;
				Decls d = MFormulaManager.makeOneOfDecl(mtup.newvar,
						tupletyperel);
				tupledFormula = MFormulaManager.makeExists(tupledFormula, d);

				if (debug_verbosity >= 2)
					MEnvironment.outStream.println("Top sorts done. Time: "
							+ (mxBean.getCurrentThreadCpuTime() - startTime)
							/ 1000000);
				startTime = mxBean.getCurrentThreadCpuTime();

				// Build new MGQuery and return the results of that
				// Simplifier will be called!
				MQuery tupledQuery = new MQuery(mtup.newvocab, tupledFormula,
						tupledIDBCollections);

				tupledQuery.mySATFactory = mySATFactory;
				tupledQuery.tupled = true;
				tupledQuery.doTupling = false;
				
				internalTupledQuery = tupledQuery; // allow access to creator's vocab, etc. for output
				internalTuplingVisitor = mtup;
				tupledQuery.internalTuplingVisitor = mtup;

				if (debug_verbosity >= 2)
					MEnvironment.outStream.println("Tupled query object created. Time: "
							+ (mxBean.getCurrentThreadCpuTime() - startTime)
							/ 1000000);
				startTime = mxBean.getCurrentThreadCpuTime();

				if (debug_verbosity >= 2) {
					MEnvironment.outStream.println("DEBUG: Tupling complete. ");

					// Get number of references in the two formulas:
					MEnvironment.outStream
							.println("DEBUG: TUPLED FORMULA NODE INSTANCE COUNT ->"
									+ measureFormulaReferences(tupledFormula));
					MEnvironment.outStream
							.println("DEBUG: ORIGINAL FORMULA NODE INSTANCE COUNT ->"
									+ measureFormulaReferences(myQueryFormula));

					// Get number of nodes (with repetition) in the two
					// formulas:
					MEnvironment.outStream.println("DEBUG: TUPLED FORMULA TOTAL SIZE ->"
							+ measureFullFormulaSize(tupledFormula));
					MEnvironment.outStream.println("DEBUG: ORIGINAL FORMULA TOTAL SIZE ->"
							+ measureFullFormulaSize(myQueryFormula));

					MEnvironment.outStream
							.println("DEBUG: TUPLED Pred Count ->"
									+ (mtup.newvocab.sorts.size() + mtup.newvocab.predicates
											.size()));
					MEnvironment.outStream.println("DEBUG: ORIGINAL Pred Count ->"
							+ (vocab.sorts.size() + vocab.predicates.size()));
					MEnvironment.outStream
							.println("DEBUG: (Note that this doesn't distinguish arity: adding binary or larger preds could be far worse even if unary pred count is reduced!");

					MEnvironment.outStream
							.println("\nDEBUG: Calling runQuery on the new tupled query.");
				}

				tupledQuery.debug_verbosity = debug_verbosity;

				// Need to update with new list 
				// But no explicit indexing needed, since we moved that into the relation names.
				tupledQuery.addIDBOutputs(indexedIDBNamesToOutput);				
								
				tupledQuery.msPreprocessingTime = msPreprocessingTime;

				if (mxBean.isCurrentThreadCpuTimeSupported()) {
					tupledQuery.msTuplingTime = (mxBean
							.getCurrentThreadCpuTime() - start) / 1000000;
				}

				return tupledQuery.runQuery();
			} // end of valid prenex check. Failing falls through to standard
				// behavior
			// PUT NOTHING HERE! Tupling stuff goes in the above if statement
		} // end if herbrandmax > 0 and doTupling

		int maxSizeToCheck;

		// Case 1: Infinite Herbrand Universe
		// Case 1a: Empty Herbrand Universe
		// Case 1b: User-provided ceiling under H.U. size?
		// Case 1c: *empty* H.U.: (forall x exists y (x != y) <--- this has
		// empty HU but no model at size 1... models at 2+)
		// We can't guarantee anything in this case, since KodKod doesn't allow
		// empty models.
		// (TODO: case 1c is a kludge.)
		if (totalHerbrandMax < 0 || totalHerbrandMax > sizeCeiling
				|| totalHerbrandMax == 0)
			maxSizeToCheck = sizeCeiling;

		// Case 2: Herbrand universe is finite and non-empty, and user's ceiling
		// is higher.
		else
			maxSizeToCheck = totalHerbrandMax;

		if (debug_verbosity >= 2) {
			MEnvironment.outStream
					.println("DEBUG: Preparing Bounds and passing query to Kodkod.");
		}

		if (debug_verbosity >= 3) {
			// Very very slow to print all this out (especially in IDE) if
			// formulas are
			// anywhere near a decent size

			MEnvironment.outStream.println("Query Formula: " + myQueryFormula);
			MEnvironment.outStream.println("Fixed Axioms Formula: " + myFixedAxioms);
			MEnvironment.outStream
					.println("Query Axioms Formula: " + queryAxiomsConjunction);
			// MREPL.outStream.println("Complete Formula: "+queryWithAxioms);
		}

		long cputime = (mxBean.getCurrentThreadCpuTime() - start) / 1000000; // result
																				// of
																				// cpu
																				// time
																				// is
																				// nanosecond
																				// precision.
		if (!mxBean.isCurrentThreadCpuTimeSupported())
			cputime = -1;

		if (debug_verbosity >= 1) {
			MEnvironment.outStream.println("DEBUG: Manager statistics: ");
			MFormulaManager.printStatistics();
			MEnvironment.outStream.println("Returning Solutions object; time so far:");
			MEnvironment.outStream.println("Preprocessing: " + msPreprocessingTime
					+ "ms, Tupling: " + msTuplingTime
					+ "ms, pre-Kodkod query processing time: " + cputime
					+ "ms.");
		}
		
		
		return new MQueryResult(this, queryWithAxioms,
				maxSizeToCheck, totalHerbrandMax, msPreprocessingTime,
				cputime, msTuplingTime);
				
	}

	private Formula makeDisjointnessEqAxiom(Variable newvar, Relation rel,
			Relation rel2, Relation eqpred)
	throws MGEManagerException
	{

		Formula atom1 = MFormulaManager.makeAtom(newvar, rel);
		Formula atom2 = MFormulaManager.makeAtom(newvar, rel2);
		Formula literal3 = MFormulaManager.makeNegation(
				MFormulaManager.makeAtom(newvar, eqpred));

		return MFormulaManager
				.makeImplication(MFormulaManager
						.makeAnd(atom1, atom2),
						literal3);
	}
	

	protected String getThirdEq(MVocab newvocab, String leftidx,
			String rightidx, String leftidx2, String rightidx2, PrenexCheckV vis)
			throws MGEBadIdentifierName {
		// the two indices for the third equality pred (if it exists)
		String left3, right3;

		if (leftidx.equals(leftidx2)) {
			// rightidx and rightidx2
			left3 = rightidx;
			right3 = rightidx2;
		} else if (rightidx.equals(leftidx2)) {
			// leftidx and rightidx2
			left3 = leftidx;
			right3 = rightidx2;
		} else if (leftidx.equals(rightidx2)) {
			// rightidx and leftidx2
			left3 = rightidx;
			right3 = leftidx2;
		} else if (rightidx.equals(rightidx2)) {
			// leftidx and leftidx2
			left3 = leftidx;
			right3 = leftidx2;
		} else
			// otherwise no match between these = preds, so no transitivity to
			// state
			return "";

		
		// There's a match: Does one of the two possible pred names exist?
		if (newvocab.predicates.containsKey("=_" + left3 + "," + right3))
			return "=_" + left3 + "," + right3;
		if (newvocab.predicates.containsKey("=_" + right3 + "," + left3))
			return "=_" + right3 + "," + left3;

		// obsolete April 2010
		/*
		// There's a match but no current equality predicate to cover it.
		// Create one.
		newvocab.addPredicate("=_" + left3 + "," + right3, vis.tupleTypeName); // name,
																				// not
																				// construct
*/
		return "";
	}

	public void removeIDBOutputIndexing(String idbname, List<String> indexing)
	{
		if(idbOutputIndexing.containsKey(idbname))
			idbOutputIndexing.get(idbname).remove(indexing);
	}
	
	public void addIDBOutputIndexing(String idbname, List<String> indexing)
			throws MGEArityMismatch, MGEUnknownIdentifier {
		// CHECK: the indexing Strings must actually be integers
		for (String inlist : indexing) {
			try {
				Integer.parseInt(inlist);
			} catch (NumberFormatException e) {
				throw new MGEArityMismatch(e.toString());
			}
		}

		// CHECK: Valid idb name

		String[] split = idbname.split(":");
		String polName = split[0].toLowerCase();

		if (split.length < 2)
			throw new MGEUnknownIdentifier(
					"IDB names for output must be of the form policyname:idbname.");

		String internal_idbname = idbname.substring(polName.length() + 1)
				.toLowerCase(); // include :

		if (!myIDBCollections.containsKey(polName))
			throw new MGEUnknownIdentifier("Unknown IDB collection: " + polName);
		MIDBCollection coll = myIDBCollections.get(polName);

		if (!coll.idbs.containsKey(internal_idbname))
			throw new MGEUnknownIdentifier("Unknown IDB: " + internal_idbname
					+ " in collection: " + polName);

		// CHECK: Arity matches between the indexing given and the idb.
		if (coll instanceof MPolicy
				&& coll.vocab.requestVectorOrder.size() != indexing.size())
			throw new MGEArityMismatch(
					"Given indexing did not match IDB arity: " + idbname + ", "
							+ indexing + "." + " Arity of IDB was: "
							+ coll.vocab.requestVectorOrder.size());
		if (coll instanceof MCustomIDB) {
			MCustomIDB custom_coll = (MCustomIDB) coll;
			if (custom_coll.varOrdering.size() != indexing.size())
				throw new MGEArityMismatch(
						"Given indexing did not match IDB arity: " + idbname
								+ ", " + indexing + "." + " Arity of IDB was: "
								+ custom_coll.varOrdering.size());
		}
		
		// Add to indexing map
		if(!idbOutputIndexing.containsKey(idbname))
			idbOutputIndexing.put(idbname, new HashSet<List<String>>());
		idbOutputIndexing.get(idbname).add(indexing);
	}

	/**
	 * 
	 * @return true if this query is satisfiable, false otherwise
	 * @throws MGEUnsortedVariable
	 * @throws MGEUnknownIdentifier
	 * @throws MGEArityMismatch
	 * @throws MGEBadQueryString
	 * @throws MGEManagerException
	 * @throws MGEBadIdentifierName
	 */
	public boolean isQuerySatisfiable() throws MGEUnsortedVariable,
			MGEUnknownIdentifier, MGEArityMismatch, MGEBadQueryString,
			MGEManagerException, MGEBadIdentifierName {
		// don't bother saving the result
		return runQuery().getTotalIterator().hasNext();
	}

	/**
	 * Used in the test suite to check whether a query had the expected number
	 * of solutions.
	 * 
	 * @param expected_size
	 * @param expected_sols
	 * @param expected_hbu
	 * @return Whether the test case passed.
	 * @throws MGEUnsortedVariable
	 * @throws MGEUnknownIdentifier
	 * @throws MGEArityMismatch
	 * @throws MGEBadQueryString
	 * @throws MGEManagerException
	 * @throws MGEBadIdentifierName
	 */
	public boolean runTestCase(int expected_size, int expected_sols,
			int expected_hbu) throws MGEUnsortedVariable, MGEUnknownIdentifier,
			MGEArityMismatch, MGEBadQueryString, MGEManagerException,
			MGEBadIdentifierName {
		MInstanceIterator sols = runQuery().getTotalIterator();

		int count = countSolutionsAtSize(expected_size);

		return (expected_sols == count)
				&& (sols.fromResult.get_hu_ceiling() == expected_hbu);
	}

	/**
	 * Given some additional restrictions, generate a new query which narrows
	 * the results of this one.
	 * 
	 * @param more
	 *            A string describing the new restrictions
	 * @return A new MG Query object
	 * @throws MGEBadQueryString
	 * @throws MGEUnknownIdentifier
	 * @throws MGEUnsortedVariable
	 * @throws MGEArityMismatch
	 * @throws MGEManagerException
	 * @throws MGEBadIdentifierName
	 */
	public MQuery refineQuery(String more) throws MGEBadQueryString,
			MGEUnknownIdentifier, MGEUnsortedVariable, MGEArityMismatch,
			MGEManagerException, MGEBadIdentifierName {
		// Take the conjunction of the existing query formula and the new one
		// (But don't duplicate request variable quantifiers...)
		Formula addFormula = constructFormulaFromString(myIDBCollections,
				vocab, new Stack<Variable>(), more, "Q");
		Formula qryFormula = MFormulaManager
				.makeAnd(myQueryFormula, addFormula);

		// Quantifiers may have been updated in the query. Add new constraints
		// IF NEEDED.
		// qryFormula = qryFormula.and(
		// myVocab.getNewFormulaDependentConstraints(myFormula, qryFormula));
		// ^^^ 12/13 removed, see comment in other call to
		// getNewFormulaDependentConstraints.

		return new MQuery(vocab, qryFormula, myIDBCollections); // overloaded
	}

	private static List<String> splitAndRespectParens(String str) {
		LinkedList<String> resultList = new LinkedList<String>();

		// Treat newlines, carriage returns, and tabs as whitespace.
		str = str.replaceAll("[\n\r\t]", " ");

		// Clean up all double-spaces
		// replaceAll does only one pass, so need to specify "two or more" in
		// the regexp below.
		str = str.replaceAll("\\s{2,}", " ");
		// If someone has neglected to put a space between their brackets
		// e.g., (and (foo ...)(bar ...))
		// becomes (and (foo ...) (bar ...))
		// for better parsing
		str = str.replaceAll("\\)\\(", ") (");

		// Loop through the string, splitting by " " unless the paren depth is
		// nonzero
		int paren_depth = 0;
		int last_split = 0;

		int ii;
		for (ii = 0; ii < str.length(); ii++) {
			if (str.charAt(ii) == '(')
				paren_depth++;
			else if (str.charAt(ii) == ')')
				paren_depth--;
			else if (str.charAt(ii) == ' ' && paren_depth == 0) {
				// split
				resultList.add(str.substring(last_split, ii));
				last_split = ii + 1;
			}
		}

		// don't forget the last part -- if it's nonempty.
		if (str.substring(last_split, ii).length() > 0)
			resultList.add(str.substring(last_split, ii));

		// debug
		// MREPL.outStream.println(str + ": "+resultList);
		return resultList;
	}

	protected static void noRequantification(String vname, Stack<Variable> ql)
			throws MGEBadQueryString {
		// Don't allow re-quantification of the same variable name.
		for (Variable tv : ql)
			if (tv.name().equals(vname))
				throw new MGEBadQueryString("Requantificiation of variable: "
						+ vname);

	}

	protected static Formula constructFormulaFromString(
			HashMap<String, MIDBCollection> myIDBCollections, MVocab myVocab,
			Stack<Variable> ql, String qryString, String treatment)
			throws MGEBadQueryString, MGEUnknownIdentifier,
			MGEManagerException, MGEBadIdentifierName {
		// Wrapper. Call this from outside.
		// Will return a Formula object for the query string given.

		// The treatement parameter defines how to handle free variables.
		// If it is "V" the parser is told to simply ignore and create a new
		// var, inserting at BEGINNING of stack
		// so as not to ever re-create.

		Formula f = constructFormulaFromStringRecursive(myIDBCollections,
				myVocab, ql, qryString, treatment);

		return f;
	}

	private static Formula constructFormulaFromStringRecursive(
			HashMap<String, MIDBCollection> myIDBCollections, MVocab myVocab,
			Stack<Variable> ql, String qryString, String treatment)
			throws MGEBadQueryString, MGEUnknownIdentifier,
			MGEManagerException, MGEBadIdentifierName {
		Formula f = Formula.FALSE; // placeholder, should never be used

		// Remove leading and trailing whitespace.
		qryString = qryString.trim();

		// Strip out initial parens (actual removal is done inline with the
		// split call.)
		if (!qryString.startsWith("(") || !qryString.endsWith(")"))
			throw new MGEBadQueryString("Query string formatted incorrectly: "
					+ qryString);

		qryString = qryString.toLowerCase();

		// Split the string into tokens. The first token is the keyword
		List<String> breakdownlist = splitAndRespectParens(qryString.substring(
				1, qryString.length() - 1));

		if (breakdownlist.size() < 1)
			throw new MGEBadQueryString("Could not extract a valid query.");

		// If this keyword is an IDB reference, break it into policy name and
		// IDB name:
		String[] idbbreak = breakdownlist.get(0).split(":");

		// See the IDB:Decision section below. Policy names may contain ':'!
		String idbbreak_last = idbbreak[idbbreak.length - 1];
		String idbbreak_start = idbbreak[0];
		for (int ii = 1; ii < idbbreak.length - 1; ii++)
			idbbreak_start = idbbreak_start + ":" + idbbreak[ii];

		// MREPL.outStream.println(qryString);
		// MREPL.outStream.println(breakdownlist);
		// MREPL.outStream.println(idbbreak_start + " vs. " + idbbreak_last);
		// MREPL.outStream.println();

		// parse based on keyword (recursively if needed)
		if (breakdownlist.get(0).toUpperCase().equals("FORALL")) {
			// forAll x sort (...)
			if (breakdownlist.size() > 4)
				throw new MGEBadQueryString(
						"Incorrect number of parameters to FORALL: "
								+ qryString);

			noRequantification(breakdownlist.get(1), ql);

			// Get the appropriate variable object for this variable name. (Here
			// it will be a new such object, and be added to the stack.)
			Variable thevar = validateQueryVariableName(myVocab, breakdownlist
					.get(1), breakdownlist.get(2), ql, true, treatment);

			Relation sort = myVocab.getRelation(breakdownlist.get(2));
			Formula f2 = MQuery.constructFormulaFromStringRecursive(
					myIDBCollections, myVocab, ql, breakdownlist.get(3),
					treatment);

			// don't keep this quantifier's variable on the stack!
			ql.pop();

			// Resulting formula for this layer of the query:
			Decl theDecl = MFormulaManager.makeOneOfDecl(thevar, sort);
			f = MFormulaManager.makeForAll(f2, theDecl);
		} else if (breakdownlist.get(0).toUpperCase().equals("FORSOME")) {
			// forSome x sort (...)
			if (breakdownlist.size() > 4)
				throw new MGEBadQueryString(
						"Incorrect number of parameters to FORSOME: "
								+ qryString);

			noRequantification(breakdownlist.get(1), ql);
			Variable thevar = validateQueryVariableName(myVocab, breakdownlist
					.get(1), breakdownlist.get(2), ql, true, treatment);
			Relation sort = myVocab.getRelation(breakdownlist.get(2));

			Formula f2 = MQuery.constructFormulaFromStringRecursive(
					myIDBCollections, myVocab, ql, breakdownlist.get(3),
					treatment);

			ql.pop();

			Decl theDecl = MFormulaManager.makeOneOfDecl(thevar, sort);
			f = MFormulaManager.makeExists(f2, theDecl);
		} else if (breakdownlist.get(0).toUpperCase().equals("AND")) {
			// and (...) (...) ...
			// don't allow the string to not be in prenex form (use boolean
			// param)

			Set<Formula> formulas = new HashSet<Formula>();
			for (int ii = 1; ii < breakdownlist.size(); ii++)
				formulas.add(MQuery.constructFormulaFromStringRecursive(
						myIDBCollections, myVocab, ql, breakdownlist.get(ii),
						treatment));
			f = MFormulaManager.makeConjunction(formulas);
		} else if (breakdownlist.get(0).toUpperCase().equals("OR")) {
			// or (...) (...) ...
			// don't allow the string to not be in prenex form (use boolean
			// param)

			Set<Formula> formulas = new HashSet<Formula>();
			for (int ii = 1; ii < breakdownlist.size(); ii++)
				formulas.add(MQuery.constructFormulaFromStringRecursive(
						myIDBCollections, myVocab, ql, breakdownlist.get(ii),
						treatment));
			f = MFormulaManager.makeDisjunction(formulas);

		} else if (breakdownlist.get(0).toUpperCase().equals("NOT")) {
			// not (...)
			if (breakdownlist.size() > 2)
				throw new MGEBadQueryString(
						"Incorrect number of parameters to NOT: " + qryString);

			f = MQuery.constructFormulaFromStringRecursive(myIDBCollections,
					myVocab, ql, breakdownlist.get(1), treatment);
			f = MFormulaManager.makeNegation(f);
		}

		// Is this of the form policyname:idbname?
		else if (idbbreak.length >= 2
				&& myIDBCollections.containsKey(idbbreak_start)
				&& myIDBCollections.get(idbbreak_start).idbs
						.containsKey(idbbreak_last)) {
			// A policy name (usually an XACML one) may contain the character
			// ':'.
			// This means we can't break this up until *2* :-separated strings.

			// pname:permit s a r
			// pname:deny x y
			// etc.

			// idbbreak broke by :

			// (Now this can be any IDB -- rules, decisions, etc.)
			// Got to substitute in the **quantified** variables in the query,
			// and toss the placeholders in the IDB Formula.
			f = doVariableRenaming(myIDBCollections.get(idbbreak_start),
					idbbreak_last, breakdownlist, qryString, ql, treatment);
		}

		else if (idbbreak.length >= 2
				&& myIDBCollections.containsKey(idbbreak[0])) {
			throw new MGEBadQueryString("Unknown IDB in collection: "
					+ idbbreak[0] + ". IDBs available are: "
					+ myIDBCollections.get(idbbreak[0]).idbs.keySet());
		}

		else if (breakdownlist.get(0).equals("=")) {
			// = a b
			// Built in binary predicate

			if (breakdownlist.size() != 3)
				throw new MGEBadQueryString(
						"Equality predicate must be binary.");

			Variable var1 = validateQueryVariableName(myVocab, breakdownlist
					.get(1), "", ql, false, treatment);
			Variable var2 = validateQueryVariableName(myVocab, breakdownlist
					.get(2), "", ql, false, treatment);

			f = MFormulaManager.makeEqAtom(var1, var2);
		}

		else if (breakdownlist.get(0).toUpperCase().equals("EMPTY")) {
			// EMPTY RelationName
			if (breakdownlist.size() > 2)
				throw new MGEBadQueryString(
						"Incorrect number of parameters to EMPTY: " + qryString);

			f = myVocab.getRelation(breakdownlist.get(1)).no();
		}

		else if (breakdownlist.get(0).toUpperCase().equals("ONE")) {
			// ONE RelationName
			if (breakdownlist.size() > 2)
				throw new MGEBadQueryString(
						"Incorrect number of parameters to ONE: " + qryString);

			f = myVocab.getRelation(breakdownlist.get(1)).one();
		}

		else if (breakdownlist.get(0).toUpperCase().equals("SOME")) {
			// SOME RelationName
			if (breakdownlist.size() > 2)
				throw new MGEBadQueryString(
						"Incorrect number of parameters to SOME: " + qryString);

			f = myVocab.getRelation(breakdownlist.get(1)).some();
		}

		else if (myVocab.isSort(breakdownlist.get(0))) {
			// type x
			if (breakdownlist.size() > 2)
				throw new MGEBadQueryString(
						"Incorrect number of parameters for membership check: "
								+ qryString);

			Variable var = validateQueryVariableName(myVocab, breakdownlist
					.get(1), "", ql, false, treatment);
			f = MFormulaManager.makeAtom(var, myVocab.getSort(breakdownlist
					.get(0)).rel);
		} else if (myVocab.predicates.keySet().contains(breakdownlist.get(0))) {
			// pred x y z ...
			// build expression for product of the vars, then see if its in the
			// predicate
			List<String> var_list = new ArrayList<String>();

			for (int ii = 1; ii < breakdownlist.size(); ii++) {
				// for each identifier
				Variable tempvar = validateQueryVariableName(myVocab,
						breakdownlist.get(ii).toLowerCase(), "", ql, false,
						treatment);
				var_list.add(tempvar.name());
			}

			Expression expr = MFormulaManager.makeVarTuple(var_list);
			f = MFormulaManager.makeAtom(expr, myVocab.predicates
					.get(breakdownlist.get(0).toLowerCase()));
		} else
			throw new MGEBadQueryString("Unknown keyword in query string: "
					+ breakdownlist.get(0) + "\n"
					+ "Available IDB Collection names are: "
					+ myIDBCollections.keySet());
		return f;
	}

	private static Formula doVariableRenaming(MIDBCollection idbs,
			String idbname, List<String> breakdownlist, String qryString,
			Stack<Variable> ql, String treatment) throws MGEBadQueryString,
			MGEUnknownIdentifier, MGEBadIdentifierName {
		HashMap<Variable, Variable> newvars = new HashMap<Variable, Variable>();

		// Which variables should be replaced with which?
		// We have the variable NAMES (in order) the user provided in
		// breakdownlist.get(i), 0<i<bdl.size()
		// We have the request variable placeholders (in order) in the vocab.

		// Use this IDB collection's vocab for replacement, not a combined
		// ubervocab.

		Formula f = idbs.idbs.get(idbname);

		// Each variable MAY have been quantified by this query/IDB definition,
		// but we could leave some or all
		// to be quantified later on. (CustomIDBs are allowed free variables,
		// after all!)
		// Hence the try/catches below.

		if (idbs instanceof MPolicy) {
			if (breakdownlist.size() != idbs.vocab.requestVariables.size() + 1)
				throw new MGEBadQueryString(
						"Incorrect number of arguments in: " + qryString);

			int iIndex = 1;
			for (Variable v : idbs.vocab.requestVectorOrder) {
				try {
					Variable newvar = validateQueryVariableName(idbs.vocab,
							breakdownlist.get(iIndex), null, ql, false,
							treatment);
					newvars.put(v, newvar);
				} catch (MGEBadQueryString e) {
					if (!e.getMessage().contains(
							"No quantification for variable"))
						throw e;

					// otherwise, it's a free variable. Kodkod will complain if
					// its used improperly.
				}
				iIndex++;
			}
		} 
		else if (idbs instanceof MCustomIDB)
		{
			// Use the custom IDBs internal var ordering.

			// Check for proper arity
			if (breakdownlist.size() != ((MCustomIDB) idbs).varOrdering.size() + 1)
				throw new MGEBadQueryString(
						"Incorrect number of arguments in: " + qryString);

			Set<Variable> oldvars = f.accept(new FreeVariableCollectionV());

			int iIndex = 1;
			for (Variable varInOrdering : idbs.varOrdering) 
			{
				String vname = varInOrdering.name();
				try {
					// Get the new variable we're using
					// breakdownlist[iIndex] will have the new NAME (if
					// different from old)

					Variable newvar = validateQueryVariableName(idbs.vocab,
							breakdownlist.get(iIndex), null, ql, false,
							treatment);

					// Get the old variables (placeholders in f) -- possibly
					// more than one.
					// (vname is the variable name within the view, not the new
					// name. So name comparison is OK.)
					for (Variable v : oldvars)
						if (v.name().equals(vname)) {
							newvars.put(v, newvar);
							break;
						}
				} catch (MGEBadQueryString e) {
					if (!e.getMessage().contains(
							"No quantification for variable"))
						throw e;

					// It's a free variable. Kodkod will complain if its used
					// improperly.
				}

				iIndex++;
			}
		}

		// (No relation substitution)
		RelationAndVariableReplacementV renaming = new RelationAndVariableReplacementV(
				new HashMap<Relation, Relation>(), newvars);
		return f.accept(renaming);
	}

	/**
	 * If not allownew, makes certain the variable name is legal and that it is
	 * currently bound by a quantifier. If allownew, will create a new Variable
	 * for this name (if the name is valid) and add it to the quantifier stack.
	 * 
	 * @param myVocab
	 * @param varname
	 * @param sortname
	 * @param qstack
	 * @param allownew
	 * @return
	 * @throws MGEBadQueryString
	 * @throws MGEUnknownIdentifier
	 * @throws MGEBadIdentifierName
	 */
	private static Variable validateQueryVariableName(MVocab myVocab,
			String varname, String sortname, Stack<Variable> qstack,
			boolean inquantifier, String treatment) throws MGEBadQueryString,
			MGEUnknownIdentifier, MGEBadIdentifierName {
		// Make sure the variable has a well-formed name.
		// If this variable name has a variable bound in scope, use it.
		// Otherwise create a new one.

		// If a variable is expected, don't allow a predicate or keyword.
		if (varname.indexOf("(") >= 0 || varname.indexOf(")") >= 0)
			throw new MGEBadQueryString("Identifier in query, \"" + varname
					+ "\" contains an illegal character.");

		// If we've seen this variable already, we aren't in a quantifier, and
		// not a custom view
		// (so free variables not allowed)
		if (!inquantifier && !"V".equals(treatment)) {
			for (Variable v : qstack)
				if (v.name().equals(varname))
					return v;

			throw new MGEBadQueryString("No quantification for variable: "
					+ varname);
		} else if (inquantifier) // used to be !"V".equals(treatment)
		{
			// This variable is BOUND at this location. Want one object
			// reference for it.
			// So create the object and push it onto the quantifier stack

			// If the sort name given isn't valid
			if (!myVocab.isSort(sortname))
				throw new MGEBadQueryString("Sort name in query, \"" + sortname
						+ "\" is not a valid sort.");

			// Always arity 1
			Variable newv = MFormulaManager.makeVariable(varname); // via
																	// quantifier,
																	// so scoped
																	// -- create
																	// at TOP of
																	// stack
			qstack.push(newv);
			return newv;
		} else {
			// Custom view, but this variable is NOT quantified. So it's a free
			// occurence.
			// Need to make sure there is only one object reference, as above.
			// (Note that this code only guarantees a single object reference
			// within the scope of this
			// query string. If another query references this view and another
			// view V, V's Variable for
			// "x" may be a different reference. This is dealt with in
			// doVariableRenaming.)

			// Has this variable been created already? (Don't have duplicate
			// references.)
			for (Variable v : qstack)
				if (v.name().equals(varname))
					return v;

			// Not found. Create at BEGINNING OF STACK (global scope within this
			// view)
			Variable newv = MFormulaManager.makeVariable(varname); // FREE
																	// variable.
			qstack.insertElementAt(newv, 0);

			return newv;
		}

	}

	public static MQuery queryThesePolicies(String qry,
			List<MIDBCollection> idbCollections) throws MGEBadQueryString,
			MGEUnknownIdentifier, MGEArityMismatch, MGECombineVocabs,
			MGEBadIdentifierName, MGEUnsortedVariable, MGEManagerException {
		// Record time used to pre-process the query: Vocab combination, string
		// parsing, etc.
		ThreadMXBean mx = ManagementFactory.getThreadMXBean();
		long starttime = 0;
		if (mx.isCurrentThreadCpuTimeSupported())
			starttime = mx.getCurrentThreadCpuTime();

		// Create a hash map for the following procedure call.
		HashMap<String, MIDBCollection> hm = new HashMap<String, MIDBCollection>();
		for (MIDBCollection p : idbCollections) {
			if (hm.containsKey(p.name))
				throw new MGEBadIdentifierName(
						"List contained multiple Policies or Views named "
								+ p.name
								+ ".\n"
								+ "Since the query string uses the policy name, they must have different names in order to be compared.");
			hm.put(p.name, p);
		}
		if (idbCollections.size() < 1)
			throw new MGECombineVocabs(
					"queryThesePolicies must be called with at least one IDB Collection.");

		// more than one policy?
		// Build the uber-vocabulary
		MVocab uber = null;

		// These policies may not share a vocabulary, but the query needs a
		// single vocab to work from.
		// (Not to mention the fact that we need to make sure they have
		// compatible request vectors and Decision sets!)
		for (MIDBCollection p : idbCollections) {
			if (uber == null) {
				uber = p.vocab;
				continue;
			}

			// Throws MGECombineVocabs if these cannot be combined (i.e.,
			// incompatible request/decisions/relation types/constraints)
			uber = uber.combineWith(p.vocab);
		}

		// Do NOT change the policy vocabs. We just want this query to have the
		// uber vocabulary.
		// However, we DO need to make sure references to objects in the query's
		// formula are unified
		// under the combined vocab. (See below, after formula is constructed.)

		// Construct a formula where IDBs from all the above policies are
		// allowed.
		// MREPL.outStream.println("DEBUG DEBUG: Parsing query: "+qry);
		Formula qryFormula = MQuery.constructFormulaFromString(hm, uber,
				new Stack<Variable>(), qry, "Q");

		// MREPL.outStream.println("Parsed: "+qryFormula + " from: "+qry);

		// Make sure relations and variables are the correct object instances
		// (the same quantified in the user query, NOT the placeholder instances
		// in the IDB formulas.)

		// MREPL.outStream.println("DEBUG DEBUG: Alpha substitution."); // takes a
		// LOOONG time now, and runs out of heap space!
		for (MIDBCollection other : idbCollections) {
			RelationAndVariableReplacementV vis = MIDBCollection
					.getReplacementVisitor(other.vocab, uber);
			qryFormula = qryFormula.accept(vis);
		}

		// NOTE!
		// Simplification takes place in the MGQuery constructor, below.

		// create the new query
		// MREPL.outStream.println("DEBUG DEBUG: Creating new query!");
		MQuery result = new MQuery(uber, qryFormula, idbCollections);
		if (mx.isCurrentThreadCpuTimeSupported())
			result.msPreprocessingTime = (mx.getCurrentThreadCpuTime() - starttime) / 1000000;

		return result;
	}

	private boolean someChildContainsTuple(Instance sol, MSort thetype,
			Tuple theTuple) {
		// if a "grandchild" contains the tuple, a "child" must.
		for (MSort child : thetype.subsorts)
		{
			// empty may be null
			if (sol.relationTuples().containsKey(child.rel) &&
					sol.relationTuples().get(child.rel).contains(theTuple))
				return true;
		}
		return false;
	}

	protected void prettyPrintSolution(MVocab localVocab,
			MSolutionInstance aSolution, MInstanceIterator solnSet)	
	{
		String result = getPrettyPrintForSolution(localVocab, aSolution, solnSet);
		MEnvironment.outStream.println(result);
	}
	
	protected String getPrettyPrintForSolution(MVocab localVocab,			
			MSolutionInstance aSolution, MInstanceIterator solnSet)
	{
		HashMap<String, String> replacements = new HashMap<String, String>();

		StringBuffer theResult = new StringBuffer();
		
		// TODO change to building string
		
		Instance sol = aSolution.getFacts();

		String pmod = "";
		//if (this.otSpec.itDefault == MQueryOutputSpec.DefaultIteratorType.outIteratePartial)
		//	pmod = "CLASS ";

		MEnvironment.outStream.println("----------------------------------------");
		MEnvironment.outStream.println("*** SOLUTION " + pmod + "FOUND at size = "
				+ sol.universe().size() + ".");

		Set<Relation> alreadyprinted = new HashSet<Relation>();

		// for each atom
		for (int ii = 0; ii < sol.universe().size(); ii++) {
			Tuple theTuple = sol.universe().factory().tuple(sol.universe().atom(ii));

			// How to display this atom? (if it's bound to a $kolemized
			// variable, rename it.)
			String disp = "";
			for (Relation r : sol.relations())
				if (r.arity() == 1 && r.name().startsWith("$")
						&& sol.relationTuples().get(r).contains(theTuple)) {
					if (disp.length() == 0)
						disp = r.name();
					else
						disp = disp + "=" + r.name();
				}
			if (disp.length() == 0)
				//disp = "Atom" + ii;
				disp = sol.universe().atom(ii).toString();

			//replacements.put("Atom" + ii, disp);
			replacements.put(sol.universe().atom(ii).toString(), disp);

			MEnvironment.outStream.print(disp + ": ");

			// For each unary predicate corresponding to a TYPE
			// (only display the most specific types -- but don't print nothing
			// if
			// -- for example -- $s is a Subject but nothing more specific.)
			for (Relation r : sol.relations())
			{
				if (r.name().startsWith("$"))
					alreadyprinted.add(r); // suppress printing of the trivial
											// binding relations
				else if (r.arity() == 1) {
					try {
						// WARNING: do not use vocab. Use localVocab.
						// Tupling returns a secondary query, which has a
						// different voc.
						MSort thetype = localVocab.getSortForExpression(r);

						// Add even sort predicates that are not most specific,
						// because we don't want to print them below.
						alreadyprinted.add(r);

						// This is a sort predicate. Only print it if it is a
						// most-specific type containing this tuple.
						if (!someChildContainsTuple(sol, thetype, theTuple))
							if (sol.relationTuples().get(r).contains(theTuple)) {
								if (thetype.subsorts.size() == 0)
									MEnvironment.outStream.print(r.name() + " ");
								else
									MEnvironment.outStream
											.print(r.name()
													+ "(but none of: "
													+ prettyPrintSortNames(thetype.subsorts)
													+ ") ");
							}

					} catch (MGEUnknownIdentifier E) {
						// Not a type predicate, so wait and display it below.
					} catch (MGEBadIdentifierName e) {
						System.err.println("Bad identifier while printing.");
						System.exit(1);
					}

				}
			}
			MEnvironment.outStream.print("\n");
		} // end for each atom

		// For each relation that isn't a sort...
		for (Relation r : sol.relations())
			if (!alreadyprinted.contains(r))
			{
				// Print out the contents of the relation. Replace atom names
				// with meaningful bindings when able.
				
				if(getIDBNamesToOutput().contains(r.name()))
					MEnvironment.outStream.print(r.name() + "["+ solnSet.idbToTup.get(r.name()) + "] = {");
				else
					MEnvironment.outStream.print(r.name() + " = {");
				boolean firsttuple = true;
				for (Tuple t : sol.relationTuples().get(r)) {
					if (firsttuple)
						firsttuple = false;
					else
						MEnvironment.outStream.print(" ");
					MEnvironment.outStream.print("[");
					for (int jj = 0; jj < t.arity(); jj++) {
						if (replacements.keySet().contains(t.atom(jj)))
							MEnvironment.outStream.print(replacements.get(t.atom(jj)));
						else
							MEnvironment.outStream.print(t.atom(jj));
						if (jj < t.arity() - 1)
							MEnvironment.outStream.print(", ");
					}

					MEnvironment.outStream.print("]");

				}
				MEnvironment.outStream.print("}\n");
			}
		MEnvironment.outStream.println("");

		// **********************************************************
		// **********************************************************
		// Annotations!
		List<String> annotations = aSolution.getAnnotations();
		for(String annotation : annotations)
		{
			MEnvironment.outStream.println(annotation);
		}
		if(annotations.size() > 0)
			MEnvironment.outStream.println("");
		
		
		// **********************************************************
		// **********************************************************
		// Don't Cares for partial models. Stop if the user asked for full
		// output.
		//if (this.otSpec.itDefault != MQueryOutputSpec.DefaultIteratorType.outIteratePartial)
		//{
			MEnvironment.outStream.println("----------------------------------------");
			return theResult.toString();
		//}
		// **********************************************************

			/*
		Instance dc = aSolution.getDontCares();

		StringBuffer dcOutput = new StringBuffer();
		dcOutput.append("\n*** Regardless of whether or not: ***\n");

		int dcCount = 0;

		// For each relation that isn't a sort...
		for (Relation r : dc.relations())
		{
			if (dc.relationTuples().get(r).size() < 1)
				continue;

			// Print out the contents of the relation. Replace atom names with
			// meaningful bindings when able.
			dcOutput.append(r.name() + " contains any of: {");
			boolean firsttuple = true;
			for (Tuple t : dc.relationTuples().get(r)) {
				dcCount++;

				if (firsttuple)
					firsttuple = false;
				else
					dcOutput.append(" ");
				dcOutput.append("[");
				for (int jj = 0; jj < t.arity(); jj++) {
					if (replacements.keySet().contains(t.atom(jj)))
						dcOutput.append(replacements.get(t.atom(jj)));
					else
						dcOutput.append(t.atom(jj));
					if (jj < t.arity() - 1)
						dcOutput.append(", ");
				}

				dcOutput.append("]");
			}
			dcOutput.append("}\n");
		}

		if (dcCount > 0) {
			MEnvironment.outStream.println(dcOutput.toString());
			MEnvironment.outStream.println("This partial solution includes "
					+ Math.round(Math.pow(2, dcCount)) + " total solutions.");
		}

		MEnvironment.outStream.println("----------------------------------------");

		return theResult.toString();*/
		
	}

	private String prettyPrintSortNames(Set<MSort> sorts) {
		String result = "";
		boolean first = true;
		for (MSort s : sorts) {
			if (first) {
				result = s.name;
				first = false;
			} else
				result += " " + s.name;
		}
		return result;
	}

	public void prettyPrintOneSolution() throws MGEUnsortedVariable,
			MGEUnknownIdentifier, MGEArityMismatch, MGEBadQueryString,
			MGEManagerException, MGEBadIdentifierName {
		prettyPrintSolutions(1);
	}

	public void prettyPrintSolutions() throws MGEUnsortedVariable,
			MGEUnknownIdentifier, MGEArityMismatch, MGEBadQueryString,
			MGEManagerException, MGEBadIdentifierName {
		// print them all
		prettyPrintSolutions(-1);
	}

	public void prettyPrintSolutionsCondensed() throws MGEUnsortedVariable,
			MGEUnknownIdentifier, MGEArityMismatch, MGEBadQueryString,
			MGEManagerException, MGEBadIdentifierName {
		// disabled special code for now
		
		//MQueryOutputSpec.DefaultIteratorType savedCondense = otSpec.itDefault;
		//otSpec.itDefault = MQueryOutputSpec.DefaultIteratorType.outIteratePartial;
		prettyPrintSolutions();
	//	otSpec.itDefault = savedCondense;
	}

	public int countSatisfyingSolutions() throws MGEUnsortedVariable,
			MGEUnknownIdentifier, MGEArityMismatch, MGEBadQueryString,
			MGEManagerException, MGEBadIdentifierName {
		return countSolutionsAtSize(0);
	}

	// TODO this method (and many others) should be removed, if they are truly replaced by new methods
	// (this one is replaced by the count methods in MQuerySolution, which avoid the runQuery call.)
	public int countSolutionsAtSize(int desired_size)
			throws MGEUnsortedVariable, MGEUnknownIdentifier, MGEArityMismatch,
			MGEBadQueryString, MGEManagerException, MGEBadIdentifierName
	{
		// If size is less than 1, will just count ALL the solutions.
		
		MInstanceIterator it = runQuery().getTotalIterator();
		  
		int count = 0; 
		while(it.hasNext()) 
		{ 
			try 
			{ 
				 Instance sol = it.next().getFacts();
				 //System.out.println("Sol size "+sol.universe().size());
				 //System.out.println(sol);
				 if(desired_size < 1 || sol.universe().size() == desired_size)
					 count++;
			}
			catch(MGENoMoreSolutions e)
			{} 
		} 
		return count;
		 
	}

	public void prettyPrintSolutions(int max_to_print)
			throws MGEUnsortedVariable, MGEUnknownIdentifier, MGEArityMismatch,
			MGEBadQueryString, MGEManagerException, MGEBadIdentifierName {
		int counter = 0;
		Statistics stats = null;

		
		ThreadMXBean mx = ManagementFactory.getThreadMXBean();
		long time = 0;
		long initialtime = 0;
		long timeforoutput = 0;

		// Start the timer BEFORE we check for !hasNext() -- first call of
		// hasNext() is expensive
		if (mx.isCurrentThreadCpuTimeSupported())
			initialtime = mx.getCurrentThreadCpuTime();

		// help separate different query results
		MEnvironment.outStream.println();

		if (max_to_print > -1)
			MEnvironment.outStream.println("  [[ Printing a maximum of " + max_to_print
					+ " models. Additional results will be ignored. ]]");
		else
			MEnvironment.outStream.println("  [[ Printing all models. ]]");

		MInstanceIterator it = runQuery().getTotalIterator();

		MEnvironment.outStream.println("============================================================");
		if (!it.hasNext())
			MEnvironment.outStream.println("* Query was unsatisfiable for the model sizes checked. *");

		while (it.hasNext() && ((max_to_print == -1) || (max_to_print > 0))) 
		{
			try {
				// Interaction with KodKod. Clock this. But it's very quick. So
				// subtract OUT the I/O delays

				MSolutionInstance partialInstance = it.next();
				if (mx.isCurrentThreadCpuTimeSupported())
					time = mx.getCurrentThreadCpuTime();

				//prettyPrintSolution(it.forQuery.vocab, partialInstance);
				
				// Output routines
				if(internalTupledQuery != null)
				{
					MSolutionInstance s = processTupledSolutionForThis(partialInstance);
					prettyPrintSolution(vocab, s, it);
				}
				else
				{									
					prettyPrintSolution(it.fromResult.forQuery.vocab, partialInstance, it);
				}
				
				counter++;

				if (mx.isCurrentThreadCpuTimeSupported()) {
					timeforoutput += (mx.getCurrentThreadCpuTime() - time);
					// Almost always 0
					// forQuery.MREPL.outStream.println(timeforoutput);
					// forQuery.MREPL.outStream.println(mx.getCurrentThreadCpuTime()-time);
				}

				if (max_to_print > -1)
					max_to_print--;
			} catch (MGENoMoreSolutions e) {
				// Never reach here due to hasNext call above
				break;
			}
		}

		String pmod = "";
		String pluralpmod = "s";
	//	if (this.otSpec.itDefault == MQueryOutputSpec.DefaultIteratorType.outIteratePartial) {
	//		pmod = " class";
	//		pluralpmod = "es";
	//	}

		if (counter == 1)
			MEnvironment.outStream.println("\n1 solution" + pmod
					+ " found at model sizes up to " + it.fromResult.get_universe_max()
					+ ".");
		else
			MEnvironment.outStream.println("\n" + counter + " solution" + pmod + pluralpmod
					+ " found at model sizes up to " + it.fromResult.get_universe_max()
					+ ".");

		if (it.fromResult.get_hu_ceiling() > -1)
			MEnvironment.outStream.println("(HU ceiling was " + it.fromResult.get_hu_ceiling()
					+ " atoms.)");
		else
			MEnvironment.outStream.println("(HU was infinite.)");

		if (it.warn_user()) {
			MEnvironment.outStream
					.println("\n*** WARNING! ***\n* Did not test high enough model sizes for certainty.");
			MEnvironment.outStream
					.println("* (The H.U. was empty, infinite, or larger than the user provided model size maximum.)\n");
		}
		if (it.fromResult.msQueryCreationTime < 0)
			MEnvironment.outStream
					.println("\nThreadMXBean's isCurrentThreadCpuTimeSupported() returned false; unable to benchmark via CPU time.");
		else {
			// Convert from nanosecond to millisecond precision
			timeforoutput /= 1000000;

			long timeused = (mx.getCurrentThreadCpuTime() - initialtime) / 1000000;

			MEnvironment.outStream.println("\nTotal Time Kodkod spent on translation: "
					+ it.msKodkodTransTime + " ms, and solving: "
					+ it.msKodkodSolveTime + " ms.");
			MEnvironment.outStream.println("CPU Time to prepare Margrave Query: "
					+ it.fromResult.msQueryCreationTime + " ms.");
			MEnvironment.outStream
					.println("CPU Time spent getting solutions from KodKod (not including I/O): "
							+ (timeused - timeforoutput) + " ms.");
		}
		if (stats != null) {
			MEnvironment.outStream.println("Kodkod's statistics: ");
			MEnvironment.outStream.println(stats);

		}
		MEnvironment.outStream.println("Used SAT Solver: " + mySATFactory);
		MEnvironment.outStream
				.println("============================================================");
	}

	protected MSolutionInstance processTupledSolutionForThis(MSolutionInstance partialInstance)
	{
		// Given a TUPLED solution, produce a PRE-tupling solution.
		// Assume this (query) is the tupled query and this.preTuplingQuery is the original query
		
		// varOrdering is only an ordering on the PUBLISHED variables in the query.
		// We need the entire ordering generated for the tupling.
		
		
		// (1) Look at the equalities, decide what the universe is
		HashMap<String, Set<String>> idxToVars = new HashMap<String, Set<String>>();
		HashMap<String, String> idxToAtom = new HashMap<String, String>();
		
		List<String> annotations = new ArrayList<String>();
		
		// populate initial atoms
		for(String idx : internalTuplingVisitor.pv.revIndexing.keySet())
		{
			Set<String> atom = new HashSet<String>();
			atom.add(internalTuplingVisitor.pv.revIndexing.get(idx).name());
			idxToVars.put(idx, atom);
		}		
		
		// Take the quotient dictated by the equalities
		for(Relation r : partialInstance.getFacts().relations())
		{
			// TODO kludge; rel may start with = anyway, but this is a problem with the names already
			if(r.name().startsWith("="))
			{
				// Only one atom, so dont need to check the atom's identity
				if(partialInstance.getFacts().relationTuples().get(r).size() > 0)
				{
					int needsLastComma = r.name().lastIndexOf(",");
					String rightidx = r.name().substring(needsLastComma + 1);
					String leftidx = r.name().substring(2, needsLastComma);
				
										
					Set<String> comb = new HashSet<String>();					
					comb.addAll(idxToVars.get(rightidx));
					comb.addAll(idxToVars.get(leftidx));
					idxToVars.put(rightidx, comb);
					idxToVars.put(leftidx, comb);										
				}								
			}
		}
		
		// Make universe
		// Need the unique elements
		Set<String> vals = new HashSet<String>();
		for(String idx: idxToVars.keySet())
		{
			// Convert the set to something more readable
			String aName = "";
			for(String varName : idxToVars.get(idx))
			{
				if(aName.length() < 1)
					aName = aName + varName;
				else
					aName = aName + "=" + varName;
			}
			
			idxToAtom.put(idx, aName);
			vals.add(aName);
		}
		Universe u = new Universe(vals);
		
		//System.err.println(u);

		
		Instance instance = new Instance(u);
		Instance dontcare = new Instance(u);
			
		
		//System.err.println(partialInstance.getFacts());				
		
		// Now fill the relations in the new instance
		for(Relation r : partialInstance.getFacts().relations())
		{
			if(r.name().startsWith("="))
				continue;
						
			if(partialInstance.getFacts().relationTuples().get(r).size() > 0)
			{
				// populated!
				int underscoreIndex = r.name().lastIndexOf("_");
				
				/// *****************
				/// IDB OUTPUT
				/// *****************
				
				// Does the pre-tupling part of this relname appear in our list of IDB names to output?
				
				// Some special tupled preds don't have an indexing
				String preunderscore = "";
				if(underscoreIndex >= 0)
					preunderscore = r.name().substring(0, underscoreIndex);
				
				//System.out.println("******"+preunderscore);
				
				if(getIDBNamesToOutput().contains(preunderscore)) // this is the pre-tupling query's set
				{
					
					// Indexing is after the underscore.
					String[] indexing = r.name().substring(underscoreIndex+1).split(",");										
					
					List<String> theTuple = new ArrayList<String>();
					for(String s : indexing)
					{
						theTuple.add(idxToAtom.get(s));
					}

						
					// 	Cannot just add, may be too many tuples (in full relation) to fit in maxint
					// if so kodkod throws an exception. 
					// instance.add(r, theTuples);
				
					// Instead, we apply duct tape. 
					// Annotate the result!
					// Only ONE tuple because of how we restrict to a certain indexing
					// Also: strip the indexing from r.name().						
					annotations.add(preunderscore + " is true for: "+ theTuple);
					
					continue; // finished handling this relation
				}
				
				/// *****************
				/// Other relations
				/// *****************
				if(underscoreIndex > 0)
				{
					TupleSet theTuples = u.factory().noneOf(1);
					
					String idx = r.name().substring(underscoreIndex+1);
					String relname = r.name().substring(0, underscoreIndex);
					try
					{
						Relation newr = vocab.getRelation(relname);						
						
						//System.err.println(idx);
						//System.err.println(relname);
						//System.err.println(newr);
						
						String theAtom = idxToAtom.get(idx);
						//System.err.println(theAtom);					
						
						Tuple t = u.factory().tuple(theAtom);
						theTuples.add(t);						
						
						if(instance.relationTuples().containsKey(newr))
							theTuples.addAll(instance.relationTuples().get(newr));
							
						instance.add(newr, theTuples);
						
						//System.err.println(newr+": "+instance.relationTuples().get(newr));
					}
					catch(MGEBadIdentifierName e)
					{
						continue; // move on
					}
					catch(MGEUnknownIdentifier e)
					{
						continue;						
					}									
				}
			} // end if relation populated
			else
			{
				// If unpopulated
				// Leave empty for now
			}
			
		} // for each relation
		
		// There is (for now) no tupling support for non-unary relations.
		
				
		
		//System.exit(1);		
		
		

		
		// TODO dealing with dontcares here...		
		

		
		
		return new MSolutionInstance(instance, dontcare, annotations);
	}
	
	static public void unitTest() throws MGEBadIdentifierName,
			MGEUnknownIdentifier, MGEUnsortedVariable, MGEArityMismatch,
			MGEBadQueryString, MGEManagerException 
			{
		System.out
				.println("----- Begin MGQuery Tests (No messages is good.) -----");

		Variable x = MFormulaManager.makeVariable("x");
		Variable y = MFormulaManager.makeVariable("y");
		Variable z = MFormulaManager.makeVariable("z");
		Variable z2 = MFormulaManager.makeVariable("z2");
		Variable z3 = MFormulaManager.makeVariable("z3");

		// *********************************************************************************
		// Establish vocabularies. Test for HU ceiling correctness.

		// Test to make sure cycles are detected.
		MVocab env = new MVocab("Test Vocab");
		env.addSort("Sort1");

		Expression sort1 = env.getRelation("Sort1");

		// Not a cycle because no ground term... but
		// unsure how to handle the case where HU = empty
		Formula f = x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort1));
		MQuery test1 = new MQuery(f, env);
		test1.runTestCase(1, 2, -1);

		// This IS a cycle.
		f = x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort1)).and(
				sort1.some());
		MQuery test2 = new MQuery(f, env);
		if (test2.runQuery().get_hu_ceiling() != -1)
			System.out.println("Test 2a failed!");

		// Test for other multiplicities that induce existentials (one)
		f = x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort1)).and(
				sort1.one());
		test2 = new MQuery(f, env);
		if (test2.runQuery().get_hu_ceiling() != -1)
			System.out.println("Test 2b failed!");

		// Test for lone (doesn't induce an existential.)
		f = x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort1)).and(
				sort1.lone());
		test2 = new MQuery(f, env);
		if (test2.runQuery().get_hu_ceiling() != 0)
			System.out.println("Test 2c failed!");

		// Test for no (doesn't induce an existential.)
		f = x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort1)).and(
				sort1.no());
		test2 = new MQuery(f, env);
		if (test2.runQuery().get_hu_ceiling() != 0)
			System.out.println("Test 2d failed!");

		// So is this (explicit quantifier)
		// (Formula.TRUE here was causing an exception, whereas something like
		// z.in(sort1) did not.
		// Leave it in to test resolution of bug.
		f = x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort1)).and(
				Formula.TRUE.forSome(z.oneOf(sort1)));
		MQuery test3 = new MQuery(f, env);
		if (test3.runQuery().get_hu_ceiling() != -1)
			System.out.println("Test 3 failed!");

		// Test multiple ground terms
		f = Formula.TRUE.forSome(z.oneOf(sort1)).and(sort1.one());
		MQuery test4 = new MQuery(f, env);
		if (test4.runQuery().get_hu_ceiling() != 1) // should filter out the
													// .one
			System.out.println("Test 4a failed!");

		f = Formula.TRUE.forSome(z.oneOf(sort1)).and(sort1.some()).or(
				Formula.TRUE.forSome(x.oneOf(sort1)));
		test4 = new MQuery(f, env);
		if (test4.runQuery().get_hu_ceiling() != 2) // prunes out the .some, but
													// still two exists
			System.out.println("Test 4b failed!");

		// *********************************************************************************
		// *********************************************************************************
		// Cannot check function term generation in that vocab because there is
		// only one sort.
		// 2nd vocab:

		env = new MVocab("Test Vocab");

		// 2 top level types
		env.addSort("Sort1");
		env.addSort("Sort2");

		// Each top level sort has 2 child sorts.
		env.addSubSort("Sort1", "Sort1A");
		env.addSubSort("Sort1", "Sort1B");
		env.addSubSort("Sort2", "Sort2A");
		env.addSubSort("Sort2", "Sort2B");

		// Sort 1's children are disjoint constrained, sort 2's are not.
		env.axioms.addConstraintDisjointAll("Sort1");

		sort1 = env.getRelation("Sort1");
		Expression sort2 = env.getRelation("Sort2");

		Expression sort1a = env.getRelation("Sort1A");
		// Expression sort1b = env.getRelation("Sort1B");
		Expression sort2a = env.getRelation("Sort2A");
		Expression sort2b = env.getRelation("Sort2B");

		// Now we can have lots of functions induced by quantifiers (and
		// multiplicities.)

		// f:A->B, x:A. Two atoms.
		f = x.eq(y).forSome(x.oneOf(sort2)).forAll(y.oneOf(sort1)).and(
				sort1.some());
		MQuery test5 = new MQuery(f, env);
		if (test5.runQuery().get_hu_ceiling() != 2)
			System.out.println("Test 5a failed!");

		// f:A->B, x:B. One atom.
		f = x.eq(y).forSome(x.oneOf(sort2)).forAll(y.oneOf(sort1)).and(
				sort2.some());
		test5 = new MQuery(f, env);
		if (test5.runQuery().get_hu_ceiling() != 1)
			System.out.println("Test 5b failed!");

		// f:A->B, x:B y:B. Two atoms.
		f = x.eq(y).forSome(x.oneOf(sort2)).forAll(y.oneOf(sort1)).and(
				Formula.TRUE.forSome(z.oneOf(sort2))).and(
				Formula.TRUE.forSome(z2.oneOf(sort2)));
		test5 = new MQuery(f, env);
		if (test5.runQuery().get_hu_ceiling() != 2)
			System.out.println("Test 5c failed!");

		// f:A->B, x:B, y:B, z:A. Four atoms.
		f = x.eq(y).forSome(x.oneOf(sort2)).forAll(y.oneOf(sort1)).and(
				Formula.TRUE.forSome(z.oneOf(sort2))).and(
				Formula.TRUE.forSome(z2.oneOf(sort2))).and(
				Formula.TRUE.forSome(z3.oneOf(sort1)));
		test5 = new MQuery(f, env);
		if (test5.runQuery().get_hu_ceiling() != 4)
			System.out.println("Test 5d failed!");

		// Special cases:

		// f:A->subA. No ground. Cannot be sure here (HU has zero terms!)
		f = x.eq(y).forSome(x.oneOf(sort1a)).forAll(y.oneOf(sort1));
		test5 = new MQuery(f, env);
		if (test5.runQuery().get_hu_ceiling() != 0)
			System.out.println("Test 5e failed!");

		// F:subA->A same as above.
		f = x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort1a));
		test5 = new MQuery(f, env);
		if (test5.runQuery().get_hu_ceiling() != 0)
			System.out.println("Test 5f failed!");

		// Unproductive function and a constant
		f = x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort1a)).and(
				sort1.some());
		test5 = new MQuery(f, env);
		if (test5.runQuery().get_hu_ceiling() != 1)
			System.out.println("Test 5g failed!");

		// f:subA->A. x:subA. subA <= A.
		// Constant in subA is also in A. (same term, don't double count)
		// Then the function application is in A
		f = x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort1a)).and(
				sort1a.some());
		test5 = new MQuery(f, env);
		if (test5.runQuery().get_hu_ceiling() != 2)
			System.out.println("Test 5h failed!");

		// f:A->subA. x:A. (inf; f obviously cyclic)
		f = x.eq(y).forSome(x.oneOf(sort1a)).forAll(y.oneOf(sort1)).and(
				sort1.some());
		test5 = new MQuery(f, env);
		if (test5.runQuery().get_hu_ceiling() != -1)
			System.out.println("Test 5i failed!");

		// f:A->subA x:subA. (inf; f obviously cyclic, and subA is a subtype of
		// A.)
		// (This tests that finding productive functions considers inclusions)
		f = x.eq(y).forSome(x.oneOf(sort1a)).forAll(y.oneOf(sort1)).and(
				sort1a.some());
		test5 = new MQuery(f, env);
		if (test5.runQuery().get_hu_ceiling() != -1)
			System.out.println("Test 5j failed!");

		// cycle detection with overlapping types
		// Sort2a and Sort2b may overlap! No disjoint constraint.

		// f:Sort2a->Sort2b. x:2a. only 2 terms -- x, f(x) -- though they get
		// propagated
		f = x.eq(y).forSome(x.oneOf(sort2b)).forAll(y.oneOf(sort2a)).and(
				sort2a.some());
		test5 = new MQuery(f, env);
		if (test5.runQuery().get_hu_ceiling() != 2)
			System.out.println("Test 5k failed!");

		// f:Sort2a->Sort2b. x:2b. Func is unproductive. Only 1 term (the
		// constant)
		f = x.eq(y).forSome(x.oneOf(sort2b)).forAll(y.oneOf(sort2a)).and(
				sort2b.some());
		test5 = new MQuery(f, env);
		if (test5.runQuery().get_hu_ceiling() != 1)
			System.out.println("Test 5l failed!");

		// f:Sort2a->Sort2b. x:2. Another unproductive function.
		f = x.eq(y).forSome(x.oneOf(sort2b)).forAll(y.oneOf(sort2a)).and(
				sort2.some());
		test5 = new MQuery(f, env);
		if (test5.runQuery().get_hu_ceiling() != 1)
			System.out.println("Test 5m failed!");

		// make sure we have distinct functions even if declared identically.
		// one level of "identity"
		f = Formula.TRUE.forSome(z.oneOf(sort1)).and(
				Formula.TRUE.forSome(z.oneOf(sort1)));
		test5 = new MQuery(f, env);
		if (test5.runQuery().get_hu_ceiling() != 2)
			System.out.println("Test 5n failed!");

		// what if we have more than one level of identity? Make sure we get 2
		// atoms...

		// Manager will notice that they are the same formula, and only have 1
		// in the AND in the first place.
		// f = MGFormulaManager.makeAnd(f, f);
		// Similarly, the simplifier will take f = f.and(f) and return f.
		// So we need to be very inefficient to test this:

		f = Formula.TRUE.forSome(z.oneOf(sort1)).or(
				Formula.TRUE.forAll(z.oneOf(sort1)));
		Formula f2 = Formula.TRUE.forSome(z.oneOf(sort1)).or(
				Formula.TRUE.forAll(z.oneOf(sort1)));
		f = f.and(f2);
		// NOW we have 2 "different" subformulas.

		test5 = new MQuery(f, env);
		// test5.debug_verbosity = 3;
		if (test5.runQuery().get_hu_ceiling() != 2)
			System.out.println("Test 5o failed!");

		// Term ids in the above must be *different*
		// TODO convert this test?
		/*
		 * WalkASTForFunctionsV vis = new WalkASTForFunctionsV(env);
		 * Set<FuncStruct> funcs = f.accept(new NNFConverterV()).accept(vis);
		 * //for(FuncStruct fx : funcs) // System.out.println(fx.getID());
		 * List<FuncStruct> lfuncs = new ArrayList<FuncStruct>(funcs);
		 * if(lfuncs.size() != 2 ||
		 * lfuncs.get(0).getID().equals(lfuncs.get(1).getID()))
		 * System.out.println("Test 5p (unique identifiers) failed!");
		 */

		// *********************************************************************************
		// Now need a vocab with multiple layers of subtypes to make sure the
		// DFS extends properly.
		// 3rd vocab:
		env = new MVocab("Test Vocab");

		// 2 top level types
		env.addSort("Sort1");
		env.addSort("Sort2");

		// Sort 2 has 3 child sorts.
		env.addSubSort("Sort2", "Sort2a");
		env.addSubSort("Sort2", "Sort2b");
		env.addSubSort("Sort2", "Sort2c");

		// 2a and 2b may overlap. b and c are disjoint.
		env.axioms.addConstraintDisjoint("Sort2b", "Sort2c");

		// Each child sort has a child sort.
		env.addSubSort("Sort2a", "Sort2ax");
		env.addSubSort("Sort2b", "Sort2bx");
		env.addSubSort("Sort2c", "Sort2cx");

		sort1 = env.getRelation("Sort1");
		sort2 = env.getRelation("Sort2");
		sort2a = env.getRelation("Sort2a");
		sort2b = env.getRelation("Sort2b");
		Expression sort2c = env.getRelation("Sort2c");
		Expression sort2ax = env.getRelation("Sort2ax");
		Expression sort2bx = env.getRelation("Sort2bx");
		Expression sort2cx = env.getRelation("Sort2cx");

		// f:2c->2cx g:2bx->2b x:2 y:2bx. The function f is unproductive.
		// Only one constant since sort2bx.some() covers sort2.some()
		// + 1 func application
		f = x.eq(y).forSome(x.oneOf(sort2cx)).forAll(y.oneOf(sort2c)).and(
				x.eq(y).forSome(x.oneOf(sort2b)).forAll(y.oneOf(sort2bx))).and(
				sort2.some()).and(sort2bx.some());
		MQuery test6 = new MQuery(f, env);
		if (test6.runQuery().get_hu_ceiling() != 2)
			System.out.println("Test 6a failed!");

		// Non-infinite with non trivial function combinations
		// f:1->2c. g:2c->2b. x:1 y:2 -- 2 constants, both funcs productive
		// x, f(x), g(f(x)), y.
		// TODO: y could be eliminated by a coverage argument. (f(x) is in
		// sort2, and y is from sort2.some().)
		f = x.eq(y).forSome(x.oneOf(sort2c)).forAll(y.oneOf(sort1)).and(
				x.eq(y).forSome(x.oneOf(sort2b)).forAll(y.oneOf(sort2c))).and(
				sort1.some()).and(sort2.some());
		test6 = new MQuery(f, env);
		if (test6.runQuery().get_hu_ceiling() != 4)
			System.out.println("Test 6b failed!");

		// f:1->2c. g:2a->2b. x:1
		// g is unproductive. only 2 terms
		f = x.eq(y).forSome(x.oneOf(sort2c)).forAll(y.oneOf(sort1)).and(
				x.eq(y).forSome(x.oneOf(sort2b)).forAll(y.oneOf(sort2a))).and(
				sort1.some());
		test6 = new MQuery(f, env);
		if (test6.runQuery().get_hu_ceiling() != 2)
			System.out.println("Test 6c failed!");

		// Cycle of longer length
		// f:1->2a, g:2a->2c h:2c->1 x:1 (inf)
		f = x.eq(y).forSome(x.oneOf(sort2a)).forAll(y.oneOf(sort1)).and(
				x.eq(y).forSome(x.oneOf(sort2c)).forAll(y.oneOf(sort2a))).and(
				x.eq(y).forSome(x.oneOf(sort1)).forAll(y.oneOf(sort2c))).and(
				sort1.some());
		test6 = new MQuery(f, env);
		if (test6.runQuery().get_hu_ceiling() != -1)
			System.out.println("Test 6d failed!");

		// Test more exotic multiplicity locations

		// lone, with ground (lone uses universals only, so no term or function
		// induced...)
		// f:1->2ax. x:2bx. lone 1.
		f = x.eq(y).forSome(x.oneOf(sort2ax)).forAll(y.oneOf(sort1)).and(
				sort2bx.some()).and(sort1.lone());
		test6 = new MQuery(f, env);
		if (test6.runQuery().get_hu_ceiling() != 1)
			System.out.println("Test 6e failed!");

		// not no
		// not no(1) -- should give us 1 atom.
		f = sort1.no().not();
		test6 = new MQuery(f, env);
		if (test6.runQuery().get_hu_ceiling() != 1)
			System.out.println("Test 6f failed!");

		// not lone -- should give us two atoms
		f = sort1.lone().not();
		test6 = new MQuery(f, env);
		if (test6.runQuery().get_hu_ceiling() != 2)
			System.out.println("Test 6g failed!");

		// not one -- same thing (since terms are generated worst-case)
		f = sort1.one().not();
		test6 = new MQuery(f, env);
		if (test6.runQuery().get_hu_ceiling() != 2)
			System.out.println("Test 6h failed!");

		// not one encased in a universal -- two functions induced. Add a ground
		// term and should get *2* plus that ground.
		f = sort1.one().not().forAll(x.oneOf(sort2)).and(sort2.one());
		test6 = new MQuery(f, env);
		if (test6.runQuery().get_hu_ceiling() != 3)
			System.out.println("Test 6i failed!");

		// We use the hash code of the nodes (Decl, not the variable!) to
		// simulate alpha renaming.
		// Test to see if two instances of the same VARIABLE will be treated
		// differently.
		f = Formula.TRUE.forSome(x.oneOf(sort1)).forAll(y.oneOf(sort2)).and(
				Formula.TRUE.forSome(x.oneOf(sort1)).forAll(y.oneOf(sort2)))
				.and(sort2.some());
		test6 = new MQuery(f, env);
		// test6.debug_show_all_formula = true;
		if (test6.runQuery().get_hu_ceiling() != 3)
			System.out.println("Test 6j (simulated alpha renaming) failed!");

		// *********************************************************************************
		// Same vocab as before, but with a custom predicate constrained to be a
		// total function.
		// We also extend the non-disjoint-constrained branch to test "deeper"
		// related sort detection.
		env = new MVocab("Test Vocab");

		// 2 top level types
		env.addSort("Sort1");
		env.addSort("Sort2");

		// Sort 2 has 3 child sorts.
		env.addSubSort("Sort2", "Sort2a");
		env.addSubSort("Sort2", "Sort2b");
		env.addSubSort("Sort2", "Sort2c");

		// 2a and 2b may overlap. b and c are disjoint.
		env.axioms.addConstraintDisjoint("Sort2b", "Sort2c");

		// Each child sort has a child sort.
		env.addSubSort("Sort2a", "Sort2ax");
		env.addSubSort("Sort2b", "Sort2bx");
		env.addSubSort("Sort2c", "Sort2cx");
		env.addSubSort("Sort2ax", "Sort2axx");
		env.addSubSort("Sort2bx", "Sort2bxx");

		env.addPredicate("F", "Sort1 Sort2");
		env.axioms.addConstraintTotalFunction("F");

		sort1 = env.getRelation("Sort1");
		sort2 = env.getRelation("Sort2");
		sort2a = env.getRelation("Sort2a");
		sort2b = env.getRelation("Sort2b");
		sort2c = env.getRelation("Sort2c");
		sort2ax = env.getRelation("Sort2ax");
		sort2bx = env.getRelation("Sort2bx");
		sort2cx = env.getRelation("Sort2cx");
		Expression sort2axx = env.getRelation("Sort2axx");
		Expression sort2bxx = env.getRelation("Sort2bxx");

		// Another unproductive function (which would otherwise induce inf. many
		// terms)
		f = x.eq(y).forSome(x.oneOf(sort2a)).forAll(y.oneOf(sort2a)).and(
				sort2.some());
		MQuery test7 = new MQuery(f, env);
		if (test7.runQuery().get_hu_ceiling() != 1)
			System.out.println("Test 7a failed!");

		// Since 2axx <= 2, infinitely many terms
		f = x.eq(y).forSome(x.oneOf(sort2axx)).forAll(y.oneOf(sort2)).and(
				sort2.some());
		test7 = new MQuery(f, env);
		if (test7.runQuery().get_hu_ceiling() != -1)
			System.out.println("Test 7b failed!");

		// Harmless
		f = x.eq(y).forSome(x.oneOf(sort2)).forAll(y.oneOf(sort2b)).and(
				sort2b.some());
		test7 = new MQuery(f, env);
		// test7.debug_show_all_formula = true;
		if (test7.runQuery().get_hu_ceiling() != 2)
			System.out.println("Test 7c failed!");

		// Make sure that "coverage" logic works correctly
		// Not yet in.
		// f = Formula.TRUE.forSome(x.oneOf(sort1)).forAll(y.oneOf(sort2))
		// .and(sort2.one()).and(sort1.one());
		// test7 = new MGQuery(f, env);
		// if(test7.runQuery().get_hu_ceiling() != 2)
		// System.out.println("Test 7d failed!");

		// We have a total function constraint -- F: 1 -> 2
		// We have a formula inducing G: 2 -> 1
		// and a ground term of sort 1.
		f = Formula.TRUE.forSome(x.oneOf(sort1)).forAll(y.oneOf(sort2)).and(
				sort1.one());
		test7 = new MQuery(f, env);
		// test7.debug_show_all_formula = true;
		if (test7.runQuery().get_hu_ceiling() != -1)
			System.out.println("Test 7e failed!");

		// But don't be overeager (no starter term)
		f = Formula.TRUE.forSome(x.oneOf(sort1)).forAll(y.oneOf(sort2));
		test7 = new MQuery(f, env);
		// test7.debug_show_all_formula = true;
		if (test7.runQuery().get_hu_ceiling() != 0)
			System.out.println("Test 7e(1) failed!");

		// 2 "one"s in the same sort. We want to be smart enough to detect that
		// they cover
		// each other, and only need 1 in sort A (but still need a 2nd in sort 2
		// for the total-function relation.)
		f = sort1.one().and(sort1.one());
		test7 = new MQuery(f, env);
		if (test7.runQuery().get_hu_ceiling() != 2)
			System.out.println("Test 7f failed!");

		// Don't want to count 2 explicit existentials as covering each other!
		// (Only multiplicities with
		// single induced GROUND existentials can be covered.)
		// (Need to include 2 for sort 1, and another 2 for their F-images.)
		f = Formula.TRUE.forSome(x.oneOf(sort1)).and(
				Formula.TRUE.forSome(y.oneOf(sort1)));
		test7 = new MQuery(f, env);
		if (test7.runQuery().get_hu_ceiling() != 4)
			System.out.println("Test 7g failed!");

		// Sub coverage
		f = sort2.one().and(sort2a.one());
		test7 = new MQuery(f, env);
		// test7.debug_show_all_formula = true;
		if (test7.runQuery().get_hu_ceiling() != 1)
			System.out.println("Test 7h failed!");

		// Cycles caused by interaction between a total function and something
		// else.
		// F goes from 1 to 2.
		// OK without the constraints formula from the vocab!
		// But NOT ok with the constraint included (which happens by default
		// now).
		f = Formula.TRUE.forSome(x.oneOf(sort1)).forAll(y.oneOf(sort2)).and(
				sort1.some());
		test7 = new MQuery(f, env);
		if (test7.runQuery().get_hu_ceiling() != -1)
			System.out.println("Test 7i failed!");

		// functions induced by "total function" constraints look like this:
		// fFC1_4579880([sort1]): sort2
		// This shows that the types have been resolved (F: Sort1 -> Sort2)

		// *********************************************************************************
		// What about subset constraints? That is, the user has said P is a
		// subset of R, even though P's parent type is S.
		// (Make sure this and types interact well.)

		// some2cx, f(some2cx) func-constraint(f(some2cx))
		f = Formula.TRUE.forSome(x.oneOf(sort1)).forAll(y.oneOf(sort2cx)).and(
				sort2cx.some());
		test7 = new MQuery(f, env);
		if (test7.runQuery().get_hu_ceiling() != 3)
			System.out.println("Test 8a failed!");

		// But add 2axx subset of 2bxx, and they must be disjoint.

		// THIS INDUCES A SORT-AS-PREDICATE in current query code.
		// oh -- (R in P) rather than (X in P)
		// need to change MFormulaManager TODO
		env.axioms.addConstraintSubset("sort2axx", "sort2bxx");

		f = Formula.TRUE.forSome(x.oneOf(sort2axx)).forAll(y.oneOf(sort2cx))
				.and(sort2cx.some());
		test7 = new MQuery(f, env);
		// test7.debug_verbosity = 2;
		if (test7.runQuery().get_hu_ceiling() != 2)
			System.out.println("Test 8b failed!");

		// System.out.println(env.buildSuperSetSet(env.getType("sort2axx")));
		// System.out.println(env.buildSuperSetSet(env.getType("sort2cx")));
		// System.out.println(env.setsSubset.get("sort2axx"));

		// *OR* add new type 2d, with 2axx < 2d < 2b, and they must be disjoint.
		// (Test multi-subset)
		// Hack to remove the constraint we just added
		env.axioms.setsSubset.get("sort2axx").remove("sort2bxx");
		// Changes
		env.addSubSort("sort2", "sort2d");
		env.axioms.addConstraintSubset("sort2axx", "sort2d");
		env.axioms.addConstraintSubset("sort2d", "sort2bxx");
		f = Formula.TRUE.forSome(x.oneOf(sort2axx)).forAll(y.oneOf(sort2cx))
				.and(sort2cx.some());
		test7 = new MQuery(f, env);
		if (test7.runQuery().get_hu_ceiling() != 2)
			System.out.println("Test 8c failed!");

		// *********************************************************************************
		// Tests to make sure use of sort names as predicates doesn't break
		// soundness/completeness.
		// Many more of these tests in FormulaSigInfo.

		env = new MVocab("Test Vocab");

		// 3 top level types
		env.addSort("Sort1");
		env.addSort("Sort2");
		env.addSort("Sort3");

		sort1 = env.getRelation("Sort1");
		sort2 = env.getRelation("Sort2");
		Relation sort3 = env.getRelation("Sort3");

		env.addSubSort("Sort1", "Sort1a");
		env.addSubSort("Sort1", "Sort1b");
		env.addSubSort("Sort1", "Sort1c");
		sort1a = env.getRelation("Sort1a");
		Relation sort1b = env.getRelation("Sort1b");
		Relation sort1c = env.getRelation("Sort1c");

		// OLD approach to sorts-as-predicates was:
		// f: 2->3. EXISTS c: sort1 AND sort2. (treats as two different
		// constants)
		// c1:1, c2:2, f(c2):3.

		// NEW handling:
		// z is in sort1, but also sort 2. same term.
		// f(z) is in sort3.
		f = Formula.TRUE.forSome(x.oneOf(sort3)).forAll(y.oneOf(sort2)).and(
				z.in(sort2).forSome(z.oneOf(sort1)));
		MQuery test9 = new MQuery(f, env);
		if (test9.runQuery().get_hu_ceiling() != 2)
			System.out.println("Test 9a failed!");

		// Same thing, using subsorts (this is really the same test as 9a,
		// actually?)
		// something in 1a that is in 1b too; and f:1b->1c.
		f = Formula.TRUE.forSome(x.oneOf(sort1c)).forAll(y.oneOf(sort1b)).and(
				z.in(sort1b).forSome(z.oneOf(sort1a)));
		test9 = new MQuery(f, env);
		if (test9.runQuery().get_hu_ceiling() != 2)
			System.out.println("Test 9b failed!");

		System.out.println("----- End MGQuery Tests -----");

		/*
		 * Relation A = Relation.unary("A"); Formula testf =
		 * x.in(A).forSome(x.oneOf(Expression.UNIV));
		 * 
		 * LinkedList<String> atoms = new LinkedList<String>(); atoms.add("1");
		 * //atoms.add("2");
		 * 
		 * Universe u = new Universe(atoms); Bounds b = new Bounds(u);
		 * b.bound(A, u.factory().allOf(1));
		 * 
		 * Options opt = new Options(); opt.setLogTranslation(2);
		 * 
		 * 
		 * try { Translation t = Translator.translate(testf, b, opt);
		 * System.out.println(t.numPrimaryVariables());
		 * Iterator<TranslationRecord> it = t.log().replay();
		 * while(it.hasNext()) { System.out.println(it.next()); }
		 * System.out.println(t.log()); } catch(Exception e) {
		 * System.out.println(e); }
		 */

	}

	// ************
	// New parser will build up an MExploreCondition object.
	// Create a query from it.

	public static MQuery createFromExplore(
			MExploreCondition mpc, List<String> publish,
			Map<String, Set<List<String>>> idbOutputMap,
			Boolean bTupling, Integer iDebugLevel, Integer iCeiling)
			throws MGEUnknownIdentifier, MGEBadIdentifierName,
			MGECombineVocabs, MGEManagerException, MGEUnsortedVariable,
			MSemanticException
			{
		
		// TODO, repurpose these exceptions in context of new qry language
		// (give them fields for row/col...)

		// Given all 3 parameters
		// outmod default: ""
		// publish default: null
		// note that null is DIFFERENT from an empty list!
		// null means publish everything in arbitrary order.
		// empty list means publish NOTHING

		// *****************
		// Validation
		// *****************
		
		// FOR CASES requires tupling (For now)
/*		if(!bTupling && outmod.forCasesOr.size() > 0)
		{
			throw new MSemanticException("Cannot use FOR CASES clause without also enabling TUPLING. If TUPLING fails, FOR CASES will be ignored.");
		}*/
		
		// Tupling <----> for cases and idboutputmap and populated all indexed
		if(bTupling)
		{
			for(String predname : idbOutputMap.keySet())
			{
				if(idbOutputMap.get(predname).size() < 1)
					throw new MSemanticException("TUPLING was enabled but IDB output for pred: "+predname+" was not indexed.");				
			}			
		/*	for(String predname : outmod.populatedCandidates.keySet())
			{
				if(outmod.populatedCandidates.get(predname).size() < 1)
					throw new MSemanticException("TUPLING was enabled for a SHOW (UN)POPULATED query but pred: "+predname+" in the SHOW clause was not indexed.");					
			}
			for(String predname : outmod.forCasesOr.keySet())
			{
				if(outmod.forCasesOr.get(predname).size() < 1)
					throw new MSemanticException("FOR CASES clause contained a pred: "+predname+" that was not indexed.");					
			}*/
		}
		else
		{
			for(String predname : idbOutputMap.keySet())
			{
				if(idbOutputMap.get(predname).size() > 0)
					throw new MSemanticException("TUPLING was not enabled but IDBOUTPUT for pred: "+predname+" was indexed.");				
			}			
			//for(String predname : outmod.populatedCandidates.keySet())
			//{
			//	if(outmod.populatedCandidates.get(predname).size() > 0)
			//		throw new MSemanticException("TUPLING was not enabled for a SHOW (UN)POPULATED query but pred: "+predname+" in the SHOW clause was indexed.");					
			//}			
		}
		// TODO to give row/col for the above, need to keep their location in the outmod until we know whether or not we're tupled
		
		
		// **********************************************************
		// (1) Assemble a combined vocabulary
		// **********************************************************
		MVocab uber = null;

		for (MIDBCollection p : mpc.seenIDBs) {
			if (uber == null) {
				uber = p.vocab;
				continue;
			}

			uber = uber.combineWith(p.vocab);
		}

		// No IDBS, no UNDER clause?
		if (uber == null) {
			// no vocab! Error out.
			throw new MSemanticException(
					"A query that mentions neither policies nor prior queries must use the UNDER clause.");
		}

		// **********************************************************
		// (2) Check for invalid EDBs
		// **********************************************************
		for (Relation r : mpc.madeEDBs) {
			if (uber.predicates.values().contains(r))
				continue;
			try {
				uber.getSortForExpression(r);
			} catch (MGEUnknownIdentifier e) {
				throw new MSemanticException("Unknown EDB " + r
						+ ". The given policies were unaware of this EDB name.");
			}
		}

		// **********************************************************
		// At this point, we have the query formula (w/ proper variables)
		// but no prefix or sorts on the vars.

		Formula qryFormula = mpc.fmla;

		// mpc has given us a bunch of assertions. Now we need to
		// (a) unpack the ones that do not refer to sort EDBs, now that we have
		// signatures of the predicates
		// (b) resolve them.
		Map<Variable, Expression> freeVars = new HashMap<Variable, Expression>();
		handleSortAssertions(uber, mpc, freeVars);

		// (3) assemble prefix
		// **********************************************************

		// For saved query use
		Formula idbFormula = qryFormula;
		List<Variable> varOrdering = new ArrayList<Variable>();

		
		// *** 
		// idbFormula: has free vars, for use later
		// qryFormula: all quantified, for use now
		
		// Need to make sure the ordering in publish is preserved, so can't just
		// iterate freeVars.keySet().

		// If the user doesn't provide a PUBLISH clause, construct one
		// arbitrary ordering TODO later use lexicographic
		if(publish == null)
		{
			publish = new ArrayList<String>(freeVars.size());
			for(Variable v : freeVars.keySet())
			{
				// Don't publish "temp" vars! 
				// TODO this is a kludge; user could still naively make a var called TempVarFoozle
				if(!v.name().startsWith(MCommandParser.tempVarPrefix))
					publish.add(v.name());
			}
			
		}

		List<String> prefixVarOrder = new ArrayList<String>();
		
		// FIRST: Published vars
		for (String vname : publish)
		{
			Variable v = MFormulaManager.makeVariable(vname);
			varOrdering.add(v);
			Decl d = MFormulaManager.makeOneOfDecl(v, freeVars.get(v));
			qryFormula = MFormulaManager.makeExists(qryFormula, d);
			prefixVarOrder.add(v.name());
		}

		
		
		// NEXT: Unpublished vars
		for (Variable v : freeVars.keySet())
		{
			if (publish != null && publish.contains(v.name()))
				continue;

			Decl d = MFormulaManager.makeOneOfDecl(v, freeVars.get(v));

			// If not published, bury the quantifier in the IDB formula
			idbFormula = MFormulaManager.makeExists(idbFormula, d);

			// include in the query as normal
			qryFormula = MFormulaManager.makeExists(qryFormula, d);
			
			prefixVarOrder.add(v.name());
		}

		//System.out.println("Temp debug info: "+qryFormula);
		//System.out.println(prefixVarOrder);

		Collections.reverse(prefixVarOrder);
		
		MQuery result = new MQuery(uber, qryFormula, mpc.seenIDBs);						
		
		// **********************************
		// Handle IDB output parameters	
		for(String idbname : idbOutputMap.keySet())
		{			
			// add to general list
			result.addIDBOutputs(idbname);
			
			for(List<String> indexing : idbOutputMap.get(idbname))
			{
				try
				{
					// Can't add directly: user has provided a vector of identifiers.
					// addIDBOutputIndexing expects an _indexing_ into the tupled ordering.
					// Need to use our knowledge (from above) about ordering of the prefix.
					
					// prefixVarOrder

					List<String> nIndexing = new ArrayList<String>(indexing.size());
					for(String varname : indexing)
					{
						// Look for this var
						int ii = prefixVarOrder.indexOf(varname);
						// if not found, error.
						if(ii < 0)
						{
							throw new MGEArityMismatch("Unrecognized identifier in IDB output indexing: "+varname);
						}
						nIndexing.add(String.valueOf(ii+1));
					}
					
					result.addIDBOutputIndexing(idbname, nIndexing);
				}
				catch(MGEArityMismatch e)
				{				
					MEnvironment.errorStream.println(e);
					return null;
				}
			}
		}
		
		// TODO better errors for "no indexing" etc.
		
		result.debug_verbosity = iDebugLevel;
		result.doTupling = bTupling;
		result.sizeCeiling = iCeiling;

		// Remember our varvector and the inferred sorts.
		// Ordering is a bit arbitrary unless the user has provided a PUBLISH
		// clause.

		result.varOrdering = varOrdering;
		for (Variable v : varOrdering) {
			result.varSorts.put(v, freeVars.get(v));
		}

		result.idbs.put("saved", idbFormula);

		MEnvironment.setLast(result);

		// System.out.println("\nQuery with vector: "+result.varOrdering+" sorts: "+result.varSorts);

		// TODO: Question of leaving existentials bound in the query. Not the
		// same as RESTRICT TO
		// it's a foundational semantics version...
		// This should be provided by the PUBLISH keyword, need to test.
		
		// TEST TEST TEST!

		return result;
	}

	static void handleSortAssertions(MVocab voc, MExploreCondition mpc,
			Map<Variable, Expression> freeVars) throws MGEBadIdentifierName,
			MGEUnknownIdentifier {
		// MREPL.outStream.println(mpc.assertPossible);
		// System.out.println(mpc.assertNecessary);
		// System.out.println(mpc.assertAtomicNecessary);

		// Stopping compilation because something is wrong here. 
		//dfadsf;
		
		Set<List<Variable>> toDo = new HashSet<List<Variable>>(
				mpc.assertNecessary.keySet());

		for (List<Variable> varvector : toDo)
		{

//			System.out.println("Checking for: "+varvector);
			
			Set<MVariableVectorAssertion> assertsN = mpc.assertNecessary
					.get(varvector);
			Set<MVariableVectorAssertion> assertsA = mpc.assertAtomicNecessary
					.get(varvector);
			Set<MVariableVectorAssertion> assertsP = mpc.assertSufficient
					.get(varvector);

			Set<MVariableVectorAssertion> allNecessary = new HashSet<MVariableVectorAssertion>(
					assertsN);
			allNecessary.addAll(assertsA);

			for (Variable v : varvector) {
				// Default to no info
				freeVars.put(v, Expression.UNIV);
				MSort runningSort = null;

				//System.out.println("var: "+v);
				
				for (MVariableVectorAssertion a : allNecessary) {
					// TODO
					// If it is NECESSARY for x to not have sort A
					// there is more inference we could do, but skipping for
					// now.
					
					//System.out.println("assertion: "+a);
					
					if (!a.positive)
						continue;

					// If it is NECESSARY for x to have sort A
					// Then x must have sort A (or a subsort thereof)

					// If rel is a sort (which means this is a singleton vector)
					// just handle that assertion.
					if (voc.isSort(a.sortExpression.toString()))
					{
						MSort theSort = voc.getSortForExpression(a.sortExpression);
						if (runningSort == null)
						{
							runningSort = theSort;
							freeVars.put(v, a.sortExpression);
						} 
						else
						{
							// is theSort a subsort of the current running sort?
							// (need to check grandchildren etc. Also use subset constraints.)
							// If not, ignore
							
							//System.out.println("SUB? --- "+runningSort.subsorts);
							//System.out.println(theSort);							
							if(voc.isSubOrSubOf(theSort, runningSort))
							{
								runningSort = theSort;
								freeVars.put(v, a.sortExpression);
							}

							// else do nothing. (we COULD see which option had
							// the most children? TODO maybe

						}
					}

					// If rel is not a sort, get the matching sort from rel's
					// signature
					else {
						// TODO --------------------
						// TODO IMPORTANT unpack for preds that are not sorts

					}
				}

			}

		}

		// TODO do we need to handle possible assertions? If so, how?

	}

	public void addIDBOutputs(String idbname)
	{
		//System.err.println("adding: "+idbname);
		if(!idbOutputIndexing.containsKey(idbname))
			idbOutputIndexing.put(idbname, new HashSet<List<String>>());
		//System.err.println(idbOutputIndexing);
	}
	
	public void addIDBOutputs(List<String> idbnames)
	{	
		for(String s : idbnames)
			addIDBOutputs(s);
	}
	
}

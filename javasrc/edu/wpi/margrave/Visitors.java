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

package edu.wpi.margrave;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;

import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.ConstantFormula;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.NaryFormula;
import kodkod.ast.Node;
import kodkod.ast.NotFormula;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Relation;
import kodkod.ast.UnaryExpression;
import kodkod.ast.Variable;
import kodkod.ast.operator.ExprCompOperator;
import kodkod.ast.operator.ExprOperator;
import kodkod.ast.operator.Quantifier;
import kodkod.ast.visitor.AbstractCollector;
import kodkod.ast.visitor.AbstractDetector;
import kodkod.ast.visitor.AbstractReplacer;
import kodkod.ast.visitor.AbstractVoidVisitor;


// Kodkod's AbstractReplacer doesn't cache ANYTHING by default, and we would rather it cached everything.
// I have not added all possible node types yet, but the big ones are here...

abstract class AbstractCacheAllReplacer extends AbstractReplacer {

	AbstractCacheAllReplacer(HashSet<Node> beginCached) {
		super(beginCached);
	}

	public Formula visit(ComparisonFormula comp) 
	{
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
			MEnvironment.writeErrLine(e.getLocalizedMessage());
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
			MEnvironment.writeErrLine(e.getLocalizedMessage());
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

class FormulaIndentPrintV extends AbstractVoidVisitor
{
	int depth = 0;
	static int depthinc = 4;
	
	private void printDepth()
	{
		for(int ii=0;ii<depth;ii++)
			MEnvironment.writeErr(" ");			
	}
	
	public void visit(ComparisonFormula comp)
	{
		printDepth();
		MEnvironment.writeErrLine(comp.toString());		
	}

	public void visit(BinaryFormula binFormula) {
		printDepth();
		MEnvironment.writeErrLine("("+binFormula.op().toString()+" ");
		
		depth += depthinc;
		binFormula.left().accept(this);
		binFormula.right().accept(this);
		depth -= depthinc;
		
		printDepth();
		MEnvironment.writeErrLine(")");
	}

	public void visit(NotFormula not) {
		printDepth();
		MEnvironment.writeErrLine("(not ");
		
		depth += depthinc;
		not.formula().accept(this);
		depth -= depthinc;
		
		printDepth();
		MEnvironment.writeErrLine(")");
	}

	public void visit(QuantifiedFormula quantFormula) {
		printDepth();
		MEnvironment.writeErrLine("("+quantFormula.quantifier().toString() +" "+quantFormula.decls().toString()+" ");
		
		depth += depthinc;
		quantFormula.formula().accept(this);
		depth -= depthinc;
		
		printDepth();
		MEnvironment.writeErrLine(")");
	}

	public void visit(NaryFormula nFormula) {
		
		printDepth();
		MEnvironment.writeErrLine("("+nFormula.op().toString()+" ");
		depth += depthinc;
		for(Formula child : nFormula)
			child.accept(this);
		depth -= depthinc;
		
		printDepth();
		MEnvironment.writeErrLine(")");
	}

	@Override
	protected boolean visited(Node n) {
		// Auto-generated method stub
		return false;
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
 * Visitor that replaces instances for some Relations and Variables
 *
 * @author tn
 *
 */
class RelationAndVariableReplacementV extends AbstractCacheAllReplacer
{
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

		// MEnvironment.writeErrLine("NEW REPLACEMENT VISITOR:");
		// MEnvironment.writeErrLine(pps);
		// MEnvironment.writeErrLine(vps);
	}

	public QuantifiedFormula visit(QuantifiedFormula qf)	
	{
		// If qf binds one of the "to" variables in the substitution, 
		// refuse the substitution and throw an error
		
		Set<Variable> toVars = new HashSet<Variable>(varpairs.values());
		for(Decl d: qf.decls())
		{
			if(toVars.contains(d.variable()))
				throw new MGEVariableAlreadyBound(d.variable(), "The variable name "+d.variable()+
						" is already used by some part of this query, and could not be safely substituted. " +
						"(See the ``Substitution'' section of the documentation for more information.)");
		}
		
		if(qf.quantifier().equals(Quantifier.ALL))		
			return (QuantifiedFormula) MFormulaManager.makeForAll(qf.formula().accept(this), qf.decls().accept(this));
		else	
			return (QuantifiedFormula) MFormulaManager.makeExists(qf.formula().accept(this), qf.decls().accept(this)); 
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
				MEnvironment.writeErrLine("Warning: ComparisonFormula with EQUALS operator with non-Variable children visited: "
								+ comp);
				return cache(comp, newlhs.eq(newrhs));
			}

			try {
				return cache(comp, MFormulaManager.makeEqAtom(
						(Variable) newlhs, (Variable) newrhs));
			} catch (MGEManagerException e) {
				MEnvironment.writeErrLine(e);
				return cache(comp, newlhs.eq(newrhs));
			}

		} else {
			// IN (SUBSET)

			// Left hand side (at least) may be a BinaryExpression.
			// If we just call accept(this), may end up constructing new tuples
			// that the manager doesn't know about. Instead...

			Expression newlhs;
			Expression newrhs;

			try {
				// We have a var tuple? Replace vars AS NEEDED!
				if (comp.left() instanceof BinaryExpression)
					newlhs = MFormulaManager.substituteVarTuple((BinaryExpression) comp.left(), varpairs);
				else
					newlhs = comp.left().accept(this);

				if(comp.right() instanceof BinaryExpression)
					newrhs = MFormulaManager.substituteVarTuple((BinaryExpression) comp.right(), varpairs);
				else
					newrhs = comp.right().accept(this);

				return cache(comp, MFormulaManager.makeAtom(newlhs, newrhs));
			} catch (MGEManagerException e) {
				MEnvironment.writeErrLine(e);

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
			MEnvironment.writeErrLine("Error: Expected " + post.toString() + ", got: "
					+ pre.accept(vrepl).toString());
	}

	public static void unitTests() {
		MEnvironment.writeErrLine("----- Begin RelationReplacementV Tests (No messages is good.) -----");

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
			MEnvironment.writeErrLine("Error in RelationReplacementV test case: visitor is overwriting old reference.");

		runUnitTest(v, x.eq(y), y.eq(y));

		oldf = x.eq(y);
		oldf.accept(v);
		if (!oldf.toString().equals("(x = y)"))
			MEnvironment.writeErrLine("Error in VariableReplacementV test case: visitor is overwriting old reference.");

		MEnvironment.writeErrLine("----- End RelationReplacementV Tests -----");
	}
}

class ReplaceComparisonFormulasV extends AbstractCacheAllReplacer
{
	Map<Node, Node> toReplace;
	
	ReplaceComparisonFormulasV(Map<Node, Node> toReplace)
	{
		super(new HashSet<Node>());
		this.toReplace = toReplace;
	}
	
	public Formula visit(ComparisonFormula comp)
	{
		if (cache.containsKey(comp))
			return lookup(comp);
		cached.add(comp);
		
		if(toReplace.containsKey(comp))
		{
			return (Formula) cache(comp, toReplace.get(comp));
		}
		
		return cache(comp, comp);
	}
	
	
}

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
	private static WeakHashMap<Node, Boolean> closureUseCache = new WeakHashMap<Node, Boolean>();

	protected Boolean lookup(Node f) {
		return closureUseCache.get(f);
	}

	protected Boolean cache(Node f, Boolean value) {

		closureUseCache.put(f, value);
		return value;
	}

	public FindClosureUseV() {
		super(new HashSet<Node>());
	}

	public Boolean visit(UnaryExpression ue) 
	{
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


			// MEnvironment.writeErrLine("Indexed "+String.valueOf(qCount) + " "
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

class MatrixTuplingV extends AbstractCacheAllReplacer
{
	// Visitor to translate a formula to its tupled analogue.
	// (No axioms are added, caller is responsible for adding
	//  the necessary tupling axioms.)

	// pre-tupling vocabulary
	public MVocab oldvocab;
	
	// New vocabulary. Visitor will add new sorts as needed.
	// Same sort structure as before duplicated (at most) pren.qCount times.
	// E.g., where A < B before, now A_1 < B_1, A_2 < B_2, and so on.
	public MVocab newvocab;
	
	// The new variable (usually "z")
	public Variable newvar;
	
	// Set of equality predicates that we created
	public Set<String> equalAxiomsNeeded;

	// Used privately to get information about this formula's prefix
	protected PrenexCheckV pv;

	// Store that (e.g.) P became P_1, P_2, and P_4, R became R_2,3,5, etc.
	// Applies for BOTH sorts and ordindary predicates
	private HashMap<String, Set<String>> cachePredicateToIndexings = new HashMap<String, Set<String>>();

	// ii -> { old pred names we tupled with index ii }
	// In case the indexing is >1-ary, ii is the string "i_1, ..., i_n"
	// e.g. R_1,2,3 induces "1,2,3" ---> { R, <and possibly others>}
	private HashMap<String, Set<String>> cacheIndexingToPredicates = new HashMap<String, Set<String>>();

	// Given a single index string, returns the tupled predicates that use
	// that index somewhere in their indexing.
	// Used in equality axiom generation for >1-ary predicates
	private HashMap<String, Set<String>> cacheNewRelationsUsingIndex = new HashMap<String, Set<String>>();
	
	Set<String> getIndexingToPredicates(String indexing)
	{
		Set<String> result = cacheIndexingToPredicates.get(indexing);
		if(result != null)
			return result;
		return new HashSet<String>();
	}

	Set<String> getNewRelationsUsingIndex(String indexing)
	{
		Set<String> result = cacheNewRelationsUsingIndex.get(indexing);
		if(result != null)
			return result;
		return new HashSet<String>();
	}
	
	Set<String> getPredicateToIndexings(String indexing)
	{
		Set<String> result = cachePredicateToIndexings.get(indexing);
		if(result != null)
			return result;
		return new HashSet<String>();
	}

	String getCacheStringForDebug()
	{
		return "  Indexing to Predicates: "+cacheIndexingToPredicates + MEnvironment.eol +
		"  Predicate to Indexings: "+cachePredicateToIndexings +MEnvironment.eol +
		"  Index to Used: "+cacheNewRelationsUsingIndex +MEnvironment.eol;
	}
	
	MatrixTuplingV(PrenexCheckV pren, MVocab old) 
	throws MGEBadIdentifierName // should never occur, but just in case
	{
		super(new HashSet<Node>());

		oldvocab = old;
		newvocab = new MVocab();
		newvar = MFormulaManager.makeVariable("z");
		equalAxiomsNeeded = new HashSet<String>();

		// FOR NOW, all equality predicates. 
		// OPT: no need to model i=j if their sorts are always disjoint
		for (int ileft = 1; ileft <= pren.qCount; ileft++)
			for (int iright = ileft + 1; iright <= pren.qCount; iright++)
			{
				String name = "=_" + ileft + "," + iright;
				equalAxiomsNeeded.add(name);
				newvocab.addPredicate(name, pren.tupleTypeName);
			}

		
		// Since we tuple any arity predicate, cannot initialize cacheIndexingToPredicates
		// for each index. Instead, check on use below.

		
		pv = pren;

		// New top-level sort is created AFTER this visitor runs.
		// Not elegant, but avoids problem of A < B < C
		
	}

	Formula tuplingFail(Formula fmla, String msg)
	{
		MEnvironment.writeErrLine("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
		MEnvironment.writeErrLine(msg);
		MEnvironment.writeErrLine("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
		System.exit(2);
		return fmla; // don't know what to do
	}
	
	void tuplingFail(String msg)
	{
		MEnvironment.writeErrLine("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
		MEnvironment.writeErrLine(msg);
		MEnvironment.writeErrLine("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
		System.exit(2);		
	}	
	
	public Formula visit(ComparisonFormula cf)
	{
		if (cache.containsKey(cf))
			return lookup(cf);

		Formula newf;

		// EQUAL: var1 = var2.
		if (cf.op().equals(ExprCompOperator.EQUALS)) 
		{
			if (!(cf.left() instanceof Variable) || !(cf.right() instanceof Variable))
			{
				return tuplingFail(cf, "Comparison: " + cf + " must be over variables.");	
			}

			int leftidx = pv.indexing.get(cf.left());
			int rightidx = pv.indexing.get(cf.right());

			// Have we already seen these, just in reverse order? (Equality is
			// symmetric; don't create an =21 predicate when we've already seen 
			// and created =12.)
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
			// [[[ Already added in constructor, but it's a set so leaving in - TN]]]
			equalAxiomsNeeded.add(predname);

			// Add the new equality predicate =_i,j
			try 
			{
				newvocab.addPredicate(predname, pv.tupleTypeName);																	
			} catch (MGEBadIdentifierName e) 
			{
				return tuplingFail(cf, e.getLocalizedMessage());
			}

			// Construct =_i,j(z) and return it instead of x_i=x_j
			try 
			{
				Relation newrel = newvocab.getRelation(predname);
				newf = MFormulaManager.makeAtom(newvar, newrel);

				cached.add(cf);
				return cache(cf, newf);
			} catch (Exception e) {
				return tuplingFail(cf, e.getLocalizedMessage());
			}
		}

		// SUBSET: (var1 X ... X varN) in Relation
		else if (cf.op().equals(ExprCompOperator.SUBSET)) {

			if (!(cf.right() instanceof Relation))
			{
				return tuplingFail(cf, "Comparison: " + cf + " must be vs. a Relation.");
			}

			// DFS of left hand side (like in well-sortedness test) to get
			// indexing

			String suffix;
			if (cf.left() instanceof Variable)
				suffix = "_" + pv.indexing.get(cf.left());
			else if (cf.left() instanceof BinaryExpression)
				suffix = "_" + MVocab.constructIndexing((BinaryExpression) cf.left(), pv.indexing);
			else {
				return tuplingFail(cf, "Comparison: " + cf + " -- improper LHS.");
			}

			// Make sure we were able to create the indexing.
			if (suffix.equals("_null")) {
				return tuplingFail(cf, "Bad indexing for " + cf.left());
			}

			// New indexed predicate name
			String oldpredname = cf.right().toString();
			String newpredname = oldpredname + suffix;

			try
			{

				// Is this a predicate or a sort? If a sort, does it have a
				// parent?
				boolean isSort = oldvocab.fastIsSort(oldpredname);
				if (!isSort)
				{
					// This is a predicate
					newvocab.addPredicate(newpredname, pv.tupleTypeName);
					addToCaches(oldpredname, suffix, newpredname);
				} 
				else
				{
					addSortWithSupers(newvocab, oldvocab, oldpredname, suffix);
					// addToMap called by addSortWithSupers, no need to call here
				}

				Relation newrel = newvocab.getRelation(newpredname);
				newf = MFormulaManager.makeAtom(newvar, newrel);

				cached.add(cf);
				return cache(cf, newf);

			} catch (Exception e) {
				return tuplingFail(cf, e.getLocalizedMessage());
			}

		}

		// for new features
		MEnvironment.writeErrLine("Comparison: " + cf + "; unrecognized operator.");
		System.exit(1);
		return cf;
	}

	void addToCaches(String oldname, String suffix, String newToAdd)
	{
		// ----------------------------------------------------------
		// Cache ->
		if (!cachePredicateToIndexings.containsKey(oldname))
			cachePredicateToIndexings.put(oldname, new HashSet<String>());
		cachePredicateToIndexings.get(oldname).add(newToAdd);

		// ----------------------------------------------------------
		// Cache <-
		String indexing = suffix.substring(1); // remove the underscore

		if (!cacheIndexingToPredicates.containsKey(indexing))
			cacheIndexingToPredicates.put(indexing, new HashSet<String>());

		cacheIndexingToPredicates.get(indexing).add(oldname);
		
		// ----------------------------------------------------------
		// idx -> uses idx cache
		String[] indices = indexing.split(",");
		
		for(String idx : indices)
		{
			if (!cacheNewRelationsUsingIndex.containsKey(idx))
				cacheNewRelationsUsingIndex.put(idx, new HashSet<String>());
			
			cacheNewRelationsUsingIndex.get(idx).add(newToAdd);
		}
		
		
	}

	void addSortWithSupers(MVocab newvocab, MVocab oldvocab,
			String oldpredname, String suffix)
	throws MGEUnknownIdentifier, MGEBadIdentifierName 
	{
		
		// What if A < B < C, and A_1 is what we've found?
		// Create both B_1 and C_1
		// (IP Addr > Range > Single IP. -- play nice with tuple axioms)

		MSort oldsort = oldvocab.getSort(oldpredname);
		if (oldsort.parents.size() > 0)
		{
			// Add parent's parents (if any) recursively
			for(MSort aParent : oldsort.parents)
			{
				addSortWithSupers(newvocab, oldvocab, aParent.name, suffix);

				newvocab.addSubSort(aParent.name + suffix, 
						oldpredname + suffix);
				addToCaches(oldpredname, suffix, oldpredname + suffix);
			}
		} 
		else 
		{
			newvocab.addSort(oldpredname + suffix);
			addToCaches(oldpredname, suffix, oldpredname + suffix);
		}
	}

	public void forceIncludeEDB(String edbname, List<String> indexingVars)
	{
		// Numeric indexing (convert var names to indices)
		String numIndexing = MVocab.constructIndexing(indexingVars, pv.indexing);
		String suffix = "_" + numIndexing;
		String newpredname = edbname + suffix;
		
		
		// Is this a predicate or a sort? If a sort, does it have a parent?
		boolean isSort = oldvocab.fastIsSort(edbname);
		
		try
		{
			if (!isSort)
			{
				// This is a predicate
				newvocab.addPredicate(newpredname, pv.tupleTypeName);
				addToCaches(edbname, suffix, newpredname);
			} 
			else
			{
				addSortWithSupers(newvocab, oldvocab, edbname, suffix);
				// addToMap called by addSortWithSupers, no need to call here
			}
		}
		catch (Exception e) 
		{
			tuplingFail(e.getLocalizedMessage());
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



/*
// Return a set of all expressions in the join.
// Do not inherit AbstractCacheAllDetector. May not be safe, and this should only be called on a small Expression, not a Formula.

class BreakUpJoinsV extends AbstractDetector
{
	List<Expression> elist;
	boolean seenRelation;

	public BreakUpJoinsV() {
		super(new HashSet<Node>());
		elist = new LinkedList<Expression>();
		seenRelation = false;
	}

	public Boolean visit(Relation r) {
		// DO NOT CACHE RELATIONS

		if (seenRelation)
			return false;
		seenRelation = true;
		elist.add(r);
		return true;
	}

	public Boolean visit(Variable v) {
		// DO NOT CACHE VARIABLES

		elist.add(v);
		return true;
	}

	public Boolean visit(NaryExpression expr) {
		// This is untested.
		MEnvironment.errorStream
				.println("Entering unsafe functionality: BreakUpJoinV.visit(NaryExpression). Please notify developer.");

		if (expr.op().equals(ExprOperator.JOIN))
			for (int ii = 0; ii < expr.arity(); ii++) {
				if (!expr.child(ii).accept(this))
					return cache(expr, false);
			}
		else {
			return cache(expr, false);
		}

		return cache(expr, true);
	}

	public Boolean visit(BinaryExpression expr) {

		if (expr.op().equals(ExprOperator.JOIN)) {
			if (!expr.left().accept(this))
				return cache(expr, false);
			if (!expr.right().accept(this))
				return cache(expr, false);
		} else
			return cache(expr, false);

		return cache(expr, true);
	}
}

*/
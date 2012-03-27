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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.ConstantFormula;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.NaryExpression;
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

abstract class AbstractCacheAllReplacer extends AbstractReplacer
{	
	// Store the meaning of new term expressions
	Map<Expression, MTerm> termMap = new HashMap<Expression, MTerm>();
	
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

		return cache(nFormula, MFormulaManager.makeComposition(nFormula.op(), newformulas));
	}
	
	public Expression visit(NaryExpression expr)
	{
		if(cache.containsKey(expr))
			return lookup(expr);
		
		cached.add(expr);
		
		List<Expression> newsubs = new ArrayList<Expression>();
		for(Expression sub : expr)
		{
			newsubs.add(sub.accept(this));
		}
		
		Expression result;
		
		if(ExprOperator.JOIN.equals(expr.op()))
			result = cache(expr, MFormulaManager.makeJoinE(newsubs));
		else if(ExprOperator.PRODUCT.equals(expr.op()))
			result = cache(expr, MFormulaManager.makeExprTupleE(newsubs));
		else if(ExprOperator.INTERSECTION.equals(expr.op()))
			result = cache(expr, MFormulaManager.makeIntersection(newsubs));
		else if(ExprOperator.UNION.equals(expr.op()))
			result = cache(expr, MFormulaManager.makeUnion(newsubs));	
		else
		{
			// TODO no support for difference, etc. 		
			throw new MUserException("AbstractCacheAllReplacer saw expression with unsupported operator: "+expr);
		}
		
		// Do we have a term map?
		// Did we actually change the expression?
		// Is this complex expression a term? (IFF it's a join)
		if(termMap != null && expr != result && ExprOperator.JOIN.equals(expr.op()))
		{
			MCommunicator.writeToLog("\nReplacing NaryExpression and had a new term: "+result+" with old: "+expr);
			// Need to let the context map the new term from it's Expression		
			MTerm oldTerm = termMap.get(expr);
			termMap.put(result, MTerm.makeTermFromExpression(result));				
		}
		
		return result;
	}
	
	public Expression visit(BinaryExpression expr)
	{
		if(cache.containsKey(expr))
			return lookup(expr);
		
		cached.add(expr);
		
		List<Expression> newsubs = new ArrayList<Expression>(2);
		newsubs.add(expr.left().accept(this));
		newsubs.add(expr.right().accept(this));
		
		Expression result;
		
		if(ExprOperator.JOIN.equals(expr.op()))
			result = cache(expr, MFormulaManager.makeJoinE(newsubs));
		else if(ExprOperator.PRODUCT.equals(expr.op()))
			result = cache(expr, MFormulaManager.makeExprTupleE(newsubs));
		else if(ExprOperator.INTERSECTION.equals(expr.op()))
			result = cache(expr, MFormulaManager.makeIntersection(newsubs));
		else if(ExprOperator.UNION.equals(expr.op()))
			result = cache(expr, MFormulaManager.makeUnion(newsubs));	
		else
		{
			// TODO no support for difference, etc. 		
			throw new MUserException("AbstractCacheAllReplacer saw expression with unsupported operator: "+expr);
		}		
		
		// Do we have a term map?
		// Did we actually change the expression?
		// Is this complex expression a term? (IFF it's a join)
		if(termMap != null && expr != result && ExprOperator.JOIN.equals(expr.op()))
		{
			// Need to let the context map the new term from it's Expression	
			MCommunicator.writeToLog("\nReplacing NaryExpression and had a new term: "+result+" with old: "+expr);
			MTerm oldTerm = termMap.get(expr);
			termMap.put(result, MTerm.makeTermFromExpression(result));				
		}

		return result;
	}
	
}

abstract class AbstractCacheAllDetector extends AbstractDetector
{
	AbstractCacheAllDetector(HashSet<Node> beginCache)
	{
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

class FindClosureUseV extends AbstractCacheAllDetector
{
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
		{
			counter++;
			iComparisonFormula++;
		}
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
class RelationAndTermReplacementV extends AbstractCacheAllReplacer
{
	private Map<Relation, Relation> relpairs;
	private Map<Variable, Expression> termpairs;
	private boolean no_change;
	
	public RelationAndTermReplacementV(Map<Relation, Relation> pps,
			Map<Variable, Expression> vps, Map<Expression, MTerm> termMap)
	{
		super(new HashSet<Node>());
		
		this.termMap = termMap;
		if(this.termMap == null)
			this.termMap = new HashMap<Expression, MTerm>();

		// Replace P with Q in the pair.
		this.relpairs = pps;

		// Replace x with t in the pair.
		this.termpairs = vps;

		// Only visit if there is SOMETHING different
		no_change = true;
		for (Relation r : relpairs.keySet())
			if (!r.equals(relpairs.get(r))) {
				no_change = false;
				break;
			}

		for (Variable v : termpairs.keySet())
			if (!v.equals(termpairs.get(v))) {
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
		
		Set<Expression> toTerms = new HashSet<Expression>(termpairs.values());
		for(Decl d: qf.decls())
		{
			if(toTerms.contains(d.variable()))
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
		{
			Expression newrel = relpairs.get(therel);	
			
			// No clue if this is a constant term or not... but should be harmless to create a term for it?			
			termMap.put(newrel, MTerm.makeTermFromExpression(newrel));
			MCommunicator.writeToLog("\nRelationAndTermReplacementV: visited Relation and replaced. New="+newrel+" Old="+therel);
			return cache(therel, newrel);
		}
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

		if (ExprCompOperator.EQUALS.equals(comp.op())) 		
		{
			// EQUALITY
			// newly created terms will be collected here:
			Expression newlhs = comp.left().accept(this);
			Expression newrhs = comp.right().accept(this);
			
			// No longer require that both sides of an equality be either a relation or a variable;
			// could have complex terms of type Expression, but not LeafExpression.
			
			return cache(comp, MFormulaManager.makeEqAtom(newlhs, newrhs));

		} else {
			// IN (SUBSET)

			// Left hand side (at least) may be a BinaryExpression.
			// If we just call accept(this), may end up constructing new tuples
			// that the manager doesn't know about. Instead...

			Expression newlhs;
			Expression newrhs;

			
			
			// TODO do we need substituteExprTuple anymore???
			// Updated visitor seems to do everything it does?
			
				// We have a var tuple? Replace vars AS NEEDED!
			//	if (comp.left() instanceof BinaryExpression ||
			//			comp.left() instanceof NaryExpression)
			//		newlhs = MFormulaManager.substituteExprTuple(comp.left(), termpairs);
			//					
			//	else
					newlhs = comp.left().accept(this);

			//	if(comp.right() instanceof BinaryExpression ||
			//			comp.right() instanceof NaryExpression)
			//		newrhs = MFormulaManager.substituteExprTuple(comp.right(), termpairs);
			//	else
					newrhs = comp.right().accept(this);

			return cache(comp, MFormulaManager.makeAtom(newlhs, newrhs));			
		}
	}

	public Expression visit(Variable var) 
	{		
		if (cache.containsKey(var))
			return lookup(var);
		cached.add(var);
		
		// Perform the variable replacement here, if needed.
		if (termpairs.containsKey(var))
		{
			Expression newvar = termpairs.get(var);			
			termMap.put(newvar, MTerm.makeTermFromExpression(newvar));
			MCommunicator.writeToLog("\nRelationAndTermReplacementV: visited Variable and replaced. New="+newvar+" Old="+var);
			return cache(var, newvar);
		}
		else
			return cache(var, var);
	}

	private static void runUnitTest(String name, RelationAndTermReplacementV vrepl,
			Formula pre, Formula post)
	{			
		if (!pre.accept(vrepl).toString().equals(post.toString()))
			MEnvironment.writeErrLine("Error in test "+name+": Expected " + post.toString() + ", got: "
					+ pre.accept(vrepl).toString());
	}

	public static void unitTests() {
		MEnvironment.writeErrLine("----- Begin RelationReplacementV Tests (No messages is good.) -----");

		HashMap<Relation, Relation> rtestset = new HashMap<Relation, Relation>();
		HashMap<Variable, Expression> vtestset = new HashMap<Variable, Expression>();

		Relation R = MFormulaManager.makeRelation("R", 1);
		Relation P = MFormulaManager.makeRelation("P", 1);
		Relation Q = MFormulaManager.makeRelation("Q", 1);
		rtestset.put(R, P);

		Variable x = MFormulaManager.makeVariable("x");
		Variable y = MFormulaManager.makeVariable("y");
		vtestset.put(x, y);

		RelationAndTermReplacementV v = new RelationAndTermReplacementV(rtestset, vtestset, null);

		runUnitTest("1", v, R.no(), P.no());

		Formula oldf = R.intersection(Q).union(P).some();
		oldf.accept(v);
		if (!oldf.toString().equals(
				R.intersection(Q).union(P).some().toString()))
			MEnvironment.writeErrLine("Error in RelationReplacementV test case: visitor is overwriting old reference.");

		runUnitTest("2", v, x.eq(y), y.eq(y));

		oldf = x.eq(y);
		oldf.accept(v);
		if (!oldf.toString().equals("(x = y)"))
			MEnvironment.writeErrLine("Error in VariableReplacementV test case: visitor is overwriting old reference.");

		
		///////////////////
		
		Relation c = MFormulaManager.makeRelation("c", 1);
		@SuppressWarnings("unused")
		Relation d = MFormulaManager.makeRelation("d", 1);		
		Relation Connected = MFormulaManager.makeRelation("Connected", 2);

		////////////////////////
		// Pre-existing constants -- ok?		
		Formula testConstants = MFormulaManager.makeAtom(c, R);		
		vtestset.clear();
		rtestset.clear();
		vtestset.put(x, c);
		runUnitTest("3", new RelationAndTermReplacementV(rtestset, vtestset, null), testConstants, MFormulaManager.makeAtom(c, R));
		
		////////////////////////
		// Corner case: auto-dropping of un-necessary conjuncts
		Formula testConstants2 = MFormulaManager.makeAnd(MFormulaManager.makeAtom(x, R),
				                                         MFormulaManager.makeAtom(c, R));		
		// This is correct because the subs. result would be (x in C) and (x in C) == (x in C)
		runUnitTest("4", new RelationAndTermReplacementV(rtestset, vtestset, null), testConstants2, MFormulaManager.makeAtom(c, R));
		
		////////////////////////
		// Test with binary (trigger MFormulaManager.substituteVarTuple)
		List<Expression> tuple1 = new ArrayList<Expression>(2);
		tuple1.add(x); tuple1.add(c);
		Formula testConstants3 = MFormulaManager.makeAtom(MFormulaManager.makeExprTupleE(tuple1), Connected);		
		runUnitTest("5", new RelationAndTermReplacementV(rtestset, vtestset, null), 
				    testConstants3, 
				    MFormulaManager.makeAtom(c.product(c), Connected));
		
		////////////////////////		
		// Test with FUNCTIONS (which are represented as joins)
		// Remember ORDERING: f(x, y) ~= y.(x.f)
		Relation f = MFormulaManager.makeRelation("f", 2);
		tuple1.clear();
		tuple1.add(x.join(f));
		tuple1.add(y.join(f));
		Formula testConstants4 = MFormulaManager.makeAtom(MFormulaManager.makeExprTupleE(tuple1), Connected);		
		runUnitTest("6", new RelationAndTermReplacementV(rtestset, vtestset, null), 
				    testConstants4, 
				    MFormulaManager.makeAtom(c.join(f).product(y.join(f)), Connected));		
		
		////////////////////////
		// Test equality with functions
		Formula testConstants5 = MFormulaManager.makeEqAtom(x.join(f), c);		
		runUnitTest("7", new RelationAndTermReplacementV(rtestset, vtestset, null), 
				    testConstants5, 
				    MFormulaManager.makeEqAtom(c.join(f), c));		
		
		
		MEnvironment.writeErrLine("----- End RelationReplacementV Tests -----");
	}
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
		// Re-create the set because we may get an immutable singleton back, and we remove from it below.
		Set<Variable> tempset = new HashSet<Variable>(qf.formula().accept(this));

		// These variables are quantified in this scope.
		// (Don't worry about re-quantification later, since Kodkod won't run
		// vs. such a formula.)
		for (Decl d : qf.decls())
		{
			
			tempset.remove(d.variable());
		}

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


class MIDBReplacementV extends AbstractCacheAllReplacer
{
	// Visitor will find ComparisonFormula, need to extract ... what?								
	// soln: voc.getInOrderTerms(expr)
	
	Expression rhsTarget;
	List<Variable> varVector;
	Formula idbFormula;
	
	MIDBReplacementV(Expression rhsTarget, List<Variable> varVector, Formula idbFormula, Map<Expression, MTerm> theMap)
	{		
		super(new HashSet<Node>());
		
		this.termMap = theMap;
		
		this.rhsTarget = rhsTarget;
		this.varVector = varVector;
		this.idbFormula = idbFormula;		
	}
	
	public Formula visit(ComparisonFormula comp) 
	{
		if (cache.containsKey(comp))
			return lookup(comp);

		Expression rhs = comp.right();
		if(!rhs.equals(rhsTarget))
		{
			cached.add(comp);
			return cache(comp, comp);
		}
		
		// This is an IDB reference that we want to replace.		
		Expression lhs = comp.left();
				
		List<Expression> toExprOrdering = MVocab.getInOrderTerms(lhs); 
		
		if(toExprOrdering.size() != varVector.size())
		{
			// error!
			throw new MGEArityMismatch("MIDBReplacementV given varVector: "+varVector+" whose arity did not match LHS breakdown: "+toExprOrdering+". LHS was: "+lhs);
		}
				
		// Substitute each var from varVector in idbFormula with the
		// matching expr in toExprOrdering, then return the result.
		
		HashMap<Variable, Expression> toReplace = new HashMap<Variable, Expression>();
		
		int ii = 0;
		MEnvironment.writeToLog("\nMIDBReplacementV replacing occurrence of relation "+rhsTarget+" for terms "+toExprOrdering);
		for(Variable oldv : varVector)		
		{
			Expression newterm = toExprOrdering.get(ii);
			toReplace.put(oldv, newterm);					
			ii ++;	
		}

		Formula newComp = idbFormula.accept(new RelationAndTermReplacementV(new HashMap<Relation, Relation>(), toReplace, termMap));	
		
		cached.add(newComp);
		return cache(comp, newComp);
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
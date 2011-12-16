/*
Copyright (c) 2009-2010 Brown University and Worcester Polytechnic Institute.

This file is part of Margrave.

Margrave is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Margrave is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License
along with Margrave.  If not, see <http://www.gnu.org/licenses/>.
*/ 

// Constructor takes a Kodkod Formula object and a specification
// of the sort hierarchy. See below for expectations about
// the input.

// Some optimization has been done: see SigFunction.noCondition,
// AbstractCacheAllCollector, etc.

package edu.wpi.margrave;

import kodkod.ast.*;
import kodkod.ast.operator.ExprCompOperator;
import kodkod.ast.operator.Multiplicity;
import kodkod.ast.operator.Quantifier;
import kodkod.ast.visitor.*;

//import java.lang.management.ManagementFactory;
//import java.lang.management.ThreadMXBean;
import java.util.*;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.math.BigInteger;


// -------- Helper class: SigFunction ----------
// Represents a function: EITHER a Skolem function or an original function.
	
class SigFunction
{
	String name;		
	List<LeafExpression> arity = new ArrayList<LeafExpression>();
	LeafExpression sort;
	
	// Does this function in fact have no conditions?
	// e.g. "\exists x^A ." 
	// Used to optimize counting: If another function of the
	// same arity and sort exists, no need to consider this one.
	boolean noCondition = false;	
	
	// Was this function induced by a sort symbol appearing as a
	// predicate? (e.g., \forall x^A B(x) where A and B are sorts,
	// is interpreted by this visitor as \forall x^A \exists y^B (x=y).)
	boolean fromSortAsPredicate = false; 	
	// For SAP B(x), this will be the variable x.
	Variable variableCause = null;
	Expression theCause = null;
	// if x is eventually existentially quantified, will be the function 
	// induced by that quantifier.
	SigFunction funcCause = null;
	
	// A LOCAL SAP is one where the variable is existentially quantified.
	// A GLOBAL SAP is one where the variable is universally quantified.
	
	public String getID()
	{
		// ID is the descriptor plus the object's hashcode.			
		return name + "_" +hashCode();
	}
	
	public String toString()
	{
		if(arity.size() < 1)
			return getID() + ": " + sort;
		return getID() + arity  + ": "+ sort; 
	}
	
	public String toPrettyString()
	{		
		String result;		
		if(arity.size() < 1)
			result = name + ": " + sort;
		else
			result = name +": " +arity + " -> "+ sort;
		
		if(fromSortAsPredicate)
		{
			if(funcCause == null)
				result += " [s.a.p. GLOBAL]";
			else
				result += " [s.a.p. LOCAL: " + funcCause.name + "]";
		}
		
		return result;
	}
	
	public SigFunction safeClone(String addToID)
	{
		SigFunction result = new SigFunction(this.name+addToID, this.sort, this.noCondition, this.fromSortAsPredicate, this.variableCause);
		result.arity = new ArrayList<LeafExpression>(this.arity); 
		result.funcCause = this.funcCause;
		result.theCause = this.theCause;		
		
		MCommunicator.writeToLog("\nCloning a pre-existing SigFunction: "+name);
		
		return result;
	}
	
	SigFunction(String n, LeafExpression r, boolean noCondition, boolean fromSortAsPredicate, Variable variableCause)
	{
		this.name = n;
		this.arity = new ArrayList<LeafExpression>();
		this.sort = r;
		this.noCondition = noCondition;
		this.fromSortAsPredicate = fromSortAsPredicate;
		this.variableCause = variableCause; 
		MCommunicator.writeToLog("\nCreating new SigFunction: name="+n+", leafexpr="+r+", nocond="+noCondition+
				", fromsap="+fromSortAsPredicate+", variableCause="+variableCause);		
	}
	
}




public class FormulaSigInfo
{
	// -------- Helper: FuncClause -----------
	// Used in unproductive function removal
	class FuncClause
	{
		SigFunction theFunction;
		Set<LeafExpression> needed;
		LeafExpression result;
		
		FuncClause(SigFunction theFunction)
		{
			this.theFunction = theFunction;
			needed = new HashSet<LeafExpression>(theFunction.arity);
			result = theFunction.sort;
		}
		
		FuncClause(LeafExpression arity, LeafExpression sort)
		{
			// Used for inclusion functions
			this.theFunction = null;
			needed = new HashSet<LeafExpression>();
			needed.add(arity);
			result = sort;			
		}
		
		public String toString()
		{
			return "<<<Clause: Result="+result+"; needed="+needed+" for function:"+theFunction+">>>";
		}
	}
	
		
	// -------- Helper class: WalkAST ----------
	// Visitor pattern: Walks the Formula's AST
	// finding Skolem functions. Caches as much as possible. 
	
	class WalkAST extends AbstractCollector<SigFunction>
	{
		// Don't keep track of scope, so that we can cache functions.

		// A unique function object is a unique function. Make sure to clone and avoid
		// potential duplicates when the formula branches.
		
		// Note that we don't need to collect quantifiers under negations, 
		// because the formula is assumed to be in NNF.
		
		String error_condition;
		boolean error;
						
		public Set<SigFunction> newSet()
		{
			return new HashSet<SigFunction>();
		}
		
		public WalkAST()
		{		
			super(new HashSet<Node>());
			error_condition = "";
			error = false;
		}
			
		public Set<SigFunction> visit(NotFormula nf)	
		{			
			if(cache.containsKey(nf))
				return lookup(nf);
			
			cached.add(nf); // will cache a result below
			
			Set<SigFunction>  t;
			
			// Negative instance of a multiplicity?
			if(nf.formula() instanceof MultiplicityFormula)
			{
				// Negative multiplicities
				MultiplicityFormula within = (MultiplicityFormula)nf.formula();
				switch(within.multiplicity())
				{			
				case LONE:
					// 2+ -- needs 2 existentials
					t = handleMultiplicity("!lone1"+within.expression(), within.expression(), false);
					t.addAll(handleMultiplicity("!lone2"+within.expression(), within.expression(), false));
					return cache(nf, t);				
				case ONE:
					// none or 2+ -- needs 2 existentials
					t = handleMultiplicity("!one1"+within.expression(), within.expression(), false);
					t.addAll(handleMultiplicity("!one2"+within.expression(), within.expression(), false));
					return cache(nf, t);							
				case SOME:
					// negative .some is the same as positive .no, and is doable with universals only
					return cache(nf, new HashSet<SigFunction>());
				case NO:
					// same as positive .some; needs one existential.
					return cache(nf, handleMultiplicity("!no"+within.expression(), within.expression(),  true));
				}
				 
			}
			
			// Don't descend, even seeking sorts-as-predicates: negation will cause 
			// the existential they induce to become a safe forall.
			return cache(nf, new HashSet<SigFunction>());
		}
				
		public Set<SigFunction> visit(ComparisonFormula comp)
		{			
			if(cache.containsKey(comp))
				return lookup(comp);
			
			cached.add(comp);
			
			if(ExprCompOperator.SUBSET.equals(comp.op()))
			{
				/////////////////////////////////////////////////////
				// Sorts-as-predicates handling
				// Extract a SAP coercion from a ComparisonFormula
				// e.g. (x in A)
				// or ( f(c) in A )											
				
				if(! (comp.right() instanceof LeafExpression))
				{
					error = true;
					error_condition += "  Unsupported ComparisonFormula: "+comp;				
					return cache(comp, new HashSet<SigFunction>()); // fail
				}
				
				LeafExpression rel = (LeafExpression) comp.right();
								
				// Not a sort? Not a sort-as-pred!
				if(!sorts.contains(rel))
					return cache(comp, new HashSet<SigFunction>());
				
				// sort as predicate: variable
				if(comp.left() instanceof Variable)					
				{					
					if(sap.equals(FormulaSigInfo.EnumSAPHandling.sapIgnore))
						return cache(comp, new HashSet<SigFunction>());
					
					// The caller will decide what to do
					SigFunction newfunc = new SigFunction("SAP_VR_"+comp.toString()+getUniqueSuffix(), rel, false, true, (Variable) comp.left());										
										
					Set<SigFunction> result = new HashSet<SigFunction>();
					result.add(newfunc);									
				
					return cache(comp, result);
				}
				
				// Other kind of Expression. Hopefully we know what sort it has.
				if(!termTypes.containsKey(comp.left()))
				{
					error = true;
					error_condition += "  Did not have a type for expression: "+comp.left()+" used in sort-as-predicate.";				
					return cache(comp, new HashSet<SigFunction>()); // fail
				}
								
				// Now suppose the trigger is a complex term, not a variable. 
				// For instance: (forall x A (forsome y B (C (f x y))))
				// The conservative option is to make a GLOBAL coercion no matter what. 
				
				// Create a func for this SAP.
				LeafExpression theSort = termTypes.get(comp.left());
				// System.err.println(theSort);
				SigFunction newfunc = new SigFunction("SAP_TERMR_"+comp.toString()+getUniqueSuffix(), rel, false, true, null);
				newfunc.theCause = comp.left();
				newfunc.funcCause = null; // global!
				newfunc.arity.add(theSort);
								
				Set<SigFunction> result = new HashSet<SigFunction>();
				result.add(newfunc);  
				return cache(comp, result);
				
				
			}
			
			// otherwise it is an equality comparison, which can't hide any Skolem functions.
			return cache(comp, new HashSet<SigFunction>());
		}
		
		public Set<SigFunction> visit(MultiplicityFormula mf)
		{
			// Positive multiplicities
			
			if(cache.containsKey(mf))
				return lookup(mf);
			
			cached.add(mf); // will cache a result below

			
			switch(mf.multiplicity())
			{
			case LONE:
				// positive .lone() is doable with universals only.
				return cache(mf, new HashSet<SigFunction>());
			case ONE:
				// one existental
				// TFC: "Total function constraint" -- want to support this...
				return cache(mf, handleMultiplicity("one"+mf.expression(), mf.expression(), true));
			case SOME:
				// one existential
				return cache(mf, handleMultiplicity("some"+mf.expression(), mf.expression(),  true));
			case NO:
				// positive .no() is doable with universals only.
				return cache(mf, new HashSet<SigFunction>());
			}
				 
			return cache(mf, new HashSet<SigFunction>());		
		}
		
		
		HashSet<SigFunction> handleMultiplicity(String name, Expression rex, boolean isSing) 
		{
			if(! (rex instanceof LeafExpression))
			{
				error = true;
				error_condition += " handleMultiplicity called with non-LeafExpression: "+rex;
				return new HashSet<SigFunction>();
			}
			LeafExpression r = (LeafExpression)rex;
						
			HashSet<SigFunction> result = new HashSet<SigFunction>();
		
			if(predicates.containsKey(r))
			{
				// Predicate may be bigger than unary.
				for(LeafExpression asort : predicates.get(r))
				{
					SigFunction f = new SigFunction(name+getUniqueSuffix(), asort, isSing, false, null);										
					result.add(f);
				}
					
			}
						
			else if(sorts.contains(r))
			{
				// copy constructor: make sure to use a different list object than what's passed.
				SigFunction f = new SigFunction(name+getUniqueSuffix(), r, isSing, false, null);
				result.add(f);				
			}
			else if(termTypes.containsKey(r))
			{
				// constant, etc.
				// cannot be isSing because c.one() and c2.one() indicate different terms
				LeafExpression theTypeRel = termTypes.get(r);
				SigFunction f = new SigFunction(name+getUniqueSuffix(), theTypeRel, false, false, null);
				result.add(f);
			}
			else
			{
				error = true;
				error_condition += " Multiplicity over non pred, non sort: "+r;
				return result;
			}
			
			return result;
		}
		
		public Set<SigFunction> visit(QuantifiedFormula q)
		{
			if(cache.containsKey(q))
				return lookup(q);
			cached.add(q);
			
			Formula within = q.formula();
			
			// Deal with the quantifier.
			if(q.quantifier().equals(Quantifier.ALL))
			{
				
				// Universal: 
				// See what functions are induced under this universal, and then add this to their params.

				// Walk the AST.
				Set<SigFunction> temp = within.accept(this);

				// We cache results of walking subtrees. Consider
				// (and (forall y (exists x (P x))) (exists x (P x)))
				// If we walk the RHS first, the universal LHS case will 
				// increase the arity of BOTH Skolem functions. Avoid that.
				Set<SigFunction> clonedtemp = new HashSet<SigFunction>();
				for(SigFunction f : temp)
					clonedtemp.add(f.safeClone(""));
				
				for(Decl d : q.decls())
				{
					// If not a LeafExpression, not supported.
					if(! (d.expression() instanceof LeafExpression))
					{
						error = true;
						error_condition += " Decl "+d+" was not over a LeafExpression";
						return new HashSet<SigFunction>();
					}
					
					if(! (Multiplicity.ONE.equals(d.multiplicity())))
					{
						error = true;
						error_condition += " Decl "+d+" did not use the ONE multiplicity. Only singleton variables are supported.";
						return new HashSet<SigFunction>();						
					}
										
														
					for(SigFunction f : clonedtemp)
					{						
						if(!f.fromSortAsPredicate)
							f.arity.add((LeafExpression) d.expression());
						else
						{
							// Special handling for SAP functions. 
							// GLOBAL coercions: Only collect the first universal.
							// LOCAL: treat as normal
							
							// That seems wrong. SAPs should always be unary.
							
							// This is a GLOBAL coercion on this variable.
							if(f.funcCause == null && d.variable().equals(f.variableCause))
								f.arity.add((LeafExpression) d.expression());
														
							//if(f.arity.size() == 0) // always collect first universal
							//	f.arity.add((LeafExpression) d.expression());
							//else if(f.funcCause != null) // local
							//	f.arity.add((LeafExpression) d.expression());
						}
					}
						
				}
				
							
				return cache(q, clonedtemp);
			}
			else
			{
				// Existential:
						
				Set<SigFunction> innerfuncs = within.accept(this);
				
				HashSet<SigFunction> thesefuncs = new HashSet<SigFunction>();
				for(Decl d : q.decls())
				{
					// If not a LeafExpression, not supported.
					if(! (d.expression() instanceof LeafExpression))
					{
						error = true;
						error_condition += " Decl "+d+" was not over a LeafExpression";
						return new HashSet<SigFunction>();
					}
					
					if(!Expression.UNIV.equals(d.expression()) && ! (d.expression() instanceof Relation))
					{
						error = true;
						error_condition += " Decl "+d+" was over a LeafExpression that was not a Relation or UNIV.";
						return new HashSet<SigFunction>();
					}					
					
					if(! (Multiplicity.ONE.equals(d.multiplicity())))
					{
						error = true;
						error_condition += " Decl "+d+" did not use the ONE multiplicity. Only singleton variables are supported.";
						return new HashSet<SigFunction>();						
					}
					
					// New induced Skolem function! (At this point, it's a constant; 
					//  arity will be added later as we pass back up through the scope of universals, if any.)
					// OPT Note: assumes NEVER a sap if the existential is explicit. Future optimization possible, here.
					SigFunction f = new SigFunction(d.variable().toString()+getUniqueSuffix(), 
							(LeafExpression)d.expression(), false, false, d.variable());
					thesefuncs.add(f);
					
					// SAP handling: Is this quantified variable the "cause" of a SAP function?
					// e.g., the x in B(x)
					for(SigFunction sap : innerfuncs )
					{
						if(!sap.fromSortAsPredicate)
							continue;
						
						// This must be a LOCAL SAP, because its trigger variable was this
						// existential. Only take the innermost one! Don't overwrite if
						// the variable is originally used higher up.
						if(sap.funcCause == null && d.variable().equals(sap.variableCause))
						{
							sap.funcCause = f;							
							sap.arity.add((LeafExpression) d.expression());
						}
					}
					
					
				}
				
				// (Don't trust returned set to be writable.)
				Set<SigFunction> result = new HashSet<SigFunction>();
				result.addAll(innerfuncs);
				result.addAll(thesefuncs);
				return cache(q, result);
			}				
		}
		
		public Set<SigFunction> visit(NaryFormula naryf)
		{
			// See visit(BinaryFormula) below for reasoning
			if(cache.containsKey(naryf))
				return lookup(naryf);
			cached.add(naryf);
			
			Set<SigFunction> result = new HashSet<SigFunction>();
			Set<SigFunction> intersection = new HashSet<SigFunction>();
			
			// Add each sub-formula's FuncStructs, but check for duplication.
			// If there are any duplicates, be sure to double-count!
			for(Formula child : naryf)
			{
				Set<SigFunction> newfuncs = child.accept(this);
				
				// Rolling check for duplicates
				intersection.clear();
				intersection.addAll(result);
				intersection.retainAll(newfuncs);
				if(newfuncs.size() > 0 && intersection.size() > 0)
				{																	
					for(SigFunction dopple : intersection)
						if(!dopple.fromSortAsPredicate) // SAP is the identity
							result.add(dopple.safeClone("+"));
				}
				
				result.addAll(newfuncs);
				
			}
			
			
			return result;
		}
		
		
		public Set<SigFunction> visit(BinaryFormula bf)
		{
			// There are multiple branches here. Kodkod will do its best to
			// duplicate formula references, and we're using a cache in
			// this visitor. That means that we could end up having
			// the left and right branch pointing to the "same"
			// existential QuantifiedFormula object, but we need them 
			// induce separate functions. Hence the "safe cloning" below. 
								
			// But note: it's vital that we take differences in arity seriously!

			
			if(cache.containsKey(bf))
				return lookup(bf);
			cached.add(bf);

			Set<SigFunction> lfs = bf.left().accept(this);
			Set<SigFunction> rfs = bf.right().accept(this);
			
			Set<SigFunction> result = new HashSet<SigFunction>(lfs);
			result.addAll(rfs);
			
			// Do we have any references to the same FuncStruct in both sets?
			if(rfs.size() > 0 && lfs.size() > 0) // prevent exception
			{
				// (lfs, rfs may be singleton, need another mutable set to do this)
				Set<SigFunction> overlaps = new HashSet<SigFunction>(lfs);
				overlaps.retainAll(rfs);
								
				for(SigFunction dupe : overlaps)			
				{
					if(!dupe.fromSortAsPredicate) // SAP is the identity
						result.add(dupe.safeClone("+"));
				}
			}
			
			return result;
		}		
	}

	
	
	
	
	
	
	
	
	
	
	// ------------- Sig definition ----------------
	
	// The Formula object that this object is created to analyze.
	// The formula is assumed to be in negation normal form!
	private Formula fmla;
	
	// predicate symbols (non-sort LeafExpressions)
	// Key is the predicate LeafExpression
	// Value is the vector of sorts describing the type of the predicate
	// (e.g., R \in A \times B)
	private Map<LeafExpression, List<LeafExpression>> predicates;
	
	// Set of LeafExpressions treated as sorts
	private Set<LeafExpression> sorts;
	
	// mapping from sorts to the set of all their supersorts.
	// this mapping is **assumed** to be
	// - A partial order
	// - transitively closed
	// - contain entries for each LeafExpression in this.sorts.	
	private Map<LeafExpression, Set<LeafExpression>> supersorts;
		
	// These are in the pre-Skolem signature.
	// Algorithmics will consider both these and the Skolem functions together.
	private Set<SigFunction> originalFunctions;
	private Set<SigFunction> originalConstants;
	
	private Map<LeafExpression, Set<LeafExpression>> disjointConstraints;
	
	static public boolean enableDebug = false; 
	static public boolean enableDisjointness = true;
	
	private Map<Expression, LeafExpression> termTypes = new HashMap<Expression, LeafExpression>();
	
	//  ------------- Calculated fields  ----------------
	
	// transitive closure of blue edges: calculated in cycle check
	private Map<LeafExpression, Set<LeafExpression>> supersAndCoercionsFromTC =
		new HashMap<LeafExpression, Set<LeafExpression>>();
	
	// Reverse of supersorts: calculated in constructor
	private Map<LeafExpression, Set<LeafExpression>> subsorts =
		new HashMap<LeafExpression, Set<LeafExpression>>();
	
	// Set of all Skolem functions within fmla
	private Set<SigFunction> skolemFunctions = 
		new HashSet<SigFunction>();
	// Set of all Skolem constants within fmla
	private Set<SigFunction> skolemConstants = 
		new HashSet<SigFunction>();
	
	// Set of all Skolem SAP functions within fmla
	private Set<SigFunction> sapFunctions = 
		new HashSet<SigFunction>();		
	
	// Set of all the Skolem functions which can be used to build
	// terms over the Skolem signature (not SAP, though)
	private Set<SigFunction> productiveFunctions = 
		new HashSet<SigFunction>();
	private Set<SigFunction> productiveSAPFunctions = 
		new HashSet<SigFunction>();
	
	
	// Set of sorts which contain finitely many terms 
	private Set<LeafExpression> finitarySorts = new HashSet<LeafExpression>();
	
	// mapping from finitary sorts to the number of terms in them 
	private Map<LeafExpression, BigInteger> termCounts =
		new HashMap<LeafExpression, BigInteger>();
	
	private BigInteger totalTerms = BigInteger.ZERO;
	
	private int uniqueFuncSuffix = 0;
	
	String getUniqueSuffix()
	{
		return "_"+String.valueOf(uniqueFuncSuffix++);			
	
	}
	
	// ------------- Constructor and helpers ----------------
		
	ThreadMXBean mxBean = ManagementFactory.getThreadMXBean();
	long msCollect = 0;
	long msProductive = 0;
	long msFinitary = 0;
	long msBounds = 0;

	//QuotaService qs = QuotaServiceFactory.getQuotaService();
    
	/**
	 * sapThrowException: Throws an exception if SAP is encountered.
	 * sapIgnore: Ignores SAP functions entirely, discarding them before counting. 
	 */
	public enum EnumSAPHandling
	{
	   sapKeep, sapThrowException, sapIgnore 
	}
	
	EnumSAPHandling sap;
	boolean htmlOutput; 
	
	// ASSUMPTIONS:
	// (1) fmla is in NNF
	// (2) supersorts is a poset (no cycles, transitive, etc.)
	FormulaSigInfo(Set<LeafExpression> sorts, 
			Map<LeafExpression, Set<LeafExpression>> supersorts,
			Map<LeafExpression, List<LeafExpression>> predicates, 
			Set<SigFunction> originalFunctions,
			Set<SigFunction> originalConstants,
			Formula fmla,
			EnumSAPHandling sap,
			Map<Expression, LeafExpression> termTypes,
			boolean htmlOutput)
				throws MUnsupportedFormulaException, MNotASortException
	{
		init(sorts, supersorts, predicates, originalFunctions, originalConstants,
				fmla, sap, htmlOutput, new HashMap<LeafExpression, Set<LeafExpression>>(), termTypes);
	}
	
	FormulaSigInfo()
	{
		// ERROR CASE
	}
	
	FormulaSigInfo(Set<LeafExpression> sorts, 
			Map<LeafExpression, Set<LeafExpression>> supersorts,
			Map<LeafExpression, List<LeafExpression>> predicates, 
			Set<SigFunction> originalFunctions,
			Set<SigFunction> originalConstants,
			Formula fmla,
			EnumSAPHandling sap,
			Map<Expression, LeafExpression> termTypes)
	throws MUnsupportedFormulaException, MNotASortException
	{
		init(sorts, supersorts, predicates, originalFunctions, originalConstants,
				fmla, sap, false, new HashMap<LeafExpression, Set<LeafExpression>>(), termTypes);
	}
	
	FormulaSigInfo(Set<LeafExpression> sorts, 
			Map<LeafExpression, Set<LeafExpression>> supersorts,
			Map<LeafExpression, List<LeafExpression>> predicates, 
			Set<SigFunction> originalFunctions,
			Set<SigFunction> originalConstants,
			Formula fmla,
			EnumSAPHandling sap,
			Map<LeafExpression, Set<LeafExpression>> disjointConstraints,
			Map<Expression, LeafExpression> termTypes)
			throws MUserException
	{
		init(sorts, supersorts, predicates, originalFunctions, originalConstants, 
				fmla, sap, false, disjointConstraints, termTypes);
	}
	
	private void handleUNIV()
	{	
		// Maximal sort is maximal
		sorts.add((LeafExpression)Expression.UNIV);
		
		for(LeafExpression s : supersorts.keySet())
			supersorts.get(s).add((LeafExpression)Expression.UNIV);
		
	}
	
	private void init(Set<LeafExpression> sorts, 
			Map<LeafExpression, Set<LeafExpression>> supersorts,
			Map<LeafExpression, List<LeafExpression>> predicates, 
			Set<SigFunction> originalFunctions,
			Set<SigFunction> originalConstants,
			Formula fmla,
			EnumSAPHandling sap,
			boolean htmlOutput,
			Map<LeafExpression, Set<LeafExpression>> disjointConstraints,
			Map<Expression, LeafExpression> termTypes)
	throws MUnsupportedFormulaException, MNotASortException
	{
		// Fix a set of Sorts, a partial order on them, and a Formula.
		this.fmla = fmla;
		this.supersorts = supersorts;
		this.sorts = sorts;
		this.predicates = predicates;
		
		this.sap = sap;
		this.htmlOutput = htmlOutput;
		
		this.termTypes = termTypes;
		
		// pre-Skolem functions
		this.originalConstants = originalConstants;
		this.originalFunctions = originalFunctions;
		
		this.disjointConstraints = disjointConstraints;
		
		// Add Expression.UNIV as the ultimate supersort.
		handleUNIV();
		
		// If the caller passed a reflexive LeafExpression in supersorts, remove the self-reference.
		for(LeafExpression r : supersorts.keySet())		
			supersorts.get(r).remove(r);				
		
		// populate supersAndCoercionsFromTC
		for(LeafExpression s : sorts)
			supersAndCoercionsFromTC.put(s, new HashSet<LeafExpression>());
		
		
		// Populate subsorts map 
		// (This is used repeatedly by DFS in findProductiveFunctions; calculate ONCE.)
		for(LeafExpression parent : sorts)
		{
			subsorts.put(parent, new HashSet<LeafExpression>());
			
			// correct potential lack in supersorts
			if(!supersorts.containsKey(parent))
				supersorts.put(parent, new HashSet<LeafExpression>());
			
			// Subsorts for this parent
			for(LeafExpression sub : supersorts.keySet())
				if(supersorts.get(sub).contains(parent))
					subsorts.get(parent).add(sub);
		}		
		
		MCommunicator.writeToLog("\nInitializing FormulaSigInfo. #sorts="+sorts.size());
		
		//long startMegacycles;
		long startTime;
		
		// Parse fmla looking for Skolem functions.				
		startTime = mxBean.getCurrentThreadCpuTime();
		//startMegacycles = qs.getCpuTimeInMegaCycles();
		collectSkolemFunctions();
		msCollect = (mxBean.getCurrentThreadCpuTime() - startTime) / 1000000;
		//mCycCollect = (qs.getCpuTimeInMegaCycles() - startMegacycles);
		
		// Make sure the formula and functions given are well-formed w/r/t the sorts given
		for(SigFunction f : skolemFunctions)
			validateFunction(f, "in the formula");
		for(SigFunction c : skolemConstants)
			validateFunction(c, "in the formula");
		for(SigFunction f : originalFunctions)
			validateFunction(f, "in the original signature");
		for(SigFunction c : originalConstants)
			validateFunction(c, "in the original signature");							
		
		// Discover and cache the set of functions which are productive.
		startTime = mxBean.getCurrentThreadCpuTime();
		//startMegacycles = qs.getCpuTimeInMegaCycles();
		findProductiveFunctions();
		msProductive = (mxBean.getCurrentThreadCpuTime() - startTime) / 1000000;
		//mCycProductive = (qs.getCpuTimeInMegaCycles() - startMegacycles);		
		
		// Do a cycle check to find finitary sorts
		startTime = mxBean.getCurrentThreadCpuTime();
		//startMegacycles = qs.getCpuTimeInMegaCycles();
		
		//findFinitarySorts();
		//findFinitarySortsNew();
		
		// !!!!!!!!!!! 
		// IF CHANGING BACK
		// IMPORTANT: need to make findFinitarySortsNew calculate blue TC and populate
		// supersAndCoercionsFromTC. NewDisj already does it.
		
		findFinitarySortsNewDisj();
				
		msFinitary = (mxBean.getCurrentThreadCpuTime() - startTime) / 1000000;
		//mCycFinitary = (qs.getCpuTimeInMegaCycles() - startMegacycles);
		
		// Finally, calculate bounds for finitary sorts
		startTime = mxBean.getCurrentThreadCpuTime();
		//startMegacycles = qs.getCpuTimeInMegaCycles();
		calculateBounds();		
		msBounds = (mxBean.getCurrentThreadCpuTime() - startTime) / 1000000;	
		//mCycBounds = (qs.getCpuTimeInMegaCycles() - startMegacycles);
		
		MCommunicator.writeToLog("\nFinished initializing FormulaSigInfo. #sorts="+sorts.size());
	}
	
	public String toString()
	{
		StringBuffer result = new StringBuffer();
		result.append("FormulaSigInfo:\n");
		result.append("Sorts: "+sorts+"\n");
		result.append("Predicates: "+predicates+"\n");
		result.append("SAP Functions: "+getSAPFunctions()+"\n");
		result.append("Skolem Functions: "+getSkolemFunctions()+"\n");
		result.append("Skolem Constants: "+getSkolemConstants()+"\n");
		return result.toString();
	}
	
	private void validateFunction(SigFunction f, String appearedAs)
	throws MNotASortException
	{
		for(LeafExpression inArity : f.arity)
			if(!sorts.contains(inArity))
				throw new MNotASortException("The LeafExpression "+inArity.toString() + 
						" appeared as a sort symbol "+appearedAs+", but was not declared to be a sort.");
		
		if(!sorts.contains(f.sort))
			throw new MNotASortException("The LeafExpression "+f.sort.toString() + 
				" appeared as a sort symbol "+appearedAs+", but was not declared to be a sort.");		
	}
	
	private void collectSkolemFunctions()
	throws MUnsupportedFormulaException
	{
		WalkAST walker = new WalkAST();
		Set<SigFunction> results = fmla.accept(walker);
		if(walker.error)
		{
			MEnvironment.writeErrLine(walker.error_condition);
			throw new MUnsupportedFormulaException(walker.error_condition);
		}
		
		// **************************
		// Which of these were SAP?
		// **************************
		
		for(SigFunction f : results)
			if(f.fromSortAsPredicate)
				sapFunctions.add(f);
					
		results.removeAll(sapFunctions);
		
		if(sap.equals(EnumSAPHandling.sapThrowException))
		{
			// sapThrowException
			throw new MUnsupportedFormulaException("Sort symbol was used as predicate and handler was set to disallow this.");
		}
		else if(sap.equals(EnumSAPHandling.sapIgnore))
		{
			sapFunctions.clear();
		}
		// Otherwise, keep and deal with in term counting
				
		skolemFunctions.clear();
		skolemConstants.clear();
		for(SigFunction f : results)
		{
			if(f.arity.size() == 0)
				skolemConstants.add(f);
			else
				skolemFunctions.add(f);
		}
		
		//MCommunicator.writeToLog("\nFormulaSigInfo: Gathered Skolem and SAP functions:");
		//MCommunicator.writeToLog("\nSkolem Functions"+skolemFunctions);
		//MCommunicator.writeToLog("\nSkolem Constants"+skolemConstants);
		//MCommunicator.writeToLog("\nSAP Functions"+sapFunctions);
		
		// Optimization:
		// Remove "no condition" constants covered by others.
		// (e.g., suppose "exists x^A and some A" -- don't need
		// to include the "some A" constant.)
		Set<SigFunction> toRemove = new HashSet<SigFunction>();
		for(SigFunction c : skolemConstants)
		{
			if(c.noCondition)
			{
				// All constants (not just Skolem)
				Set<SigFunction> allConstants = new HashSet<SigFunction>(originalConstants);
				allConstants.addAll(skolemConstants);
				
				for(SigFunction otherc : allConstants)
				{
					// Found a different constant of same (or super-) sort
					// which is not currently flagged for removal by this loop
					
					if(!supersorts.containsKey(c.sort))
						throw new MNotASortException(c.sort.name());
					if(!supersorts.containsKey(otherc.sort))
						throw new MNotASortException(otherc.sort.name());

					
					if(c != otherc && otherc.arity.size() == 0 && !toRemove.contains(otherc) &&
							(c.sort == otherc.sort || supersorts.get(otherc.sort).contains(c.sort)))
					{
						toRemove.add(c);
						break; // next outer loop iteration
					}
				}
				
			}			
		}
		skolemConstants.removeAll(toRemove); // don't keep superfluous "some" constants
	}
	
	private void findProductiveFunctions()
	{	
		productiveFunctions.clear();

		// Start with an ordering on all the functions, represented as clauses
		Set<FuncClause> funcList = new HashSet<FuncClause>();
		for(SigFunction f : skolemFunctions)
			funcList.add(new FuncClause(f));	
		for(SigFunction f : originalFunctions)
			funcList.add(new FuncClause(f));	
		for(SigFunction f : sapFunctions)
			funcList.add(new FuncClause(f));	
				
		// For simplicity in this algorithm, treat the sort ordering as a set of inclusion functions.		
		for(LeafExpression sub : supersorts.keySet())
			for(LeafExpression sup : supersorts.get(sub))
				funcList.add(new FuncClause(sub, sup));
		
		if(enableDebug)
			MEnvironment.writeErrLine("Clause list: \n"+funcList);
				
		// and an ordering on the sorts of constant symbols (to start)
		Queue<LeafExpression> units = new LinkedList<LeafExpression>();
		for(SigFunction c : skolemConstants)
			if(!units.contains(c.sort))
				units.add(c.sort);
		for(SigFunction c : originalConstants)
			if(!units.contains(c.sort))
				units.add(c.sort);
		
		// for each sort, keep a list of clauses in whose arity it appears
		Map<LeafExpression, Set<FuncClause>> sortsToFuncs = new HashMap<LeafExpression, Set<FuncClause>>();
		for(FuncClause fc : funcList)
		{
			for(LeafExpression anAritySort : fc.needed)
			{
				if(!sortsToFuncs.containsKey(anAritySort))
					sortsToFuncs.put(anAritySort, new HashSet<FuncClause>());
				sortsToFuncs.get(anAritySort).add(fc);				
			}					
		}
		
		// propagate units until done
		Set<LeafExpression> alreadyPropagated = new HashSet<LeafExpression>();
		while(units.size() > 0)			
		{
			LeafExpression rel = units.remove();
			if(alreadyPropagated.contains(rel))
				continue;			
			alreadyPropagated.add(rel);
			
			// OPT: This LeafExpression (rel) is _populated_ (not term-free). useful info.
			
			// what func clauses require rel to fire?
			if(sortsToFuncs.containsKey(rel))
			{				
				for(FuncClause fc : sortsToFuncs.get(rel))
				{
					fc.needed.remove(rel); // this part of f's arity is populated
					
					if(fc.needed.size() == 0) // f's arities are all populated; f can fire 
					{
						if(fc.theFunction != null) // not an inclusion
							productiveFunctions.add(fc.theFunction);						
						units.add(fc.result);
					}
				}
			}
		}
		
		// Split productiveFunctions and productiveSAPFunctions.
		productiveSAPFunctions.clear();
		for(SigFunction f : productiveFunctions)
			if(f.fromSortAsPredicate)
				productiveSAPFunctions.add(f);
		
		productiveFunctions.removeAll(productiveSAPFunctions);
					
	}
	
	class MLeafExpressionComparator implements Comparator<LeafExpression>
	{
		public int compare(LeafExpression leaf1, LeafExpression leaf2)
		{			
			return leaf1.toString().compareTo(leaf2.toString());				
		}
	}
	
	class MSigFunctionComparator implements Comparator<SigFunction>
	{
		public int compare(SigFunction f1, SigFunction f2)
		{			
			return f1.toPrettyString().compareTo(f2.toPrettyString());				
		}
	}
			
	private void printBooleanMatrix(boolean[][] connM, int max)
	{
		long theHash = 0;
		int theBit = 0;
		for(int row=0;row<max;row++)
		{
			MEnvironment.writeErr(row+": ");
			for(int col=0;col<max;col++)
			{
				if(connM[row][col])
					theHash += Math.pow(2, theBit);
				theBit++;
				
				if(col % 10 == 0)
					MEnvironment.writeErr("/");
				
				if(connM[row][col])
					MEnvironment.writeErr("1");
				else
					MEnvironment.writeErr("0");
			}
			MEnvironment.writeErrLine("");
			
		}	
		MCommunicator.writeToLog("\n    MATRIX HASH: "+theHash);
	}
	
	private void findFinitarySortsNew()
	{
		// Build the reachability matrix for the sort-graph via Warshall.
		// Then we can just check for each red edge (x, y) whether (y,x) 
		// is reachable. If so, there is a red cycle!
		// Anything reachable from y (or x) is infinitary.
		
		int max = sorts.size();		
		boolean[][] connM = new boolean[max][max];
		
		// init to all falses
		for(int ii=0;ii<max;ii++)
			for(int jj=0;jj<max;jj++)			
				connM[ii][jj] = false;						
		
		// Fix an ordering of the sorts
		// Make the ordering deterministic (by alphabetic order)
		List<LeafExpression> sortedSorts = new ArrayList<LeafExpression>(sorts);
		Collections.sort(sortedSorts, new MLeafExpressionComparator());
		
		Map<LeafExpression, Integer> sortToInt = new HashMap<LeafExpression, Integer>();
		Map<Integer, LeafExpression> intToSort = new HashMap<Integer, LeafExpression>();		
		int iIndex = 0;
		for(LeafExpression s : sortedSorts)
		//for(LeafExpression s : sorts)
		{
			sortToInt.put(s, iIndex);
			intToSort.put(iIndex, s);
			if(enableDebug)
				MEnvironment.writeErrLine("SORT: "+s+" idx: "+iIndex);			
			
			// everything finitary by default, remove below
			finitarySorts.add(s);
			
			iIndex++;
		}
		
		// f, g, etc.
		for(SigFunction f : productiveFunctions)	
		{
			for(LeafExpression arg : f.arity)
			{
				if(enableDebug)
					MEnvironment.writeErrLine("FUNC: "+f.sort+" -> "+arg);
				int src = sortToInt.get(f.sort);
				int dest = sortToInt.get(arg);
				connM[src][dest] = true;
			}
		}
		
		// Subsorts
		for(LeafExpression curr : sorts)
		{
			for(LeafExpression sub : subsorts.get(curr))
			{
				if(enableDebug)
					MEnvironment.writeErrLine("SUBSORT: "+curr+" > "+sub);
				int src = sortToInt.get(curr);
				int dest = sortToInt.get(sub);
				connM[src][dest] = true;			
			}			
		}
				
		// coercions due to SAP functions (global and local)
		for(SigFunction f : productiveSAPFunctions)
		{
			for(LeafExpression arg : f.arity)
			{
				if(enableDebug)
					MEnvironment.writeErrLine("SAP: "+f.sort+" -> "+arg);
				int src = sortToInt.get(f.sort);
				int dest = sortToInt.get(arg);
				connM[src][dest] = true;
			}				
		}					
		
		if(enableDebug)
			printBooleanMatrix(connM, max);
		
		// Floyd-Warshall
		// Extend from ii to jj via kk
		for(int kk=0;kk<max;kk++)
		{
			for(int ii=0;ii<max;ii++)
			{
				for(int jj=0;jj<max;jj++)
				{
					connM[ii][jj] = connM[ii][jj] || (connM[ii][kk] && connM[kk][jj]);						
				}				
			}
		}
		// done with building connectivity matrix				
			
		if(enableDebug)
			printBooleanMatrix(connM, max);
		
		for(SigFunction f : productiveFunctions)	
		{
			for(LeafExpression arg : f.arity)
			{
				int src = sortToInt.get(f.sort);
				int dest = sortToInt.get(arg);
				
				// Does this shadow-function sit on a cycle? (path from dest to src)
				if(connM[dest][src])
				{
					//MCommunicator.writeToLog("\nTAINTED CYCLE: Function "+f+"("+src+","+dest+").");
					
					// tainted cycle! Everything reachable from dest is infinitary.
					for(int option=0;option<max;option++)
					{
						if(connM[option][dest])
						{							
							LeafExpression infSort = intToSort.get(option);							
							finitarySorts.remove(infSort);
						}
					}
				}
				//else
					//MCommunicator.writeToLog("\nFunction "+f+"("+src+","+dest+") did not have a path from dest to src.");
				
			}
		}				
	}
	
	enum SortEdge { 		
		BLUE (1), 
		RED (2), 
		REDBLUE (4), 
		BLUERED (8);
		
		// TODO: may need blueredblue?
		
		private final int bitvalue;
		
		SortEdge(int bitvalue)
		{
			this.bitvalue = bitvalue;
		}
		
		boolean isIn(int arg)
		{
			// Is the correct bit flipped?
			return (arg & bitvalue) != 0;
		}
	};
	
	private boolean oneDirectionDisjCheck(Set<LeafExpression> superA, Set<LeafExpression> superB)
	{					
		for (LeafExpression s1 : superA)
		{
			Set<LeafExpression> disjWith1 = disjointConstraints.get(s1);	
			
			if(disjWith1 == null)
				continue;
			
			for (LeafExpression s2 : superB)
				if(disjWith1.contains(s2))
					return true; // supersort of one disj from supersort of other
		}
		
		return false;
	}
	
	private boolean areDisj(LeafExpression a, LeafExpression b)
	{
		// We assume the supersort relation given is a partial order, 
		// so transitivity is included.
		Set<LeafExpression> superA = supersorts.get(a);
		Set<LeafExpression> superB = supersorts.get(b);
					
		// Enforce reflexivity
		superA.add(a);
		superB.add(b);
		
		return oneDirectionDisjCheck(superA, superB) ||
		       oneDirectionDisjCheck(superB, superA);
		
	}
	
	private void printSetMatrix(Set<?>[][] validM, int max)
	{
		for(int row=0;row<max;row++)
		{
			MEnvironment.writeErr(row+": ");
			for(int col=0;col<max;col++)
			{				
				if(col % 10 == 0)
					MEnvironment.writeErr("/");
				
				if(validM[row][col].contains(SortEdge.BLUE))
					MEnvironment.writeErr("|BB|");
				if(validM[row][col].contains(SortEdge.RED))
					MEnvironment.writeErr("|RR|");
				if(validM[row][col].contains(SortEdge.REDBLUE))
					MEnvironment.writeErr("|RB|");
				if(validM[row][col].contains(SortEdge.BLUERED))
					MEnvironment.writeErr("|BR|");
				
				MEnvironment.writeErr(".");
			}
			MEnvironment.writeErrLine("");
		}
	}
	
	private void printMatrix(SortEdge[][] connM, int max)
	{
		// hash the matrix
		long theHash = 0;
		int theBit = 0;		
		for(int row=0;row<max;row++)
		{
			MEnvironment.writeErr(row+": ");
			for(int col=0;col<max;col++)
			{
				if(connM[row][col] == SortEdge.REDBLUE)
					theHash += 1 * Math.pow(5, theBit);			
				else if(connM[row][col] == SortEdge.BLUE)
					theHash += 2 * Math.pow(5, theBit);
				else if(connM[row][col] == SortEdge.RED)
					theHash += 3 * Math.pow(5, theBit);
				else if(connM[row][col] == SortEdge.BLUERED)
					theHash += 4 * Math.pow(5, theBit);
				
				theBit++;
				
				if(col % 10 == 0)
					MEnvironment.writeErr("/");
				
				if(connM[row][col] == SortEdge.BLUE)
					MEnvironment.writeErr("B");
				else if(connM[row][col] == SortEdge.RED)
					MEnvironment.writeErr("R");
				else if(connM[row][col] == SortEdge.REDBLUE)
					MEnvironment.writeErr(">");
				else if(connM[row][col] == SortEdge.BLUERED)
					MEnvironment.writeErr("<");
				else
					MEnvironment.writeErr("-");
			}
			MEnvironment.writeErrLine("");
			
		}	
		MEnvironment.writeErrLine("    MATRIX HASH: "+theHash+"\n\n");

	}
	
	private void booleanWarshall(boolean[][] matrix, boolean requireNotDisj, int max, Map<Integer, LeafExpression> intToSort)
	{
		for(int kk=0;kk<max;kk++)
		{
			for(int ii=0;ii<max;ii++)
			{
				for(int jj=0;jj<max;jj++)
				{
					if(enableDebug)
						MEnvironment.writeErrLine("Trying to extend "+ii+" to "+jj+" via "+kk+"; was "+matrix[ii][jj]);											

					// Do we have a blue edge from i->k and k->j, and i is not disjoint from j?
					if(matrix[ii][kk] && matrix[kk][jj])
					{
						// Blue requires not disj
                       if(requireNotDisj && !areDisj(intToSort.get(ii), intToSort.get(jj)))                    	   
                    	   matrix[ii][jj] = true;
                       else if(!requireNotDisj)
                    	   matrix[ii][jj] = true;
					}
					else if(enableDebug)						
						MEnvironment.writeErrLine("   NO CHANGE");
						
					
				} // jj loop		
			} // ii loop
		} // kk loop
	}
	
	SortEdge combineColors(SortEdge e1, SortEdge e2)
	{
		// R + B = RB
		// R + RB = invalid
		// R + R = invalid
		// R + BR = R
		if(e1 == SortEdge.RED)
		{
			if(e2 == SortEdge.BLUE)
				return SortEdge.REDBLUE;
			if(e2 == SortEdge.BLUERED)
				return SortEdge.RED;				
		}
		
		// B + R = BR
		// B + RB = B
		// B + BR = invalid
		// B + B = invalid
		else if (e1 == SortEdge.BLUE)
		{
			if(e2 == SortEdge.RED)
				return SortEdge.BLUERED;
			if(e2 == SortEdge.REDBLUE)
				return SortEdge.BLUE;
		}
		// BR + R = invalid
		// BR + BR = BR
		// BR + RB = invalid
		// BR + B = B
		else if (e1 == SortEdge.BLUERED)
		{
			if(e2 == SortEdge.BLUERED)
				return SortEdge.BLUERED;
			if(e2 == SortEdge.BLUE)
				return SortEdge.BLUE;
		}
		// RB + RB = RB
		// RB + R = R
		// RB + B = invalid
		// RB + BR = invalid
		else if (e1 == SortEdge.REDBLUE)
		{
			if(e2 == SortEdge.REDBLUE)
				return SortEdge.REDBLUE;
			if(e2 == SortEdge.RED)
				return SortEdge.RED;	
		}
		
		return null;
	}
	
	int combineColorSets(int first, int second, int result)
	{							
		// Add new combinations where possible.			
		if(first == 0 || second == 0)
			return result; // nothing new to add
		
		// Option 1: loop by SortEdges twice, combine if they exist, 
		// and add if anything new to add.
		for(SortEdge e1 : SortEdge.values())
		{
			if(e1.isIn(first))
			{
				for(SortEdge e2 : SortEdge.values())
				{
					if(e2.isIn(second))
					{
						SortEdge combination = combineColors(e1, e2);
						if(combination != null && !combination.isIn(result))
							result += combination.bitvalue;
					}
				}
			}
		}
		
		// Option 2: notice that there are only 4 possible edge types in result.
		// TODO !!!
		
		return result;
	}
	
	private void findFinitarySortsNewDisj()
	{
		int max = sorts.size();	

		MCommunicator.writeToLog("\n@"+System.currentTimeMillis()+": Starting findFinitarySortsNewDisj.");
		
		// Fix an ordering of the sorts
		// Make the ordering deterministic (by alphabetic order)
		List<LeafExpression> sortedSorts = new ArrayList<LeafExpression>(sorts);
		Collections.sort(sortedSorts, new MLeafExpressionComparator());
		
		Map<LeafExpression, Integer> sortToInt = new HashMap<LeafExpression, Integer>();
		Map<Integer, LeafExpression> intToSort = new HashMap<Integer, LeafExpression>();		
		int iIndex = 0;
		for(LeafExpression s : sortedSorts)
		{
			sortToInt.put(s, iIndex);
			intToSort.put(iIndex, s);
			//MCommunicator.writeToLog("\nSORT: "+s+" idx: "+iIndex);			
						
			// everything finitary by default, remove below
			finitarySorts.add(s);
			
			iIndex++;
		}
				
		//////////////////////////////////////////////////////////////
		// Build set of valid blue (src,dest) pairs first. A valid blue pair
		// uses only coercions to travel from src to dest, without crossing
		// any disjointness boundries.
		
		final boolean[][] blueM = new boolean[max][max];				
		for(int ii=0;ii<max;ii++)
			for(int jj=0;jj<max;jj++)			
				blueM[ii][jj] = false;	
		
		MCommunicator.writeToLog("\n@"+System.currentTimeMillis()+": Done with initialization.");
		
		// Subsorts
		for(LeafExpression curr : sortedSorts)
		{
			for(LeafExpression sub : subsorts.get(curr))
			{
				//MCommunicator.writeToLog("\nSUBSORT: "+curr+" > "+sub);
				int src = sortToInt.get(curr);
				int dest = sortToInt.get(sub);
				
				boolean areTheyDisj = areDisj(curr, sub);
				
				if(enableDebug)
					MEnvironment.writeErrLine("Sort "+curr+" had subsort "+sub+". areTheyDisj="+areTheyDisj);
								
				if(!areTheyDisj) 
					blueM[src][dest] = true;
			}			
		}
		
		MCommunicator.writeToLog("\n@"+System.currentTimeMillis()+": Done with adding subsort relation to blue array.");
		
		// coercions due to SAP functions (global and local)
		for(SigFunction f : productiveSAPFunctions)
		{
			for(LeafExpression arg : f.arity)
			{
				//MCommunicator.writeToLog("\nSAP: "+f.sort+" -> "+arg);
				int src = sortToInt.get(f.sort);
				int dest = sortToInt.get(arg);

				if(!areDisj(f.sort, arg)) 
					blueM[src][dest] = true;
			}				
		}	
		
		// TODO ^^ should this be global only? Seems so.
		
		MCommunicator.writeToLog("\n@"+System.currentTimeMillis()+": Done with adding SAP coercions to blue array.");

		if(enableDebug)
			printBooleanMatrix(blueM, max);
		booleanWarshall(blueM, true, max, intToSort);									
		if(enableDebug)
			printBooleanMatrix(blueM, max);
				
		MCommunicator.writeToLog("\n@"+System.currentTimeMillis()+": Done with Warshall on blue array.");
		
		// This blue-TC is exactly what should go in
		// supersAndCoercionsFromTC for use in counter.
		// They are the valid propagations.
		for(int row=0;row<max;row++)
		{
			// Matrix is reversed since edges in search are reversed.
			// To be useful for propagation, flip from/to
			LeafExpression toSort = intToSort.get(row);
			
			for(int col=0;col<max;col++)
			{
				if(row==col) continue; // don't self-propagate				
				LeafExpression fromSort = intToSort.get(col);
				
				if(blueM[row][col])
				{					
					supersAndCoercionsFromTC.get(fromSort).add(toSort);
				}
			}
		}
		
		
		
		//////////////////////////////////////////////////////////////
		// Then build the set of valid red pairs. A valid red pair uses
		// only real functions, and can span disjointness boundries.
		final boolean[][] redM = new boolean[max][max];				
		for(int ii=0;ii<max;ii++)
			for(int jj=0;jj<max;jj++)			
				redM[ii][jj] = false;	
									
		// f, g, etc.
		for(SigFunction f : productiveFunctions)	
		{
			for(LeafExpression arg : f.arity)
			{
				//MCommunicator.writeToLog("\nFUNC: "+f.sort+" -> "+arg);
				int src = sortToInt.get(f.sort);
				int dest = sortToInt.get(arg);
				redM[src][dest] = true;
			}
		}
		
		if(enableDebug)
			printBooleanMatrix(redM, max);	
		booleanWarshall(redM, false, max, intToSort);											
		if(enableDebug)
			printBooleanMatrix(redM, max);
		
		MCommunicator.writeToLog("\n@"+System.currentTimeMillis()+": Done with Warshall on red array.");
		
		/////////////////////////////////////////
		
		// Now, construct reachability matrix for paths made up of
		// red->blue->... pairs. This matches terms created from 
		// real functions and valid chains of coercions.

		// B, R, BR, RB. 
		// Need to collect SETS of those, and propagate with union as our "min"
		// and path concatenation as our "+".
		
		final int[][] validM = new int[max][max];				
		for(int ii=0;ii<max;ii++)
			for(int jj=0;jj<max;jj++)
			{										
				if(redM[ii][jj])
					validM[ii][jj] += SortEdge.RED.bitvalue;
				
				if(blueM[ii][jj])
					validM[ii][jj] += SortEdge.BLUE.bitvalue;
			}
		
		//if(enableDebug)
		//	printSetMatrix(validM, max);
		
		// Modified Warshall
		for(int kk=0;kk<max;kk++)
		{
			for(int ii=0;ii<max;ii++)
			{
				for(int jj=0;jj<max;jj++)
				{
					// Warshall with UNION for min and color concat. for +
					// M(i, j) = UNION {M(i, j) , M(i, k) ~+ M(k, j) } 					
					//final Set<SortEdge> combined = combineColorSets(validM[ii][kk], validM[kk][jj]);
					//System.err.println("Combined: "+validM[ii][kk]+" and "+validM[kk][jj]);					
					//System.err.println("ii, jj Was: "+validM[ii][jj]);

					// Keep old trues, don't overwrite!
					validM[ii][jj] = combineColorSets(validM[ii][kk], validM[kk][jj], validM[ii][jj]);					
					
					//System.err.println("ii, jj Is now: "+validM[ii][jj]);
					//if(validM[ii][jj].addAll(combined))
					//	if(enableDebug)
					//		MEnvironment.writeErrLine("Changed: "+intToSort.get(ii)+","+intToSort.get(jj)+" to "+validM[ii][jj]+" via "+intToSort.get(kk));
				}				
			}
		}
				
		MCommunicator.writeToLog("\n@"+System.currentTimeMillis()+": Done with blue/red Warshall.");
		
		// debug only
		//if(enableDebug)
		//	printSetMatrix(validM, max);
		
		
		for(SigFunction f : productiveFunctions)	
		{
			for(LeafExpression arg : f.arity)
			{
				int src = sortToInt.get(f.sort);
				int dest = sortToInt.get(arg);
				
				// Does this productive shadow-function sit on a cycle? (path from dest to src)
				if(validM[dest][src] > 0)
				{
					//MCommunicator.writeToLog("\nTAINTED CYCLE: Function "+f+"("+src+","+dest+").");
					
					// tainted cycle! Everything reachable from dest is infinitary.
					for(int option=0;option<max;option++)
					{
						if(validM[option][dest] > 0)
						{							
							LeafExpression infSort = intToSort.get(option);							
							finitarySorts.remove(infSort);
						}
					}
				}
				//else
					//MCommunicator.writeToLog("\nFunction "+f+"("+src+","+dest+") did not have a path from dest to src.");
				
			}
		}	
		
		MCommunicator.writeToLog("\n@"+System.currentTimeMillis()+": Done with function!");
	}

	private void calculateBounds()
	{
		// If all sorts are infinitary...
		if(finitarySorts.size() < 1)
			return;
		
		// ---------------------------------------------------------------
		// Create table for DP. Rows = term heights. Cols = finitary sorts.
		BigInteger[][] totals = new BigInteger[productiveFunctions.size() + 1][finitarySorts.size()];	
		
		// Initialize
		for(int iRow = 0; iRow <= productiveFunctions.size();iRow++)
			for(int iCol=0;iCol < finitarySorts.size();iCol++)
				totals[iRow][iCol] = BigInteger.ZERO;
		
		// ---------------------------------------------------------------
		// Fix an ordering of the finitary sorts
		// Make the ordering deterministic (by alphabetic order)
		List<LeafExpression> sortedFinitarySorts = new ArrayList<LeafExpression>(finitarySorts);
		Collections.sort(sortedFinitarySorts, new MLeafExpressionComparator());
		
		Map<LeafExpression, Integer> sortsInOrder = new HashMap<LeafExpression, Integer>();
		int ii = 0;
		for(LeafExpression s : sortedFinitarySorts)
		{
			sortsInOrder.put(s, Integer.valueOf(ii));
			ii++;
		}
		
		// ---------------------------------------------------------------
		// And an ordering of the non-constant functions
		Map<SigFunction, Integer> funcsInOrder = new HashMap<SigFunction, Integer>();
		List<SigFunction> sortedProductiveFunctions = new ArrayList<SigFunction>(productiveFunctions);
		Collections.sort(sortedProductiveFunctions, new MSigFunctionComparator());		
		ii = 0;
		for(SigFunction f : productiveFunctions)
		{
			funcsInOrder.put(f, Integer.valueOf(ii));
			ii++;
		}
		
		
		// We increment the term count in 2 phases: populate and propagate.
		// POPULATE phase: the native type of the term.
		// PROPAGATE phase: propagate the term through subsortness and coercions
		
		
		// ---------------------------------------------------------------
		// Step 1: Populate the height=0 row with constant terms
		Set<SigFunction> allConstants = new HashSet<SigFunction>(originalConstants);
		allConstants.addAll(skolemConstants);
		
		for(SigFunction c : allConstants)
		{			
			// First, build a list of all native sorts for c. 
			// (c.sort, but also coercions due to *local* sort-as-predicate appearance)			
			Set<LeafExpression> toPopulate = new HashSet<LeafExpression>();
			
			// native w/o coercion
			if(finitarySorts.contains(c.sort))
				toPopulate.add(c.sort);
			
			// LOCAL SAP coercions 
			// not true coercions, just statements that "c" may also be a B as well as an A.
			for(SigFunction sf : sapFunctions)
				if(sf.funcCause != null) // locals only
					if(sf.funcCause.equals(c) && finitarySorts.contains(sf.sort))
						toPopulate.add(sf.sort);
									
			// GLOBALS are applied below by supersAndCoercionsFromTC. Not needed here.
			
			// GLOBAL SAP coercions (from SAP functions)
			//for(SigFunction sf : sapFunctions)
			//{
			//	System.out.println(sf + " ... "+sf.funcCause);
			//	System.out.println(c.sort);
			//	if(sf.funcCause == null) // coercion not specific to a skolem function
			//		if(sf.arity.get(0).equals(c.sort) && finitarySorts.contains(sf.sort))
			//			toPopulate.add(sf.sort);
			//}

			// only one actual element bound to this constant
			totalTerms = totalTerms.add(BigInteger.ONE);
			
			// Remember where we have incremented the counter for this constant.
			// Don't double-count!
			Set<LeafExpression> countedIn = new HashSet<LeafExpression>();						
			
			for(LeafExpression pop : toPopulate)
			{						
				// don't duplicate
				if(countedIn.contains(pop))
					continue;
				
				// Populate
				totals[0][sortsInOrder.get(pop).intValue()] =
					totals[0][sortsInOrder.get(pop).intValue()].add(BigInteger.ONE);
				//MEnvironment.writeErrLine("Populated: "+pop);
										
				for(LeafExpression r : supersAndCoercionsFromTC.get(pop))
				{
					if(r == pop) continue; // trivial case, ignore
					if(!finitarySorts.contains(r)) continue; // don't try to prop to an infinitary sort
					if(countedIn.contains(r)) continue; // don't double-count
						
					totals[0][sortsInOrder.get(r).intValue()] = 
						totals[0][sortsInOrder.get(r).intValue()].add(BigInteger.ONE);
					
					//MEnvironment.writeErrLine("Propagated: "+pop);
					
					countedIn.add(r); 
				} // end for each sort to propagate

				countedIn.add(pop); 
			} // end for each native sort to populate
										
		} // end for each constant

		
		
		
		// ---------------------------------------------------------------
		// Step 2: Populate each height > 0
		
		for(int height = 1; height<= productiveFunctions.size(); height++)
		{			
			// No point in going to the next height if this height has no terms.
			boolean thisHeightExists = false;
			
			for(SigFunction f : productiveFunctions)
			{
				// First, build a list of all native sorts for f's result. 
				// (f.sort, but also local coercions due to sort-as-predicate appearance)			
				Set<LeafExpression> toPopulate = new HashSet<LeafExpression>();
				// native w/o coercion
				if(finitarySorts.contains(f.sort))
					toPopulate.add(f.sort);
				// SAP coercions (from SAP functions local to f)
				for(SigFunction sc : sapFunctions)
					if(f.equals(sc.funcCause) && finitarySorts.contains(sc.sort))
						toPopulate.add(sc.sort);
				// SAP coercions (from SAP functions which are global coercions)
				for(SigFunction sf : sapFunctions)
					if(sf.funcCause == null) // coercion not specific to a Skolem function
						if(sf.arity.get(0).equals(f.sort) && finitarySorts.contains(sf.sort))
							toPopulate.add(sf.sort);
				
				if(toPopulate.size() == 0)
					continue; // don't calculate if nowhere to populate.
				
				// Calculate
				BigInteger num_this_height = BigInteger.ZERO;
				
				// To make a term of height h, there must be at least one subterm of 
				// height h-1. Consider all cases for this subterm.
				for(int ileftmost = 0; ileftmost < f.arity.size(); ileftmost++)
				{
					BigInteger num_leftmost = BigInteger.ONE;
					
					for(int icol = 0; icol < f.arity.size(); icol++ )
					{
						BigInteger coltotal = BigInteger.ZERO;
						
						if(!finitarySorts.contains(f.arity.get(icol)))
						{
							MEnvironment.writeErrLine("Error: "+f+"\n"+finitarySorts);
							MEnvironment.writeErrLine(toPopulate.toString());
							MEnvironment.writeErrLine(sapFunctions.toString());
							MEnvironment.writeErrLine(productiveFunctions.toString());
							MEnvironment.quitMargrave();						
						}
						// icol is indexing by position in arity list. Sort ordering is different.
						int actual_col = sortsInOrder.get(f.arity.get(icol)).intValue();
											
						
						// OPT cache column totals up to h-2 in order to save totaling time	
						
						if(icol < ileftmost)
							for(int irow=0;irow<=height-2;irow++)
								coltotal = coltotal.add(totals[irow][actual_col]);
						else if(icol > ileftmost)
							for(int irow=0;irow<=height-1;irow++)
								coltotal = coltotal.add(totals[irow][actual_col]);
						else
							coltotal = totals[height-1][actual_col];
													
						num_leftmost = num_leftmost.multiply(coltotal);
					}
										
					num_this_height = num_this_height.add(num_leftmost);
				}
								
				// Only this many actual bindings
				totalTerms = totalTerms.add(num_this_height);
				
				Set<LeafExpression> countedIn = new HashSet<LeafExpression>();
				for(LeafExpression pop : toPopulate)
				{
					// don't duplicate
					if(countedIn.contains(pop))
						continue;

					// Populate
					totals[height][sortsInOrder.get(pop).intValue()] =
						totals[height][sortsInOrder.get(pop).intValue()].add(num_this_height);						
					
					// Propagate
					for(LeafExpression r : supersAndCoercionsFromTC.get(pop))
					{
						// don't duplicate
						if(countedIn.contains(r))
							continue;
						
						if(r == pop) continue;
						if(!finitarySorts.contains(r)) continue; // don't prop to inf sort
						totals[height][sortsInOrder.get(r).intValue()] =
							totals[height][sortsInOrder.get(r).intValue()].add(num_this_height);
						countedIn.add(r);
					}
					
					countedIn.add(pop);
				}
				
				if(num_this_height.compareTo(BigInteger.ZERO) == 1)
					thisHeightExists = true;
			}
			
			if(!thisHeightExists)
				break; // didn't make any height=h terms. So no h+1 can exist either.
		}
		
		// ---------------------------------------------------------------
		// Step 3: Column totals are our term counts (for finitary sorts).
		for(LeafExpression s : finitarySorts)
		{
			BigInteger total = BigInteger.ZERO;
			for(ii = 0; ii <= productiveFunctions.size(); ii++)
				total = total.add(totals[ii][sortsInOrder.get(s).intValue()]);
			termCounts.put(s, total);			
		}
		
	}
	
	
	// ------------- Unit tests ----------------
	
	public static void unitTests() 
	throws MUnsupportedFormulaException, MNotASortException
	{
		MEnvironment.writeErrLine("----- Begin FormulaSigInfo Tests (No messages is good.) -----");
		LeafExpression Sort1 = Relation.unary("Sort1");
		LeafExpression Sort2 = Relation.unary("Sort2");
		
		Set<LeafExpression> sorts1 = new HashSet<LeafExpression>();
		sorts1.add(Sort1); sorts1.add(Sort2);
		
		Map<LeafExpression, Set<LeafExpression>> order1 = new HashMap<LeafExpression, Set<LeafExpression>>();
		order1.put(Sort1, new HashSet<LeafExpression>());
		order1.put(Sort2, new HashSet<LeafExpression>());
		
		Map<LeafExpression, List<LeafExpression>> predicates = new HashMap<LeafExpression, List<LeafExpression>>();
		
		Variable x = Variable.unary("x");
		Variable y = Variable.unary("y");
		Variable z = Variable.unary("z");
		
		Formula fmla1 = Sort1.some();
		Formula fmla2 = Sort1.some().and(Sort2.lone());
		Formula fmla3 = Sort1.some().and(Sort2.one());
		Formula fmla4 = Sort1.some().and(Formula.TRUE.forSome(x.oneOf(Sort2)).forAll(y.oneOf(Sort1)));

		// two _different_ formula values
		Formula fmla5 = fmla4.and(Formula.TRUE.forSome(x.oneOf(Sort2)).forAll(y.oneOf(Sort1)));
		Formula fmla6 = fmla5.and(Formula.TRUE.forSome(x.oneOf(Sort1)));
		
		// SAME formula values.
		Formula subf = Formula.TRUE.forSome(x.oneOf(Sort2)).forAll(y.oneOf(Sort1));
		Formula fmla7 = Sort1.some().and(subf).and(subf);
		
		// Same as fmla7, but using an nary formula (3+ subfs)
		// Need to hide them (since set doesn't allow real duplicates)
		Set<Formula> theconj = new HashSet<Formula>();
		theconj.add(subf);
		theconj.add(subf.or(Formula.FALSE));
		theconj.add(subf.and(Formula.TRUE));
		Formula fmla8 = Sort1.some().and(Formula.and(theconj));
		
		// Test arity>1
		Formula fmla9 = Sort1.some()
  		  .and(Formula.TRUE.forSome(x.oneOf(Sort2)).forAll(y.oneOf(Sort1)).forAll(z.oneOf(Sort1)));
		
		// Test w/ more than one Decl per quantifier
		Formula fmla9a = Sort1.some()
		  .and(Formula.TRUE.forSome(x.oneOf(Sort2)).forAll(y.oneOf(Sort1).and(z.oneOf(Sort1))));
		
		Set<SigFunction> emptyFunctions = new HashSet<SigFunction>();
		Set<SigFunction> emptyConstants = new HashSet<SigFunction>();
		
		Map<Expression, LeafExpression> termTypes = new HashMap<Expression, LeafExpression>();
		
		FormulaSigInfo test1 = new FormulaSigInfo(sorts1, order1, predicates, emptyFunctions, emptyConstants, fmla1, EnumSAPHandling.sapKeep, termTypes);
		FormulaSigInfo test2 = new FormulaSigInfo(sorts1, order1, predicates, emptyFunctions, emptyConstants, fmla2, EnumSAPHandling.sapKeep, termTypes);
		FormulaSigInfo test3 = new FormulaSigInfo(sorts1, order1, predicates, emptyFunctions, emptyConstants, fmla3, EnumSAPHandling.sapKeep, termTypes);
		FormulaSigInfo test4 = new FormulaSigInfo(sorts1, order1, predicates, emptyFunctions, emptyConstants, fmla4, EnumSAPHandling.sapKeep, termTypes);
		FormulaSigInfo test5 = new FormulaSigInfo(sorts1, order1, predicates, emptyFunctions, emptyConstants, fmla5, EnumSAPHandling.sapKeep, termTypes);
		FormulaSigInfo test6 = new FormulaSigInfo(sorts1, order1, predicates, emptyFunctions, emptyConstants, fmla6, EnumSAPHandling.sapKeep, termTypes);
		FormulaSigInfo test7 = new FormulaSigInfo(sorts1, order1, predicates, emptyFunctions, emptyConstants, fmla7, EnumSAPHandling.sapKeep, termTypes);
		FormulaSigInfo test8 = new FormulaSigInfo(sorts1, order1, predicates, emptyFunctions, emptyConstants, fmla8, EnumSAPHandling.sapKeep, termTypes);
		FormulaSigInfo test9 = new FormulaSigInfo(sorts1, order1, predicates, emptyFunctions, emptyConstants, fmla9, EnumSAPHandling.sapKeep, termTypes);
		FormulaSigInfo test9a = new FormulaSigInfo(sorts1, order1, predicates, emptyFunctions, emptyConstants, fmla9a, EnumSAPHandling.sapKeep, termTypes);
		
		// 1 in Sort1
		if(test1.getTermCount() != 1)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 1 failed.");
		
		// 1 in Sort1 (lone doesn't induce a skolem constant)
		if(test2.getTermCount() != 1)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 2 failed.");

		// 1 in Sort1, 1 in Sort2 
		if(test3.getTermCount() != 2 || test3.finitarySorts.size() != 3)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 3 failed.");

		// f: Sort1->Sort2, one const of sort 1
		if(test4.getTermCount() != 2 || test4.finitarySorts.size() != 3)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 4 failed.");
		
		// Sort2: 2, Sort1: 1. (c1, f(c1), g(c1))
		if(test5.getTermCount() != 3 || test5.finitarySorts.size() != 3)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 5 failed.");

		// f and g: 1->2 but only one constant (one was induced by a .some())
		if(test6.getTermCount() != 3 || test6.finitarySorts.size() != 3)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 6 failed.");
				
		// same as test5, but identical skolem function inducing nodes
		// Sort2: 2, Sort1: 1
		if(test7.getTermCount() != 3 || test7.finitarySorts.size() != 3)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 7 failed.");
		
		// now test the same phenomenon on an Nary formula.
		// Sort2: 3, Sort1=1
		if(test8.getTermCount() != 4 || test8.finitarySorts.size() != 3)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 8 failed.");
		
		// Both have f(1, 1) -> 2, only one constant in sort 1.
		if(test9.getTermCount() != 2 || test9.finitarySorts.size() != 3)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 9 failed.");
		if(test9a.getTermCount() != 2 || test9a.finitarySorts.size() != 3)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 9a failed.");

		// Nothing but a A->A func
		// make sure self-reference is dealt with
		Formula fmla9b =  Sort1.some().and(Formula.TRUE.forSome(x.oneOf(Sort1)).forAll(y.oneOf(Sort1)));
		FormulaSigInfo test9b = new FormulaSigInfo(sorts1, order1, predicates, emptyFunctions, emptyConstants, fmla9b, EnumSAPHandling.sapKeep, termTypes);
		if(test9b.getTermCount() != -1 )
			MEnvironment.errorWriter.println("FormulaSigInfo test case 9b failed.");		
		
		LeafExpression Sort3 = Relation.unary("Sort3");
		Formula fmla10 = Formula.TRUE.forSome(x.oneOf(Sort2)).forAll(y.oneOf(Sort1))
						.and(Formula.TRUE.forSome(x.oneOf(Sort2)).forAll(y.oneOf(Sort3)))
						.and(Sort1.some());
		sorts1.add(Sort3);
		order1.put(Sort3, new HashSet<LeafExpression>());
		FormulaSigInfo test10 = new FormulaSigInfo(sorts1, order1, predicates, emptyFunctions, emptyConstants, fmla10, EnumSAPHandling.sapKeep, termTypes);
		if(test10.getTermCount() != 2 || test10.productiveFunctions.size() != 1)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 10 failed.");
		
		
		// A, B, C: A < B, A < C. (This is not locally filtered, but should count properly.)
		LeafExpression A = Relation.unary("A");
		LeafExpression B = Relation.unary("B");
		LeafExpression C = Relation.unary("C");
		Set<LeafExpression> sorts2 = new HashSet<LeafExpression>();
		sorts2.add(A);
		sorts2.add(B);
		sorts2.add(C);
		Map<LeafExpression, Set<LeafExpression>> order2 = new HashMap<LeafExpression, Set<LeafExpression>>();
		order2.put(A, new HashSet<LeafExpression>());
		order2.put(A, new HashSet<LeafExpression>());
		order2.get(A).add(B);
		order2.get(A).add(C);
		
		Formula fmla11 = Formula.TRUE.forSome(x.oneOf(A)).forSome(y.oneOf(B)).forSome(z.oneOf(C));
		FormulaSigInfo test11 = new FormulaSigInfo(sorts2, order2, predicates, emptyFunctions, emptyConstants, fmla11,EnumSAPHandling.sapKeep, termTypes);
		if(test11.getTermCount() != 3)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 11 failed.");		
		
		// *************************
		// SAP Tests
		// *************************
		
		// Test simple constant->constant SAP coercion
		Formula fmla12 = x.in(C).forSome(x.oneOf(B));
		FormulaSigInfo test12 = new FormulaSigInfo(sorts2, order2, predicates, emptyFunctions, emptyConstants, fmla12, EnumSAPHandling.sapKeep, termTypes);
		if(test12.getTermCount() != 1 || test12.getTermCount(B) != 1 || test12.getTermCount(C) != 1 || test12.getTermCount(A) != 0)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 12 failed.");		
		
		// Test simple constant + coercion function SAP
		Formula fmla13 = x.in(C).forAll(x.oneOf(B)).forSome(y.oneOf(A)).forSome(z.oneOf(B));
		FormulaSigInfo test13 = new FormulaSigInfo(sorts2, order2, predicates, emptyFunctions, emptyConstants, fmla13, EnumSAPHandling.sapKeep, termTypes);		
		if(test13.getTermCount() != 2 || test13.getTermCount(B) != 2 || test13.getTermCount(C) != 2 || test13.getTermCount(A) != 1)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 13 failed.");		
		
		// Test finite number of terms via coercion function to subsort
		// (Cycle detection needs to require a normal function on the cycle)
		Formula fmla14 = x.in(A).forAll(x.oneOf(B)).forSome(y.oneOf(B));
		FormulaSigInfo test14 = new FormulaSigInfo(sorts2, order2, predicates, emptyFunctions, emptyConstants, fmla14, EnumSAPHandling.sapKeep, termTypes);
		if(test14.getTermCount() != 1 || test14.getTermCount(B) != 1 || test14.getTermCount(C) != 1 || test14.getTermCount(A) != 1)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 14 failed.");							
		
		// Test infinitary (y: B, z[B]:B -- so B infinitary)
		// The result of z[b] is always in A as well as B (by SAP coercion). So inf A too.
		// Since C is a supersort of A, it must also be infinitary!
		Formula fmla15 = z.in(A).forSome(z.oneOf(B)).forAll(x.oneOf(B)).forSome(y.oneOf(B)).and(C.some());;
		FormulaSigInfo test15 = new FormulaSigInfo(sorts2, order2, predicates, emptyFunctions, emptyConstants, fmla15, EnumSAPHandling.sapKeep, termTypes);
		if(test15.getTermCount() != -1 || test15.getTermCount(B) != -1 || test15.getTermCount(C) != -1 || test15.getTermCount(A) != -1)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 15 failed.");		
		
		// Test partial infinitary, partial finitary
		// Changed only the coercion sort from test 15, but it prevents the infinitaryness from leaking into A (and from there, to C)
		Formula fmla16 = z.in(B).forSome(z.oneOf(B)).forAll(x.oneOf(B)).forSome(y.oneOf(B)).and(C.some());
		FormulaSigInfo test16 = new FormulaSigInfo(sorts2, order2, predicates, emptyFunctions, emptyConstants, fmla16, EnumSAPHandling.sapKeep, termTypes);
		if(test16.getTermCount() != -1 || test16.getTermCount(B) != -1 || test16.getTermCount(C) != 1 || test16.getTermCount(A) != 0)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 16 failed.");	
				
		// Test coercion function in presence of real function
		
		// First, no coercion. x:A (and hence in both B and C.) f:B->C. So 2 terms in C.
		Formula fmla17 = Formula.TRUE.forSome(y.oneOf(C)).forAll(x.oneOf(B)).and(A.some());
		FormulaSigInfo test17 = new FormulaSigInfo(sorts2, order2, predicates, emptyFunctions, emptyConstants, fmla17, EnumSAPHandling.sapKeep, termTypes);
		if(test17.getTermCount() != 2 || test17.getTermCount(B) != 1 || test17.getTermCount(C) != 2 || test17.getTermCount(A) != 1)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 17 failed.");
		
		// second, force C in B via SAP. Now only A is still finitary
		Formula fmla18 = Formula.TRUE.forSome(y.oneOf(C)).forAll(x.oneOf(B)).and(A.some())
		                 .and(x.in(B).forAll(x.oneOf(C)));
		FormulaSigInfo test18 = new FormulaSigInfo(sorts2, order2, predicates, emptyFunctions, emptyConstants, fmla18, EnumSAPHandling.sapKeep, termTypes);
		if(test18.getTermCount() != -1 || test18.getTermCount(B) != -1 || test18.getTermCount(C) != -1 || test18.getTermCount(A) != 1)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 18 failed.");	
		

		// Term counting *propagates* for SAP coercion functions
		// z: A, f: A->B. But coerce all of B into C. f(z) must be propagated to C. (so |C| = 2)
		Formula fmla19 = Formula.TRUE.forSome(y.oneOf(B)).forAll(x.oneOf(A)).and(A.some())
        		          .and(x.in(C).forAll(x.oneOf(B)));
		FormulaSigInfo test19 = new FormulaSigInfo(sorts2, order2, predicates, emptyFunctions, emptyConstants, fmla19, EnumSAPHandling.sapKeep, termTypes);
		if(test19.getTermCount() != 2 || test19.getTermCount(B) != 2 || test19.getTermCount(C) != 2 || test19.getTermCount(A) != 1)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 19 failed.");	
		//test19.printInfo();

		// Term counting *populates* for SAP "extra sort" functions
		// z:A. f: A->B. But f's output is coerced to C as well.
		Formula fmla20 = y.in(C).forSome(y.oneOf(B)).forAll(x.oneOf(A)).and(A.some());        
		FormulaSigInfo test20 = new FormulaSigInfo(sorts2, order2, predicates, emptyFunctions, emptyConstants, fmla20, EnumSAPHandling.sapKeep, termTypes);
		if(test20.getTermCount() != 2 || test20.getTermCount(B) != 2 || test20.getTermCount(C) != 2 || test20.getTermCount(A) != 1)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 20 failed.");					
		
		// test local SAP coercions have same arity as the real function they come from
		Formula fmla21 = z.in(A).forSome(z.oneOf(C)).forAll(y.oneOf(B)).forAll(x.oneOf(A)).and(B.some());        
		FormulaSigInfo test21 = new FormulaSigInfo(sorts2, order2, predicates, emptyFunctions, emptyConstants, fmla21, EnumSAPHandling.sapKeep, termTypes);
		if(test21.getTermCount() != 1 || test21.getTermCount(B) != 1 || test21.getTermCount(C) != 0 || test21.getTermCount(A) != 0)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 21 failed.");	
		
		// both funcs are useless (A is empty unless f can produce a term, but f needs something from A.)
		if(test21.productiveFunctions.size() != 0 || test21.productiveSAPFunctions.size() != 0)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 21(a) failed.");
		
		
		// test no doublecounting.
		// Say forall x^B, C(x) or A(x).
		Formula fmla22 = (x.in(A).or(x.in(C))).forAll(x.oneOf(B)).and(B.some());        
		FormulaSigInfo test22 = new FormulaSigInfo(sorts2, order2, predicates, emptyFunctions, emptyConstants, fmla22, EnumSAPHandling.sapKeep, termTypes);
		if(test22.getTermCount() != 1 || test22.getTermCount(B) != 1 || test22.getTermCount(C) != 1 || test22.getTermCount(A) != 1)
			MEnvironment.errorWriter.println("FormulaSigInfo test case 22 failed.");	
		
		// Test SAP propagation (through <=) for constants
		// A, B, C... D, E
		// A < B, A < C, D < E
		// SAP coercion from C -> D
		// constant in A. Should propagate through SAP to both D and E.
		LeafExpression D = Relation.unary("D");
		LeafExpression E = Relation.unary("E");
		sorts2.add(D);
		sorts2.add(E);
		order2.put(D, new HashSet<LeafExpression>());
		order2.get(D).add(E);
		
		Formula fmla23 = 
			A.some() // a in A
		.and(x.in(D).forAll(x.oneOf(C))); // SAP from C to D
		
		FormulaSigInfo test23 = new FormulaSigInfo(sorts2, order2, predicates, emptyFunctions, emptyConstants, fmla23, EnumSAPHandling.sapKeep, termTypes);
		if(test23.getTermCount() != 1 || test23.getTermCount(A) != 1 || test23.getTermCount(C) != 1
				|| test23.getTermCount(D) != 1 ||  test23.getTermCount(E) != 1)
		{
			MEnvironment.errorWriter.println("FormulaSigInfo test case 23 failed.");
			test23.printInfo();
		}
			
		// Test SAP propagation after function application
		// (This is separate propagation code from constant propagation, hence separate tests)
		// F < G
		LeafExpression F = Relation.unary("F");
		LeafExpression G = Relation.unary("G");
		sorts2.add(F);
		sorts2.add(G);
		order2.put(F, new HashSet<LeafExpression>());
		order2.get(F).add(G);
		
		Formula fmla23a = 
			A.some() // a in A (and A < C)
		.and(Formula.TRUE.forSome(y.oneOf(D)).forAll(x.oneOf(C))) // f: C -> D (and D < E)
		.and(x.in(F).forAll(x.oneOf(E))) // SAP from E to F
		.and(x.in(B).forAll(x.oneOf(F))); // sap from F to B (so B will have 2 in it)
		
		FormulaSigInfo test23a = new FormulaSigInfo(sorts2, order2, predicates, emptyFunctions, emptyConstants, fmla23a, EnumSAPHandling.sapKeep, termTypes);
		if(test23a.getTermCount() != 2 || test23a.getTermCount(A) != 1 || test23a.getTermCount(C) != 1 ||
				test23a.getTermCount(B) != 2 || 
				test23a.getTermCount(D) != 1 || test23a.getTermCount(E) != 1 || 
				test23a.getTermCount(F) != 1 || test23a.getTermCount(G) != 1)
		{
			MEnvironment.errorWriter.println("FormulaSigInfo test case 23a failed.");
			test23a.printInfo();
		}
		
		// A < B
		// a in A
		// _local_ SAP coercion for a into B.
		// Make sure we do not double-count in A due to improper population phase for a.
		Formula fmla24 = x.in(C).forSome(x.oneOf(A));
		FormulaSigInfo test24 = new FormulaSigInfo(sorts2, order2, predicates, emptyFunctions, emptyConstants, fmla24, EnumSAPHandling.sapKeep, termTypes);
		if(test24.getTermCount() != 1 || test24.getTermCount(A) != 1 || test24.getTermCount(B) != 1)
		{
			MEnvironment.errorWriter.println("FormulaSigInfo test case 24 failed.");
			test24.printInfo();
		}		
		
		/////////////////////////////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////////////////////////////
		// ****** Disjointness tests
		// do not enable these if reverting to prior version of Warshall
		if(!enableDisjointness)
			return;
		
		
		Set<LeafExpression> sortsd1 = new HashSet<LeafExpression>();
		sortsd1.add(Sort1); sortsd1.add(Sort2); sortsd1.add(Sort3);
		
		Map<LeafExpression, Set<LeafExpression>> orderd1 = new HashMap<LeafExpression, Set<LeafExpression>>();
		// maps sorts to their SUPERSORTS
		orderd1.put(Sort1, new HashSet<LeafExpression>());
		orderd1.put(Sort2, new HashSet<LeafExpression>());
		orderd1.put(Sort3, new HashSet<LeafExpression>());
		
		// 3-element balanced tree
		orderd1.get(Sort2).add(Sort1);
		orderd1.get(Sort3).add(Sort1);
		
		Map<LeafExpression, Set<LeafExpression>> disjs1 = new HashMap<LeafExpression, Set<LeafExpression>>();
		disjs1.put(Sort2, new HashSet<LeafExpression>());
		disjs1.get(Sort2).add(Sort3); // children disjoint (S2 disj S3)
		
		// Abstract S1, separate constants in S1 and S2, func from S2 to S3.
		// Should not register as infinitary
		Variable c1 = Variable.unary("c1");
		Variable c2 = Variable.unary("c2");
		// 2 constants (one in parent, one in sort2 child)
		Formula fmlad1 = Formula.TRUE.forSome(c1.oneOf(Sort1)).and(Sort2.some())
		// parent is abstract
		  .and(x.in(Sort2).or(x.in(Sort3)).forAll(x.oneOf(Sort1)))
		 // func from child to other child
		  .and(Formula.TRUE.forSome(y.oneOf(Sort3)).forAll(z.oneOf(Sort2)));
		
		
		//enableDebug = true;
		FormulaSigInfo testd1 = new FormulaSigInfo(sortsd1, orderd1, predicates, emptyFunctions, emptyConstants, fmlad1, EnumSAPHandling.sapKeep, disjs1, termTypes);
		if(testd1.getTermCount() != 4 || testd1.getTermCount(Sort1) != 4 || testd1.getTermCount(Sort2) != 2 || testd1.getTermCount(Sort3) != 3)
		{
			MEnvironment.errorWriter.println("FormulaSigInfo test case DISJ-1 failed.");
			testd1.printInfo();
		}		
		
		///////////////////////////////////////////////////////////
		// DISJ-2
		// Make sure *necessary* self-loops are covered in the R-B path computation
		// TODO this test doesn't cover that case very well since Sort1 disj Sort2...
		// Is there such a thing as a necessary self-loop? (Go back to the proof)
		Map<LeafExpression, Set<LeafExpression>> orderd2 = new HashMap<LeafExpression, Set<LeafExpression>>();
		orderd2.put(Sort1, new HashSet<LeafExpression>());
		orderd2.put(Sort2, new HashSet<LeafExpression>());
		orderd2.put(Sort3, new HashSet<LeafExpression>());
		// Sort1 < Sort2 < Sort3
		orderd2.get(Sort1).add(Sort2);
		orderd2.get(Sort1).add(Sort3);
		orderd2.get(Sort2).add(Sort3);
		
		Map<LeafExpression, Set<LeafExpression>> disjs2 = new HashMap<LeafExpression, Set<LeafExpression>>();
		disjs2.put(Sort1, new HashSet<LeafExpression>());
		disjs2.get(Sort1).add(Sort3); 
		
		// something is in Sort1.
		// There is a function from Sort2 to Sort2.
		Formula fmlad2 = Sort1.some()
		.and(Formula.TRUE.forSome(y.oneOf(Sort2)).forAll(x.oneOf(Sort2)));
		
		//enableDebug = true;
		
		FormulaSigInfo testd2 = new FormulaSigInfo(sortsd1, orderd2, predicates, emptyFunctions, emptyConstants, fmlad2, EnumSAPHandling.sapKeep, disjs2, termTypes);
		
		if(testd2.getTermCount() != -1 || testd2.getTermCount(Sort1) != 1 || testd2.getTermCount(Sort2) != -1 || testd2.getTermCount(Sort3) != -1)
		{
			MEnvironment.errorWriter.println("FormulaSigInfo test case DISJ-2 failed.");
			testd2.printInfo();
		}	
		
		
		
		
		
		
		
		// need more devious test cases involving case 20's kind of SAP (not a coercion -- just another result sort)
		
		
		
		
				
		// TODO what is the semantics of \forall x^A B.some ?
		// It seems as if these "noCondition" functions never go beyond arity 0?
		// (Double-check in Alloy book. If so, prevent application of \forall to them in WalkAST)
		
		MEnvironment.writeErrLine("----- End FormulaSigInfo Tests -----");	
	}
	
	// ------------- Interface ----------------

	public int getTermCount(LeafExpression s)
	throws MNotASortException
	{		
		if(!sorts.contains(s))
			throw new MNotASortException("isSortFinitary: LeafExpression "+s+" was not declared as a sort.");
		if(!finitarySorts.contains(s))
			return -1;
		if(termCounts.containsKey(s))
			return termCounts.get(s).intValue();
		return -1;
	}
	
	public int getTermCount()
	{
		// It is not safe to add together all "top" sorts, because the caller 
		// may have given us an ordering that isn't locally filtered. Consider:
		// A<B, A<C with terms in A.
				
		if(finitarySorts.size() != sorts.size())
			return -1; 	
		
		// Don't allow huge amounts of terms. Can change this if it is needed. 		
		if(totalTerms.bitLength() > 30)
			return -1;
		
		return totalTerms.intValue(); 
	}
	
	public boolean isSortFinitary(LeafExpression s)
	throws MNotASortException
	{
		if(!sorts.contains(s))
			throw new MNotASortException("isSortFinitary: LeafExpression "+s+" was not declared as a sort.");
		return finitarySorts.contains(s);
	}
	
	public void printInfo()
	{
		MEnvironment.writeErrLine(getInfoString());
	}
			
	public String getInfoString()
	{
		String eol = "\n";
		String boldOn = "";
		String boldOff = "";
		if(htmlOutput)
		{
			eol = "<BR>"+eol;
			boldOn = "<B>";
			boldOff = "</B>";
		}
		
		StringBuffer result = new StringBuffer();
		
		if(sapFunctions.size() > 0)
		{
			result.append(boldOn+"A sort symbol occured as a predicate. Setting for Sort-as-predicate handling was:" + boldOff+eol);
			if(sap.equals(EnumSAPHandling.sapKeep))
				result.append("Keep."+eol);
			else if(sap.equals(EnumSAPHandling.sapIgnore))
				result.append("Ignore. (Totals may be incorrect.)"+eol);
			else
				result.append("Throw exception. (SAP would have caused an error; this should never be seen)."+eol);
			result.append(eol);
		}
		
		result.append(boldOn+"Constants (both original and Skolem):"+boldOff+eol);
		for(SigFunction c : skolemConstants)
			result.append("  "+ c.toPrettyString() + ""+eol);
		for(SigFunction c : originalConstants)
			result.append("  "+ c.toPrettyString() + ""+eol);
		
		result.append(""+eol);
		
		result.append(boldOn+"Functions (both original and Skolem): "+boldOff+eol);
		for(SigFunction f : skolemFunctions)
			result.append("  "+ f.toPrettyString() +""+eol);
		for(SigFunction f : originalFunctions)
			result.append("  "+ f.toPrettyString() +""+eol);
		if(skolemFunctions.size() + originalFunctions.size() < 1)
			result.append("  (No non-nullary functions.)"+eol);
		
		if(sapFunctions.size() > 0)
		{
			result.append(boldOn+"Coercions due to sorts-as-predicates:"+boldOff+eol);
			for(SigFunction sf : sapFunctions)
				result.append("  "+ sf.toPrettyString() + ""+eol);
		}
		
		// OPT distinction between finitary and finitary+populated?
		
		
		
		result.append(""+eol);
		result.append(boldOn+"Functions that contribute to the count: "+boldOff+eol);
		for(SigFunction f : productiveFunctions)
			result.append(f.toPrettyString()+eol);
		if(productiveFunctions.size() == 0)
			result.append("(None!)"+eol);
		if(productiveSAPFunctions.size() > 0)
		{
			result.append(boldOn+"Coercions due to sorts-as-predicates which contributed:"+boldOff+eol);
			for(SigFunction sf : productiveSAPFunctions)
				result.append("  "+ sf.toPrettyString() + ""+eol);
		}
		
		
		result.append(eol);
		
		// infinitary sorts
		Set<LeafExpression> infSorts = new HashSet<LeafExpression>(sorts);
		infSorts.removeAll(finitarySorts);		
		
		result.append(boldOn+"Finitary sorts:"+boldOff+eol + finitarySorts.toString()+""+eol); 
		result.append(boldOn+"Infinitary sorts:"+boldOff+eol + infSorts.toString()+""+eol);
			
		result.append(""+eol); 
		
		String sPopTermCounts = "";
		for(LeafExpression r : termCounts.keySet())
		{
			if(termCounts.get(r).intValue() > 0)
				sPopTermCounts += r.name() + ": " + termCounts.get(r).intValue() + " ";
		}
		
		if(htmlOutput)		
			result.append("<HR><div style=\"text-align:center\">\n");
		
		if(termCounts.keySet().size() > 0)
			result.append("Counts for finitary, populated sorts: "+boldOn+sPopTermCounts+boldOff+eol);
		
		Set<LeafExpression> unpopulatedSorts = new HashSet<LeafExpression>();
		for(LeafExpression r : finitarySorts)
		{
			if(!termCounts.containsKey(r) || termCounts.get(r).intValue() == 0)
				unpopulatedSorts.add(r);

		}
		if(unpopulatedSorts.size() > 0)
			result.append("The following sorts were finitary but unpopulated by ground terms: "+unpopulatedSorts + ""+eol);
		
		int termCount = getTermCount();
		
		if(termCount > 0)
			result.append("Number of "+boldOn+"distinct"+boldOff+" terms across all finitary sorts: "+boldOn+getTermCount()+boldOff+eol);
		else if(termCount == 0)
			result.append("Unable to count any terms at all; there were no constants.");
		else
			result.append("There were infinitary sorts, so could not establish a bound for the entire signature."+eol);
		

		if(htmlOutput)
			result.append("<HR>\n");
		result.append(eol);
		
		// Docs: "Expresses a value in megaCycles as its approximate equivalent
		//        of CPU seconds on a theoretical 1.2 GHz CPU. "
		// For now, just print megacycles. Better than nothing?
		// not really. all zero even if supports is true. Disabling for now.
		//double msCollect = qs.convertMegacyclesToCpuSeconds(mCycCollect) * 1000;
		//double msProductive = qs.convertMegacyclesToCpuSeconds(mCycProductive) * 1000;
		//double msFinitary = qs.convertMegacyclesToCpuSeconds(mCycFinitary) * 1000;
		//double msBounds = qs.convertMegacyclesToCpuSeconds(mCycBounds) * 1000;
		if(!htmlOutput)
		{
			result.append("Time to collect Skolem functions: "+msCollect +eol);
			result.append("Time to identify productive functions: "+msProductive +eol);
			result.append("Time to identify finitary sorts: "+msFinitary +eol);
			result.append("Time to calculate number of terms: "+msBounds +eol);
				
		result.append("-----------------------------------"+eol);
		}
		return result.toString();
	}	
	
	Map<String, Integer> getCountMapping()
	{
		Map<String, Integer> result = new HashMap<String, Integer>();
		for(LeafExpression e : sorts)
		{
			result.put(e.toString(), getTermCount(e));
		}
		
		result.put("", getTermCount());
		return result;
	}
	
	Set<SigFunction> getLocalSAPFunctions()
	{
		Set<SigFunction> result = new HashSet<SigFunction>();
		for(SigFunction f : sapFunctions)
			if(f.funcCause != null)
				result.add(f);
		return result;
	}

	Set<SigFunction> getGlobalSAPFunctions()
	{
		Set<SigFunction> result = new HashSet<SigFunction>();
		for(SigFunction f : sapFunctions)
			if(f.funcCause == null)
				result.add(f);
		return result;

	}

	Set<SigFunction> getSAPFunctions()
	{
		return new HashSet<SigFunction>(sapFunctions);
	}
	
	Set<SigFunction> getSkolemFunctions()
	{
		return new HashSet<SigFunction>(skolemFunctions);
	}

	Set<SigFunction> getSkolemConstants()
	{
		return new HashSet<SigFunction>(skolemConstants);
	}

	
}

class InvalidFormulaSigInfo extends FormulaSigInfo
{
	int reason;
	String explanation = "";
	
	public String toString()
	{
		return "InvalidFormulaSigInfo: failed for reason code: "+reason+". "+explanation;
		
	}
	
	InvalidFormulaSigInfo(int reason)
	{		
		this.reason = reason;
	}
	
	InvalidFormulaSigInfo(int reason, String explanation)
	{		
		this.reason = reason;
		this.explanation = explanation;
	}
}


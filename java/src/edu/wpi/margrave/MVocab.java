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

import java.io.*;
import java.util.*;

import kodkod.ast.*;
import kodkod.ast.operator.ExprOperator;
import kodkod.ast.operator.Multiplicity;


class MSort
{
	String name;
	Relation rel;
	
	Set<MSort> parents; 
	Set<MSort> subsorts;

	MSort(String n) {
		name = n;
		rel = MFormulaManager.makeRelation(n, 1);
		subsorts = new HashSet<MSort>();
		parents = new HashSet<MSort>();
	}

	public String toString() {
		return name + "~" + rel.hashCode();
	}
	
	public boolean equals(Object other)
	{
		if (this == other)
            return true;

        if (!(other instanceof MSort))
            return false;

        MSort othSort = (MSort)other;
        
        // Equal if names are equal.
        return othSort.name.equals(this.name);
	}
	
	public int hashCode()
	{
		return name.hashCode();
	}
	
	

	
}

class MSortPair
{
	MSort s1, s2;
	
	MSortPair(MSort s1, MSort s2)
	{
		this.s1 = s1;
		this.s2 = s2;
	}
	
	// Want set-semantics for equality
	public boolean equals(Object other)
	{
		if(this == other)
			return true;
		if(!(other instanceof MSortPair))
			return false;
		
		MSortPair mspOther = (MSortPair) other;
		return ((s1.equals(mspOther.s1) && s2.equals(mspOther.s2)) ||
				(s1.equals(mspOther.s2) && s2.equals(mspOther.s1)));
	}
	
	public int hashCode()
	{
		return s1.hashCode() + s2.hashCode();
	}
}

abstract class MTerm
{	
	Expression expr;
	Set<Variable> seenVariables = new HashSet<Variable>();
	Set<Relation> seenRelations = new HashSet<Relation>();
	
	abstract public String toString();
	abstract public boolean equals(Object other);
	abstract public int hashCode();
	
	static public MTerm makeTermFromExpression(Expression e)
	{
		if(e instanceof Variable)
		{
			// This is a variable term
			return new MVariableTerm(((Variable) e).name());			
		}
		if(e instanceof Relation)
		{
			// This is a constant term if seen where we expect a term (rather than a function rel)
			return new MConstantTerm(((Relation) e).name());
		}
		if(e instanceof BinaryExpression)
		{
			// This will be a function term.
			
			BinaryExpression bine = (BinaryExpression) e;
			if(!(ExprOperator.JOIN.equals(bine.op())))
				throw new MUserException("makeTermFromExpression: Expression should be join. Instead, got:"+e);
			
			// If we have a relation on the RHS, then it is the func ID, and the LHS must be a term: (t . F) ~= f(t)
			if(bine.right() instanceof Relation)
			{			
				Relation r = (Relation) bine.right();
				MTerm sub = makeTermFromExpression(bine.left());
				List<MTerm> subs = new ArrayList<MTerm>();
				subs.add(sub);
				return new MFunctionTerm(r.name(), subs);
			}
			
			// Expect a BinaryExpression or a 2-element NaryExpression in RHS
			//Expression rhlhs;
			//Expression rhrhs;
			ExprOperator op;
			
			// Otherwise, this had BETTER have a join on the RHS, denoting the (smaller) function term.
			// t1 . (t2 . F)) = f(t1, t2)
			if(bine.right() instanceof BinaryExpression)
			{
				//rhlhs = ((BinaryExpression)bine.right()).left();
				//rhrhs = ((BinaryExpression)bine.right()).right();	
				op = ((BinaryExpression)bine.right()).op();
			}
			else if(bine.right() instanceof NaryExpression)
			{
				NaryExpression naryrhs = (NaryExpression) bine.right();
				if(naryrhs.size() != 2)
					throw new MUserException("makeTermFromExpression: NaryExpression had size != 2: "+e);
				//rhlhs = naryrhs.child(0);
				//rhrhs = naryrhs.child(1);
				op = naryrhs.op();
			}
			else
				throw new MUserException("makeTermFromExpression: RHS was not a Relation, BinaryExpression, or NaryExpression:"+e);
							
			// And it had better be a JOIN:
			if(!ExprOperator.JOIN.equals(op))
				throw new MUserException("makeTermFromExpression: RHS was BinaryExpression but not a JOIN:"+e);			
			
			MTerm smallerFunc = makeTermFromExpression(bine.right());
			// RHS had better give us a function term:
			if(!(smallerFunc instanceof MFunctionTerm))
				throw new MUserException("makeTermFromExpression: RHS did not result in a MFunctionTerm:"+e);
						
			// Need to extend by the term in the LHS. For instance, consider:
			// (y.(c.G)).(x.F) = f(x, g(c, y))
			// RHS will give us the term f(x). We need to extend it by the term g(c, y).
			MTerm extension = makeTermFromExpression(bine.left());
		
			return new MFunctionTerm((MFunctionTerm)smallerFunc, extension);
			
		}
		if(e instanceof NaryExpression)
		{			
			throw new MUserException("makeTermFromExpression got NaryExpression when all function invocations should be chained BinaryExpressions:"+e);		
		}
		throw new MUserException("makeTermFromExpression got unsupported Expression:"+e);
	}
}

class MFunctionTerm extends MTerm
{
	String funcName;
	List<MTerm> subTerms;
	
	MFunctionTerm(MFunctionTerm smallerFunc, MTerm extension)
	{
		// Create the term that extends smallerFunc by extension. E.g:
		// f(x) g(y) ---> f(x, g(y))
		
		this.funcName = smallerFunc.funcName;
		this.subTerms = new ArrayList<MTerm>(smallerFunc.subTerms); // copy! caller may re-use
		this.subTerms.add(extension);
		
		init();
	}
	
	private void init()
	{
		// Room for each subterm, plus one
		Relation funcRel = MFormulaManager.makeRelation(funcName, subTerms.size()+1);
		this.seenRelations.add(funcRel);
		
		this.expr = funcRel;
		
		// Beware ordering here:
		// f(x, y) =  y.(x.F)  		
		for(MTerm child : subTerms)			
		{
			this.seenRelations.addAll(child.seenRelations);
			this.seenVariables.addAll(child.seenVariables);
			
			// Join two at a time.
			List<Expression> joinList = new ArrayList<Expression>(2);
			joinList.add(child.expr);
			joinList.add(this.expr);
			this.expr = MFormulaManager.makeJoinE(joinList);			
			//this.expr = child.expr.join(this.expr);
		}			

	}
	
	MFunctionTerm(String funcName, List<MTerm> subTerms)
	{
		this.funcName = funcName;
		this.subTerms = new ArrayList<MTerm>(subTerms); // copy! caller may re-use
		
		init();
	}
	
	public String toString()
	{
		String childStr = "";
		boolean first = true;
		for(MTerm child : subTerms)
		{
			if(first)
				first = false;
			else
				childStr += ", ";
			childStr += child.toString();
		}
		return funcName+"("+childStr+")";
	}
	
	public boolean equals(Object other)
	{
		if (this == other)
            return true;

        if (!(other instanceof MFunctionTerm))
            return false;

        MFunctionTerm othx = (MFunctionTerm)other;
        
        return othx.funcName.equals(funcName) &&
               othx.subTerms.equals(subTerms);
	}
	
	public int hashCode()
	{
		return funcName.hashCode() + subTerms.hashCode();
	}
}

class MConstantTerm extends MTerm
{
	String constName;
	
	MConstantTerm(String constName)
	{
		this.constName = constName;
		expr = MFormulaManager.makeRelation(constName, 1);		
		this.seenRelations.add((Relation)expr);
	}
	
	public String toString()
	{
		return constName; 
	}
	
	public boolean equals(Object other)
	{
		if (this == other)
            return true;

        if (!(other instanceof MConstantTerm))
            return false;

        MConstantTerm othx = (MConstantTerm)other;
        
        return othx.constName.equals(constName);
	}
	
	public int hashCode()
	{
		return constName.hashCode();
	}
}

class MVariableTerm extends MTerm
{
	String variableName;
	
	MVariableTerm(String variableName)
	{
		this.variableName = variableName;
		expr = MFormulaManager.makeVariable(variableName);		
		this.seenVariables.add((Variable)expr);
	}
	
	public String toString()
	{
		return variableName; 
	}	
	
	public boolean equals(Object other)
	{
		if (this == other)
            return true;

        if (!(other instanceof MVariableTerm))
            return false;

        MVariableTerm othx = (MVariableTerm)other;
        
        return othx.variableName.equals(variableName);
	}
	
	public int hashCode()
	{
		return variableName.hashCode();
	}
}

class MConstant extends MPredicate
{	
	public String toString()
	{
		return "Constant "+name+": "+type;
	}	
	
	MConstant(String name, Relation rel, MSort type)
	{
		super(name, rel, type);
	}
	
	public boolean equals(Object other)
	{
		if (this == other)
            return true;
		
        if (!(other instanceof MConstant))
        	return false;

        MConstant othConst = (MConstant)other;
        
        return othConst.name.equals(this.name) && 
               othConst.type.equals(this.type);
	}

	public int hashCode()
	{
		return name.hashCode() + type.hashCode();
	}
	
}

class MFunction extends MPredicate
{
	List<MSort> arity;
	MSort result;
	
	public String toString()
	{
		return "Function "+ name + ": "+arity+" -> "+result;
	}	
	
	MFunction(String name, Relation rel, List<MSort> arity, MSort result)
	{
		super(name, rel, arity, result);
		this.result = result;
		this.arity = arity;
	}
	
	public boolean equals(Object other)
	{
		if (this == other)
            return true;

        if (!(other instanceof MFunction))
            return false;

        MFunction othFunc = (MFunction)other;
        
        return othFunc.name.equals(this.name) &&
               othFunc.rel.equals(this.rel) &&
               othFunc.result.equals(this.result) &&
               othFunc.arity.equals(this.arity);
	}
	
	public int hashCode()
	{
		return name.hashCode() + rel.hashCode() + result.hashCode() + arity.hashCode();
	}

	
}

class MPredicate
{
	String name;
	Relation rel;
	List<MSort> type;
	
	public String toString()
	{
		return "Predicate "+ name + ": "+type;
	}
	
	MPredicate(String name, Relation rel, List<MSort> type)
	{
		this.name = name;
		this.rel = rel;
		this.type = type;
	}
	
	// Used for functions
	MPredicate(String name, Relation rel, List<MSort> arity, MSort result)
	{
		this.name = name;
		this.rel = rel;
		this.type = new ArrayList<MSort>(arity);
		this.type.add(result);
	}	
	
	// Used for constants
	MPredicate(String name, Relation rel, MSort cType)
	{
		this.name = name;
		this.rel = rel;
		this.type = new ArrayList<MSort>(1);
		this.type.add(cType);
	}	
	
	public boolean equals(Object other)
	{
		if (other == null)
			return false;		
		if (this == other)
            return true;
		
		// Make it safe to extend this class and override equals:
		if(!this.getClass().equals(other.getClass()))
			return false;

		// Unnecessary due to getClass() comparison above.
		// If included, FindBugs is unhappy.
        //  if (!(other instanceof MPredicate))
        //     return false;

        MPredicate othPred = (MPredicate)other;
        
        return othPred.name.equals(this.name) &&
               othPred.rel.equals(this.rel) &&
               othPred.type.equals(this.type);
	}
	
	public int hashCode()
	{
		return name.hashCode() + rel.hashCode() + type.hashCode();
	}

	public void asSexpression(StringBuffer buf) {
		buf.append("    ("+name+" ");		
		for(MSort arg : type)
			buf.append(" "+arg.name);
		buf.append(")"+MEnvironment.eol);	
	}
	
}

/**
 * MVocab
 * effectively describes the domain of discourse.
 */
public class MVocab {

	// Sort symbols
	Map<String, MSort> sorts;

	// Constant symbols
	Map<String, MConstant> constants;
	
	// Function symbols
	Map<String, MFunction> functions;
	
	// Non-sort predicate symbols
	Map<String, MPredicate> predicates;	
	
	// More constraints on the domain of discourse
	public MConstraints axioms;
	
	// Backward map from Expression to MTerm, used by FormulaSigInfo
	Map<Expression, MTerm> exprToTerm = new HashMap<Expression, MTerm>();

	///////////////////////////////////////////////////////////////////////
	// CACHE
	///////////////////////////////////////////////////////////////////////
	Set<MSort> cacheTopLevelSorts = new HashSet<MSort>();
	Map<MSort, Set<MSort>> cacheAncestors = new HashMap<MSort, Set<MSort>>();
	Map<MSort, Set<MSort>> cacheDescend = new HashMap<MSort, Set<MSort>>();
	Map<MSort, Set<MSort>> cacheConciseDisj = new HashMap<MSort, Set<MSort>>();
	///////////////////////////////////////////////////////////////////////
	

	String asSExpression(String name)
	{
		StringBuffer buf = new StringBuffer();
		buf.append("(Theory "+name+MEnvironment.eol);
		buf.append("  (Vocab "+name+MEnvironment.eol);
	
		buf.append("    (Types "+MEnvironment.eol);
		for(MSort s : sorts.values())
		{
			if(s.subsorts.size() == 0)
				buf.append("        "+s.name+MEnvironment.eol);
			else
			{
				buf.append("        ("+s.name+MEnvironment.eol+" > ");
				for(MSort sub : s.subsorts)
					buf.append(" "+sub.name); 
				buf.append(")"+MEnvironment.eol);
			}
		}
		buf.append(")"+MEnvironment.eol); // end types	
			
		sexprPreds("Functions", buf, functions.values());
		sexprPreds("Constants", buf, constants.values());
		sexprPreds("Predicates", buf, predicates.values());
		
		buf.append(")"+MEnvironment.eol); // end vocab	
		
		axioms.sexprAxioms(buf);
		buf.append(")"+MEnvironment.eol); // end theory	
		
		return buf.toString();
	}
	
	void sexprPreds(String cat, StringBuffer buf, Collection<? extends MPredicate> coll)
	{
		if(coll.size() < 1)
			return;
		
		buf.append("    ("+cat+MEnvironment.eol);
		for(MPredicate p : coll)
		{
			p.asSexpression(buf);			
		}
		buf.append(")"+MEnvironment.eol); 
	}
		
	void clearCache()
	{
		// Called when adding a new sort or subsort assertion.
		
		cacheTopLevelSorts.clear();
		cacheAncestors.clear();
		cacheDescend.clear();
		cacheConciseDisj.clear();
	}
	
	public MVocab()
	{

		sorts = new HashMap<String, MSort>();
		predicates = new HashMap<String, MPredicate>();
		constants = new HashMap<String, MConstant>();
		functions = new HashMap<String, MFunction>();
		
		axioms = new MConstraints(this);
	}

	/*
	 * Used by XACML, SQS to fix their identifiers. 
	 * Should NOT be used by MVocab directly. Caller is responsible.
	 * 
	 */
	static String validateIdentifier(String n, boolean substitute)
			throws MGEBadIdentifierName 
	{
		// Make sure n is an OK Predicate or Variable name

		if (n.length() < 1)
			throw new MGEBadIdentifierName(
					"Identifier must not be the empty string.");
	
		if(substitute)
		{
			// No whitespace allowed, but sometimes XACML wants spaces
			n = n.replace(" ", "_");
			// No quote symbols allowed (just in case)
			n = n.replace("\"", "`");
			
			// Removed: No reason to do this internally
			// Must be lower-case
			//n = n.toLowerCase();
		}

		String allowedchars;

		// old
		//allowedchars = "[A-Za-z0-9.\\\\-/:=_(),<>{}|+*@]*";
		// now just forbid whitespace, all else is legal.
		allowedchars = "[\\S]*";
		
		if (!n.matches(allowedchars))
			throw new MGEBadIdentifierName("Illegal identifier: " + n
					+ ". Only names matching " + allowedchars
					+ " are allowed as names.");


		return n;
	}


	public void addSort(String name) throws MGEBadIdentifierName
	{	
		clearCache();
		
		if (!isSort(name)) // no dupes
			sorts.put(name, new MSort(name));
	}

	public void addSort(String name, String childnames)
			throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		clearCache();
		
		// We declare that there is a type "name" and types 
		//(each child name, separated by whitespace)
		// And that name is a parent of all such.

		if (!isSort(name))
			sorts.put(name, new MSort(name));

		// don't add an empty-named type!
		if (childnames.length() < 1)
			return;

		MSort t = getSort(name);
		String[] cnames = childnames.split("\\s");

		// for each child in this declaration
		for (String cname : cnames)
		{		 
			
			if (!isSort(cname))
				sorts.put(cname, new MSort(cname));

			MSort c = getSort(cname);
			boolean addedNewSubsort = false;
			if (!t.subsorts.contains(c))	
			{
				t.subsorts.add(c);
				addedNewSubsort = true;
			}
		
	
			// may be an update, adding a parent, rather than a new child
			Set<MSort> oldparents = c.parents;
			c.parents = new HashSet<MSort>(c.parents);
			c.parents.add(t);

			// Does this result in a cycle? Then revert and fail.
			if (searchForSelfAncestor(c)) {
				
				if(addedNewSubsort)
					t.subsorts.remove(c);				
				c.parents = oldparents;				
				throw new MGEBadIdentifierName(
						"Cyclic type structure not allowed: " + cname);
			}
		}

	}

	protected void makeThisTheTopLevelSort(String name)
			throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// Creates a new sort and makes it the parent of all parentless types
		// used in tupling

		clearCache();
		
		if (!isSort(name))
			sorts.put(name, new MSort(name));
		MSort t = getSort(name);

		if(isSubtype(t))
			throw new MGEBadIdentifierName("Sort "+name+" had supersorts; could not make it top level.");
		
		for (MSort candidate : sorts.values())
		{
			if (!isSubtype(candidate) && !(candidate.name.equals(name)))
			{
				t.subsorts.add(candidate);
				candidate.parents.add(t);
			}
		}
	}

	Set<MConstant> getConstantsWithLocalSort(MSort s)
	{
		Set<MConstant> result = new HashSet<MConstant>();
		
		for(MConstant c : constants.values())
		{
			if(c.type.get(0).equals(s))
				result.add(c);
		}
		
		return result;
	}
	
	
	MSort getSort(String n) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{ 
		if (sorts.containsKey(n))
			return sorts.get(n);
		throw new MGEUnknownIdentifier("No such type: " + n);
	}

	MSort getSortForExpression(Expression e) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// Easy case: if e is a simple type relation
		if (isSort(e.toString()))
			return getSort(e.toString());

		throw new MGEUnknownIdentifier("No such relation: " + e + "~"
				+ e.hashCode() + " for any of these types: " + sorts);
	}

	Set<String> getSortNamesWithPrefix(String prefix) throws MGEBadIdentifierName
	{
		Set<String> result = new HashSet<String>();

		
		// Must traverse entire list of sorts.
		// Could probably use a snazzy data structure to avoid this.
		for (MSort t : sorts.values())
			if (t.name.startsWith(prefix))
				result.add(t.name);
		return result;
	}

	boolean isSort(String n) 
	{
		// No need to validate, validation is done inside getSort.
		try
		{
			getSort(n);
			return true;
		} catch (Exception E) {
			return false;
		}
	}

	boolean isPredicate(String n) 
	{
		return predicates.containsKey(n);
	}

	
	/**
	 * Does not call validateIdentifier before checking, but much faster.
	 * Should be used internally or when the parameter has already been
	 * validated.
	 * @param n
	 * @return
	 */
	boolean fastIsSort(String n)
	{
		return fastGetSort(n) != null;
	}
	
	/**
	 * See fastIsSort doc
	 * @param n
	 * @return
	 */
	MSort fastGetSort(String n)
	{
		return sorts.get(n);
	}

	boolean searchForSelfAncestor(MSort t) {
		// Did we accidentally introduce a cycle?

		List<MSort> tocheck = new ArrayList<MSort>(t.subsorts);
		Set<MSort> seen = new HashSet<MSort>();
		seen.add(t);

		while (tocheck.size() > 0) {
			MSort pos = tocheck.get(0);
			if (seen.contains(pos))
				return true;
			seen.add(pos);
			tocheck.addAll(pos.subsorts);
			tocheck.remove(0);
		}

		return false;
	}

	public void addSubSort(String parentName, String name)
			throws MGEBadIdentifierName, MGEUnknownIdentifier
	{
		clearCache();
		
		addSort(name);
		addSort(parentName);

		MSort childtype = getSort(name);
		MSort parenttype = getSort(parentName);

		Set<MSort> oldparents = childtype.parents;

		childtype.parents = new HashSet<MSort>(childtype.parents); 
		childtype.parents.add(parenttype);				
		
		boolean addedChild = false;
		if(!parenttype.subsorts.contains(childtype))
		{
			parenttype.subsorts.add(childtype);
			addedChild = true;
		}

		if (searchForSelfAncestor(childtype)) {
			if(addedChild)
				parenttype.subsorts.remove(childtype);
			childtype.parents = oldparents;
			throw new MGEBadIdentifierName(
					"Cyclic type structure not allowed: " + name);
		}
	}

	public void addPredicate(String name, String typeconstruct)
			throws MGEBadIdentifierName {
		// construct contains a String with relation names in it.
		// "A B C" means type of (A x B x C)
				
		if (predicates.keySet().contains(name))
			return; // already have a pred named this		
		
		List<MSort> arity = processTypeConstructStr(typeconstruct);
	
		Relation theRel = MFormulaManager.makeRelation(name, arity.size());
		
		MPredicate thePred = new MPredicate(name, theRel, arity);
		
		predicates.put(name, thePred);
	}

	List<MSort> processTypeConstructStr(String typeconstruct)
	{
		List<MSort> arity = new ArrayList<MSort>();
		String[] arityArr = typeconstruct.split(" ");
		for(String s : arityArr)
			arity.add(getSort(s));
		return arity;
	}

	Relation getRelation(String rname) throws MGEUnknownIdentifier, MGEBadIdentifierName 
	{	
		try
		{
			MSort t = getSort(rname);
			return t.rel;
		}
		catch (MBaseException E) 
		{
			if (predicates.containsKey(rname))
				return predicates.get(rname).rel;
			throw new MGEUnknownIdentifier("Error: Unable to get Relation for unknown sort name: " + rname);
		}
	}	

	private Formula getPredTypeFormula(MPredicate thePred)
			throws MGEUnknownIdentifier, MGEBadIdentifierName 
	{
		// Returns a Formula expressing constraints on the parent relation
		// This method works only for non-type EDB predicates, not types
		// themselves

		Expression rin = Expression.NONE; // will be overwritten below

		boolean initialized = false;
		for (MSort relSub : thePred.type)
		{
			if (!initialized) 
			{
				rin = relSub.rel;
				initialized = true;
			} 
			else 
				rin = rin.product(relSub.rel);			
		}

		Formula f = thePred.rel.in(rin);

		return f;
	}

	boolean isSubtype(MSort thetype) 
	{
		// If this type has a parent...
		if (thetype.parents.size() != 0)
			return true;
		return false;
	}

	Set<MSort> getTopLevelSorts()
	{
		if(cacheTopLevelSorts.size() > 0)
		{
			return cacheTopLevelSorts;
		}
		
		Set<MSort> results = new HashSet<MSort>();
		for(MSort s : sorts.values())
		{
			if(!isSubtype(s))
				results.add(s);
		}
		
		cacheTopLevelSorts = results;
		return results;
	}
	
	Set<MSortPair> getConciseDisjointSortsAsymm()
	{
		// Gives the result of calling getConciseDisjointSorts on
		// each sort, *but*, will only assert the disjointness between
		// A and B in one direction. Saves work. 
		
		Set<MSortPair> results = new HashSet<MSortPair>();
		
		for(MSort aSort : sorts.values())
		{
			Set<MSort> theseDisjs = getConciseDisjointSorts(aSort);
			
			// Set will use equality and not double-populate.
			for(MSort othSort : theseDisjs)
			{
				MSortPair aPair = new MSortPair(aSort, othSort);
				results.add(aPair);
			}
			
		}
		
		return results;
	}
	
	Set<MSort> getConciseDisjointSorts(MSort t)
	{
		// Which sorts are disjoint from t?
		// ONLY includes largest disjoint sorts: if A has child B, and A disj C,
		// this function returns only A, not {A, B}.
		
		// To avoid excess verbosity, do a DFS of the sort tree and, if 
		// sort A is disjoint from t, don't bother checking its sub-sorts.
		
		if(cacheConciseDisj.containsKey(t))
			return cacheConciseDisj.get(t);
		
		List<MSort> todo = new LinkedList<MSort>();
		Set<MSort> alreadyChecked = new HashSet<MSort>();
		
		Set<MSort> tDescend = buildSubSortSet(t);
		
		// Add all top-level sorts
		todo.addAll(getTopLevelSorts());

		Set<MSort> results = new HashSet<MSort>();	
		
		while (todo.size() > 0) 
		{
			MSort next = todo.get(0);
			todo.remove(0);
			alreadyChecked.add(next);
		
			if(next == t)
				continue;
			
			Set<MSort> nextDescend = buildSubSortSet(next);
			
			if(!tDescend.contains(next) && !nextDescend.contains(t) && Collections.disjoint(nextDescend, tDescend))
			{
				// If next is neither comparable with nor shares a lower bound with t, 
				// assert disjointness and leave its children alone.
				results.add(next);
			}
			else
			{			
				// If next is comparable, continue the search.
				// If next is not comparable and shares a lower bound, continue the search.
				
				for(MSort aChild : next.subsorts)
				{
					if(!alreadyChecked.contains(aChild) && !todo.contains(aChild))
					todo.add(aChild);					
				}	
			}
		} // end while todo
			
		cacheConciseDisj.put(t, results);
		return results;
	}
	
	Set<Formula> getDisjointness(MSort t) 
	{		
		// Returns a fmla asserting that t is disjoint from sorts that are 
		// (a) non-comparable with tand
		// (b) do not share a lower bound with t
						
		Expression unions = Expression.NONE;
		boolean first = true;
		
		Set<MSort> conciseDisjs = getConciseDisjointSorts(t);
		
		for(MSort aSort : conciseDisjs)
		{
			if(first)
			{
				unions = aSort.rel;
				first = false;
			}
			else
			{
				unions = unions.union(aSort.rel);			
			}
		}
												
		HashSet<Formula> results = new HashSet<Formula>();
		results.add(t.rel.intersection(unions).no());
		
		return results;
	}
	
	Formula getAxiomFormulaNoEffectOnSize() throws MGEUnknownIdentifier, MGEBadIdentifierName
	{		
		// Returns a formula stating "This is a model of order-sorted FoL over
		// these sorts and this ordering." 
				
		// Sub-sorts need not be mutually exclusive!

		// If using tupling:
		// Requires current vocab to be the tupled vocabulary, and
		// thus must be separated from user axioms which could affect
		// tupling.

		Set<Formula> axiomSet = new HashSet<Formula>();

		//////////////////////////////////////////////
		// Every atom is contained in some type.		
		
		// Removed this constraint to allow for 1-invocation-per-query 10/11 - TN
		/*Expression univunion = Expression.NONE;
		for(MSort aSort : sorts.values())
		{
			if(!isSubtype(aSort))
				univunion = univunion.union(aSort.rel);
		}
		axiomSet.add(Expression.UNIV.in(univunion));		
		*/
		
		//////////////////////////////////////////////		
		// All sorts contain their subsorts
		for (MSort basetype : sorts.values())
		{
			for (MSort subtype : basetype.subsorts)
			{
				Relation subd = subtype.rel;
				axiomSet.add(subd.in(basetype.rel));
			}

				//axiomSet.add(basetype.rel.in(allsubs));
			
			//////////////////////////////////////////////
			// and assert disjointness where appropriate
			axiomSet.addAll(getDisjointness(basetype));
		}
		
		//////////////////////////////////////////////					
		// Predicates have a sig which must be respected
		// e.g.: EdgePredicate in (Nodes x Nodes)
		// This also covers constants and functions due to inheritance.
		for (MPredicate aPred : predicates.values())
			axiomSet.add(getPredTypeFormula(aPred));		
				
		//////////////////////////////////////////////
		// sig is respected
		for(MFunction aFunc : functions.values())	
		{
			axiomSet.add(getPredTypeFormula(aFunc));
		}
		
		//////////////////////////////////////////////
		// sig is respected
		for(MConstant aConst : constants.values())
		{
			axiomSet.add(getPredTypeFormula(aConst));
		}
		// The other functional/const axioms are in the other function
		
		
		//System.err.println(axiomSet);
		
		return MFormulaManager.makeConjunction(axiomSet); // .accept(new SimplifyFormulaV());
	}

	Set<Formula> getAxiomFormulasThatMayAffectSize() throws MGEUnknownIdentifier,
			MGEArityMismatch, MGEBadQueryString, MGEManagerException, MGEBadIdentifierName {
		// Returns a set of formulas asserting sig restrictions
		// AXIOMS for our domain of discourse that will be added to the query
		// passed to KodKod. These axioms can affect the shape of the
		// optimized query signature, and so must be obtained before
		// optimization.

		Set<Formula> results = new HashSet<Formula>();

		
		
		//////////////////////////////////////////////
		// Functions are total
		// and their sig is respected
		for(MFunction aFunc : functions.values())	
		{
			results.add(makeFunctionalFormula(aFunc, "T"));
		}
		
		//////////////////////////////////////////////
		// Constants have only one atom
		// and their sig is respected
		for(MConstant aConst : constants.values())
		{
			results.add(MFormulaManager.makeMultiplicity(aConst.rel, Multiplicity.ONE));
		}
		
		
		// User-defined axioms -- things like "lone R", "A disjoint B", etc.
		results.addAll(axioms.getConstraintFormulas());

		return results;
	}
	
	Formula makeFunctionalFormula(MPredicate thePred, String type)
	throws MGEUnknownIdentifier, MGEArityMismatch, MGEBadIdentifierName, MGEManagerException
	{
		// Used to do forall x, y, z (x.y.z).R.one()  (where R is 4-ary)
		// Changed to forall x, y, z, exists r | (x y z r) in R and forall k ((x y z k) in R implies k=r)
		// This is longer, but complies with current FormulaSigInfo
		
		if(thePred == null)
			throw new MGEUnknownIdentifier("Unknown predicate in makeFunctionalFormula.");
		
		Relation r = thePred.rel;
		
		if (thePred.type.size() != r.arity())
			throw new MGEArityMismatch(
					"Arity mismatch between expected for relation " + r.name()
							+ ".");
		
				
		List<Decl> quants = new ArrayList<Decl>(); // x through z
		List<Variable> tuple1vars = new ArrayList<Variable>();
		List<Variable> tuple2vars = new ArrayList<Variable>();
		Expression tuple1 = Expression.NONE; // (x, y, ..., z, r_1)
		Expression tuple2 = Expression.NONE; // (x, y, ..., z, r_2)
				
		
		int x_counter = 1;	
		for(MSort argtype : thePred.type)
		{
			// Don't include the last component.
			if(x_counter == r.arity())
				break;
			
			Variable tempvar = MFormulaManager.makeVariable(r.name() + "_"
					+ type + "_" + x_counter);
			
			// "forall xi^Ai"
			quants.add(MFormulaManager.makeOneOfDecl(tempvar, argtype.rel));
			
			tuple1vars.add(tempvar);
			tuple2vars.add(tempvar);

			x_counter++;			
		}
		
		Variable fc1 = MFormulaManager.makeVariable(r.name()+"FC1");
		Variable fc2 = MFormulaManager.makeVariable(r.name()+"FC2");
		tuple1vars.add(fc1);
		tuple2vars.add(fc2);
		tuple1 = MFormulaManager.makeVarTupleV(tuple1vars);
		tuple2 = MFormulaManager.makeVarTupleV(tuple2vars);
		
		// Tuples and outside quantification now built.

		Decl dfc1 = MFormulaManager.makeOneOfDecl(fc1, thePred.type.get(r.arity()-1).rel);
		Decl dfc2 = MFormulaManager.makeOneOfDecl(fc2, thePred.type.get(r.arity()-1).rel);
		
		Formula f;
		Formula pt1, pt2;
		
		if("T".equals(type))
		{
			// tuple1 = (..., fc1)
			// tuple2 = (..., fc2)
			// EXISTS fc1 [(tuple1 in R) and (FORALL fc2 (tuple2 in R implies fc1=fc2))] 
			pt1 = MFormulaManager.makeAtom(tuple1, r);
			
			pt2 = MFormulaManager.makeAtom(tuple2, r);
			pt2 = MFormulaManager.makeImplication(pt2, MFormulaManager.makeEqAtom(fc1, fc2));
			pt2 = MFormulaManager.makeForAll(pt2, dfc2);
			
			f = MFormulaManager.makeAnd(pt1, pt2);
			f = MFormulaManager.makeExists(f, dfc1);
		}
		else if("P".equals(type))
		{
			// FORALL fc1 FORALL fc2 ( fc1!=fc2 implies (!(tuple1 in R) or !(tuple2 in R)))
			
			Formula ante = MFormulaManager.makeNegation(MFormulaManager.makeEqAtom(fc1, fc2));
			
			pt1 = MFormulaManager.makeNegation(MFormulaManager.makeAtom(tuple1, r));
			pt2 = MFormulaManager.makeNegation(MFormulaManager.makeAtom(tuple2, r));
			Formula conseq = MFormulaManager.makeOr(pt1, pt2);
						
			f = MFormulaManager.makeImplication(ante, conseq);			
			f = MFormulaManager.makeForAll(f, dfc2);
			f = MFormulaManager.makeForAll(f, dfc1);
		}
		else if("R".equals(type))
		{
			// total relation: like T but without 2nd part for uniqueness
			// EXISTS fc1 ((tuple1) in R) and <true> 
			pt1 = MFormulaManager.makeAtom(tuple1, r);							
			f = MFormulaManager.makeExists(pt1, dfc1);
			
		}
		else
		{
			throw new MGEUnknownIdentifier("Unknown functional/relational constraint type: "+type);
		}
		 
		// now close the foralls on the outside
		// reverse to nest them in the correct order. (no real impact here, but easier to read if needed)
		Collections.reverse(quants);
		for(Decl d : quants)
		{
			f = MFormulaManager.makeForAll(f, d);
		}
		
		//System.err.println(thePred.toString() + " --- " +f);
		
		//MEnvironment.errorStream.println(r + " "+ type);
		//MEnvironment.errorStream.println(f);
		
		return f; 		
	}
	

	public boolean isSubOrSubOf(String sub, String sup) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		return isSubOrSubOf(getSort(sub), getSort(sup));
	}
	
	public boolean isSubOrSubOf(MSort sub, MSort sup) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		Set<MSort> supers = buildSuperSortSet(sub);
		if(supers.contains(sup))
			return true;
		return false;
	}

	// Called by MatrixTuplingV
	/*public static String constructIndexing(Expression be,
			HashMap<Variable, Integer> indexing) {
		List<String> lst = inorderTraversalOfVariableProduct(be, indexing, null);
		if (lst.size() < 1)
			return "";

		StringBuffer result = new StringBuffer(lst.get(0));
		for (int ii = 1; ii < lst.size(); ii++)
			result.append("," + lst.get(ii));
		return result.toString();
	}
	
	public static String constructIndexing(List<String> vars,
			HashMap<Variable, Integer> indexing)
	{		
		if (vars.size() < 1)
			return "";

		Variable theVar = MFormulaManager.makeVariable(vars.get(0));
		StringBuffer result = new StringBuffer(String.valueOf(indexing.get(theVar)));
		for (int ii = 1; ii < vars.size(); ii++)
		{
			theVar = MFormulaManager.makeVariable(vars.get(ii));
			result.append("," + String.valueOf(indexing.get(theVar)));
		}
		return result.toString();
	}*/	
	
	// OPT Variant on inorderTraversalOfVariableProduct below.
	// Duplicate code but sufficient complex that I've just
	// copied and modified for now. - TN 5/11
	static List<Expression> getInOrderTerms(Expression e)
	{
		List<Expression> result = new ArrayList<Expression>();
				
		// Base case:
		if(e instanceof LeafExpression)
		{
			result.add(e);
			return result;
		}
		
		/////////////////////////////////////////////////////////////
		// Setup for DFS
		List<Expression> dfslist = new LinkedList<Expression>();
		if(e instanceof BinaryExpression)
		{			
			BinaryExpression be = (BinaryExpression) e;
			
			if(!be.op().equals(ExprOperator.PRODUCT))
			{
				// Not a product, so must be a term.
				result.add(be);
				return result;
			}
			
			dfslist.add(be.left());
			dfslist.add(be.right());
		}
		else if (e instanceof NaryExpression)
		{
			NaryExpression ne = (NaryExpression) e;
			
			if(!ne.op().equals(ExprOperator.PRODUCT))
			{
				// Not a product, so must be a term.
				result.add(ne);
				return result;
			}

			for(int ii=0;ii<ne.size();ii++)
			{
				dfslist.add(ne.child(ii));
			}			
		}
		
		/////////////////////////////////////////////////////////////
		// DFS
		while (dfslist.size() > 0)
		{
			Expression next = dfslist.get(0);
			dfslist.remove(0);

			if (next instanceof Variable)
			{
				result.add(next);
			}
			else if (next instanceof BinaryExpression)
			{
				BinaryExpression benext = (BinaryExpression) next;
				if(!benext.op().equals(ExprOperator.PRODUCT))
				{
					// Not a product, so must be a term.
					result.add(benext);				
				}
				else
				{
					dfslist.add(0, benext.right());
					dfslist.add(0, benext.left());
				}
			} // end BinaryExpression
			else if(next instanceof NaryExpression)
			{
				NaryExpression nenext = (NaryExpression) next;
				
				if(!nenext.op().equals(ExprOperator.PRODUCT))
				{
					// Not a product, so must be a term.
					result.add(nenext);				
				}
				else
				{
					// In reverse order
					for(int ii=nenext.size()-1;ii>=0;ii--)
					{
						Expression childexpr = nenext.child(ii);
						dfslist.add(0, childexpr);
					}
				}
			} // end NaryExpression

			else
				throw new MUserException("getInOrderTerms: Unsupported Expression type: "+next);
		} // while there remain DFS nodes to explore
		
		
		return result;
	} // end getInOrderTerms
	
	
	// OPT Duplicate code with above function for now. - TN
	/*
	private static List<String> inorderTraversalOfVariableProduct(
			Expression e, HashMap<Variable, Integer> indexing,
			HashMap<Variable, String> sortenv)
	{
		// DFS this expression. Assume either a BinaryExpression node or a
		// Variable. Otherwise yell.
		List<String> sort_result = new ArrayList<String>();
		List<String> index_result = new ArrayList<String>();
		List<String> varname_result = new ArrayList<String>();

		/////////////////////////////////////////////////////////////
		// Setup for DFS
		List<Expression> dfslist = new LinkedList<Expression>();
		if(e instanceof BinaryExpression)
		{			
			BinaryExpression be = (BinaryExpression) e;
			if(!be.op().equals(ExprOperator.PRODUCT))
				throw new MUserException("inorderTraversalOfVariableProduct: Not a variable: "+be);
			
			dfslist.add(be.left());
			dfslist.add(be.right());
		}
		
		// This method is NOT meant to handle more than a variable product. The above "term" traversal method does that.
		// This method is called from tupling, which requires only variables!
		
		else if (e instanceof NaryExpression)
		{
			NaryExpression ne = (NaryExpression) e;
			if(!ne.op().equals(ExprOperator.PRODUCT))
				throw new MUserException("inorderTraversalOfVariableProduct: Not a variable: "+ne);

			for(int ii=0;ii<ne.size();ii++)
			{
				dfslist.add(ne.child(ii));
			}			
		}

		/////////////////////////////////////////////////////////////
		// DFS
		while (dfslist.size() > 0)
		{
			Expression next = dfslist.get(0);
			dfslist.remove(0);

			// What does sortenv say the sort of this variable is?
			if (next instanceof Variable)
			{
				if (sortenv != null)
					sort_result.add(sortenv.get(next));
				if (indexing != null)
					index_result.add(indexing.get(next).toString());
				varname_result.add(next.toString());

			}
			else if (next instanceof BinaryExpression)
			{
				BinaryExpression benext = (BinaryExpression) next;
				// Don't .add to the END. Need to put at the beginning,
				// or else this will not be in-order.
				//dfslist.add(benext.left()); <--- NO
				//dfslist.add(benext.right()); <--- NO
				// Add right first (so left ends up in its proper place)
				dfslist.add(0, benext.right());
				dfslist.add(0, benext.left());
			}
			else if(next instanceof NaryExpression)
			{
				NaryExpression nenext = (NaryExpression) next;
				
				// In reverse order
				for(int ii=nenext.size()-1;ii>=0;ii--)
				{
					Expression childexpr = nenext.child(ii);
					dfslist.add(0, childexpr);
				}
			}

			else
				return new ArrayList<String>(); // will warn user
		} // while there remain DFS nodes to explore

		if (sortenv != null)
			return sort_result;
		if (indexing != null)
			return index_result;
		return varname_result;

	}*/

	Set<MSort> buildSubSortSet(String tname)
	{
		return buildSubSortSet(getSort(tname));
	}
	
	protected Set<MSort> buildSubSortSet(MSort t)
	{
		if(cacheDescend.containsKey(t))
			return cacheDescend.get(t);
		
		List<MSort> todo = new LinkedList<MSort>();
		Set<MSort> result = new HashSet<MSort>();
		todo.add(t);

		while (todo.size() > 0) 
		{
			MSort next = todo.get(0);
			todo.remove(0);
			result.add(next);
		
			for(MSort aChild : next.subsorts)
			{
				if(!result.contains(aChild) && !todo.contains(aChild))
				todo.add(aChild);
			}			
		}

		cacheDescend.put(t, result);
		return result;
		
	}
		
	protected Set<MSort> buildSuperSortSet(MSort t)
			throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		if(cacheAncestors.containsKey(t))
			return cacheAncestors.get(t);

		List<MSort> todo = new LinkedList<MSort>();
		Set<MSort> result = new HashSet<MSort>();
		todo.add(t);

		while (todo.size() > 0) 
		{
			// Pop todo, add to result
			MSort next = todo.get(0);
			todo.remove(0);
			result.add(next);

			// expand next, queue supers/supertypes not already dealt with or
			// already queued
			for(MSort aParent : next.parents)
			{
				if(!result.contains(aParent) && !todo.contains(aParent))
				todo.add(aParent);
			}			
		}
		cacheAncestors.put(t, result);
		return result;
	}

	protected boolean possibleOverlap(String st1, String st2)
			throws MGEUnknownIdentifier, MGEBadIdentifierName {
		return possibleOverlap(getSort(st1), getSort(st2));
	}

	protected boolean possibleOverlap(Expression et1, Expression et2)
			throws MGEUnknownIdentifier, MGEBadIdentifierName {
		return possibleOverlap(getSortForExpression(et1),
				getSortForExpression(et2));
	}

	protected boolean possibleOverlap(MSort t1, MSort t2)
			throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// TODO use cache (and construct it!)
		
		// Since we require top-level sorts to be disjoint 
		if (t1.parents.size() == 0 && t2.parents.size() == 0 && t1 != t2)
			return false;
		
		Set<MSort> subs1 = buildSubSortSet(t1);
		Set<MSort> subs2 = buildSubSortSet(t2);
		
		// <-related?
		if(subs1 == subs2 || subs1.contains(t2) || subs2.contains(t1))
			return true;
		
		// common lower bound?
		
		if(!Collections.disjoint(subs1, subs2))
			return true;
		
		// unrelated sorts, no common lower-bound
		return false; 
	}
	
	
	protected MVocab combineWith(MVocab other) throws MGECombineVocabs,
			MGEBadIdentifierName, MGEUnknownIdentifier 
	{
		
		// Names (strings) used to detect equivalence (since object references
		// are obviously untrustworthy.)
		
		// Shared predicates must have the same signature 
		for (String pname : predicates.keySet())
			if (other.predicates.keySet().contains(pname))
			{
				MPredicate predIn1 = predicates.get(pname);
				MPredicate predIn2 = other.predicates.get(pname);
				
				if (!predIn1.type.equals(predIn2.type))
					throw new MGECombineVocabs(
							"Different custom predicate signature: " + pname);
			}

		// Generate a list of shared type names
		Set<String> shared = new HashSet<String>();
		for (MSort t : sorts.values())
		{
			try
			{
				// Is this a type name that both vocabs use?
				other.getSort(t.name); // will throw an exception if no such sort
				shared.add(t.name);				
			} catch (MGEUnknownIdentifier e)
			{
			}
			
		}

		// Combine everything and return the result.
		MVocab uber = new MVocab();

		// For each sort
		for (MSort t : sorts.values()) {
			String childnames = "";
			for (MSort c : t.subsorts) {
				if (childnames.length() < 1)
					childnames = c.name;
				else
					childnames = childnames + " " + c.name;
			}
			uber.addSort(t.name, childnames);
		}
		for (MSort t : other.sorts.values()) {
			String childnames = "";
			for (MSort c : t.subsorts) {
				if (childnames.length() < 1)
					childnames = c.name;
				else
					childnames = childnames + " " + c.name;
			}
			uber.addSort(t.name, childnames);
		}
		
		// Predicates (equal methods will prevent overlap)
		for (String d : predicates.keySet())			
			uber.predicates.put(d, predicates.get(d));
		for (String d : other.predicates.keySet())
			uber.predicates.put(d, other.predicates.get(d));
		
		// Constants!
		for (String cname : constants.keySet())			
			uber.constants.put(cname, constants.get(cname));
		for (String cname : other.constants.keySet())
			uber.constants.put(cname, other.constants.get(cname));
		
		// Functions!
		for (String fname : functions.keySet())			
			uber.functions.put(fname, functions.get(fname));
		for (String fname : other.functions.keySet())
			uber.functions.put(fname, other.functions.get(fname));
		
		// Constraints
		for (String con : axioms.setsSingleton) {
			if (shared.contains(con))
				if (!other.axioms.setsSingleton.contains(con))
					throw new MGECombineVocabs(
							"Singleton constraint missing between vocabs -> "+con);
			uber.axioms.addConstraintSingleton(con);
		}
		for (String con : other.axioms.setsSingleton) {
			if (shared.contains(con))
				if (!axioms.setsSingleton.contains(con))
					throw new MGECombineVocabs(
							"Singleton constraint missing between vocabs <- "+con);
			uber.axioms.addConstraintSingleton(con);
		}

		for (String con : axioms.setsAtMostOne) {
			if (shared.contains(con))
				if (!other.axioms.setsAtMostOne.contains(con))
					throw new MGECombineVocabs(
							"Atmostone constraint missing between vocabs -> "+con);
			uber.axioms.addConstraintAtMostOne(con);
		}
		for (String con : other.axioms.setsAtMostOne) {
			if (shared.contains(con))
				if (!axioms.setsAtMostOne.contains(con))
					throw new MGECombineVocabs(
							"Atmostone constraint missing between vocabs <- "+con);
			uber.axioms.addConstraintAtMostOne(con);
		}

		for (String con : axioms.setsNonempty) {
			if (shared.contains(con))
				if (!other.axioms.setsNonempty.contains(con))
					throw new MGECombineVocabs(
							"Nonempty constraint missing between vocabs -> "+con);
			uber.axioms.addConstraintNonempty(con);
		}
		for (String con : other.axioms.setsNonempty) {
			if (shared.contains(con))
				if (!axioms.setsNonempty.contains(con))
					throw new MGECombineVocabs(
							"Nonempty constraint missing between vocabs <- "+con);
			uber.axioms.addConstraintNonempty(con);
		}

		for (String con : axioms.setsAbstract) {
			if (shared.contains(con))
				if (!other.axioms.setsAbstract.contains(con))
					throw new MGECombineVocabs(
							"Abstract constraint missing between vocabs -> "+con);
			uber.axioms.addConstraintAbstract(con);
		}
		for (String con : other.axioms.setsAbstract) {
			if (shared.contains(con))
				if (!axioms.setsAbstract.contains(con))
					throw new MGECombineVocabs(
							"Abstract constraint missing between vocabs <- "+con);
			uber.axioms.addConstraintAbstract(con);
		}
		
		for (String con : axioms.setsConstantsNeqAll) 
			uber.axioms.addConstraintConstantsNeqAll(con);		
		for (String con : other.axioms.setsConstantsNeqAll) 
			uber.axioms.addConstraintConstantsNeqAll(con);
	
		for (List<String> con : axioms.constantsNeq)
		{
			assert(con.size() == 2);
			uber.axioms.addConstraintConstantsNeq(con.get(0), con.get(1));
		}
		for (List<String> con : other.axioms.constantsNeq)
		{
			assert(con.size() == 2);
			uber.axioms.addConstraintConstantsNeq(con.get(0), con.get(1));
		}
		
		for (String con : axioms.setsConstantsCover) 
			uber.axioms.addConstraintConstantsCover(con);		
		for (String con : other.axioms.setsConstantsCover) 
			uber.axioms.addConstraintConstantsCover(con);

		// Disjointness between predicates
		for (List<String> disj : axioms.setsDisjoint) 
			uber.axioms.addConstraintDisjoint(disj.get(0), disj.get(1));		
		for (List<String> disj : other.axioms.setsDisjoint) 
			uber.axioms.addConstraintDisjoint(disj.get(0), disj.get(1));
		
		
		for (Formula f : axioms.otherAxioms) 
			uber.axioms.addConstraintFormula(f);		
		for (Formula f : other.axioms.otherAxioms) 
			uber.axioms.addConstraintFormula(f);
					
		for (String con : axioms.funcPartial) {
			if (shared.contains(con))
				if (!other.axioms.funcPartial.contains(con))
					throw new MGECombineVocabs(
							"Partial func constraint missing between vocabs -> "+con);
			uber.axioms.addConstraintPartialFunction(con);
		}
		for (String con : other.axioms.funcPartial) {
			if (shared.contains(con))
				if (!axioms.funcPartial.contains(con))
					throw new MGECombineVocabs(
							"Partial func constraint missing between vocabs <- "+con);
			uber.axioms.addConstraintPartialFunction(con);
		}

		for (String con : axioms.funcTotal) {
			if (shared.contains(con))
				if (!other.axioms.funcTotal.contains(con))
					throw new MGECombineVocabs(
							"Total func constraint missing between vocabs -> "+con);
			uber.axioms.addConstraintTotalFunction(con);
		}
		for (String con : other.axioms.funcTotal) {
			if (shared.contains(con))
				if (!axioms.funcTotal.contains(con))
					throw new MGECombineVocabs(
							"Total func constraint missing between vocabs <- "+con);
			uber.axioms.addConstraintTotalFunction(con);
		}
		
		for (String con : axioms.relTotal) {
			if (shared.contains(con))
				if (!other.axioms.relTotal.contains(con))
					throw new MGECombineVocabs(
							"Total relation constraint missing between vocabs -> "+con);
			uber.axioms.addConstraintTotalRelation(con);
		}
		for (String con : other.axioms.relTotal) {
			if (shared.contains(con))
				if (!axioms.relTotal.contains(con))
					throw new MGECombineVocabs(
							"Total relation constraint missing between vocabs <- "+con);
			uber.axioms.addConstraintTotalRelation(con);
		}
		
		/////////////////////////////////////
		// Finally, make sure that the term types known by both vocabs are shared by the uber-vocab:
		for(Expression e : exprToTerm.keySet())
		{
			uber.exprToTerm.put(e, exprToTerm.get(e));
		}
		for(Expression e: other.exprToTerm.keySet())
		{
			uber.exprToTerm.put(e, other.exprToTerm.get(e));
		}
		
		
		
		return uber;
	}
	
	public boolean writeAsDOT(String filename)
	{
		// Write out the vocabulary as a DOT file for GraphViz.
		// Potential issues in future: In particular, only replacing "-" and "/", may be other forbidden characters...

		// There is also the problem of displaying LARGE vocabularies (image is too big for graphviz) 
		
		try
		{
			BufferedWriter out = new BufferedWriter(new FileWriter(filename));
			String eol = System.getProperty("line.separator");
		
			
			out.write("digraph vocab {"+eol);
			
			String strSortNames = "";
			String strNonAbstractArrows = "";
			String strAbstractPoints = "";
			String strAbstractArrows = "";
			for(MSort aSort : sorts.values())
			{
				String aSortName = aSort.name.replace("-", "DASH").replace("/", "FSLASH");				
				
				
				strSortNames += " node [label=\""+aSort.name+"\" shape=rectangle];"+aSortName+";"+eol;
				
				if(axioms.setsAbstract.contains(aSort.name) && aSort.subsorts.size() > 0)
				{
					strAbstractPoints += "node [shape = point label = \"\"]; "+aSortName+"_Abstract;"+eol;
					
					strAbstractArrows += aSortName +"->" +aSortName+"_Abstract [arrowtail=crow dir = back arrowhead=none]; "+eol;
					for(MSort aChild : aSort.subsorts)			
					{
						String aChildName = aChild.name.replace("-", "DASH");
						strAbstractArrows += aSortName+"_Abstract -> "+aChildName+";"+eol;
					}
				}
				else
				{
					for(MSort aChild : aSort.subsorts)
					{
						String aChildName = aChild.name.replace("-", "DASH").replace("/", "FSLASH");
						strNonAbstractArrows += aSortName +" -> "+aChildName+";"+eol;
					}
				}
			}
			
				
			// Declare nodes for each sort
			out.write(eol+strSortNames+eol);
			
			// For every non-abstract sort, just declare the arrows
			out.write(eol+strNonAbstractArrows+eol);
			
			// Add a point node for each abstract sort w/ children
			// and "crow" edge to the point for each abstract sort
			// and declare arrows from that point
			out.write(eol+strAbstractPoints+eol);
			out.write(eol+strAbstractArrows+eol);
			out.write("}"+eol);
			
			out.flush();
			out.close();
		}
		catch(IOException e)
		{
			MEnvironment.errorWriter.println(e);
			// don't do this. would go through System.err
			//e.printStackTrace();
			return false;
		}
		
		return true;
	}

	/**
	 * Given a set of sort names, asserts the internal sort ordering 
	 * and returns the set as a list.
	 * @param theSet
	 * @return A list of sorts, topologically sorted.
	 */
	public List<String> assertSortOrdering(Set<String> theSet) 
	{
		List<String> result = new ArrayList<String>(theSet.size());
		
		// BFS through the sort hierarchy, adding sorts from theSet as they match.
		
		List<MSort> todo = new ArrayList<MSort>();
		Set<MSort> done = new HashSet<MSort>();
		
		// Start with the top-level sorts.
		for(MSort aSort : sorts.values())
			if(aSort.parents.size() == 0)
				todo.add(aSort);
		
		while(todo.size() > 0)
		{
			MSort current = todo.get(0);
			todo.remove(0);
			done.add(current);
			
			// This is the correct position (or at least "a" correct position)
			// since there are no cycles in the sort hierarchy
			
			if(theSet.contains(current.name))
				result.add(current.name);
			
			for(MSort aSort : current.subsorts)
				if(!done.contains(aSort))
				{
					todo.add(aSort);
					done.add(aSort); // don't do more than once
				}
		}
		
		return result;
	}

	static public void unitTest() throws MBaseException
	{
		MEnvironment.writeErrLine("----- Begin MVocab Tests (No messages is good.) -----");
		
		/////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////
		// assertSortOrdering
		// Need to allow multiple valid results since the hierarchy is a poset
		// some non-determinism for incomparable sorts due to internal Set representation.
		MVocab voc = new MVocab();
		
		voc.addSort("a");
		voc.addSort("b");
		
		Set<String> testSet = new HashSet<String>();
		testSet.add("a"); testSet.add("b");
		
		Set<String> validResults = new HashSet<String>();
		validResults.add("[a, b]"); validResults.add("[b, a]");
		
		if(!validResults.contains(voc.assertSortOrdering(testSet).toString()))
		{
			MEnvironment.errorWriter.println("********** MVocab test 1: FAILED!");
		}
		
		voc.addSubSort("a", "c");
		testSet.clear();
		testSet.add("a"); testSet.add("c"); testSet.add("b");
		validResults.clear();
		validResults.add("[a, b, c]"); validResults.add("[b, a, c]");
		
		if(!validResults.contains(voc.assertSortOrdering(testSet).toString()))
		{
			MEnvironment.errorWriter.println("********** MVocab test 2: FAILED!");
		}
		
		/////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////
		// Disjointness:
		// getConciseDisjointSorts
		// getConciseDisjointSortsAsymm
		
		voc = new MVocab();
		voc.addSort("User");
		voc.addSubSort("User", "Faculty");
		voc.addSubSort("User", "Student");
		MSort Faculty = voc.getSort("Faculty");
		MSort Student = voc.getSort("Student");
		MSort User = voc.getSort("User");
		
		// Since Faculty and Student do not contain a lower bound, 
		// and they are incomparable, vocab should make them disjoint.
		if(!voc.getConciseDisjointSorts(Faculty).contains(Student) ||
				!voc.getConciseDisjointSorts(Student).contains(Faculty))
		{
			MEnvironment.errorWriter.println("********** MVocab test 3: FAILED!");
		}
		
		// But neither of them is disjoint from their parent.
		if(voc.getConciseDisjointSorts(Faculty).contains(User) ||
				voc.getConciseDisjointSorts(User).contains(Faculty))
		{
			MEnvironment.errorWriter.println("********** MVocab test 4: FAILED!");
		}
		
		// Now we add a lower bound. Making sure the LB is not immediate:
		voc.addSubSort("Faculty", "DeptChairs");
		voc.addSubSort("Student", "GradStudents");		
		voc.addSubSort("DeptChairs", "LB");
		voc.addSubSort("GradStudents", "LB");
		
		if(voc.getConciseDisjointSorts(Faculty).contains(Student) ||
				voc.getConciseDisjointSorts(Student).contains(Faculty))
		{
			MEnvironment.errorWriter.println("********** MVocab test 4: FAILED!");
		}
						
		//voc.writeAsDOT("C:\\foo.dot");
		
		/////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////
		
		// Test makeTermFromExpression and function term's extension constructor
		MTerm constant = MTerm.makeTermFromExpression(MFormulaManager.makeRelation("c", 1));
		MTerm variable = MTerm.makeTermFromExpression(MFormulaManager.makeVariable("x"));
		List<MTerm> lst = new ArrayList<MTerm>();
		lst.add(constant);
		MFunctionTerm functerm1 = new MFunctionTerm("f", lst);
		lst.add(variable);
		MFunctionTerm functerm2 = new MFunctionTerm("f", lst);
		MFunctionTerm functerm2a = new MFunctionTerm(functerm1, variable); // extension
		
		if(!functerm2.equals(functerm2a))
		{
			MEnvironment.errorWriter.println("********** MVocab test 5: FAILED!");
		}
		if(!functerm2.equals(MTerm.makeTermFromExpression(functerm2.expr)))
		{
			MEnvironment.errorWriter.println("********** MVocab test 6: FAILED!");
		}	
		
		lst.clear();		
		lst.add(functerm2);
		lst.add(constant);
		MFunctionTerm functerm3 = new MFunctionTerm("g", lst);		
		if(!functerm3.equals(MTerm.makeTermFromExpression(functerm3.expr)))
		{
			MEnvironment.errorWriter.println("********** MVocab test 7: FAILED!");
		}	
		
		
		
		MEnvironment.writeErrLine("----- End MVocab Tests -----");	
	}
}

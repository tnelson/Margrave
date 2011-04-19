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
//import java.lang.management.ManagementFactory;
//import java.lang.management.ThreadMXBean;
import java.util.*;

import kodkod.ast.*;


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
}

abstract class MTerm
{	
	Expression expr;
	Set<Variable> seenVariables = new HashSet<Variable>();
	Set<Relation> seenRelations = new HashSet<Relation>();
	
	abstract public String toString();
}

class MFunctionTerm extends MTerm
{
	String funcName;
	List<MTerm> subTerms;
	
	MFunctionTerm(String funcName, List<MTerm> subTerms)
	{
		this.funcName = funcName;
		this.subTerms = subTerms;
		
		Relation funcRel = MFormulaManager.makeRelation(funcName, subTerms.size());
		
		this.seenRelations.add(funcRel);
		
		this.expr = funcRel;
		
		for(MTerm child : subTerms)			
		{
			this.seenRelations.addAll(child.seenRelations);
			this.seenVariables.addAll(child.seenVariables);
			
			// is this in the right order? TODO
			this.expr = this.expr.join(child.expr);
		}				
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
}

class MConstant
{
	String name;
	Relation rel;
	MSort type;
	
	public String toString()
	{
		return name;
	}
	
	MConstant(String name, Relation rel, MSort type)
	{
		this.name = name;
		this.rel = rel;
		this.type = type;
	}
}

class MFunction
{
	String name;
	Relation rel;
	List<MSort> arity;
	MSort type;
	
	public String toString()
	{
		return name;
	}
	
	MFunction(String name, Relation rel, List<MSort> arity, MSort type)
	{
		this.name = name;
		this.rel = rel;
		this.type = type;
		this.arity = arity;
	}
	
}

class MPredicate
{
	String name;
	Relation rel;
	List<MSort> type;
	
	public String toString()
	{
		return name;
	}
	
	MPredicate(String name, Relation rel, List<MSort> type)
	{
		this.name = name;
		this.rel = rel;
		this.type = type;
	}
}

/**
 * MVocab
 * effectively describes the domain of discourse.
 */
public class MVocab {

	// Sort symbols
	HashMap<String, MSort> sorts;

	// Constant symbols
	HashMap<String, MConstant> constants;
	
	// Function symbols
	HashMap<String, MFunction> functions;
	
	// Non-sort predicate symbols
	HashMap<String, MPredicate> predicates;	

	// Subset of the predicate symbols that can be used as IDBs in a policy
	HashSet<String> decisions;
	
	// More constraints on the domain of discourse
	public MConstraints axioms;

	public MVocab()
	{

		sorts = new HashMap<String, MSort>();
		predicates = new HashMap<String, MPredicate>();
		constants = new HashMap<String, MConstant>();
		functions = new HashMap<String, MFunction>();
		
		decisions = new HashSet<String>();

		axioms = new MConstraints(this);
	}

	String validateIdentifier(String n, boolean substitute)
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
			// Must be lower-case
			n = n.toLowerCase();
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
		name = validateIdentifier(name, true);
		if (!isSort(name)) // no dupes
			sorts.put(name, new MSort(name));
	}

	public void addSort(String name, String childnames)
			throws MGEUnknownIdentifier, MGEBadIdentifierName
			{

		// We declare that there is a type "name" and types 
		//(each child name, separated by whitespace)
		// And that name is a parent of all such.
		name = validateIdentifier(name, true);

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
			cname = validateIdentifier(cname, true); 
			
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

	protected void addSingleTopLevelSort(String name)
			throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// Creates a new sort and makes it the parent of all parentless types
		// used in tupling

		name = validateIdentifier(name, true);
		if (!isSort(name))
			sorts.put(name, new MSort(name));
		MSort t = getSort(name);

		for (MSort candidate : sorts.values())
		{
			if (!isSubtype(candidate) && !(candidate.name.equals(name)))
			{
				t.subsorts.add(candidate);
				candidate.parents.add(t);
			}
		}
	}

	MSort getSort(String n) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		n = validateIdentifier(n, true); 
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

		prefix = validateIdentifier(prefix, true);
		
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
			throws MGEBadIdentifierName, MGEUnknownIdentifier {
		parentName = parentName.toLowerCase();
		name = name.toLowerCase();

		name = validateIdentifier(name, true);

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
		
		name = validateIdentifier(name, true);
		
		if (predicates.keySet().contains(name))
			return; // already have a pred named this

		name = validateIdentifier(name, true);

		String[] arityArr = typeconstruct.split(" ");
		List<MSort> arity = new ArrayList<MSort>();
		for(String s : arityArr)
			arity.add(getSort(s));
	
		Relation theRel = MFormulaManager.makeRelation(name, arity.size());
		
		MPredicate thePred = new MPredicate(name, theRel, arity);
		
		predicates.put(name, thePred);
	}

	public void addDecision(String d) throws MGEBadIdentifierName
	{
		d = validateIdentifier(d, false);
		
		d = d.toLowerCase(); // TODO validateIdentifier is being called too much, maybe really should be 2 diff funcs?
		
		decisions.add(d);
		// The MGPolicy object is responsible for initializing the actual IDBs.
	}

	Relation getRelation(String rname) throws MGEUnknownIdentifier, MGEBadIdentifierName 
	{
		rname = validateIdentifier(rname, true);
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

	private Formula getPredTypeFormula(String relname)
			throws MGEUnknownIdentifier, MGEBadIdentifierName 
	{
		// Accepts a relation name
		// Returns a Formula expressing constraints on the parent relation
		// This method works only for non-type EDB predicates, not types
		// themselves
		
		MPredicate thePred = predicates.get(relname); 		

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

	
	Set<Formula> getDisjointness(MSort t)
	throws MGEUnknownIdentifier
	{		

		// TODO
		
		if(disjoints == null || disjoints.size() < 1)
			return new HashSet<Formula>();

		Iterator<MSort> it = disjoints.iterator();
		Expression unions = it.next().rel;

		while(it.hasNext())
			unions = unions.union(it.next().rel);

		HashSet<Formula> results = new HashSet<Formula>();
		results.add(t.rel.intersection(unions).no());
		return results;
	}
	
	Formula getFixedAxiomFormula() throws MGEUnknownIdentifier, MGEBadIdentifierName
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
		Expression univunion = Expression.NONE;
		for(MSort aSort : sorts.values())
		{
			if(!isSubtype(aSort))
				univunion = univunion.union(aSort.rel);
		}
		axiomSet.add(Expression.UNIV.in(univunion));		

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
		for (MPredicate aPred : predicates.values())
			axiomSet.add(getPredTypeFormula(aPred.name));		

		return MFormulaManager.makeConjunction(axiomSet); // .accept(new SimplifyFormulaV());
	}

	Set<Formula> getUserAxiomFormulas() throws MGEUnknownIdentifier,
			MGEArityMismatch, MGEBadQueryString, MGEManagerException, MGEBadIdentifierName {
		// Returns a set of formulas asserting sig restrictions
		// AXIOMS for our domain of discourse that will be added to the query
		// passed to KodKod. These axioms can affect the shape of the
		// optimized query signature, and so must be obtained before
		// optimization.

		Set<Formula> results = new HashSet<Formula>();

		// (6) User-defined axioms -- things like "lone R", "A disjoint B", etc.
		results.addAll(axioms.getConstraintFormulas());

		// This restriction is necessary because other constraint strings may
		// reference IDBs and such.
		if (axioms.otherConstraintStrings.size() > 0)
			throw new MGEBadQueryString(
					"Axioms may not contain `other constraint' strings.");

		return results;
	}


	Formula makeFunctionalFormula(Relation r, String type)
	throws MGEUnknownIdentifier, MGEArityMismatch, MGEBadIdentifierName, MGEManagerException
	{
		// Used to do forall x, y, z (x.y.z).R.one()  (where R is 4-ary)
		// Changed to forall x, y, z, exists r | (x y z r) in R and forall k ((x y z k) in R implies k=r)
		// This is longer, but complies with current FormulaSigInfo

		if (!predtypes.containsKey(r.name()))
			throw new MGEUnknownIdentifier(
					"Unable to find definition for predicate: " + r.name());

		String[] rels = predtypes.get(r.name()).split(" ");

		if (rels.length < r.arity())
			throw new MGEArityMismatch(
					"Arity mismatch between expected for relation " + r.name()
							+ ".");
		
				
		List<Decl> quants = new ArrayList<Decl>(); // x through z
		List<Variable> tuple1vars = new ArrayList<Variable>();
		List<Variable> tuple2vars = new ArrayList<Variable>();
		Expression tuple1 = Expression.NONE; // (x, y, ..., z, r_1)
		Expression tuple2 = Expression.NONE; // (x, y, ..., z, r_2)
		
		int x_counter = r.arity() - 1; 
		
		while(x_counter > 0)
		{
			Variable tempvar = MFormulaManager.makeVariable(r.name() + "_"
					+ type + "_" + x_counter);
			
			// "forall xi^Ai"
			quants.add( MFormulaManager.makeOneOfDecl(tempvar, getRelation(rels[x_counter - 1])));
			
			tuple1vars.add(tempvar);
			tuple2vars.add(tempvar);
			
			x_counter--;
		}

		Variable fc1 = MFormulaManager.makeVariable(r.name()+"FC1");
		Variable fc2 = MFormulaManager.makeVariable(r.name()+"FC2");
		tuple1vars.add(fc1);
		tuple2vars.add(fc2);
		tuple1 = MFormulaManager.makeVarTupleV(tuple1vars);
		tuple2 = MFormulaManager.makeVarTupleV(tuple2vars);
		
		// Tuples and outside quantification now built.

		Decl dfc1 = MFormulaManager.makeOneOfDecl(fc1, getRelation(rels[r.arity()-1]));
		Decl dfc2 = MFormulaManager.makeOneOfDecl(fc2, getRelation(rels[r.arity()-1]));
		
		Formula f;
		Formula pt1, pt2;
		
		if("T".equals(type))
		{
			// EXISTS fc1 ((tuple1) in R) and (FORALL fc2 (tuple2 in R implies fc1=fc2)) 
			pt1 = MFormulaManager.makeAtom(tuple1, r);
			
			pt2 = MFormulaManager.makeAtom(tuple2, r);
			pt2 = MFormulaManager.makeIFF(pt2, MFormulaManager.makeEqAtom(fc1, fc2));
			pt2 = MFormulaManager.makeForAll(pt2, dfc2);
			
			f = MFormulaManager.makeAnd(pt1, pt2);
			f = MFormulaManager.makeExists(f, dfc1);
		}
		else if("P".equals(type))
		{
			// FORALL fc1 FORALL fc2 ( (tuple1 in R) and (tuple2 in R) iff fc1=fc2)
			
			pt1 = MFormulaManager.makeAtom(tuple1, r);
			pt2 = MFormulaManager.makeAtom(tuple2, r);
			
			f = MFormulaManager.makeAnd(pt1, pt2);
			f = MFormulaManager.makeIFF(f, MFormulaManager.makeEqAtom(fc1, fc2));
			
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
		for(Decl d : quants)
		{
			f = MFormulaManager.makeForAll(f, d);
		}
		
		//MEnvironment.errorStream.println(r + " "+ type);
		//MEnvironment.errorStream.println(f);
		
		return f; 		
	}
	
	protected boolean isAllDecs(List<String> strs)
	{
		List<String> lcstrs = new ArrayList<String>();
		for (String s : strs)
			lcstrs.add(s.toLowerCase());
		
		return decisions.containsAll(lcstrs) && lcstrs.containsAll(decisions);
	}

	protected boolean isAllDecs(String[] strs) {
		// Is the array given a list of all decisions?
		return isAllDecs(Arrays.asList(strs));
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

	public boolean isSubOf(String sub, String sup) throws MGEUnknownIdentifier, MGEBadIdentifierName {
		MSort subt = getSort(sub);
		MSort supt = getSort(sup); // trigger name check

		// This is SYNTACTIC SUBSORTING:
		// On the poset of sorts, is sup reachable from sub?
		// subSET constraints are NOT INCLUDED.
		if (subsortClosureOf(supt).contains(subt))
			return true;
		return false;
	}

	public Set<MSort> subsortClosureOf(MSort t) 
	{
		// Do not take subSET constraints into consideration: only use the sort tree.
		
		List<MSort> tocheck = new LinkedList<MSort>();
		Set<MSort> results = new HashSet<MSort>();
		tocheck.add(t);

		while (tocheck.size() > 0) {
			MSort current = tocheck.get(0);
			tocheck.remove(0);
			results.add(current);
			tocheck.addAll(current.subsorts);
		}

		return results;
	}

	public static String constructAdornment(BinaryExpression be,
			HashMap<Variable, String> sortenv)
	{
		List<String> lst = inorderTraversalOfVariableProduct(be, null, sortenv);
		if (lst.size() < 1)
			return "";

		String result = lst.get(0);
		for (int ii = 1; ii < lst.size(); ii++)
			result += " " + lst.get(ii);

		return result;
	}

	public static String constructIndexing(BinaryExpression be,
			HashMap<Variable, Integer> indexing) {
		List<String> lst = inorderTraversalOfVariableProduct(be, indexing, null);
		if (lst.size() < 1)
			return "";

		String result = lst.get(0);
		for (int ii = 1; ii < lst.size(); ii++)
			result += "," + lst.get(ii);
		return result;
	}
	
	public static String constructIndexing(List<String> vars,
			HashMap<Variable, Integer> indexing)
	{		
		if (vars.size() < 1)
			return "";

		Variable theVar = MFormulaManager.makeVariable(vars.get(0));
		String result = String.valueOf(indexing.get(theVar));
		for (int ii = 1; ii < vars.size(); ii++)
		{
			theVar = MFormulaManager.makeVariable(vars.get(ii));
			result += "," + String.valueOf(indexing.get(theVar));
		}
		return result;
	}	

	public static List<String> constructVarNameList(BinaryExpression be) {
		return inorderTraversalOfVariableProduct(be, null, null);
	}

	private static List<String> inorderTraversalOfVariableProduct(
			BinaryExpression be, HashMap<Variable, Integer> indexing,
			HashMap<Variable, String> sortenv)
	{
		// DFS this expression. Assume either a BinaryExpression node or a
		// Variable. Otherwise yell.
		List<String> sort_result = new ArrayList<String>();
		List<String> index_result = new ArrayList<String>();
		List<String> varname_result = new ArrayList<String>();

		List<Expression> dfslist = new LinkedList<Expression>();
		dfslist.add(be.left());
		dfslist.add(be.right());

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

			else
				return new ArrayList<String>(); // will warn user
		} // while there remain DFS nodes to explore

		if (sortenv != null)
			return sort_result;
		if (indexing != null)
			return index_result;
		return varname_result;

	}

	protected Set<MSort> buildSubSortSet(MSort t)
	{
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

		return result;
		
	}
		
	protected Set<MSort> buildSuperSortSet(MSort t)
			throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
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
			MGEBadIdentifierName, MGEUnknownIdentifier {
		
		// Names (strings) used to detect equivalence (since object references
		// are obviously untrustworthy.)
		
		// Shared predicates must have the same signature 
		for (String pname : predicates.keySet())
			if (other.predicates.keySet().contains(pname))
				if (predtypes.get(pname).compareTo(other.predtypes.get(pname)) != 0)
					throw new MGECombineVocabs(
							"Different custom predicate signature: " + pname);

		// Generate a list of shared type names
		Set<String> shared = new HashSet<String>();
		for (MSort t : sorts.values())
		{
			try
			{
				// Is this a type name that both vocabs use?
				MSort t2 = other.getSort(t.name);

				shared.add(t.name);
				
			} catch (MGEUnknownIdentifier e) {
			}
			
		}

		// Combine everything and return the result.
		MVocab uber = new MVocab(vocab_name + "+" + other.vocab_name);

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

		// Decisions
		for (String d : decisions)
			uber.addDecision(d);
		for (String d : other.decisions)
			uber.addDecision(d);
		
		// Request vars IN ORDER
		for (Variable v : requestVectorOrder)
			uber.addRequestVar(v.name(), requestVarDomains.get(v.name()).name());
		for (Variable v : other.requestVectorOrder)
			if(!uber.requestVariables.containsKey(v.name()))
				uber.addRequestVar(v.name(), other.requestVarDomains.get(v.name()).name());		

		// Predicates and Predtypes
		for (String d : predicates.keySet())
			uber.addPredicate(d, predtypes.get(d));
		for (String d : other.predicates.keySet())
			uber.addPredicate(d, other.predtypes.get(d));

		// Declared other vars (if same type, it's ok.)
		for (String ov : otherVarDomains.keySet())
		{
			if (other.otherVarDomains.containsKey(ov))
			{
				if (!other.otherVarDomains.get(ov).name().equals(otherVarDomains.get(ov).name()))
					throw new MGECombineVocabs("Declared rule-scope variable type mismatch: "+
							other.otherVarDomains.get(ov).name() +
							" vs. "+
							otherVarDomains.get(ov).name());
			}
			uber.addOtherVar(ov, otherVarDomains.get(ov).name());
		}
		for (String ov : other.otherVarDomains.keySet()) {
			if (!otherVarDomains.containsKey(ov))
				uber.addOtherVar(ov, other.otherVarDomains.get(ov).name());
		}

		// Other variables and request variables must be disjoint
		Set<String> testSet = new HashSet<String>(uber.requestVariables.keySet());
		testSet.retainAll(uber.otherVarDomains.keySet());
		if(!testSet.isEmpty())
		{			 
			throw new MGEVariableAlreadyBound(testSet, 
					"Request (free) variables and ``other'' (bound) variables in all a query's vocabularies must never overlap. "+ 
					" The vocabularies "+this.vocab_name+" and "+other.vocab_name+" disagreed on whether the variables "+testSet+" were free or bound."+
					" For more information, see the ``Substitution'' section of the documentation.");
		}
		
		
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

		// Disjointness
		//MEnvironment.errorStream.println("Beginning disj helper.");
		combineDisjHelper(uber, this, other, shared);		
		// used to need 2nd call, but no longer
		//combineDisjHelper(uber, other, this, shared);
		//MEnvironment.errorStream.println("Ending disj helper.");
		
		return uber;
	}

	private static void checkSharedDisjointness(MVocab A, MVocab B, Set<String> shared)
	 throws MGECombineVocabs, MGEUnknownIdentifier, MGEBadIdentifierName
	 {
		// Don't need to loop through ALL sorts, just the ones that have disj constraints.
		
		// Note: If A asserts a disj over shared sorts that B does not, BUT
		// this assertion is due to non-shared sort interaction, 
		// we don't complain.
		
		for(MSort key : A.axioms.axiomDisjoints.keySet())
		{
			// this sort isn't shared, never any possible disagreement!
			if(!shared.contains(key.name)) 
				continue;
			
			for(MSort val : A.axioms.axiomDisjoints.get(key))			
			{
				
				// this sort isn't shared, never any possible disagreement!
				if(!shared.contains(val.name))
					continue;
				
				MSort b_key = B.getSort(key.name);
				MSort b_val = B.getSort(val.name);
				
				// A asserts that val and key are disjoint.
				// B needs to assert the same thing, or we have a problem.
				// (but B may assert it indirectly!)
				if(B.possibleOverlap(b_key, b_val))
				{
					throw new MGECombineVocabs(
							"Disjointness constraint not agreed upon between vocabs: "
							+ key.name + " and " + val.name);
				}					
			}
		}
	 }
	
	private static void addDisjsToSet(MVocab uber, MVocab voc, HashMap<MSort, Set<MSort>> new_ax_disjoints) 
	throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// Copy all of voc's axiom_disjoints.
		// (Can't do a copy of the set contents, since one vocab's MGSort isn't valid in another vocab.)
		
	
		for(MSort key : voc.axioms.axiomDisjoints.keySet())
		{
			MSort u_key = uber.getSort(key.name);
			
			if(!new_ax_disjoints.containsKey(u_key))
				new_ax_disjoints.put(u_key, new HashSet<MSort>());
			
			for(MSort val : voc.axioms.axiomDisjoints.get(key))
			{
				MSort u_val = uber.getSort(val.name);
				new_ax_disjoints.get(u_key).add(u_val);
			}				
		}		
	}
	
	private static void combineDisjHelper(MVocab uber, MVocab A, MVocab B, Set<String> shared)
	throws MGECombineVocabs, MGEUnknownIdentifier, MGEBadIdentifierName
	{
	//	ThreadMXBean mxBean = ManagementFactory.getThreadMXBean();
		//long start = mxBean.getCurrentThreadCpuTime();		
		
		//MEnvironment.errorStream.println("Checking shared disj.");
		// Make sure A -> B in shared
		checkSharedDisjointness(A, B, shared);
		// Make sure B -> A in shared
		checkSharedDisjointness(B, A, shared);

//		MEnvironment.errorStream.println("Time (ms) to checkSharedDisj both directions (voc comb): "+ (mxBean.getCurrentThreadCpuTime() - start) / 1000000);
	//	start = mxBean.getCurrentThreadCpuTime();

		
		// uber contains all the sorts that A and B do, regardless of whether both have them
		// So uber needs to have all the disjoint constraints from both.

		//MEnvironment.errorStream.println("Copying disj info.");
		
		// construct a new disjoints set for uber.
		HashMap<MSort, Set<MSort>> new_ax_disjoints = new HashMap<MSort, Set<MSort>>();
			
		//MEnvironment.errorStream.println(A.sorts);		
		//MEnvironment.errorStream.println(A.axioms.axiom_disjoints.keySet());
		// eh? _1 under one?
		
		
		addDisjsToSet(uber, A, new_ax_disjoints);
		addDisjsToSet(uber, B, new_ax_disjoints);		
		
		uber.axioms.axiomDisjoints = new_ax_disjoints;
		
		//MEnvironment.errorStream.println("Time (ms) to adddisjsToSet (voc comb): "+ (mxBean.getCurrentThreadCpuTime() - start) / 1000000);
		//start = mxBean.getCurrentThreadCpuTime();
		

	}

	public boolean writeAsDOT(String filename)
	{
		// Write out the vocabulary as a DOT file for GraphViz.
		// Not quite done yet! (TODO?)
		// In particular, only replacing "-" and "/", may be other forbidden characters...

		// There is also the problem of displaying LARGE vocabularies (image is too big for graphviz) 
		
		try
		{
			BufferedWriter out = new BufferedWriter(new FileWriter(filename));
			String eol = System.getProperty("line.separator");
			
			String safeVocabName = vocab_name.replace(" ", "_");
			
			
			out.write("digraph "+safeVocabName+" {"+eol);
			
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
	 * @return 
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
		
		// assertSortOrdering
		// Need to allow multiple valid results since the hierarchy is a poset
		// some non-determinism for incomparable sorts due to internal Set representation.
		MVocab voc = new MVocab("test");
		
		voc.addSort("a");
		voc.addSort("b");
		
		Set<String> testSet = new HashSet<String>();
		testSet.add("a"); testSet.add("b");
		
		Set<String> validResults = new HashSet<String>();
		validResults.add("[a, b]"); validResults.add("[b, a]");
		
		if(!validResults.contains(voc.assertSortOrdering(testSet).toString()))
		{
			MEnvironment.errorWriter.println("MVocab test 1: FAILED!");
		}
		
		voc.addSubSort("a", "c");
		testSet.clear();
		testSet.add("a"); testSet.add("c"); testSet.add("b");
		validResults.clear();
		validResults.add("[a, b, c]"); validResults.add("[b, a, c]");
		
		if(!validResults.contains(voc.assertSortOrdering(testSet).toString()))
		{
			MEnvironment.errorWriter.println("MVocab test 2: FAILED!");
		}
		
		
		
		// TODO more tests for other funcs
		
		MEnvironment.writeErrLine("----- End MVocab Tests -----");	
	}
}

/*
  	Copyright 2009 Brown University and Worcester Polytechnic Institute.
    
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
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.util.*;

import kodkod.ast.*;
import kodkod.ast.operator.ExprOperator;
import kodkod.ast.visitor.*;

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
		System.out
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

class MSort
{
	String name;
	Relation rel;
	MSort parent; // if any

	// In parent/child tree -- other subset constraints go in MGConstraints
	// object.
	Set<MSort> subsorts;

	MSort(String n) {
		name = n;
		rel = MFormulaManager.makeRelation(n, 1);
		subsorts = new HashSet<MSort>();
		parent = null;
	}

	public String toString() {
		return name + "~" + rel.hashCode();
	}
}

/**
 * MGVocab class holds a lexicon of "placeholder" variables and relations. It
 * effectively describes the domain of discourse.
 */
public class MVocab {

	// Caller creates this and fills it before creating the policy.
	// Then caller passes it to new policies.

	String vocab_name;

	// Some types of input need special treatment.
	// It is conceivable that both flags may be set (if people are comparing
	// policies that use both)
	public boolean bUsesXACML;
	public boolean bUsesIOS;

	// KodKod Relation objects used by the policy.
	HashMap<String, MSort> sorts;

	HashMap<String, Relation> predicates;
	HashMap<String, String> predtypes;

	// KodKod Variables used in the Formulas.
	// requestVariables = s, a, r...
	HashMap<String, Variable> requestVariables;

	// Order of the variable place holders
	ArrayList<Variable> requestVectorOrder;

	// Constraints on the domain of discourse
	public MConstraints axioms;

	// "other" variables are rule-scope existentials. For instance
	// (Permit s) :- (Admin x) (Delegated* x s)
	// Users never come into contact with them.
	// So we store a mapping of the NAME to the sort they are in. But the rule
	// itself
	// handles creation of the variable object.
	HashMap<String, Relation> otherVarDomains;

	// Same for request vars
	HashMap<String, Relation> requestVarDomains;

	// List of possible decisions this policy can return.
	// For instance, an access control policy would have ["Permit", "Deny"]
	// This list does not include N/a, as that response is implied for all
	// policies.
	// Q: Do we need to distinguish N/a and EC?
	HashSet<String> decisions;

	public MVocab(String voc_name) {
		vocab_name = voc_name;
		bUsesXACML = false;
		bUsesIOS = false;

		sorts = new HashMap<String, MSort>();
		predicates = new HashMap<String, Relation>();
		predtypes = new HashMap<String, String>();

		requestVariables = new HashMap<String, Variable>();
		requestVarDomains = new HashMap<String, Relation>();
		requestVectorOrder = new ArrayList<Variable>();

		// Allows the user to give a type for rule-scope variables -- this
		// gives great benefit in H.U. calculation.
		otherVarDomains = new HashMap<String, Relation>();

		decisions = new HashSet<String>();

		axioms = new MConstraints(voc_name + "_axioms", this);
	}

	MSort getUniverseSort(MSort t) {
		if (t.parent == null)
			return t;
		MSort temp = t;
		while (temp.parent != null)
			temp = temp.parent;
		return temp;
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

	public String getExpectedRequestVarOrder()
	{
		// Return a string that gives the expected order for request vars.
		String result = "";
		for (Variable v : requestVectorOrder)
			result += v.name() + " ";

		// DO NOT trim trailing whitespace
		return result;
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

		// We declare that there is a type "name" and types (each child name,
		// separated by whitespace)
		// And that name is a parent of all such.
		name = validateIdentifier(name, true);

		if (!isSort(name))
			sorts.put(name, new MSort(name));

		// don't add an empty-named type!
		if (childnames.length() < 1)
			return;

		MSort t = getSort(name);
		String[] cnames = childnames.split("\\s");

		for (String cname : cnames) {
			
			cname = validateIdentifier(cname, true); 
			
			if (!isSort(cname))
				sorts.put(cname, new MSort(cname));

			MSort c = getSort(cname);
			if (!t.subsorts.contains(c))
				t.subsorts.add(c);
			MSort oldparent = c.parent;
			c.parent = t;

			if (searchForSelfAncestor(c)) {
				t.subsorts.remove(c);
				c.parent = oldparent;
				throw new MGEBadIdentifierName(
						"Cyclic type structure not allowed: " + cname);
			}
		}

	}

	protected void addSingleTopLevelSort(String name)
			throws MGEUnknownIdentifier, MGEBadIdentifierName {
		// Creates a new sort and makes it the parent of all parentless types

		name = validateIdentifier(name, true);
		if (!isSort(name))
			sorts.put(name, new MSort(name));
		MSort t = getSort(name);

		for (MSort candidate : sorts.values()) {
			if (!isSubtype(candidate) && !(candidate.name.equals(name))) {
				t.subsorts.add(candidate);
				candidate.parent = t;
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

		MSort oldparent = childtype.parent;

		childtype.parent = parenttype; // save time later
		parenttype.subsorts.add(childtype);

		if (searchForSelfAncestor(childtype)) {
			parenttype.subsorts.remove(childtype);
			childtype.parent = oldparent;
			throw new MGEBadIdentifierName(
					"Cyclic type structure not allowed: " + name);
		}
	}

	public void addPredicate(String name, String typeconstruct)
			throws MGEBadIdentifierName {
		// construct contains a String with relation names in it.
		// "A B C" means type of (A x B x C)
		name = validateIdentifier(name, true);
		typeconstruct = typeconstruct.toLowerCase();

		if (predicates.keySet().contains(name))
			return; // already have a pred named this

		name = validateIdentifier(name, true);

		predicates.put(name, MFormulaManager.makeRelation(name, typeconstruct
				.split(" ").length));

		// System.out.println(predicates.get(name) +
		// ": "+predicates.get(name).arity());
		predtypes.put(name, typeconstruct);
	}

	public void addRequestVar(String varname, String domain)
			throws MGEUnknownIdentifier, MGEBadIdentifierName {
		varname = varname.toLowerCase();
		domain = domain.toLowerCase();
		varname = validateIdentifier(varname, false);

		Variable newvar = MFormulaManager.makeVariable(varname);
		requestVariables.put(varname, newvar);
		requestVectorOrder.add(newvar);
		requestVarDomains.put(varname, getRelation(domain));
	}

	public void addOtherVar(String varname, String domain)
			throws MGEUnknownIdentifier, MGEBadIdentifierName {
		varname = varname.toLowerCase();
		domain = domain.toLowerCase();

		// No Variable added here: Will be added dynamically in rule parser

		varname = validateIdentifier(varname, false);
		otherVarDomains.put(varname, getRelation(domain));
	}

	public void addDecision(String d) throws MGEBadIdentifierName
	{
		d = validateIdentifier(d, false);
		
		d = d.toLowerCase(); // TODO validateIdentifier is being called too much, maybe really should be 2 diff funcs?
		
		decisions.add(d);
		// The MGPolicy object is responsible for initializing the actual IDBs.
	}

	Relation getRelation(String rname) throws MGEUnknownIdentifier, MGEBadIdentifierName {
		rname = validateIdentifier(rname, true);
		try {
			MSort t = getSort(rname);
			return t.rel;
		} catch (MGException E) {
			if (predicates.containsKey(rname))
				return predicates.get(rname);
			throw new MGEUnknownIdentifier("Error: Unable to get Relation for unknown sort name: " + rname);
		}
	}

	Variable getRequestVariable(String varname) throws MGEUnknownIdentifier {
		varname = varname.toLowerCase();
		if (requestVariables.containsKey(varname))
			return requestVariables.get(varname);

		throw new MGEUnknownIdentifier("Unknown variable name: " + varname);
	}

	private Formula getPredTypeFormula(String relname)
			throws MGEUnknownIdentifier, MGEBadIdentifierName {
		// Accepts a relation name
		// Returns a Formula expressing constraints on the parent relation
		// This method works only for non-type EDB predicates, not types
		// themselves
		Relation r = predicates.get(relname);
		String typeconstruct = predtypes.get(relname);

		Expression rin = Expression.NONE; // will be overwritten below

		String[] subrels = typeconstruct.split(" ");
		boolean initialized = false;
		for (String relSubName : subrels) {
			if (!initialized && isSort(relSubName)) {
				rin = getSort(relSubName).rel;
				initialized = true;
			} else if (isSort(relSubName))
				rin = rin.product(getSort(relSubName).rel);
			else
				throw new MGEUnknownIdentifier("Could not find type named "
						+ relSubName);
		}

		Formula f = r.in(rin);

		return f;
	}

	boolean isSubtype(MSort thetype) {
		// If this type has a parent...
		if (thetype.parent != null)
			return true;
		return false;
	}

	Formula getFixedAxiomFormula() throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// No longer require subsorts to exhaust their parents. (This is equivalent
		// to alloy's "abstract" keyword on the parent sorts.) It made the theory
		// troublesome, and users can always manually force such a constraint. 
		
		
		// Returns a formula stating "This is a model of order-sorted FoL over
		// these sorts and this ordering." 
				
		// Sub-sorts need not be mutually exclusive!

		// If using tupling:
		// Requires current vocab to be the tupled vocabulary, and
		// thus must be separated from user axioms which could affect
		// tupling.

		Set<Formula> axiomSet = new HashSet<Formula>();

		// (1) Specific to Margrave (not order-sorted FOL)
		//     Disjointness is safe: universals only
		//     
		// Top-level sorts partition the universe. They are always disjoint.
		Expression univunion = Expression.NONE; // safe default
		boolean firstuniv = true;
		for (MSort t : sorts.values()) {
			if (!isSubtype(t)) {
				if (firstuniv) {
					firstuniv = false;
					univunion = t.rel;
				} else
					univunion = univunion.union(t.rel);

				// fresh set for each...
				HashSet<MSort> others = new HashSet<MSort>();

				for (MSort t2 : sorts.values())
					if (!isSubtype(t2) && t != t2)
						others.add(t2);

				// for universe types, so this is indeed axiomatic
				axiomSet.addAll(axioms.getDisjointness(t, others));
			} // end if !subtype
		} // end for each type

		// (2)
		// Every atom is contained in some universe type.
		axiomSet.add(Expression.UNIV.in(univunion));

		// (3) All sorts contain their subsorts
		for (MSort basetype : sorts.values())
		{
			//Expression allsubs = Expression.NONE;
			
			// If there are subsorts...
			if (basetype.subsorts.size() > 0)
			{

				// All subsorts of this type are in it
				for (MSort subtype : basetype.subsorts) {
					Relation subd = subtype.rel;
					axiomSet.add(subd.in(basetype.rel));
					//allsubs = allsubs.union(subd);
				}

				//axiomSet.add(basetype.rel.in(allsubs));
			}

			// disjointness CONSTRAINTS on this type are handled in the
			// MGConstraints object
		}

		// 3(a): subSET constraints treated in the same way
		for(String child : axioms.setsSubset.keySet())
		{
			// TODO this procedure isn't using MGFormulaManager, but
			// these axioms should not be duplicated anyway.
			
			for(String parent : axioms.setsSubset.get(child))
				axiomSet.add(getRelation(child).in(getRelation(parent))); // in, not eq 
		}
		
		

		
		
		// (5)
		// State predicates have a sig which must be respected
		// e.g.: EdgePredicate in (Nodes x Nodes)
		for (Relation r : predicates.values()) {
			// We should have a type construct stored for this predicate
			// Break it down and express the constraints!
			axiomSet.add(getPredTypeFormula(r.name()));
		}

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
		else
		{
			// FORALL fc1 FORALL fc2 ( (tuple1 in R) and (tuple2 in R) iff fc1=fc2)
			
			pt1 = MFormulaManager.makeAtom(tuple1, r);
			pt2 = MFormulaManager.makeAtom(tuple2, r);
			
			f = MFormulaManager.makeAnd(pt1, pt2);
			f = MFormulaManager.makeIFF(f, MFormulaManager.makeEqAtom(fc1, fc2));
			
			f = MFormulaManager.makeForAll(f, dfc2);
			f = MFormulaManager.makeForAll(f, dfc1);
		}
		 
		// now close the foralls on the outside
		
		for(Decl d : quants)
		{
			f = MFormulaManager.makeForAll(f, d);
		}
		
		//System.out.println(r + " "+ type);
		//System.out.println(f);
		
		return f; 
		
		/*
		// ---- saved old code. Was NOT using MGFormulaManager.
		// For all of (Variable for each in LHS)
		Expression lhsreduce = Expression.NONE;
		Decls temp_quant = null;

		int lhs_counter = r.arity() - 1; // all but last is the LHS

		while (lhs_counter > 0) {
			// Create a variable for this part of the LHS
			Variable tempvar = MGFormulaManager.makeVariable(r.name() + "_"
					+ type + "_" + lhs_counter);

			if (temp_quant == null) {
				temp_quant = tempvar.oneOf(getRelation(rels[lhs_counter - 1]));
				lhsreduce = tempvar.join(r);
			} else {
				temp_quant = temp_quant.and(tempvar
						.oneOf(getRelation(rels[lhs_counter - 1])));
				lhsreduce = tempvar.join(lhsreduce);
			}

			lhs_counter--;
		}


		//System.out.println(lhsreduce); 
		  		if (type.equals("P"))
			return lhsreduce.lone().forAll(temp_quant);
		else
			return lhsreduce.one().forAll(temp_quant);
		 
		  */
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

	public MSort getMaximalSupersort(MSort t) {
		while (t.parent != null)
			t = t.parent;
		return t;
	}
	
	public boolean isSubOrSubOf(String sub, String sup) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		return isSubOrSubOf(getSort(sub), getSort(sup));
	}
	
	public boolean isSubOrSubOf(MSort sub, MSort sup) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		Set<MSort> supers = buildSuperSetSet(sub);
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

	public static List<String> constructVarNameList(BinaryExpression be) {
		return inorderTraversalOfVariableProduct(be, null, null);
	}

	private static List<String> inorderTraversalOfVariableProduct(
			BinaryExpression be, HashMap<Variable, Integer> indexing,
			HashMap<Variable, String> sortenv) {
		// DFS this expression. Assume either a BinaryExpression node or a
		// Variable. Otherwise yell.
		List<String> sort_result = new ArrayList<String>();
		List<String> index_result = new ArrayList<String>();
		List<String> varname_result = new ArrayList<String>();

		List<Expression> dfslist = new LinkedList<Expression>();
		dfslist.add(be.left());
		dfslist.add(be.right());

		while (dfslist.size() > 0) {
			Expression next = dfslist.get(0);
			dfslist.remove(0);

			// What does sortenv say the sort of this variable is?
			if (next instanceof Variable) {
				if (sortenv != null)
					sort_result.add(sortenv.get(next));
				if (indexing != null)
					index_result.add(indexing.get(next).toString());
				varname_result.add(next.toString());

			} else if (next instanceof BinaryExpression) {
				BinaryExpression benext = (BinaryExpression) next;
				dfslist.add(benext.left());
				dfslist.add(benext.right());
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

	protected Set<MSort> buildSuperSetSet(MSort t)
			throws MGEUnknownIdentifier, MGEBadIdentifierName {
		// Build the list of types that MUST be supersets of type t.
		// This is more or less the transitive closure of supertype and subset
		// constraints on t.

		List<MSort> todo = new LinkedList<MSort>();
		Set<MSort> result = new HashSet<MSort>();
		todo.add(t);

		while (todo.size() > 0) {
			// Pop todo, add to result
			MSort next = todo.get(0);
			todo.remove(0);
			result.add(next);

			// expand next, queue supers/supertypes not already dealt with or
			// already queued
			if (next.parent != null && !result.contains(next.parent)
					&& !todo.contains(next.parent))
				todo.add(next.parent);

			// indexed by CHILD
			if (axioms.setsSubset.containsKey(next.name))
				for (String supname : axioms.setsSubset.get(next.name)) {
					MSort sup = getSort(supname);
					if (!result.contains(sup) && !todo.contains(sup))
						todo.add(sup);
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
			throws MGEUnknownIdentifier, MGEBadIdentifierName {

		// Default to the SAFE decision: That the types are related.

		// Let sups1 be the set of necessary supersets of type 1, and sups2
		// similarly for type 2.
		// Then the types are only necessarily disjoint if:
		// (1) t1 and t2 belong to different universe types (e.g., Subject vs.
		// Action) or
		// (2) there is some superset s1 in sups1 and some s2 in sups2 having a
		// disjointness constraint.

		// It doesn't matter if (say) t1 = t2 -- if x subset y and x subset z, y
		// disjoint from z... x must be empty.

		Set<MSort> sups1 = buildSuperSetSet(t1);
		Set<MSort> sups2 = buildSuperSetSet(t2);

		// now check for disjointness
		// 11/16/09 TN changed to use ax_disj instead of disj. Now we must check in both directions.
		for (MSort s1 : sups1)
		{
			for (MSort s2 : sups2)
			{
				// removed: only axiom_disjoints now.
				//if (axioms.disjoints.containsKey(s1)
				//		&& axioms.disjoints.get(s1).contains(s2))
				
				if (axioms.axiomDisjoints.containsKey(s1)
								&& axioms.axiomDisjoints.get(s1).contains(s2))
					return false;
				
				// test in both directions since axiom_disj isn't necessarily symmetric
				if (axioms.axiomDisjoints.containsKey(s2)
						&& axioms.axiomDisjoints.get(s2).contains(s1))
					return false;

				// Since we require universe types to be disjoint.
				// If we stop requiring that, remove this!
				if (s1.parent == null && s2.parent == null && s1 != s2)
					return false;
			}
		}

		return true; // safe default
	}

	protected MVocab combineWith(MVocab other) throws MGECombineVocabs,
			MGEBadIdentifierName, MGEUnknownIdentifier {
		// Names (strings) used to detect equivalence (since object references
		// are obviously untrustworthy.)

		// If request sets (and types!), ordering, or decisions are not the
		// same, complain.
		if (!other.decisions.containsAll(decisions)
				|| !decisions.containsAll(other.decisions))
			throw new MGECombineVocabs(
					"Decision sets were not the same between " + decisions
							+ " and " + other.decisions);
		if (other.requestVariables.size() != requestVariables.size())
			throw new MGECombineVocabs("Different request vector.");
		for (int ii = 0; ii < requestVectorOrder.size(); ii++) {
			if (requestVectorOrder.get(ii).name().compareTo(
					other.requestVectorOrder.get(ii).name()) != 0)
				throw new MGECombineVocabs(
						"Different request vector: Mismatched request variable name or ordering.");

		}

		// If (shared) custom predicates do not have the same signature,
		// complain
		for (String pname : predicates.keySet())
			if (other.predicates.keySet().contains(pname))
				if (predtypes.get(pname).compareTo(other.predtypes.get(pname)) != 0)
					throw new MGECombineVocabs(
							"Different custom predicate signature: " + pname);

		// (Note that base types can differ as much as allowed by the above --
		// request vars must share SOME common type!)

		// Generate a list of shared type names
		Set<String> shared = new HashSet<String>();
		for (MSort t : sorts.values()) {
			try {
				// Is this a type name that both vocabs use?
				MSort t2 = other.getSort(t.name);

				shared.add(t.name);

				// If they both have parents, Then their parents must have the
				// same name.
				if (t.parent != null && t2.parent != null
						&& t.parent.name.compareTo(t2.parent.name) != 0)
					throw new MGECombineVocabs("Type " + t.name
							+ " did not have the same parent in the vocabs.");

				// Their relation must have the same name.
				if (t.rel.name().compareTo(t2.rel.name()) != 0
						|| t.rel.name().compareTo(t.name) != 0)
					throw new MGECombineVocabs(
							"Type "
									+ t.name
									+ " did not have the same relation name in the vocabs.");
			} catch (MGEUnknownIdentifier e) {
			}
		}

		// Combine everything and return the result.
		MVocab uber = new MVocab(vocab_name + "+" + other.vocab_name);

		// If either vocab uses XACML or IOS features, the combined one does as
		// well.
		if (bUsesXACML || other.bUsesXACML)
			uber.bUsesXACML = true;
		if (bUsesIOS || other.bUsesIOS)
			uber.bUsesIOS = true;

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

		// Decisions, Request vars IN ORDER
		for (String d : decisions)
			uber.addDecision(d);
		for (Variable v : requestVectorOrder)
			uber
					.addRequestVar(v.name(), requestVarDomains.get(v.name())
							.name());

		// Predicates and Predtypes
		for (String d : predicates.keySet())
			uber.addPredicate(d, predtypes.get(d));
		for (String d : other.predicates.keySet())
			uber.addPredicate(d, other.predtypes.get(d));

		// Declared other vars (if same type, it's ok.)
		for (String ov : otherVarDomains.keySet()) {
			if (other.otherVarDomains.containsKey(ov))
				if (!other.otherVarDomains.get(ov).name().equals(
						otherVarDomains.get(ov)))
					throw new MGECombineVocabs(
							"Declared rule-scope variable type mismatch.");
			uber.addOtherVar(ov, otherVarDomains.get(ov).name());
		}
		for (String ov : other.otherVarDomains.keySet()) {
			if (!otherVarDomains.containsKey(ov))
				uber.addOtherVar(ov, other.otherVarDomains.get(ov).name());
		}

		// Constraints
		for (String con : axioms.setsSingleton) {
			if (shared.contains(con))
				if (!other.axioms.setsSingleton.contains(con))
					throw new MGECombineVocabs(
							"Constraint missing between vocabs ->.");
			uber.axioms.addConstraintSingleton(con);
		}
		for (String con : other.axioms.setsSingleton) {
			if (shared.contains(con))
				if (!axioms.setsSingleton.contains(con))
					throw new MGECombineVocabs(
							"Constraint missing between vocabs <-.");
			uber.axioms.addConstraintSingleton(con);
		}

		for (String con : axioms.setsAtMostOne) {
			if (shared.contains(con))
				if (!other.axioms.setsAtMostOne.contains(con))
					throw new MGECombineVocabs(
							"Constraint missing between vocabs ->.");
			uber.axioms.addConstraintAtMostOne(con);
		}
		for (String con : other.axioms.setsAtMostOne) {
			if (shared.contains(con))
				if (!axioms.setsAtMostOne.contains(con))
					throw new MGECombineVocabs(
							"Constraint missing between vocabs <-.");
			uber.axioms.addConstraintAtMostOne(con);
		}

		for (String con : axioms.setsNonempty) {
			if (shared.contains(con))
				if (!other.axioms.setsNonempty.contains(con))
					throw new MGECombineVocabs(
							"Constraint missing between vocabs ->.");
			uber.axioms.addConstraintNonempty(con);
		}
		for (String con : other.axioms.setsNonempty) {
			if (shared.contains(con))
				if (!axioms.setsNonempty.contains(con))
					throw new MGECombineVocabs(
							"Constraint missing between vocabs <-.");
			uber.axioms.addConstraintNonempty(con);
		}

		for (String con : axioms.setsAbstract) {
			if (shared.contains(con))
				if (!other.axioms.setsAbstract.contains(con))
					throw new MGECombineVocabs(
							"Constraint missing between vocabs ->.");
			uber.axioms.addConstraintAbstract(con);
		}
		for (String con : other.axioms.setsAbstract) {
			if (shared.contains(con))
				if (!axioms.setsAbstract.contains(con))
					throw new MGECombineVocabs(
							"Constraint missing between vocabs <-.");
			uber.axioms.addConstraintAbstract(con);
		}
		
		for (String con : axioms.funcPartial) {
			if (shared.contains(con))
				if (!other.axioms.funcPartial.contains(con))
					throw new MGECombineVocabs(
							"Constraint missing between vocabs ->.");
			uber.axioms.addConstraintPartialFunction(con);
		}
		for (String con : other.axioms.funcPartial) {
			if (shared.contains(con))
				if (!axioms.funcPartial.contains(con))
					throw new MGECombineVocabs(
							"Constraint missing between vocabs <-.");
			uber.axioms.addConstraintPartialFunction(con);
		}

		for (String con : axioms.funcTotal) {
			if (shared.contains(con))
				if (!other.axioms.funcTotal.contains(con))
					throw new MGECombineVocabs(
							"Constraint missing between vocabs ->.");
			uber.axioms.addConstraintTotalFunction(con);
		}
		for (String con : other.axioms.funcTotal) {
			if (shared.contains(con))
				if (!axioms.funcTotal.contains(con))
					throw new MGECombineVocabs(
							"Constraint missing between vocabs <-.");
			uber.axioms.addConstraintTotalFunction(con);
		}

		for (String child : axioms.setsSubset.keySet()) {
			for (String parent : axioms.setsSubset.get(child)) {
				// make sure they agree on constraints on types they share.
				if (shared.contains(parent) && shared.contains(child))
					if (!other.axioms.setsSubset.containsKey(child)
							|| !other.axioms.setsSubset.get(child).contains(
									parent))
						throw new MGECombineVocabs(
								"Constraint missing between vocabs ->.");

				// always add the constraint
				uber.axioms.addConstraintSubset(child, parent);
			}
		}
		for (String child : other.axioms.setsSubset.keySet()) {
			for (String parent : other.axioms.setsSubset.get(child)) {
				// make sure they agree on constraints on types they share.
				if (shared.contains(parent) && shared.contains(child))
					if (!axioms.setsSubset.containsKey(child)
							|| !axioms.setsSubset.get(child).contains(parent))
						throw new MGECombineVocabs(
								"Constraint missing between vocabs ->.");

				// always add the constraint
				uber.axioms.addConstraintSubset(child, parent);
			}
		}

		// Disjointness
		//System.out.println("Beginning disj helper.");
		combineDisjHelper(uber, this, other, shared);		
		// used to need 2nd call, but no longer
		//combineDisjHelper(uber, other, this, shared);
		//System.out.println("Ending disj helper.");
		
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
		
		//System.out.println("Checking shared disj.");
		// Make sure A -> B in shared
		checkSharedDisjointness(A, B, shared);
		// Make sure B -> A in shared
		checkSharedDisjointness(B, A, shared);

//		System.err.println("Time (ms) to checkSharedDisj both directions (voc comb): "+ (mxBean.getCurrentThreadCpuTime() - start) / 1000000);
	//	start = mxBean.getCurrentThreadCpuTime();

		
		// uber contains all the sorts that A and B do, regardless of whether both have them
		// So uber needs to have all the disjoint constraints from both.

		//System.out.println("Copying disj info.");
		
		// construct a new disjoints set for uber.
		HashMap<MSort, Set<MSort>> new_ax_disjoints = new HashMap<MSort, Set<MSort>>();
			
		//System.err.println(A.sorts);		
		//System.err.println(A.axioms.axiom_disjoints.keySet());
		// eh? _1 under one?
		
		
		addDisjsToSet(uber, A, new_ax_disjoints);
		addDisjsToSet(uber, B, new_ax_disjoints);		
		
		uber.axioms.axiomDisjoints = new_ax_disjoints;
		
		//System.err.println("Time (ms) to adddisjsToSet (voc comb): "+ (mxBean.getCurrentThreadCpuTime() - start) / 1000000);
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
			System.err.println(e);
			e.printStackTrace();
			return false;
		}
		
		return true;
	}

	public String getInfo()
	{		
		List<String> outList = new ArrayList<String>();				
		outList.add("VOCABULARY");
		outList.add(vocab_name);
		outList.add(MEnvironment.convertSetToSexp(sorts.keySet()));
	
		
		return MEnvironment.convertListToSexp(outList);
	}
	
}

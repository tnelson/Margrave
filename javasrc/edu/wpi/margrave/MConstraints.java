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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import kodkod.ast.*;
import kodkod.ast.operator.Multiplicity;


public class MConstraints
{
	String id;

	// So we can validate type names, etc.
	MVocab vocab;

	// Axiomatic disjointness
	// Which disjointness assertions need to be made, because
	// they are not covered by similar assertions higher in
	// the sort tree?
	HashMap<MSort, Set<MSort>> axiomDisjoints;

	Set<String> setsSingleton;
	Set<String> setsAtMostOne;
	Set<String> setsNonempty;
	HashMap<String, Set<String>> setsSubset;

	// Relations constrained to be functions
	HashSet<String> funcTotal;
	HashSet<String> funcPartial;

	Set<String> setsAbstract;

	// Custom constraints not supported in any way but to say "this must be satisfied."
	// In string format so that a policy's assumptions can contain IDB references.
	// (Yes this is inefficient, but the user may add more rules later which could change the formulas.)

	Set<String> otherConstraintStrings;

	public MConstraints(String desc, MVocab voc)
	{
		id = desc;
		vocab = voc;

		setsSingleton = new HashSet<String>();
		setsNonempty = new HashSet<String>();
		setsAtMostOne = new HashSet<String>();
		setsSubset = new HashMap<String, Set<String>>();
		setsAbstract = new HashSet<String>();

		funcTotal = new HashSet<String>();
		funcPartial = new HashSet<String>();

		axiomDisjoints = new HashMap<MSort, Set<MSort>>();

		otherConstraintStrings = new HashSet<String>();
	}

	public void addOtherConstraint(String con)
	{
		otherConstraintStrings.add(con);
	}

	public void printConstraints()
	{
		MEnvironment.errorStream.println("<Placeholder>");
	}

	public void addConstraintAbstractAll(String d)
	throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		d = vocab.validateIdentifier(d, true);
		if(!vocab.isSort(d))
			throw new MGEUnknownIdentifier("Could not add constraint. Unknown type: "+d);

		MSort parent = vocab.getSort(d);
		for(MSort child : parent.subsorts)
			addConstraintAbstract(child.name);
	}

	public void addConstraintAbstract(String d)
	throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// D equals the union of its subsorts

		d = vocab.validateIdentifier(d, true);
		if(!vocab.isSort(d))
			throw new MGEUnknownIdentifier("Could not add constraint. Unknown type: "+d);

		setsAbstract.add(d);
	}


	public void addConstraintDisjointAll(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		d = vocab.validateIdentifier(d, true);
		if(!vocab.isSort(d))
			throw new MGEUnknownIdentifier("Could not add constraint. Unknown type: "+d);

		MSort parent = vocab.getSort(d);

		for(MSort t1 : parent.subsorts)
			for(MSort t2: parent.subsorts)
				if(t1 != t2)
					addConstraintDisjoint(t1, t2);
	}

	protected boolean addConstraintDisjoint(MSort t1, MSort t2) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// t1, t2 are disjoint in this constraint set

		//if(!axiom_disjoints.containsKey(t1))
		//	axiom_disjoints.put(t1, new HashSet<MGSort>());
		//if(!axiom_disjoints.containsKey(t2))
		//	axiom_disjoints.put(t2, new HashSet<MGSort>());

		//Set<MGSort> disjoints = axiom_disjoints.get(t1);
		//disjoints.add(t2); // no put needed, same object

		// TODO debug

		// Is this assertion meaningful, or is it already a consequence of other disj axioms?
		if(vocab.possibleOverlap(t1, t2))
		{
			// Prepare the data structures if needed
			if(!axiomDisjoints.containsKey(t1))
				axiomDisjoints.put(t1, new HashSet<MSort>());
			if(!axiomDisjoints.containsKey(t2))
				axiomDisjoints.put(t2, new HashSet<MSort>());

			// ************
			// Step 1: Add!
			Set<MSort> disjoints = axiomDisjoints.get(t1);
			disjoints.add(t2); // no put needed, same object

			// Axioms are not symmetric: We don't add that t2 disj t1...
			// Why make the query bigger than it has to be?

			// ********************************
			// Step 2: Does this disjointness "cover" pre-existing disjointnesses lower in the tree?
			// Since we aren't being symmetric, need to check in _both_ directions.

			// Any of t1's descendants have an ax_disj involving a descendant of t2?
			// (Warning: subsortClosureOf doesn't include subSET constraints, just the sort tree.)

			// TODO larger sorts generally have disjointness given before smaller ones,
			// making this block apply only rarely. commented out for now

			/*Set<MGSort> t1desc = vocab.subsortClosureOf(t1);
			Set<MGSort> t2desc = vocab.subsortClosureOf(t2);

			for(MGSort child1 : t1desc)
				for(MGSort child2 : t2desc)
				{
					// Does this combination have a disj? If so remove.
					if(!child1.equals(t1) && vocab.axioms.axiom_disjoints.containsKey(child1))
						if(vocab.axioms.axiom_disjoints.get(child1).contains(child2))
						{
							System.err.println("Removed");
							vocab.axioms.axiom_disjoints.get(child1).remove(child2);
						}

					// Same for t2, t1.
					if(!child2.equals(t2) && vocab.axioms.axiom_disjoints.containsKey(child2))
						if(vocab.axioms.axiom_disjoints.get(child2).contains(child1))
						{
							System.err.println("Removed");
							vocab.axioms.axiom_disjoints.get(child2).remove(child1);
						}
				}		*/

			return true;
		}
		else
		{
			// Do nothing: won't need to mention these because they are already
			// disjoint given existing constraints.
			
			return false;
		}


		// We do NOT "promote" disjointness. Unsafe!
	}

	public void addConstraintDisjoint(String d, Set<String> others) throws MGEBadIdentifierName, MGEUnknownIdentifier
	{
		d = vocab.validateIdentifier(d, true);
		if(!vocab.isSort(d))
			throw new MGEUnknownIdentifier("Could not add constraint. Unknown type: "+d);

		for(String other_ : others)
		{
			String other = vocab.validateIdentifier(other_, true);
			if(!vocab.isSort(other))
				throw new MGEUnknownIdentifier("Could not add constraint. Unknown type: "+other);

			addConstraintDisjoint(vocab.getSort(d), vocab.getSort(other));
		}
	}

	public void addConstraintDisjoint(String d1, String d2) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		d1 = vocab.validateIdentifier(d1, true);
		d2 = vocab.validateIdentifier(d2, true);
		if(!vocab.isSort(d1))
			throw new MGEUnknownIdentifier("Could not add constraint. Unknown type: "+d1);
		if(!vocab.isSort(d2))
			throw new MGEUnknownIdentifier("Could not add constraint. Unknown type: "+d2);

		addConstraintDisjoint(vocab.getSort(d1), vocab.getSort(d2));
	}

	public void addConstraintDisjoint(Set<String> rels) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// Don't require the user to enter pairwise disjointness by PAIR.
		// (This method is a happy medium between disjoint(x, y) and disjoint_all(domain)

		for(String d : rels)
		{
			d = vocab.validateIdentifier(d, true);
			if(!vocab.isSort(d))
				throw new MGEUnknownIdentifier("Could not add constraint. Unknown type: "+d);

			MSort t1 = vocab.getSort(d);
			for(String d2 : rels)
			{
				if(d.equals(d2))
					continue;
				MSort t2 = vocab.getSort(d2);
				addConstraintDisjoint(t1, t2);
			}
		}
	}

	public void addConstraintDisjoint(MSort t, Set<MSort> disj) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		for(MSort td : disj)
			addConstraintDisjoint(t, td);
	}

	public void addConstraintDisjoint(List<String> dlist) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		addConstraintDisjoint(new HashSet<String>(dlist));
	}

	public void addConstraintNonempty(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		d = vocab.validateIdentifier(d, true);
		if(!vocab.isSort(d))
			throw new MGEUnknownIdentifier("Could not add constraint. Unknown type: "+d);

		setsNonempty.add(d);
	}

	public void addConstraintSubset(String child, String parent) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// child is subset of parent
		child = vocab.validateIdentifier(child, true);
		parent = vocab.validateIdentifier(parent, true);
		if(!vocab.isSort(child) || !vocab.isSort(parent))
			throw new MGEUnknownIdentifier("Could not add constraint. One of the following was an unknown type: "+child+", "+parent);

		if(!setsSubset.containsKey(child))
			setsSubset.put(child, new HashSet<String>());

		setsSubset.get(child).add(parent);
	}

	public void addConstraintSingleton(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		d = vocab.validateIdentifier(d, true);
		if(!vocab.isSort(d))
			throw new MGEUnknownIdentifier("Could not add constraint. Unknown type: "+d);

		setsSingleton.add(d);
	}

	public void addConstraintAtMostOne(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		d = vocab.validateIdentifier(d, true);
		if(!vocab.isSort(d))
			throw new MGEUnknownIdentifier("Could not add constraint. Unknown type: "+d);

		setsAtMostOne.add(d);
	}

	public void addConstraintAtMostOneAll(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		d = vocab.validateIdentifier(d, true);
		if(!vocab.isSort(d))
			throw new MGEUnknownIdentifier("Could not add constraint. Unknown type: "+d);

		MSort parent = vocab.getSort(d);

		for(MSort t : parent.subsorts)
			setsAtMostOne.add(t.name);
	}

	public void addConstraintPartialFunction(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// Relation d is a partial function. The LHS of the function restriction is the first lhs_size subrelations.
		d = vocab.validateIdentifier(d, true);
		funcPartial.add(d);
	}

	public void addConstraintTotalFunction(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// Relation d is a TOTAL function. The LHS of the function restriction is the first lhs_size subrelations.
		d = vocab.validateIdentifier(d, true);
		funcTotal.add(d);
	}

	// "All" constraints can be applied to types, and influence the SUBtypes of that type
    // For instance, (singleton-all Action) means that each subtype of Action gets exactly one
	// atom -- not that action itself does.

	public void addConstraintSingletonAll(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		d = vocab.validateIdentifier(d, true);
	 	// Policy places a constraint on a domain with this name.
		// We should know about a relation of this name.
		if(!vocab.isSort(d))
			throw new MGEUnknownIdentifier("Could not add constraint. Unknown type: "+d);

		MSort parent = vocab.getSort(d);
		for(MSort t : parent.subsorts)
			addConstraintSingleton(t.rel.name());

		if(parent.subsorts.size() > 0)
			addConstraintNonempty(d);

	}

	public void addConstraintNonemptyAll(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		d = vocab.validateIdentifier(d, true);
		if(!vocab.isSort(d))
			throw new MGEUnknownIdentifier("Could not add constraint. Unknown type: "+d);
		// "All" implies that each subdomain of this domain has exactly one member.

		MSort parent = vocab.getSort(d);
		for(MSort t : parent.subsorts)
			addConstraintNonempty(t.rel.name());

		// if there are any subdomains, the domain must also be non-empty.
		if(parent.subsorts.size() > 0)
			addConstraintNonempty(d);
	}

	Set<Formula> getConstraintFormulas()
	throws MGEUnknownIdentifier, MGEArityMismatch, MGEBadQueryString, MGEManagerException, MGEBadIdentifierName
	{
		return getConstraintFormulas(null);
	}

	Set<Formula> getConstraintFormulas(MIDBCollection idbContext)
	throws MGEUnknownIdentifier, MGEArityMismatch, MGEBadQueryString, MGEManagerException, MGEBadIdentifierName
	{
		Set<Formula> results = new HashSet<Formula>();

		// Don't utter everything that must be disjoint from a type.
		// Be smart: only make disjointness an axiom if it wouldn't already
		// be true due to a disjointness higher in the sort order.


		for(MSort basetype : axiomDisjoints.keySet())
		{
			Set<Formula> theseDisjs = getDisjointness(basetype, axiomDisjoints.get(basetype));
			results.addAll(theseDisjs);
		}

		for(String r : setsAbstract)
		{
			MSort theSort = vocab.getSort(r);
			
			// Do nothing if this sort has no children
			if(theSort.subsorts.size() < 1)
				continue;
			
			Variable theVar = MFormulaManager.makeVariable("abst_"+r);
			Decl theDecl = MFormulaManager.makeOneOfDecl(theVar, theSort.rel);
			Set<Formula> subDisj = new HashSet<Formula>();
			for(MSort theChild : theSort.subsorts)
			{
				Formula theFormula = MFormulaManager.makeAtom(theVar, theChild.rel);
				subDisj.add(theFormula);
			}


			Formula abstractFormula = MFormulaManager.makeForAll(MFormulaManager.makeDisjunction(subDisj), theDecl);
			results.add(abstractFormula);
		}


		for(String r : setsAtMostOne)
		{
			// No more than one atom in this relation.
			// This is "safe" w/r/t HU terms because this could be simulated with a universal w/o
			// existentials in scope.

			results.add(MFormulaManager.makeMultiplicity(vocab.getRelation(r), Multiplicity.LONE));
		}

		for(String r : setsSingleton)
		{
			results.add(MFormulaManager.makeMultiplicity(vocab.getRelation(r), Multiplicity.ONE));
		}

		for(String r : setsNonempty)
		{
			results.add(MFormulaManager.makeMultiplicity(vocab.getRelation(r), Multiplicity.SOME));
		}

		// Moved subset constraints to Fixed (just another part of the poset for sort ordering,
		// and thus included in the sig).

		// Constrain total and partial functions to be such
		for(String r : funcPartial)
		{
			results.add(vocab.makeFunctionalFormula(vocab.getRelation(r), "P"));
		}

		for(String r : funcTotal)
		{
			results.add(vocab.makeFunctionalFormula(vocab.getRelation(r), "T"));
		}

		// Finally, other constraint strings. These need to be parsed.
		// "Other constraints" are arbitrarily complex constraints that the user
		// has provided in the Margrave query language.

		HashMap<String, MIDBCollection> hmpol = new HashMap<String, MIDBCollection>();
		if(idbContext != null)
			hmpol.put(idbContext.name, idbContext);


		
		//SimplifyFormulaV simplifierV = new SimplifyFormulaV();
		for(String s : otherConstraintStrings)
		{
			results.add(MQuery.constructFormulaFromString(hmpol, vocab, new Stack<Variable>(), s, "Q"));
					//.accept(simplifierV));
		}
		
		return results;
	}

	Set<Formula> getDisjointness(MSort t, Set<MSort> disjoints)
	throws MGEUnknownIdentifier
	{
		// Strange behavior of this method is static. So not static.

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

}

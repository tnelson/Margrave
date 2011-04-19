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
	// So we can validate type names, etc.
	MVocab vocab;

	Set<String> setsSingleton;
	Set<String> setsAtMostOne;
	Set<String> setsNonempty;

	// Relations constrained to be functions
	HashSet<String> funcTotal;
	HashSet<String> funcPartial;
	
	// Relations constrained to be total, but not functional
	HashSet<String> relTotal;

	Set<String> setsAbstract;

	// Custom constraints not supported in any way but to say "this must be satisfied."
	// In string format so that a policy's assumptions can contain IDB references.
	// (Yes this is inefficient, but the user may add more rules later which could change the formulas.)

	Set<String> otherConstraintStrings;

	public MConstraints(MVocab voc)
	{		
		vocab = voc;

		setsSingleton = new HashSet<String>();
		setsNonempty = new HashSet<String>();
		setsAtMostOne = new HashSet<String>();
		setsAbstract = new HashSet<String>();

		funcTotal = new HashSet<String>();
		funcPartial = new HashSet<String>();
		relTotal = new HashSet<String>();		

		otherConstraintStrings = new HashSet<String>();
	}

	public void addOtherConstraint(String con)
	{
		otherConstraintStrings.add(con);
	}

	public void printConstraints()
	{
		MEnvironment.errorWriter.println("<Placeholder>");
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

	public void addConstraintNonempty(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		d = vocab.validateIdentifier(d, true);
		if(!vocab.isSort(d))
			throw new MGEUnknownIdentifier("Could not add constraint. Unknown type: "+d);

		setsNonempty.add(d);
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

	public void addConstraintTotalRelation(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// Relation d is a TOTAL relation. 
		d = vocab.validateIdentifier(d, true);
		relTotal.add(d);
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
		
		for(String r : relTotal)
		{
			results.add(vocab.makeFunctionalFormula(vocab.getRelation(r), "R"));
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

}

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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import kodkod.ast.*;
import kodkod.ast.operator.Multiplicity;


public class MConstraints
{
	// So we can validate type names, etc.
	MVocab vocab;

	// sorts only
	Set<String> setsAbstract = new HashSet<String>();
	
	// Either predicates OR sorts
	Set<String> setsSingleton = new HashSet<String>();
	Set<String> setsAtMostOne = new HashSet<String>();
	Set<String> setsNonempty = new HashSet<String>();

	// Relations constrained to be functions
	Set<String> funcTotal = new HashSet<String>();
	Set<String> funcPartial = new HashSet<String>();
	
	// Relations constrained to be total, but not functional
	Set<String> relTotal = new HashSet<String>();
 
	// only predicates allowed
	Set<List<String>> setsDisjoint = new HashSet<List<String>>();	
	Set<List<String>> setsSubset = new HashSet<List<String>>();

	// These sorts are entirely covered by the elements denoted by constants of that sort (or subsorts):
	Set<String> setsConstantsCover = new HashSet<String>();

	// Constants of type T are all pairwise disjoint.
	Set<String> setsConstantsNeqAll = new HashSet<String>();
	
	// These pairs of constants are non-equal.
	Set<List<String>> constantsNeq = new HashSet<List<String>>();
	
	// Custom constraints not supported in any way but to say "this must be satisfied."
	// In string format so that a policy's assumptions can contain IDB references.
	// (Yes this is inefficient, but the user may add more rules later which could change the formulas.)

	Set<String> otherConstraintStrings;

	public MConstraints(MVocab voc)
	{		
		vocab = voc;
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

	public void addConstraintAbstract(String d)
	throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// D equals the union of its subsorts
		if(!vocab.isSort(d))
			throw new MGEUnknownIdentifier("Could not add constraint. Unknown type: "+d);

		setsAbstract.add(d);
	}

	public void addConstraintNonempty(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		if(!vocab.isSort(d) && !vocab.isPredicate(d))
			throw new MGEUnknownIdentifier("Could not add constraint. "+d+" is neither a type nor a predicate.");

		setsNonempty.add(d);
	}

	public void addConstraintSingleton(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		if(!vocab.isSort(d) && !vocab.isPredicate(d))
			throw new MGEUnknownIdentifier("Could not add constraint. "+d+" is neither a type nor a predicate.");

		setsSingleton.add(d);
	}

	public void addConstraintAtMostOne(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		if(!vocab.isSort(d) && !vocab.isPredicate(d))
			throw new MGEUnknownIdentifier("Could not add constraint. "+d+" is neither a type nor a predicate.");

		setsAtMostOne.add(d);
	}

	public void addConstraintDisjoint(String d1, String d2) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		if(!vocab.isPredicate(d1))
			throw new MGEUnknownIdentifier("Could not add constraint. "+d1+" is not a predicate.");
		if(!vocab.isPredicate(d2))
			throw new MGEUnknownIdentifier("Could not add constraint. "+d2+" is not a predicate.");

		List<String> lst = new ArrayList<String>(2);
		lst.add(d1); lst.add(d2);
		setsDisjoint.add(lst);
	}
	
	public void addConstraintConstantsNeq(String d1, String d2) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		if(!vocab.constants.containsKey(d1))
			throw new MGEUnknownIdentifier("Could not add constraint. "+d1+" is not a constant symbol.");
		if(!vocab.constants.containsKey(d2))
			throw new MGEUnknownIdentifier("Could not add constraint. "+d2+" is not a constant symbol.");

		List<String> lst = new ArrayList<String>(2);
		lst.add(d1); lst.add(d2);
		constantsNeq.add(lst);
	}
	
	public void addConstraintConstantsNeqAll(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		if(!vocab.isSort(d))
			throw new MGEUnknownIdentifier("Could not add constraint. "+d+" is not a type.");

		setsConstantsNeqAll.add(d);
	}

	public void addConstraintConstantsCover(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		if(!vocab.isSort(d))
			throw new MGEUnknownIdentifier("Could not add constraint. "+d+" is not a type.");

		setsConstantsCover.add(d);
	}

	
	public void addConstraintSubset(String d1, String d2) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		if(!vocab.isPredicate(d1))
			throw new MGEUnknownIdentifier("Could not add constraint. "+d1+" is not a predicate.");
		if(!vocab.isPredicate(d2))
			throw new MGEUnknownIdentifier("Could not add constraint. "+d2+" is not a predicate.");

		List<String> lst = new ArrayList<String>(2);
		lst.add(d1); lst.add(d2);
		setsSubset.add(lst);
	}
	
	/////////////////////////////////////////////////////////////////
	
	public void addConstraintPartialFunction(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// Relation d is a partial function. The LHS of the function restriction is the first lhs_size subrelations.
		funcPartial.add(d);
	}

	public void addConstraintTotalFunction(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// Relation d is a TOTAL function. The LHS of the function restriction is the first lhs_size subrelations.
		funcTotal.add(d);
	}

	public void addConstraintTotalRelation(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		// Relation d is a TOTAL relation. 		
		relTotal.add(d);
	}
	
	/////////////////////////////////////////////////////////////////
	
	// "All" constraints can be applied to types, and influence the SUBtypes of that type
    // For instance, (singleton-all Action) means that each subtype of Action gets exactly one
	// atom -- not that action itself does.

	public void addConstraintSingletonAll(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
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

	public void addConstraintAtMostOneAll(String d) throws MGEUnknownIdentifier, MGEBadIdentifierName
	{
		if(!vocab.isSort(d))
			throw new MGEUnknownIdentifier("Could not add constraint. Unknown type: "+d);

		MSort parent = vocab.getSort(d);

		for(MSort t : parent.subsorts)
			setsAtMostOne.add(t.name);
	}

	public void addConstraintAbstractAll(String d)
	throws MGEUnknownIdentifier, MGEBadIdentifierName
	{

		if(!vocab.isSort(d))
			throw new MGEUnknownIdentifier("Could not add constraint. Unknown type: "+d);

		MSort parent = vocab.getSort(d);
		for(MSort child : parent.subsorts)
			addConstraintAbstract(child.name);
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
			
			// Get the constants with local sort = theSort.
			Set<MConstant> localConstants = vocab.getConstantsWithLocalSort(theSort);
			
			// Do nothing if this sort has no children and no constants
			if((theSort.subsorts.size() + localConstants.size()) < 1)
				continue;
			
			// Now, construct the formula. Everything in theSort is in the union of child sorts and local constants.
			Variable theVar = MFormulaManager.makeVariable("abst_"+r);
			Decl theDecl = MFormulaManager.makeOneOfDecl(theVar, theSort.rel);
			Set<Formula> subDisj = new HashSet<Formula>();
			
			for(MSort theChild : theSort.subsorts)
			{
				Formula theFormula = MFormulaManager.makeAtom(theVar, theChild.rel);
				subDisj.add(theFormula);
			}
			for(MConstant theConst : localConstants)
			{
				Formula theFormula = MFormulaManager.makeAtom(theVar, theConst.rel);
				subDisj.add(theFormula);
			}

			Formula abstractFormula = MFormulaManager.makeForAll(MFormulaManager.makeDisjunction(subDisj), theDecl);
			results.add(abstractFormula);
		}


		for(String r : setsAtMostOne)
		{
			results.add(MFormulaManager.makeMultiplicity(vocab.getRelation(r), Multiplicity.LONE));
		}

		for(String r : setsSingleton)
		{
			results.add(MFormulaManager.makeMultiplicity(vocab.getRelation(r), Multiplicity.ONE));
		}
		
		for(String r : setsConstantsCover)
		{
			Set<Expression> theConstantRels = new HashSet<Expression>();
			Relation theSortRelation =  vocab.getRelation(r);
			for(MConstant c : vocab.constants.values())
			{
				if(c.type.get(0).rel.equals(theSortRelation))
						theConstantRels.add(c.rel);
			}
			Expression theUnion = MFormulaManager.makeUnion(theConstantRels);
			Formula theFmla = MFormulaManager.makeEqAtom(theUnion, theSortRelation); 
			results.add(theFmla);			
		}
		for(String r : setsConstantsNeqAll)
		{
			// TODO: setsConstantsNeqAll	
			MEnvironment.writeErrLine("The constraint type CONSTANT-NEQ-ALL is not yet supported. Passing over it for type: "+r);
		}
		for(List<String> lst : constantsNeq)
		{
			String c1name = lst.get(0);
			String c2name = lst.get(1);			
			results.add(MFormulaManager.makeMultiplicity(MFormulaManager.makeIntersection(vocab.constants.get(c1name).rel, 
					                                                                      vocab.constants.get(c2name).rel), 
					                                     Multiplicity.NO));
		}
		
		
		for(String r : setsNonempty)
		{
			results.add(MFormulaManager.makeMultiplicity(vocab.getRelation(r), Multiplicity.SOME));
		}
		
		for(List<String> lst : setsDisjoint)
		{
			assert(lst.size() == 2);
			Expression theIntersection = MFormulaManager.makeIntersection(vocab.getRelation(lst.get(0)), vocab.getRelation(lst.get(1)));
			results.add(MFormulaManager.makeMultiplicity(theIntersection, Multiplicity.NO));
		}

		for(List<String> lst : setsSubset)
		{
			assert(lst.size() == 2);
			results.add(MFormulaManager.makeAtom(vocab.getRelation(lst.get(0)), vocab.getRelation(lst.get(1))));
		}
		
		// Constrain total and partial functions to be such
		for(String r : funcPartial)
		{
			results.add(vocab.makeFunctionalFormula(vocab.predicates.get(r), "P"));
		}

		for(String r : funcTotal)
		{
			results.add(vocab.makeFunctionalFormula(vocab.predicates.get(r), "T"));
		}
		
		for(String r : relTotal)
		{
			results.add(vocab.makeFunctionalFormula(vocab.predicates.get(r), "R"));
		}

		// Finally, other constraint strings. These need to be parsed.
		// "Other constraints" are arbitrarily complex constraints that the user
		// has provided in the Margrave query language.

		//HashMap<String, MIDBCollection> hmpol = new HashMap<String, MIDBCollection>();
		//if(idbContext != null)
	//		hmpol.put(idbContext.name, idbContext);


		
		// 4/11 disabled for now
		//SimplifyFormulaV simplifierV = new SimplifyFormulaV();
		//for(String s : otherConstraintStrings)
		//{
		//	results.add(MQuery.constructFormulaFromString(hmpol, vocab, new Stack<Variable>(), s, "Q"));
		//			//.accept(simplifierV));
		//}
		
		return results;
	}

}

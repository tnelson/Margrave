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

import java.util.*;

import kodkod.ast.Formula;
import kodkod.ast.Variable;

/**
 * MGCustomIDB
 * @author tn
 *
 * 
 */
public class MCustomIDB extends MIDBCollection
{
	// Tells us how to interpret IDBName(x, y, z) in a query where x, y, and z get bound.
		
	private void init(MVocab v, String n, List<MIDBCollection> others, List<String> varorder, String viewstring)
	throws MGEBadQueryString, MGEUnknownIdentifier, MGEManagerException, MGEBadIdentifierName
	{
		name = n.toLowerCase();		
		vocab = v;		
		idbs = new HashMap<String, Formula>();
		
		varOrdering.clear();
		for(String s : varorder)
			varOrdering.add(MFormulaManager.makeVariable(s));		
		
		HashMap<String, MIDBCollection> othermap = new HashMap<String, MIDBCollection>();
		for(MIDBCollection idbs : others)
			othermap.put(idbs.name, idbs);
		
		// User is responsible for type restrictions when defining the view.		
		idbs.put("view", MQuery.constructFormulaFromString(othermap, vocab, new Stack<Variable>(), viewstring, "V"));
		// "V" tells the method to handle free variables by creating a new object and keeping track of it.
		
		// Now we must unify relation and variable references
		// (Need to make sure that the IDB formula doesn't contain an EDB object from others[0] and another from
		//  others[1] that need to be the same but aren't.)
		for(String dec : idbs.keySet())
			for(MIDBCollection other : others)
			{
				RelationAndVariableReplacementV vis = MIDBCollection.getReplacementVisitor(other.vocab, vocab);
				idbs.put(dec, idbs.get(dec).accept(vis));
			}
	}
	
	public MCustomIDB(String n, List<MIDBCollection> others, List<String> varorder, String viewstring) 
	throws MGEBadQueryString, MGEUnknownIdentifier, MGECombineVocabs, MGEBadIdentifierName, MGEManagerException
	{
		MVocab uber;
 
		if(others.size() < 1)
			throw new MGEBadQueryString("Custom IDB must have at least one dependency.");
		
		uber = others.get(0).vocab;
		
		if(others.size() > 1)
		{
			// Multiple dependencies. Need to combine vocabs.			
			for(int ii = 1; ii<others.size();ii++)
				uber = uber.combineWith(others.get(ii).vocab);	
		}
		
		init(uber, n, others, varorder, viewstring);
	}
	
	public MCustomIDB(MVocab v, String n, List<MIDBCollection> others, List<String> varorder, String viewstring) 
	throws MGEBadQueryString, MGEUnknownIdentifier, MGEManagerException, MGEBadIdentifierName
	{
		init(v, n, others, varorder, viewstring);
	}
	
}

// Used for tupling
class MInternalIDBCollection extends MIDBCollection
{
	
	protected MInternalIDBCollection(String n, MVocab voc)
	{
		idbs = new HashMap<String, Formula>();
		name = n;		
		vocab = voc;
	}

	protected void addIDB(String idbname, Formula idb)
	{
		idbs.put(idbname, idb);
	}
	
}


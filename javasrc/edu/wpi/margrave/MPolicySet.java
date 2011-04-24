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

import java.util.*;

import kodkod.ast.Formula;


public class MPolicySet extends MPolicy
{
	// (List, rather than Set or HashMap, because we need ordering for first-applicable.)
	List<MPolicy> children = new LinkedList<MPolicy>();

	// See MPolicyLeaf for more information 
	Set <String> pCombineFA = new HashSet<String>();
	Map<String, Set<String>> pCombineWhatOverrides = new HashMap<String, Set<String>>();

	
	public MPolicySet(String n, MVocab env)
	{
		super(n, env);				
	}
	
	public void addChild(MPolicy ch)
	{
		children.add(ch); // allow adding multiple times
	}
	
	public void initIDBs() throws MUserException
	{
		super.initIDBs();
		
		// Recalling this method should "reset" all IDBs. Start out with a clean slate:		
		idbs.clear();						
				
		// Prepare the IDBs of child policies
		for(MPolicy dc : children)
			dc.initIDBs();

		// Perform combination
			
		if(pCombine.toUpperCase().startsWith("O "))
		{
			// Total ordering of decisions given by children.							
				
			String[] ordering = pCombine.substring(2).split(" ");
									
			Set<Formula> negprior = new HashSet<Formula>();
			for(String dec : ordering)
			{
				Set<Formula> decf = new HashSet<Formula>();
				for(MPolicy child : children)	
				{
					// Child exposes some IDBs.
					// We must expose them, after making sure CHILD'S target applies, and no more important decisions given.
					for(String idbname : child.idbs.keySet())					
						idbs.put(child.name+":"+idbname, 
								MFormulaManager.makeAnd(child.idbs.get(idbname), 
										                 MFormulaManager.makeAnd(child.target, 
										                		        MFormulaManager.makeConjunction(negprior))));
					
					
					decf.add(MFormulaManager.makeAnd(child.target, child.idbs.get(dec)));
				}
					
				Formula decision_formula = MFormulaManager.makeDisjunction(decf);
				idbs.put(dec, MFormulaManager.makeAnd(decision_formula,  MFormulaManager.makeConjunction(negprior)));
				negprior.add(MFormulaManager.makeNegation(decision_formula));
			}
				
		}
		else if(pCombine.toUpperCase().startsWith("FA"))
		{
			//MCommunicator.writeToLog("\nPolicySet.initIDBs(); FA* combinator. Children = "+children+"\n");
			
			// Total ordering of children. Applicability is given by the child policy's Target.
			for(String dec : vocab.decisions)
			{
				
				Set<Formula> thisdec = new HashSet<Formula>(); // disj
				Set<Formula> negpriortargets = new HashSet<Formula>(); // conj
													
				// Order matters. Make sure to use the correct ordering.
				for(MPolicy child : children)
				{
					// This decision ALSO applies if
					// (1) No older child said what to do (even N/a!)
					// (2) The child's IDB for this decision applies
					// (3) The child's target applies to the request.
					thisdec.add(MFormulaManager.makeAnd(MFormulaManager.makeAnd(MFormulaManager.makeConjunction(negpriortargets),
							                                                      child.idbs.get(dec)), 
							                             child.target));

										
					// Child exposes some IDBs. 
					// We must expose them, after making sure CHILD'S target applies, and no older children grabbed the mic.
					for(String idbname : child.idbs.keySet())						
						idbs.put(child.name+":"+idbname, MFormulaManager.makeAnd(child.idbs.get(idbname), 
								                                     MFormulaManager.makeAnd(child.target,
								                                    		 MFormulaManager.makeConjunction(negpriortargets))));
					
					// Younger children must respect their elders.
					negpriortargets.add(MFormulaManager.makeNegation(child.target));										
				}
					
				idbs.put(dec, MFormulaManager.makeDisjunction(thisdec));					
			}
							
		}
		
	} // end initIDBs

	public void printPolicyInfo()
	{
		MEnvironment.errorWriter.println("###########################");
		MEnvironment.errorWriter.println("Policy Name: "+name);
		MEnvironment.errorWriter.println("This is a policy SET with policy combinator: "+printCombinators(pCombineFA, pCombineWhatOverrides));
		MEnvironment.errorWriter.println("Target Formula: "+target);		
		
		String cstr = "";
		for(MPolicy child : children)
		{
			if(cstr.length() < 1)
				cstr = child.name;
			else
				cstr = cstr + " " + child.name;
		}
		
		MEnvironment.errorWriter.println("Children: "+cstr);					
		
		MEnvironment.errorWriter.println("\nIDB and EDB predicates available for use in queries:");
		
		MEnvironment.errorWriter.println("IDBs:");		
		prettyPrintIDBs();		
		prettyPrintEDBs();
		MEnvironment.errorWriter.print("\n\n");
		
		MEnvironment.errorWriter.println("Policy-level constraints: ");
		
		MEnvironment.errorWriter.println("###########################\n");
	}
	
	public String getDecisionForRuleIDBName(String idbname)
	{
		// may pass canonical idb name ("PolicyName:Rule12" instead of "Rule12")
		if(idbname.startsWith(this.name+":")) // name is already lowercase
		{
			idbname = idbname.substring(this.name.length()+1);
		}	
	
		// Defer to child
		String[] split = idbname.split(":");
		String polName = split[0].toLowerCase();
		
		for(MPolicy pol : children)
		{
			if(pol.name.equals(polName))			
				return pol.getDecisionForRuleIDBName(idbname);
		}
		
		return "";
	}

	public List<String> ruleIDBsWithHigherPriorityThan(String rulename)
	{	
		return new LinkedList<String>(); // unsupported for now
	}
}

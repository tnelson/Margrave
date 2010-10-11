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

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import kodkod.ast.Expression;
import kodkod.ast.Formula;

import org.json.*;

public class SQSReader 
{
	protected static MVocab createSQSVocab(String polId)
	throws MGEBadIdentifierName, MGEUnknownIdentifier
	{
		MVocab env = new MVocab("Vocab_For_"+polId);
		
		env.addSort("principal"); 
		env.addSort("action");
		env.addSort("resource");
		env.addSort("condition");
		
		env.addRequestVar("p", "principal");
		env.addRequestVar("a", "action");
		env.addRequestVar("r", "resource");
		env.addRequestVar("c", "condition");
				
		env.addDecision("allow");
		env.addDecision("deny");
		
		return env;
	}	
	

	protected static void handleStatementCondition(JSONObject obj, MRule rule, MVocab vocab) 
	throws JSONException, MGEBadIdentifierName, MGEUnknownIdentifier, MGEManagerException
	{
		// Condition block is a conjunctive list of conditions. all must apply.
		// Each condition is a conjunctive list of value sets. 
		// Each value set is a disjunctive list of values
		// Keys in the condition block are functions to apply
		// Keys in a condition are attribute names. Values are values.

		// Condition block
		//    Function : {}
		//    Function : {}
		// ...
		
		JSONArray conditionNames = obj.names();
		for(int iCondition = 0;iCondition<conditionNames.length();iCondition++)
		{
			String cFunction = (String)conditionNames.get(iCondition);
			JSONObject condition = (JSONObject) obj.get(cFunction);
			//MEnvironment.errorStream.println(cFunction + ": "+condition);
			
			// condition is a key:value pair or multiple such pairs. The value may be an array.
			// Each sub-condition must be met.
			HashSet<Formula> thisCondition = new HashSet<Formula>();
			
			JSONArray subConditionNames = condition.names();
			for(int iSubCondition = 0;iSubCondition<subConditionNames.length();iSubCondition++)
			{
				String cSubKey = (String)subConditionNames.get(iSubCondition);
				Object subcondition = condition.get(cSubKey); 
				
				// Subcondition: is it an array or a single value?
				if(subcondition instanceof JSONArray)
				{
					JSONArray subarr = (JSONArray) subcondition;
					HashSet<Formula> valuedisj = new HashSet<Formula>();
					for(int iValue = 0; iValue < subarr.length(); iValue++)
					{
						//MEnvironment.errorStream.println(cFunction+"("+cSubKey+", "+subarr.get(iValue)+")");
						Formula theatom = makeSQSAtom(vocab, "c", "Condition", 
								cFunction+"("+cSubKey+", "+subarr.get(iValue)+")");
						valuedisj.add(theatom);
					}
					
					thisCondition.add(MFormulaManager.makeDisjunction(valuedisj));
				}
				else
				{
					//MEnvironment.errorStream.println(cFunction+"("+cSubKey+", "+subcondition+")");	
					Formula theatom = makeSQSAtom(vocab, "c", "Condition", cFunction+"("+cSubKey+", "+subcondition+")");
					thisCondition.add(theatom);
				}
				
				
			}
						
			rule.condition = MFormulaManager.makeAnd(rule.condition, 
					MFormulaManager.makeConjunction(thisCondition));
		}
		
	}
	
	protected static Formula makeSQSAtom(MVocab vocab, String varname, String parentsortname, String sortname) 
	throws MGEBadIdentifierName, MGEUnknownIdentifier, MGEManagerException
	{
		// Get the variable for this varname. 
		Expression thevar = MFormulaManager.makeVariable(varname);
		
		// Add the sort (if it doesn't already exist.)
		
		sortname = vocab.validateIdentifier(sortname, true);
		parentsortname = vocab.validateIdentifier(parentsortname, true);
		
		// Kludge (for now) to make * contain subsorts:
		
		if(sortname.equals("principal.aws=*")) 
			parentsortname = "principal";
		else if(sortname.startsWith("principal.aws"))
			parentsortname = "principal.aws=*";
		
		if(sortname.equals("action=sqs:*"))
			parentsortname = "action";
		else if(sortname.startsWith("action=sqs"))
			parentsortname = "action=sqs:*";
		
		vocab.addSubSort(parentsortname, sortname);
		
		//MEnvironment.errorStream.println(sortname);
		
		
		
		// TODO Infer disjointness where appropriate
		// (e.g., Principal.AWS=555566667777 and Principal.AWS=123456789012 should be disjoint, but
		//  Principal.AWS=555566667777 with and without dashes should not be
		// More ugly code:
		
		// This should be a *specific* AWS. Disjoint from all other SPECIFIC ones.
		if(sortname.startsWith("principal.aws=") && !sortname.contains("*"))
		{
			Set<String> other_candidates = vocab.getSortNamesWithPrefix("principal.aws=");

			// Don't disj ones with a * in them.
			Set<String> others = new HashSet<String>();
			for(String s : other_candidates)
				if(!s.contains("*") && !s.equals(sortname))
					others.add(s);
			
			vocab.axioms.addConstraintDisjoint(sortname, others);			
		}
		
		// This should be a *specific* resource ID. Disjoint from all other specific ones
		if(sortname.startsWith("resource=") && !sortname.contains("*") && !sortname.contains("&"))
		{
			Set<String> other_candidates = vocab.getSortNamesWithPrefix("resource=");

			// Don't disj ones with a * or & in them.
			Set<String> others = new HashSet<String>();
			for(String s : other_candidates)
				if(!s.contains("*") && !s.contains("&") && !s.equals(sortname))
					others.add(s);
			
			vocab.axioms.addConstraintDisjoint(sortname, others);
		}
		
			
		
		
		
		
		// From docs:
		// Although most of the information in this appendix is ***service-agnostic***, 
		// there are some SQS-specific details you need to know. For more information, 
		// see Special Information for SQS Policies.
		
		// So we support only the SQS-specific stuff here for now.
		
		
		MSort thesort = vocab.getSort(sortname);
		return MFormulaManager.makeAtom(thevar, thesort.rel);
	}
	
	protected static void handleStatementPAR(Object obj, 
			String varname, String parentsortname, MRule rule, MVocab vocab, String prepend) 
	throws MGEUnsupportedSQS, JSONException, MGEBadIdentifierName, MGEUnknownIdentifier, MGEManagerException
	{
		// obj may be a JSONObject (Principal examples)
		//   with a child with a value OR array of values
		
		//"Principal": {
        //"AWS": "*"
        //}
		
		//"Principal": {
        //"AWS": ["123456789012","555566667777"]
		//}
		
		// may also be a simple value (Action example)
		// or an array of values (Resource examples)
		
		// "Action": ["SQS:SendMessage","SQS:ReceiveMessage"],
        // "Resource": "/987654321098/queue1"

		// TODO: the example principal from "Element Descriptions" doesn't parse...
		//"Principal":[
		//             "AWS": "123456789012",
		//             "AWS": "999999999999"
		//          ]
		// is this just a bad example?
		
		
		
		// *****************************
		// Step 1: Is this a JSONObject? If so, it should have only one key. Prepend that key
		// to all predicate names produced by its value.
		if(obj instanceof JSONObject)
		{
			JSONObject jobj = (JSONObject) obj;
			JSONArray names = jobj.names();
			if(names.length() != 1)
				throw new MGEUnsupportedSQS("Number of keys != 1 as expected: "+obj.toString());			
			
			Object inner = jobj.get((String)names.get(0));
			handleStatementPAR(inner, varname, parentsortname, rule, vocab, prepend+"."+names.get(0));			
			return; 
		}
		
		// Now if obj is a simple value, we have a predicate name.
		// If it is an array of length n, we have n predicate names.		
		if(obj instanceof JSONArray)
		{
			JSONArray jarr = (JSONArray) obj;
			HashSet<Formula> possibleValues = new HashSet<Formula>();
			
			
			for(int ii=0;ii<jarr.length();ii++)
			{
				//MEnvironment.errorStream.println(prepend+"="+jarr.get(ii));
				
				Formula theatom = makeSQSAtom(vocab,varname, parentsortname, prepend+"="+jarr.get(ii));				
				possibleValues.add(theatom);
				
			}
			
			rule.target = MFormulaManager.makeAnd(rule.target, 
					MFormulaManager.makeDisjunction(possibleValues));
			
		}
		else
		{			
			Formula theatom = makeSQSAtom(vocab,varname, parentsortname, prepend+"="+obj);
			rule.target = MFormulaManager.makeAnd(rule.target, theatom);
		}
		
		
	}
	
	protected static void handleSQSStatement(JSONObject thisStatement, MPolicyLeaf result, int counter)
	throws JSONException, MGEUnsupportedSQS, MGEBadIdentifierName, MGEUnknownIdentifier, MGEManagerException
	{
		String ruleId = result.name+"_rule_"+counter;
		
		// Documentation says the statement ID is optional.
		if(!thisStatement.isNull("Sid"))
			ruleId = thisStatement.getString("Sid");
		
		String effect;
		if(!thisStatement.isNull("Effect"))
			effect = thisStatement.getString("Effect");
		else
			return; // no effect means no need to add the rule

		MRule rule = new MRule();
		rule.name = ruleId;		
		rule.setDecision(effect);		
		// target starts as true. All criteria must be met, so just "and" each on.

		
		// *************************
		// "Principal": disjunction
		if(thisStatement.isNull("Principal"))
			return; // never applies, so don't create the rule.				
		handleStatementPAR(thisStatement.get("Principal"), "p", "Principal", rule, result.vocab, "Principal");
				
		// *************************		
		// "Action": disjunction
		if(thisStatement.isNull("Action"))
			return; // never applies, so don't create the rule.
		handleStatementPAR(thisStatement.get("Action"), "a", "Action", rule, result.vocab, "Action");
		
		// *************************
		// "Resource": disjunction
		if(thisStatement.isNull("Resource"))
			return; // never applies, so don't create the rule.
		handleStatementPAR(thisStatement.get("Resource"), "r", "Resource", rule, result.vocab, "Resource");
		
		// *************************
		// "Condition": more complex
		// Condition optional? TODO -- for now if no condition, just finalize the rule. 
		// (Why are P/A/R special?)
		if(!thisStatement.isNull("Condition"))
			handleStatementCondition(thisStatement.getJSONObject("Condition"), rule, result.vocab);
					
		// Finalize the rule.
		rule.target_and_condition = MFormulaManager.makeAnd(rule.target, rule.condition);
		result.rules.add(rule);		
	}
	
	protected static MPolicy loadSQS(String sFileName) 
	throws MUserException
	{		
		// Convert filename
		sFileName = MPolicy.convertSeparators(sFileName);
		
		// Read in the JSON text
		try
		{
			BufferedReader reader = new BufferedReader(new FileReader(sFileName));
			String target = "";
			while(reader.ready())
			{
				String line = reader.readLine();
				target += line + "\n";
			}			
			
			JSONObject json = new JSONObject(target);
			String polId;
			
			if(!json.isNull("Id"))
				polId = json.getString("Id");
			else
				throw new MGEUnsupportedSQS("Id element must be present.");
			
			MVocab env = createSQSVocab(polId);
			MPolicyLeaf result = new MPolicyLeaf(polId, env);
			
			// Get statements; may be an array or a single statement object.
			Object statement_s = json.get("Statement");
			if(statement_s instanceof JSONArray)
			{
				JSONArray statements = json.getJSONArray("Statement");
			
				for(int ii=0;ii<statements.length();ii++)
				{
					JSONObject thisStatement = statements.getJSONObject(ii);
					handleSQSStatement(thisStatement, result, ii);
				}
			}
			else
				handleSQSStatement((JSONObject)statement_s, result, 0);
			
						
			result.rCombine = "O Deny Allow";						
			result.initIDBs();
			
			//result.prettyPrintRules();
			return result;
			
		}		
		catch(IOException e)
		{
			throw new MGEUnsupportedSQS(e.toString());
		}
		catch(JSONException e)
		{
			throw new MGEUnsupportedSQS(e.toString());	
		}
			
	}
}

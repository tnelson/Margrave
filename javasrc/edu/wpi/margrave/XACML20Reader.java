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

package edu.wpi.margrave;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.*;

import javax.xml.parsers.*;

import kodkod.ast.Formula;
import kodkod.ast.Relation;

class MGErrorHandler implements ErrorHandler
{
	boolean fussy;
	boolean show_fatals;
	
	MGErrorHandler(boolean fuss, boolean show_fatal)
	{
		fussy = fuss;
		show_fatals = show_fatal;
	}
	
	public void error(SAXParseException exception) throws SAXException
	{
		// For example, a validating parser would use this callback to report the violation 
		// of a validity constraint. The default behaviour is to take no action.

		// The SAX parser must continue to provide normal parsing events after invoking this method:
		// it should still be possible for the application to process the document through to the end.
		// If the application cannot do so, then the parser should report a fatal error even if the 
		// XML 1.0 recommendation does not require it to do so.
			
		if(fussy)
			MEnvironment.errorWriter.println(exception);		
		
		throw exception;
	}

	public void fatalError(SAXParseException exception) throws SAXException
	{			
		if(show_fatals)
			MEnvironment.errorWriter.println(exception);	
		
		throw exception;
	}

	public void warning(SAXParseException exception) throws SAXException
	{
		// SAX parsers will use this method to report conditions that are not errors or 
		// fatal errors as defined by the XML 1.0 recommendation. The default behaviour is to take no action.

		// The SAX parser must continue to provide normal parsing events after invoking 
		// this method: it should still be possible for the application to process the document through to the end.

		if(fussy)
			MEnvironment.errorWriter.println(exception);	
		
		throw exception;
	}
	
}


class XACML20Reader {
	
	static final String JAXP_SCHEMA_LANGUAGE =
	    "http://java.sun.com/xml/jaxp/properties/schemaLanguage";

	static final String W3C_XML_SCHEMA =
	    "http://www.w3.org/2001/XMLSchema"; 
	
	static final String JAXP_SCHEMA_SOURCE =
	    "http://java.sun.com/xml/jaxp/properties/schemaSource";	
	
	static HashMap<String, String> policyIdCache = null;
	
	static void initIdCache()
	{
		policyIdCache = new HashMap<String, String>();
	}
	
	static void addToIdCache(String id, String filename)
	{
		if(policyIdCache == null)
			initIdCache();
		policyIdCache.put(id, filename);
	}
	
	static String getFromCache(String id)
	{
		if(policyIdCache == null)
			initIdCache();
		if(policyIdCache.containsKey(id))
			return policyIdCache.get(id);
		return "";
	}
	
	static boolean isCached(String id)
	{
		if(policyIdCache == null)
			initIdCache();
		if(policyIdCache.containsKey(id))
			return true;
		return false;
	}
	
	static void doComplianceTests(String schemaFileName, String policyDir) 
	throws MUserException
	{		
			// For each compliance test
			
			Set<String> fileNameSet = new HashSet<String>();			
			
			File dir = new File(policyDir);					
			
			if(dir == null || dir.list() == null)
			{
				MEnvironment.errorWriter.println("*******************************************************************");
				MEnvironment.errorWriter.println("ERROR: Unable to run compliance tests. policyDir was: "+policyDir);
				if(!dir.exists())
					MEnvironment.errorWriter.println("Directory did not exist.");
				MEnvironment.errorWriter.println("*******************************************************************");
				return;
			}
			
			// Add all "must implement" tests
			for(String fname : dir.list())
				if(fname.endsWith("Policy.xml"))
					if(fname.startsWith("IIA") || fname.startsWith("IIB") || 
					   fname.startsWith("IIC") || fname.startsWith("IID") || 
					   fname.startsWith("IIE"))	
						fileNameSet.add(policyDir+fname);

			// ADD
			// Selected other tests
			
			// AttributeSelector in target
			fileNameSet.add(policyDir+"IIIF001Policy.xml");
	
			// AttributeSelector in condition
			fileNameSet.add(policyDir+"IIIF007Policy.xml");

			// REMOVE
			// intentional syntax error
			fileNameSet.remove(policyDir+"IIA004Policy.xml"); 
												
			MEnvironment.errorWriter.println("Beginning compliance tests... There are "+fileNameSet.size() + " policies to load.");
			
			for(String sFileName : fileNameSet)
			{
				MEnvironment.errorWriter.println("Loading test: "+sFileName);
											
				// catch exception for syntax errors that SHOULD be thrown 
				try
				{
					//MGPolicy p = 
					
					// For now, just try loading the policy. (See below for future goals.)
					loadXACML20(sFileName, schemaFileName, policyDir);
					
					// What is the expected outcome of this test?
					String fResponse = sFileName.substring(0, sFileName.length()-10) + "Response.xml";
					String decision = "";
					
					try
					{
						BufferedReader reader = new BufferedReader(new FileReader(fResponse));
						while(reader.ready())
						{
							String line = reader.readLine();
							
							if(line.contains("<Decision>NotApplicable</Decision>"))
							{
								decision = "NotApplicable";
								break;
							}
							
							if(line.contains("<Decision>Permit</Decision>"))
							{
								decision = "Permit";
								break;
							}
							
							if(line.contains("<Decision>Deny</Decision>"))
							{
								decision = "Deny";
								break;
							}
							
							if(line.contains("<Decision>Indeterminate</Decision>"))
							{
								decision = "Indeterminate";
								break;
							}
						} // for each line in the response											

						// Is this a real decision? If so, let's see what Margrave says about the request.
						if("Indeterminate".equals(decision) || decision.length() < 1)
							continue;						
						// (Is a query made up of the request and (not response) satisfiable?)
						
						
						
						// The request is inside a <Request> and contains a <Subject>, <Resource>, <Action>, <Environment>
						// See Oasis specs						
						// TODO	Produce query from request context					
						// Leaving this for now, because it may be a lot of effort.
						
						
					} // end try
					catch(FileNotFoundException e)
					{
						throw new MGEUnsupportedXACML("Test failed: Could not find file: "+fResponse);
					}
					catch(IOException e)
					{
						throw new MGEUnsupportedXACML("Test failed: IO Exception "+e);
					}
					
				}
				catch(MGEUnsupportedXACML e)
				{
					// only-one-applicable is not supported
					if(e.toString().contains("The only-one-applicable policy combination algorithm is not currently supported."))
					{
						MEnvironment.errorWriter.println("    Did not load "+sFileName+" since only-one-applicable is not yet supported.");							
					}
					else
					{					
						// Otherwise, unexpected exception. Badness.
						throw e;
					}					
				} // catch
				catch(SAXException e)
				{
					MEnvironment.errorWriter.println("  Unable to load test: "+sFileName);
				}
			} // end for each test 
			
		    


		
		MEnvironment.errorWriter.println("-------------------");
	}
	
	public static String getIdFromFile(String sFileName, String sSchemaFileName, String fieldname, ErrorHandler eh) throws MGEUnsupportedXACML
	{
		// More permissive than loadXACML20 since we may pass a filename that isn't a valid policy!
		
		try
		{
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();		
			factory.setNamespaceAware(true);
			
			// Don't validate (may not be a policy)
			//factory.setValidating(true);
			
			try 
			{
				factory.setAttribute(JAXP_SCHEMA_LANGUAGE, W3C_XML_SCHEMA);
			} 
			catch (IllegalArgumentException x)
			{
				// Happens if the parser does not support JAXP 1.2
				MEnvironment.errorWriter.println("Parser does not support JAXP");
			} 

			factory.setAttribute(JAXP_SCHEMA_SOURCE,
				    new File(sSchemaFileName)); 

						
			DocumentBuilder parser = factory.newDocumentBuilder();	
			parser.setErrorHandler(eh);
			Document thedoc = parser.parse(sFileName); 			
			
			NodeList children = thedoc.getChildNodes();

			if(children != null)
			{
				for(int ii=0;ii<children.getLength();ii++)
				{
					Node child = children.item(ii);
					if("Policy".equals(child.getNodeName()) && "PolicyId".equals(fieldname))
					{
						// Does the PolicyId match?
						if(!hasAttributeNamed(child, fieldname))
							throw new MGEUnsupportedXACML("Policy must contain PolicyId.");
						return getNodeAttributeValue(child, fieldname);
					} 
					else if("PolicySet".equals(child.getNodeName()) && "PolicySetId".equals(fieldname))
					{
						// Does the PolicySetId match?
						if(!hasAttributeNamed(child, fieldname))
							throw new MGEUnsupportedXACML("PolicySet must contain PolicySetId.");
						return getNodeAttributeValue(child, fieldname);
						
					}
				} // end for each child
			} // end if children exist
		
		}
		catch (SAXException e) {
			
			// Error thrown by error handler
			// say "cant find this"
			return "";  
		}
		catch (IOException e) { 
			MEnvironment.errorWriter.println(
					"Due to an IOException, the parser could not load the document: "+sFileName); 
		}
		catch (FactoryConfigurationError e) { 
			MEnvironment.errorWriter.println("Could not locate a factory class"); 
		}
		catch (ParserConfigurationException e) { 
			MEnvironment.errorWriter.println("Could not locate a JAXP parser"); 
		}
		
		return ""; // not a policy/policyset
	}
	
	public static MPolicy loadXACML20(String sFileName, String sSchemaFileName, String sPolicyDir) 
	throws MUserException, SAXException
	{
		try
		{
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();		
			factory.setNamespaceAware(true);
			factory.setValidating(true);
			try 
			{
				factory.setAttribute(JAXP_SCHEMA_LANGUAGE, W3C_XML_SCHEMA);
			} 
			catch (IllegalArgumentException x)
			{
				// Happens if the parser does not support JAXP 1.2
				MEnvironment.errorWriter.println("Parser does not support JAXP");
			} 

			factory.setAttribute(JAXP_SCHEMA_SOURCE,
				    new File(sSchemaFileName)); 

			DocumentBuilder parser = factory.newDocumentBuilder();
			MGErrorHandler eh = new MGErrorHandler(false, false); // fatal error causes us to try XACML 1.0 
			parser.setErrorHandler(eh);
			Document thedoc = parser.parse(sFileName); 
			
			MVocab vocab = MPolicy.makeXACMLVocab(sFileName);
			
			//NamedNodeMap attrs = thedoc.getAttributes();
			NodeList children = thedoc.getChildNodes();
			boolean seenPolicy = false;
			MPolicy result = null;
			if(children != null)
			{
				for(int ii=0;ii<children.getLength();ii++)
				{
					Node child = children.item(ii);
					if(!"#text".equals(child.getNodeName()))
					{
						if(seenPolicy)
							throw new MGEUnsupportedXACML("Parser does not support more than one Policy or PolicySet per file.");
				
						result = recXACMLPolicyTree(child, vocab, sSchemaFileName, sPolicyDir);
						seenPolicy = true;
					} // end if not a #text node
				} // end for each child
			} // end if children exist
			
			return result;
		
		}
		catch (SAXException e) {
			// Policy not well-formed.
			// But pass the exception back anyway (wrapper will see it and try to parse as xacml 1.0 instead
			throw e;
		}
		catch (IOException e) { 
			MEnvironment.errorWriter.println(
					"Due to an IOException, the parser could not load the document" 
			); 
		}
		catch (FactoryConfigurationError e) { 
			MEnvironment.errorWriter.println("Could not locate a factory class"); 
		}
		catch (ParserConfigurationException e) { 
			MEnvironment.errorWriter.println("Could not locate a JAXP parser"); 
		}

		throw new MGEUnsupportedXACML("Error loading: "+sFileName);
	}
	
	
	
	private static boolean hasChildNamed(Node n, String name)
	{
		if(!n.hasChildNodes())
			return false;
		NodeList children = n.getChildNodes();
		for(int ii=0;ii<children.getLength();ii++)
		{
			Node child = children.item(ii);
			if(name.equals(child.getNodeName()))
				return true;
		}
		return false;
	}
		
	
	private static Node getNodeChildNamed(Node n, String name)
	{
		if(!n.hasChildNodes())
			return null;
		NodeList children = n.getChildNodes();
		for(int ii=0;ii<children.getLength();ii++)
		{
			Node child = children.item(ii);
			if(name.equals(child.getNodeName()))
				return child;
		}
		return null;
	}

	private static boolean hasMultipleChildrenNamed(Node n, String name)
	{
		if(!n.hasChildNodes())
			return false;
		
		boolean seen = false;
		NodeList children = n.getChildNodes();
		for(int ii=0;ii<children.getLength();ii++)		
		{
			Node child = children.item(ii);		
			if(name.equals(child.getNodeName()))
			{
				if(!seen)
					seen = true;
				else
					return true;
			}
		}		
		return false;
	}
	
	private static String getNodeAttributeValue(Node n, String aname)
	{
		NamedNodeMap attrs = n.getAttributes();
		
		if(!n.hasAttributes())
			return "";
		
		for(int ii=0;ii<attrs.getLength();ii++)
			if(aname.equals(attrs.item(ii).getNodeName()))
			{
				return attrs.item(ii).getNodeValue();
			}
		return "";
	}
	
	private static boolean hasAttributeNamed(Node n, String aname)
	{
		NamedNodeMap attrs = n.getAttributes();
		
		if(!n.hasAttributes())
			return false;
		
		for(int ii=0;ii<attrs.getLength();ii++)
			if(aname.equals(attrs.item(ii).getNodeName()))
			{
				//MEnvironment.errorStream.println(attrs.item(ii));
				return true;
			}		
		return false;
	}
	private static Formula handleTargetMatch(Node tm, MVocab env, String varname, 
			String relname) 
	throws MGEUnsupportedXACML, MGEUnknownIdentifier, MGEBadIdentifierName, MGEManagerException
	{
		
		// MatchID attribute: which function to use?
		NamedNodeMap attrs = tm.getAttributes();
		if(attrs == null)
			throw new MGEUnsupportedXACML("Match element had no attributes: ");
		
		if(attrs.getNamedItem("MatchId") == null)
			throw new MGEUnsupportedXACML("Match element must contain a MatchID attribute.");
		
		String matchid = attrs.getNamedItem("MatchId").getNodeValue();
				
		
		// AttributeValue child 
		Node avChild = getNodeChildNamed(tm, "AttributeValue"); 		
		String av = avChild.getTextContent();		
	
		av = av.replace("!", "."); // Curb your enthusiasm. ! is not allowed.
	
	
		// = signifies match function	
		// We add relname+":" because not all attribute names are descriptive ones. Could have overlap
		// between sorts, and that would be bad.

		String newpredname ;
		
		if(!tm.hasChildNodes())
			throw new MGEUnsupportedXACML("Match element must contain children.");
		if(!hasChildNamed(tm, "AttributeValue"))
			throw new MGEUnsupportedXACML("Match element had no AttributeValue child node.");
		if(hasChildNamed(tm, relname+"AttributeDesignator"))
		{
				
			// <X>AttributeDesignator child's attribute: AttributeId
			Node adChild = getNodeChildNamed(tm, relname+"AttributeDesignator");	
			if(!hasAttributeNamed(adChild, "AttributeId"))
				throw new MGEUnsupportedXACML("Designator has no AttributeId attribute.");
			String adValue = getNodeAttributeValue(adChild, "AttributeId");
			
			newpredname = (relname+":"+adValue + "=_by_"+matchid+"_="+av);
		}
		else if(hasChildNamed(tm, "AttributeSelector"))
		{
			Node asChild = getNodeChildNamed(tm, "AttributeSelector");
			if(!hasAttributeNamed(asChild, "RequestContextPath"))
				throw new MGEUnsupportedXACML("Designator has no AttributeId attribute.");
			String asValue = getNodeAttributeValue(asChild, "RequestContextPath");
			
			newpredname = (relname+":"+asValue + "=_by_"+matchid+"_="+av);
		}
		else
			throw new MGEUnsupportedXACML("Match element did not contain an AttributeSelector or AttributedDesignator.");

				
		if(!env.isSort(newpredname))
		{			
			// New predicate
			env.addSubSort(relname, newpredname);
			
			// Cannot infer a disjointness constraint between two values for the same attribute: 			
			// A request context may contain multiple values for the same attribute.
			// For instance, a request's subject may easily involve multiple roles.			
		}
	
		Relation r = env.getRelation(newpredname);
			
		return MFormulaManager.makeAtom(MFormulaManager.makeVariable(varname), r);
	}

	
	
	private static Formula handleTarget(Node targ, MVocab thevocab) 
	throws MGEUnsupportedXACML,MGEUnknownIdentifier, MGEBadIdentifierName, MGEManagerException
	{

		// DISJUNCTION of CONJUNCTIONS -- default to false. 
		Set<Formula> subf = new HashSet<Formula>(); 
		Set<Formula> actf = new HashSet<Formula>();
		Set<Formula> resf = new HashSet<Formula>();
		Set<Formula> envf = new HashSet<Formula>();
		
		// No restrictions!
		
		if(targ == null)
			throw new MGEUnsupportedXACML("Target node passed is null!");
		if(!targ.hasChildNodes())
			return Formula.TRUE;	
		
		// null for any of these below means no restrictions to target for that sort
		
		
		if(hasMultipleChildrenNamed(targ, "Subjects"))
			throw new MGEUnsupportedXACML("Multiple Subjects children of target.");
		if(hasMultipleChildrenNamed(targ, "Actions"))
			throw new MGEUnsupportedXACML("Multiple Actions children of target.");
		if(hasMultipleChildrenNamed(targ, "Resources"))
			throw new MGEUnsupportedXACML("Multiple Resources children of target.");
		if(hasMultipleChildrenNamed(targ, "Environments"))
			throw new MGEUnsupportedXACML("Multiple Environments children of target.");
		
		// <Subjects>: A DISJUNCTIVE sequence of Subject elements.
		// <Subject>: A CONJUNCTIVE sequence of SubjectMatch elements.
		// If no <Subjects> element is given, the spec says to allow any subjects.
		
		if(!hasChildNamed(targ, "Subjects"))
		{
			subf.add(Formula.TRUE);
		}
		else
		{				
			// Each child must be a Subject (according to the schema)
			NodeList subjects = getNodeChildNamed(targ, "Subjects").getChildNodes();			
			for(int ii=0;ii<subjects.getLength();ii++)
			{
				Node subject = subjects.item(ii);
				if(!"Subject".equals(subject.getNodeName()))
					continue;
				
				Formula thissubject = Formula.TRUE;
				
				// Each child must be a SubjectMatch (according to the schema)
				NodeList subjectmatches = subject.getChildNodes();
				for(int jj = 0;jj<subjectmatches.getLength();jj++)
				{
					Node subjectmatch = subjectmatches.item(jj);
					if(!"SubjectMatch".equals(subjectmatch.getNodeName()))
						continue;

					thissubject = MFormulaManager.makeAnd(thissubject, handleTargetMatch(subjectmatch, thevocab,  "s", "Subject"));										
				}
					
				subf.add(thissubject);
			}
		} // end only one subject
		
		
		if(!hasChildNamed(targ, "Actions"))
		{
			actf.add(Formula.TRUE);
		}
		else
		{
			NodeList actions = getNodeChildNamed(targ, "Actions").getChildNodes();
			
			for(int ii=0;ii<actions.getLength();ii++)
			{
				Node action = actions.item(ii);
				if(!"Action".equals(action.getNodeName()))
					continue;

				Formula thisaction = Formula.TRUE;
				
				NodeList actionmatches = action.getChildNodes();
				for(int jj = 0;jj<actionmatches.getLength();jj++)
				{
					Node actionmatch = actionmatches.item(jj);		
					if(!"ActionMatch".equals(actionmatch.getNodeName()))
						continue;

					thisaction = MFormulaManager.makeAnd(thisaction, handleTargetMatch(actionmatch, thevocab,  "a", "Action"));										
				}
					
				actf.add(thisaction);
			}

		}
		
		if(!hasChildNamed(targ, "Resources"))
		{
			resf.add(Formula.TRUE);
		}
		else
		{
			NodeList resources = getNodeChildNamed(targ, "Resources").getChildNodes();
			
			
			for(int ii=0;ii<resources.getLength();ii++)
			{
				Node resource = resources.item(ii);		
				if(!"Resource".equals(resource.getNodeName()))
					continue;

				Formula thisresource = Formula.TRUE;
				
				NodeList resourcematches = resource.getChildNodes();
				for(int jj = 0;jj<resourcematches.getLength();jj++)
				{
					Node resourcematch = resourcematches.item(jj);	
					if(!"ResourceMatch".equals(resourcematch.getNodeName()))
						continue;

					thisresource = MFormulaManager.makeAnd(thisresource, handleTargetMatch(resourcematch, thevocab,  "r", "Resource"));										
				}
					
				resf.add(thisresource);
			}
		}
			
		if(!hasChildNamed(targ, "Environments"))
		{
			envf.add(Formula.TRUE);
		}
		else
		{
			NodeList environments = getNodeChildNamed(targ, "Environments").getChildNodes();
			
			for(int ii=0;ii<environments.getLength();ii++)
			{
				Node env = environments.item(ii);	
				if(!"Environment".equals(env.getNodeName()))
					continue;

				Formula thisenv = Formula.TRUE;
				
				NodeList envmatches = env.getChildNodes();
				for(int jj = 0;jj<envmatches.getLength();jj++)
				{
					Node envmatch = envmatches.item(jj);
					if(!"EnvironmentMatch".equals(envmatch.getNodeName()))
						continue;

					thisenv = MFormulaManager.makeAnd(thisenv, handleTargetMatch(envmatch, thevocab,  "e", "Environment"));										
				}
					
				envf.add(thisenv);
			}
		}


		// All 4 criteria must apply
		Set<Formula> allparts = new HashSet<Formula>();
		allparts.add( MFormulaManager.makeDisjunction(subf));
		allparts.add( MFormulaManager.makeDisjunction(actf));
		allparts.add( MFormulaManager.makeDisjunction(resf));
		allparts.add( MFormulaManager.makeDisjunction(envf));
		
		//MEnvironment.errorStream.println("subf: "+subf);
		//MEnvironment.errorStream.println("actf: "+actf);
		//MEnvironment.errorStream.println("resf: "+resf);
		//MEnvironment.errorStream.println("envf: "+envf);
		
		return MFormulaManager.makeConjunction(allparts);
	}

	
	
	static MRule handleRule(Node rule, MVocab env) throws MGEUnsupportedXACML, MGEUnknownIdentifier, MGEBadIdentifierName, MGEManagerException
	{
		MRule mr = new MRule();
		
		NamedNodeMap ruleattrs = rule.getAttributes();
		if(ruleattrs == null)
			throw new MGEUnsupportedXACML("Rule had no attributes.");
		
		if(ruleattrs.getNamedItem("RuleId") != null)
			mr.name = ruleattrs.getNamedItem("RuleId").getNodeValue();
		
		if(ruleattrs.getNamedItem("Effect") == null)
			throw new MGEUnsupportedXACML("Rule must have effect.");
		
		mr.setDecision(ruleattrs.getNamedItem("Effect").getNodeValue().toLowerCase());
	
				
		if(!rule.hasChildNodes())
			throw new MGEUnsupportedXACML("Rule must have child nodes.");
		NodeList children = rule.getChildNodes();
		
		boolean seenTarget = false;
		boolean seenCondition = false;
		for(int ii = 0;ii<children.getLength();ii++)
		{
			Node child = children.item(ii);
			
    		if("target".equals(child.getNodeName().toLowerCase()) && !seenTarget)
    		{
    			mr.target = handleTarget(child, env);
    			seenTarget = true;
    		}
    		else if("target".equals(child.getNodeName().toLowerCase()))
    			throw new MGEUnsupportedXACML("Two different TARGET elements.");
    		
    		if("condition".equals(child.getNodeName().toLowerCase()) && !seenCondition)
    		{
    			
    			handleRuleCondition(mr, child, env);
    			seenCondition = true;
    		}
    		else if("condition".equals(child.getNodeName().toLowerCase()))
    			throw new MGEUnsupportedXACML("Two different CONDITION elements in rule.");
    		
		}
				
		mr.target_and_condition = MFormulaManager.makeAnd(mr.target, mr.condition);
		return mr;
	}
	
	
	private static String buildConditionPredicate(Node apply, Set<String> involves) throws MGEUnsupportedXACML
	{
		//String result = func.getFunction().getIdentifier().getSchemeSpecificPart().toString() + "(";
				
		if(!hasAttributeNamed(apply, "FunctionId"))
			throw new MGEUnsupportedXACML("No FunctionId attribute in Apply element.");
						
		String result = getNodeAttributeValue(apply, "FunctionId")+ "(";
		String connective = "";
	
		NodeList children = apply.getChildNodes();
		
		// for each function parameter
		for(int ii=0;ii<children.getLength();ii++)
		{
			Node child = children.item(ii);
						
			if("Apply".equals(child.getNodeName()))
			{
				// another function application				
				result = result + connective + buildConditionPredicate(child, involves);
				connective = ",";
				
			}
			else if("Function".equals(child.getNodeName()))
			{
				// This is a *function* given as an argument to the outer apply.
				// (See e.g. IIC170)
				// any-of( string-equal, "some value", bag-of-strings)
				
				String funcid = getNodeAttributeValue(child, "FunctionId");
				result = result + connective + funcid;
				connective = ",";				
			}
			else if("AttributeValue".equals(child.getNodeName()))
			{
				// constant value				
				String av = child.getTextContent();
				av = av.replace("!", "."); // Curb your enthusiasm. ! is not allowed.
				result = result + connective + av;
				connective = ",";
			}        
			else if("AttributeSelector".equals(child.getNodeName()))
			{
				// AttributeSelector is used for resolving values from the request using XPath expressions
				// (For instance, reading from the resource file to see if it is a letter addressed to the subject.)
				// Always returns a Bag of attribute values!
				
				// Attribute RequestContextPath
				if(!hasAttributeNamed(child, "RequestContextPath"))
					throw new MGEUnsupportedXACML("AttributeSelector without RequestContextPath.");
				
				String rcp = getNodeAttributeValue(child, "RequestContextPath");				
				result = result + connective + rcp;
			}
			else if(child.getNodeName().endsWith("AttributeDesignator"))
			{
				if(!hasAttributeNamed(child, "AttributeId"))
					throw new MGEUnsupportedXACML("Attribute Designator had no AttributeId field.");
				
				String aid = getNodeAttributeValue(child, "AttributeId");
				
				if("SubjectAttributeDesignator".equals(child.getNodeName()))
				{
					result = result + connective + "subject:" + aid;
					involves.add("s");
				}
				else if("ActionAttributeDesignator".equals(child.getNodeName()))
				{
					result = result + connective + "action:" + aid;
					involves.add("a");
				}
				else if("ResourceAttributeDesignator".equals(child.getNodeName()))
				{					
					result = result + connective + "resource:" + aid;
					involves.add("r");
				}
				else if("EnvironmentAttributeDesignator".equals(child.getNodeName()))
				{
					result = result + connective + "environment:" + aid;
					involves.add("e");
				}
				else				
					throw new MGEUnsupportedXACML("AttributeDesignator with non subj/act/res/env type.");
				
				connective = ",";
			}
			else if("#text".equals(child.getNodeName()))
				; // do nothing for newlines and stuff
			else
				throw new MGEUnsupportedXACML("Unexpected type in Apply child: "+child.getNodeName());
		}
		
		//MEnvironment.errorStream.println(result+")");
				
		return result+")";		
	}

	
	private static void handleRuleCondition(MRule mr, Node cond, MVocab env)
	throws MGEUnsupportedXACML, MGEManagerException, MGEBadIdentifierName
	{
		mr.condition = Formula.TRUE;
		
		if(cond == null)
			return; // no condition!
							
		// Build a state predicate that governs whether or not this condition holds.
		// Predicate name will be this function's name (child names, possibly recursively derived)

		Set<String> involves = new HashSet<String>();	
		
		// Need to get the Apply inside the cond. 
		if(!hasChildNamed(cond, "Apply"))
			throw new MGEUnsupportedXACML("Condition element must contain an Apply element.");
		
		Node apply = getNodeChildNamed(cond, "Apply");
		
		String newname = buildConditionPredicate(apply, involves);
		
		// assemble type construct string
		// order matters!
		List<String> involves_list = new ArrayList<String>(involves);
			
		if(involves.contains("s"))
			involves_list.add("Subject");
		if(involves.contains("a"))
			involves_list.add("Action");
		if(involves.contains("r"))
			involves_list.add("Resource");
		if(involves.contains("e"))
			involves_list.add("Environment");

		// We want to support "nullary" state predicates. The problem is that 
		// Kodkod requires Relation arity to always be >= 1. 
		// So we kludge it: If a Condition is truly nullary, we pretend it depends
		// on the Environment.		

		if(involves_list.size() < 1)
			involves_list.add("Environment");

				
		try
		{			
			String construct = "";
			for(String s : involves_list)	
				construct = construct + s + " ";
			
			// create the predicate
			env.addPredicate(newname, construct);
			
		}
		catch(MGEBadIdentifierName e)
		{
			throw new MGEUnsupportedXACML("Identifier problem: "+e.getMessage());
		}
			
		
		// create the formula for this condition in the rule object		
		
		try
		{  									
			List<String> varsList = new ArrayList<String>(involves_list.size());
			for(String s : involves_list)
				varsList.add(s);			
						
			kodkod.ast.Expression tuple = MFormulaManager.makeExprTuple(varsList);
			mr.condition = MFormulaManager.makeAtom(tuple, env.getRelation(newname)); 
		}
		catch(MGEUnknownIdentifier e)
		{
			throw new MGEUnsupportedXACML("Identifier problem with getVariable: "+e.getMessage());
		}				
		
	}
	
	static MPolicy findPolicyWithId(String polid, String schemaFileName, String policyDir, String fieldname) 
	throws MUserException, SAXException
	{		
		// Don't show errors for parser problems
		MGErrorHandler eh = new MGErrorHandler(false, false);

		MPolicy result = null;
		
		// Build a list of all the XML files in that directory...
		File dir = new File(policyDir);
		for(String sFileName : dir.list())
		{			
			
			// Will return "" if a leaf when we want a set (or vice versa)
			String policyid = getIdFromFile(policyDir+sFileName, schemaFileName, fieldname, eh);
			
			if(policyid.length() > 0)
			{
				addToIdCache(policyid, policyDir+sFileName);				
				//MEnvironment.errorStream.println("Added: "+policyid + " -> "+policyDir+sFileName);
			}
			
			if(polid.equals(policyid))
			{
				// found it!
				// keep iterating to build cache.
				result = loadXACML20(policyDir+sFileName, schemaFileName, policyDir);
			}
		}
		
		return result;
	}
	
	static MPolicy recXACMLPolicyTree(Node n, MVocab env, String schemaFileName, String policyDir)
	throws MUserException, DOMException, SAXException
	{
		String nodename = n.getNodeName();		
		
		NamedNodeMap attrs = n.getAttributes();
		NodeList children = n.getChildNodes();
											
		if("PolicySet".equals(nodename))
		{
			// Should be a child target,
			
			Node polCombAlgNode = attrs.getNamedItem("PolicyCombiningAlgId");
			Node policyIDNode = attrs.getNamedItem("PolicySetId"); 
			if(polCombAlgNode == null)
				throw new MGEUnsupportedXACML("No policy combination algorithm.");
			if(policyIDNode == null)
				throw new MGEUnsupportedXACML("No policy set ID.");
			String policyID = policyIDNode.getNodeValue();			

			MPolicySet polset = new MPolicySet(policyID, env);
			polset.isXACML = true;
			polset.handleXACML2Combine(polCombAlgNode); //  "PolicyCombiningAlgId"
			
			// We have a target and a description for the set
			// we have any number of policy or policyset nodes
			
			MGErrorHandler eh = new MGErrorHandler(false, false);
			
			boolean seenTarget = false;
			boolean seenDescription = false;
			if(children != null)
	        {
	        	for(int j=0;j<children.getLength(); j++)
	        	{
	        		Node child = children.item(j);
	        		
	        		// POLICYSET
	        		// *****
	        		if("policyset".equals(child.getNodeName().toLowerCase()))
	        		{
	        			MPolicySet childset = (MPolicySet)recXACMLPolicyTree(child, env, schemaFileName, policyDir);	        			
	        			polset.addChild(childset);	        			
	        		}
	        		
	        		// POLICY
	        		// *****
	        		else if("policy".equals(child.getNodeName().toLowerCase()))
	        		{
	        			MPolicyLeaf childleaf = (MPolicyLeaf)recXACMLPolicyTree(child, env, schemaFileName, policyDir);	        			
	        			polset.addChild(childleaf);
	        		}
	        		
	        		// PolicyIdReference
	        		// *****************
	        		else if("policyidreference".equals(child.getNodeName().toLowerCase()))
	        		{
	        			// Do we have a cached filename for this id?
	        			if(isCached(child.getTextContent()))
	        			{
	        				// Make sure the cached value is still valid.
	        				//(String sFileName, String sSchemaFileName, String fieldname, ErrorHandler eh)
	        				String current = getIdFromFile(getFromCache(child.getTextContent()), schemaFileName, "PolicyId", eh);
	        				if(current.equals(child.getTextContent()))	        			
	        					return loadXACML20(getFromCache(child.getTextContent()), schemaFileName, policyDir);
	        			}
	        				    
	        			MEnvironment.errorWriter.println("  Rebuilding policy cache due to reference: "+child.getTextContent());
	        			MEnvironment.errorWriter.println("  Please wait...");
	        			
	        			// String polid, String schemaFileName, String policyDir, String fieldname
	        			MPolicyLeaf ref = (MPolicyLeaf)findPolicyWithId(child.getTextContent(), schemaFileName, policyDir, "PolicyId");
	        			
	        			if(ref == null)
	        				throw new MGEUnsupportedXACML("Could not find a policy with id: "+child.getTextContent());
	        			polset.addChild(ref);	        		
	        		}

	        		// PolicySetIdReference
	        		// *****************
	        		else if("policysetidreference".equals(child.getNodeName().toLowerCase()))
	        		{
	        			if(isCached(child.getTextContent()))
	        			{
	        				// Make sure the cached value is still valid.
	        				//(String sFileName, String sSchemaFileName, String fieldname, ErrorHandler eh)
	        				String current = getIdFromFile(getFromCache(child.getTextContent()), schemaFileName, "PolicySetId", eh);
	        				if(current.equals(child.getTextContent()))	        			
	        					return loadXACML20(getFromCache(child.getTextContent()), schemaFileName, policyDir);
	        			}

	        			MEnvironment.errorWriter.println("  Rebuilding policy cache due to reference: "+child.getTextContent());
	        			MEnvironment.errorWriter.println("  Please wait...");
	        			
	        			MPolicySet ref = (MPolicySet)findPolicyWithId(child.getTextContent(), schemaFileName, policyDir, "PolicySetId");
	        			if(ref == null)
	        				throw new MGEUnsupportedXACML("Could not find a policyset with id: "+child.getTextContent());

	        			polset.addChild(ref);	        		
	        		}	        		
	        		
	        		// TARGET
	        		// *****
	        		else if("target".equals(child.getNodeName().toLowerCase()) && !seenTarget)
	        		{
	        			polset.target = handleTarget(child, env);
	        			seenTarget = true;
	        		}
	        		else if("target".equals(child.getNodeName().toLowerCase()))
	        			throw new MGEUnsupportedXACML("Two different TARGET elements.");
	        		
	        		// DESCRIPTION
	        		// *****
	        		else if("description".equals(child.getNodeName().toLowerCase()) && !seenDescription)
	        		{
	        			// No field for description (for now)
	        			seenDescription = true;
	        		}
	        		else if("description".equals(child.getNodeName().toLowerCase()))
	        			throw new MGEUnsupportedXACML("Two different Description elements.");
	        		
	        		else if("policydefaults".equals(child.getNodeName().toLowerCase()))
	        		{
	        			// Do nothing: This only gives a default XPath version for the policy
	        		}
	        		
	        		// #text (whitespace, etc.)
	        		else if("#text".equals(child.getNodeName()))
	        			; // do nothing
	        		else
	        			throw new MGEUnsupportedXACML("Unsupported node name: "+child.getNodeName());
	        		
	        	} // end for each child
	        } // end if children exist
			
			
			//polset.printPolicyInfo();
			
			polset.initIDBs();
			return polset;
		}
		else if("Policy".equals(nodename))
		{
			// children are rules, target...
			
			Node ruleCombAlgNode = attrs.getNamedItem("RuleCombiningAlgId");
			Node policyIDNode = attrs.getNamedItem("PolicyId"); 
			if(ruleCombAlgNode == null)
				throw new MGEUnsupportedXACML("No rule combination algorithm.");
			if(policyIDNode == null)
				throw new MGEUnsupportedXACML("No policy ID.");
			String policyID = policyIDNode.getNodeValue();			

			MPolicyLeaf pol = new MPolicyLeaf(policyID, env);
			pol.isXACML = true;

			pol.handleXACML2Combine(ruleCombAlgNode); // "RuleCombiningAlgId"
			
			// CASE SENSITIVE
			
			
			boolean seenTarget = false;
			boolean seenDescription = false;
			if(children != null)
	        {
	        	for(int j=0;j<children.getLength(); j++)
	        	{
	        		Node child = children.item(j);
	        		
	        		// RULE
	        		// *****
	        		if("rule".equals(child.getNodeName().toLowerCase()))
	        		{
	        			// Add this rule
	        			MRule mr = handleRule(child, env);
	        				        			
	        			// Add the rule
	        			pol.rules.add(mr);
	        		}
	        		
	        		// TARGET
	        		// *****
	        		else if("target".equals(child.getNodeName().toLowerCase()) && !seenTarget)
	        		{
	        			pol.target = handleTarget(child, env);
	        			seenTarget = true;
	        		}
	        		else if("target".equals(child.getNodeName().toLowerCase()))
	        			throw new MGEUnsupportedXACML("Two different TARGET elements.");
	        		
	        		// DESCRIPTION
	        		// *****
	        		else if("description".equals(child.getNodeName().toLowerCase()) && !seenDescription)
	        		{
	        			// No field for description (for now)
	        			seenDescription = true;
	        		}
	        		else if("description".equals(child.getNodeName().toLowerCase()))
	        			throw new MGEUnsupportedXACML("Two different Description elements.");
	        		
	        		else if("policydefaults".equals(child.getNodeName().toLowerCase()))
	        		{
	        			// Do nothing: This only gives a default XPath version for the policy
	        		}

	        		
	        		// #text (whitespace, etc.)
	        		else if("#text".equals(child.getNodeName()))
	        			; // do nothing
	        		else
	        			throw new MGEUnsupportedXACML("Unsupported node name: "+child.getNodeName());
	        		
	        	} // end for each child
	        } // end if children exist
			
			pol.initIDBs();						
			return pol;

		}
		else
			throw new MGEUnsupportedXACML("Element "+nodename+" not supported.");
		
	}
	
}

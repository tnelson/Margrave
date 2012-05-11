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

import kodkod.ast.*;

import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.output.WriterOutputStream;
import org.w3c.dom.Document;
//import org.w3c.dom.Element;
//import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public class MCommunicator 
{
	static final InputStream in = System.in;
	static final PrintStream out = System.out;
	static final char semicolon = ';';
	
	static final String setupError = "<MARGRAVE-RESPONSE type=\"fatal-error\"><ERROR>Unable to send XML reply.</ERROR></MARGRAVE-RESPONSE>";
	
	static String sLogFileName = "margrave-log.txt";
	static BufferedWriter outLog = null; 
	static FileWriter outLogStream = null;
	
	static boolean bDoLogging = false;
	
	static String makeLastResortError()
	{
		return "<MARGRAVE-RESPONSE type=\"fatal-error\"><ERROR>Unable to produce XML document</ERROR></MARGRAVE-RESPONSE>";
	}
	
	static String makeDetailedError(String str)
	{
		return "<MARGRAVE-RESPONSE type=\"fatal-error\"><ERROR>"+str+"</ERROR></MARGRAVE-RESPONSE>";		
	}
	
	public static void main(String[] args) 
	{
		if(args.length > 0 && args[0].toLowerCase().equals("-log"))
		{
			// parser is in racket now. instead, require -log switch for logging
			//MEnvironment.debugParser = true;
			bDoLogging = true;
		}					
	 
		// Re-direct all System.err input to our custom buffer		
		// Uses Apache Commons IO for WriterOutputStream.
		System.setErr(new PrintStream(new WriterOutputStream(MEnvironment.errorWriter), true));
	
		// Re-direct all System.out input to our custom buffer.
		// (We have already saved System.out.)
		// This is useful in case we start getting GC messages from SAT4j.
		System.setOut(new PrintStream(new WriterOutputStream(MEnvironment.outWriter), true));
				
		initializeLog();
		writeToLog("\n\n");
		
		// Inform the caller that we are ready to receive commands
		sendReadyReply();
		
		while(true)
		{
			// Block until a command is received, handle it, and then return the result.
			handleXMLCommand(in);
		} // end loop while(true)
		
		// outLog will be closed as it goes out of scope
	}

	public static void sendReadyReply()
	{
		Document theResponse = MEnvironment.successResponse();
		addBuffers(theResponse);    	
		writeToLog("Returning: " + transformXMLString(theResponse) + "\n");
		try
		{
			out.write(transformXML(theResponse));
		} catch (IOException ex)
        {
            Logger.getLogger(MCommunicator.class.getName()).log(Level.SEVERE, null, ex);
            writeToLog("\nIOException in handleXMLCommand while parsing command stream: "+ex.getLocalizedMessage());
            System.exit(1);
        }
	}
	
	public static void handleXMLCommand(String str)
	{
		// For test cases only!
		InputStream theInput = new ByteArrayInputStream(str.getBytes());
		handleXMLCommand(theInput);
	}
	
	public static void handleXMLCommand(InputStream commandStream)
	{        	
		DocumentBuilder docBuilder = null;
		DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
		try
		{
			docBuilder = docFactory.newDocumentBuilder();
		}
		catch (ParserConfigurationException ex)
		{
			Logger.getLogger(MCommunicator.class.getName()).log(Level.SEVERE, null, ex);
			writeToLog("\nParserConfigurationException at beginning of handleXMLCommand: "+ex.getLocalizedMessage());
			writeToLog("\nTerminating engine...");
			System.exit(200);
        }	
            	
            // Save this command for use in exception messages
    		//MEnvironment.lastCommandReceived = command.trim();    		
            
            Document doc = null;            

            try {
               // doc = docBuilder.parse(new InputSource(new StringReader(command)));            	
            	writeToLog("========================================\n========================================\n");
            	writeToLog("\n"+commandStream.available());
            	
            	StringBuffer inputStringBuffer = new StringBuffer();            	
            	while(true)
            	{
            		// if available=0; we want to block. But don't want to allocate too much room:
            		if(commandStream.available() < 1)
            		{
            			int b = commandStream.read();
            			inputStringBuffer.append((char) b);
            			//writeToLog("\n(Blocked and then got) character: `"+(char)b+"`"); 
            		}
            		else
            		{
                		byte[] inputBytes = new byte[commandStream.available()];         
                		@SuppressWarnings("unused")
						int bytesRead = commandStream.read(inputBytes);    
                		String block = new String(inputBytes);
                		inputStringBuffer.append(block);            		
                		//writeToLog("\n(Didn't block for) String: `"+block+"`");            		         		            			
            		}            		
            		
            		// Bad kludge. Couldn't get proper XML parse function working, so
            		// did this. Should re-write (preferably with correct XML handling functions!)
            		// The trim() call below is especially egregious... - TN
            		            		
            		String sMargraveCommandEnding = "</MARGRAVE-COMMAND>";
            		String bufferStr = inputStringBuffer.toString();            		
            		if(bufferStr.trim().endsWith(sMargraveCommandEnding))
            			break;
            	}
            	
            	String cmdString = inputStringBuffer.toString();
            	writeToLog("\n\n*********************************\nDONE! Received command: `"+cmdString+"`\n");
            	
            	doc = docBuilder.parse(new InputSource(new StringReader(cmdString)));
            	
            	//doc = docBuilder.parse(commandStream);            	
            	//doc = docBuilder.parse(new InputSource(commandStream));
   				writeToLog((new Date()).toString());
   				writeToLog("\nExecuting command: " + transformXMLString(doc) + "\n");   					   				            	
            } 
            catch (SAXException ex) 
            {
                Logger.getLogger(MCommunicator.class.getName()).log(Level.SEVERE, null, ex);
                writeToLog("\nSAXException in handleXMLCommand while parsing command stream: "+ex.getLocalizedMessage());
            }
            catch (IOException ex)
            {
                Logger.getLogger(MCommunicator.class.getName()).log(Level.SEVERE, null, ex);
                writeToLog("\nIOException in handleXMLCommand while parsing command stream: "+ex.getLocalizedMessage());
            }
            
            Document theResponse;
            try
            {
                // protect against getFirstChild() call            
                if(doc != null)
                	theResponse = xmlHelper(doc.getFirstChild(), "");
                else
                	theResponse = MEnvironment.errorResponse(MEnvironment.sNotDocument, MEnvironment.sCommand, "");
            }
            catch(Exception e)
            {
            	// Construct an exception response;            	
            	theResponse = MEnvironment.exceptionResponse(e);            	
            }
            catch(Throwable e)
            {
            	// This would ordinarily be a terrible thing to do (catching Throwable)
            	// However, we need to warn the client that we're stuck.
            	
            	try
            	{
            		// First log that we got an exception:
            		writeToLog("\n~~~ Throwable caught: "+e.getClass());
            		writeToLog("\n    "+e.getLocalizedMessage());
            		writeToLog("\n"+Arrays.toString(e.getStackTrace()));
            		writeToLog("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
            		theResponse = MEnvironment.exceptionResponse(e);
            	}
            	catch(Throwable f)
            	{
            		// If we can't even warn the client, at least close down the engine. Client will detect EOF.
            		
            		theResponse = null;
            		System.exit(101);
            	}
            	System.exit(100); 
            }
            
            try 
            {
            	addBuffers(theResponse);
            	
        		writeToLog("Returning: " + transformXMLString(theResponse) + "\n");
        		out.write(transformXML(theResponse));
        	} catch (IOException e)
        	{
        		// don't do this. would go through System.err
        		//e.printStackTrace();
        	}
        	out.flush(); // ALWAYS FLUSH!
        } // end handleXMLCommand

        protected static void addBuffers(Document theResponse)
        {
        	// add in any supplemental or error information                     	
        	Element envOutChild = theResponse.createElementNS(null, "EXTRA-OUT");
        	envOutChild.appendChild(theResponse.createTextNode(MEnvironment.outBuffer.toString()));
        	Element envErrChild = theResponse.createElementNS(null, "EXTRA-ERR");
        	envErrChild.appendChild(theResponse.createTextNode(MEnvironment.errorBuffer.toString()));
        	
        	// Clear out the "out" and "error" buffers.        	
        	MEnvironment.errorBuffer.getBuffer().setLength(0);
        	MEnvironment.outBuffer.getBuffer().setLength(0);        	        	
        	
        	theResponse.getDocumentElement().appendChild(envOutChild);
        	theResponse.getDocumentElement().appendChild(envErrChild);
        }
        
        
        //Takes a MARGRAVE-COMMAND node
        private static Document xmlHelper(Node margraveCommandNode, String originalXMLText) throws MBaseException
        {        	
        	//List<Node> childNodes = getElementChildren(node);
        	
        	//String type = node.getAttributes().item(0).getNodeValue();
        	writeToLog("\n  In XML Helper...");

        	Document theResponse = null;
        	        	        	
        	if (margraveCommandNode.getNodeType() == Node.ELEMENT_NODE)
        	{
        		String type = getNodeAttribute(margraveCommandNode, "type");
        		writeToLog("  At command node. Type attribute was: " + type + "\n");
        		if(type == null)
        			return MEnvironment.errorResponse(MEnvironment.sUnknown, MEnvironment.sCommand, "<null command type>");
        		
        		
        		if(type.equalsIgnoreCase("SET"))
        		{
        			Node setNode = getChildNode(margraveCommandNode, "SET");
        			
        			String setParameter = getNodeAttribute(setNode, "parameter");
        			String setSubParameter = getNodeAttribute(setNode, "subparameter");
        			String setValue = getNodeAttribute(setNode, "value");
        			
        			if("CEILING".equalsIgnoreCase(setParameter))
        			{
        				try
        				{
        					int intValue = Integer.parseInt(setValue);
            				// Global parameters for size ceilings per sort
            				theResponse = MEnvironment.setSortCeiling(setSubParameter, intValue);

        				}
        				catch(NumberFormatException e)
        				{
        					theResponse = MEnvironment.errorResponse(MEnvironment.sNotDocument, "Not an integer", setValue);
        				}        				
        			}
        			else
        			{
        				theResponse = MEnvironment.errorResponse(MEnvironment.sCommand, MEnvironment.sUnknown, setParameter);
        			}
        			
        		}        			
        		else if (type.equalsIgnoreCase("EXPLORE"))
        		{
        			theResponse = handleExplore(originalXMLText, margraveCommandNode);        			
        		}
        		else if (type.equalsIgnoreCase("INFO")) {
        			writeToLog("In Info");
        			String idString = getInfoId(margraveCommandNode);
        			writeToLog("\nPast getting id info");
        			if (idString != null) {
        				theResponse = MEnvironment.printInfo(idString); 
        			}
        			else {
        				theResponse = MEnvironment.printSystemInfo(); 
        			}
        			writeToLog("Returning from info");
        		}

        		//Create Statement
        		else if (type.equalsIgnoreCase("CREATE POLICY LEAF")) {
        			String pname = getPolicyName(margraveCommandNode);
        			String vname = getVocabName(margraveCommandNode);
        			theResponse = MEnvironment.createPolicyLeaf(pname, vname);
        		} 
        		else if (type.equalsIgnoreCase("CREATE POLICY SET")) {
        			String pname = getPolicyName(margraveCommandNode);
        			String vname = getVocabName(margraveCommandNode);
        			theResponse = MEnvironment.createPolicySet(pname, vname);
        		} 
        		else if (type.equalsIgnoreCase("CREATE VOCABULARY")) {
        			writeToLog("In Create Vocabulary\n");
        			String vname = getVocabName(margraveCommandNode);
        			writeToLog("Got Vocab name. It is: " + vname + "\n");
        			theResponse = MEnvironment.createVocabulary(vname);
        			if (theResponse == null) {
        				writeToLog("The result of create vocabulary was null!!\n");
        			}
        			writeToLog("Finished Create Vocabulary\n");
        		} 
        		else if (type.equalsIgnoreCase("LOAD XACML POLICY")) {
        			String fileName = getLoadFileName(margraveCommandNode);
        			String schemaFileName = getLoadSchemaFileName(margraveCommandNode);
        			theResponse = MEnvironment.loadXACML(fileName, schemaFileName);
        		}
        		else if (type.equalsIgnoreCase("LOAD SQS POLICY")) {
        			String fileName = getLoadFileName(margraveCommandNode);
        			theResponse = MEnvironment.loadSQS(fileName);
        		}

        		else if (type.equalsIgnoreCase("PREPARE")) {
        			String pname = getPolicyName(margraveCommandNode);
        			theResponse = MEnvironment.preparePolicy(pname);
        		} 
        		else if (type.equalsIgnoreCase("DELETE VOCABULARY")) {
        			String vname = getVocabName(margraveCommandNode);
        			theResponse = MEnvironment.deleteVocabulary(vname);
        		} 

     

        		else if (type.equalsIgnoreCase("SET TARGET FOR POLICY")) {
        			String pname = getPolicyName(margraveCommandNode);
        			
					// Target fmla
					Node targetNode = getChildNode(margraveCommandNode, "TARGET"); 							
					assert(targetNode != null);
					MExploreCondition targetCondition = exploreHelper(targetNode.getFirstChild());
					Formula target = targetCondition.fmla;
        			        			
        			theResponse = MEnvironment.setPolicyTarget(pname, target);
        		}	
        		else if (type.equalsIgnoreCase("SET RCOMBINE FOR POLICY")) {
        			String pname = getPolicyName(margraveCommandNode);
        			//List<String> idl = getIdentifierList(n);

        			Set<String> rFA = new HashSet<String>();
        			Map<String, Set<String>> rO = new HashMap<String, Set<String>>();

        			handleComb(margraveCommandNode, rFA, rO);        				        				
        			theResponse = MEnvironment.setRCombine(pname, rFA, rO);
        		}	
        		else if (type.equalsIgnoreCase("SET PCOMBINE FOR POLICY")) {
        			String pname = getPolicyName(margraveCommandNode);        				
        			Set<String> rFA = new HashSet<String>();
        			Map<String, Set<String>> rO = new HashMap<String, Set<String>>();

        			handleComb(margraveCommandNode, rFA, rO);        				
        			theResponse = MEnvironment.setPCombine(pname, rFA, rO);
        		}
        		else if (type.equalsIgnoreCase("QUIT"))
        		{
        			MEnvironment.quitMargrave();
        		}

        		else if (type.equalsIgnoreCase("RESET"))
        		{
        			// "<MARGRAVE-COMMAND type=\"RESET\"><RESET id=\"Myqry\" /></MARGRAVE-COMMAND>"
        			String id = getAttributeOfChildNodeOrNode(margraveCommandNode, "RESET", "id");
        			theResponse = MEnvironment.resetIterator(id);        				
        		}
        		else if (type.equalsIgnoreCase("IS-POSSIBLE")) {
        			String id = getAttributeOfChildNodeOrNode(margraveCommandNode, "IS-POSSIBLE", "id");
        			theResponse = MEnvironment.isPoss(id);
        			writeToLog("Returning from IS-POSSIBLE");
        		}
        		/*else if (type.equalsIgnoreCase("IS-GUARANTEED")) {
        			String id = getAttributeOfChildNodeOrNode(margraveCommandNode, "IS-GUARANTEED", "id");
        			theResponse = MEnvironment.isGuar(id);
        		}*/
        		else if (type.equalsIgnoreCase("SHOW")) {
        			writeToLog("In show");
        			String showType = getShowType(margraveCommandNode);
        			writeToLog("In show");
        			String id = getAttributeOfChildNodeOrNode(margraveCommandNode, "SHOW", "id");

        			Node showNode = getChildNode(margraveCommandNode, "SHOW");
        			
        			writeToLog("\nshowtype: " + showType + "\n");
        			if (showType.equalsIgnoreCase("ONE"))
        			{
        				writeToLog("\nIn SHOW ONE:");

        				Node includeNode = getIncludeNode(showNode);        					
        				List<Node> idbChildNodes = getElementChildren(includeNode);    						
        				HashMap<String, Set<List<MTerm>>> includeMap = atomicFormulasToHashmap(idbChildNodes);

        				try
        				{
        					writeToLog("\n  id was: "+id+"; includeMap was: "+includeMap+"; idbChildNodes had this many elements: "+idbChildNodes.size());
        					theResponse = MEnvironment.getFirstModel(id, includeMap);
        				} catch (MBaseException e) {
        					theResponse = MEnvironment.exceptionResponse(e);						
        				}
        			}
        			else if (showType.equalsIgnoreCase("NEXT"))
        			{
        				writeToLog("\nIn SHOW NEXT:");
        				
        				Node includeNode = getIncludeNode(showNode);
        				List<Node> idbChildNodes = getElementChildren(includeNode);          				
        				HashMap<String, Set<List<MTerm>>> includeMap = atomicFormulasToHashmap(idbChildNodes);

        				try
        				{
        					writeToLog("\n  id was: "+id+"; includeMap was: "+includeMap+"; idbChildNodes had this many elements: "+idbChildNodes.size());
        					theResponse = MEnvironment.getNextModel(id, includeMap);
        				} catch (MBaseException e) {
        					theResponse = MEnvironment.exceptionResponse(e);						
        				}
        			}
        			/*else if (showType.equalsIgnoreCase("CEILING")) {
        				theResponse = MEnvironment.showCeiling(id);
        			}*/
        			else if (showType.equalsIgnoreCase("REALIZED") ||
        					showType.equalsIgnoreCase("UNREALIZED")) 
        			{
        				String popId;
        				if (showType.equalsIgnoreCase("REALIZED")) {
        					popId = getPopulatedId(margraveCommandNode); // command
        				}
        				else {
        					popId = getUnpopulatedId(margraveCommandNode); // command
        				}
        				
        				Node forCasesNode = getForCasesNode(showNode);
        				List<Node> atomicFormulaNodes = getElementChildren(showNode);

        				// not this call
        				//NodeList atomicFormulaNodes = getAtomicFormulaNodesFromList(n);

        				// atomicFormulasToHashmap will ignore the FOR-CASES element.
        				Map<String, Set<List<MTerm>>> atomicFormulas = atomicFormulasToHashmap(atomicFormulaNodes);


        				// Default map is empty. If FOR CASES, populate it.
        				Map<String, Set<List<MTerm>>> forCasesAtomicFormulas = new HashMap<String, Set<List<MTerm>>>();
        				if (forCasesNode != null)
        				{
        					//NodeList forCasesAtomicFormulaNodes = getAtomicFormulaNodesFromList(forCasesNode);
        					List<Node> forCasesAtomicFormulaNodes = getElementChildren(forCasesNode);
        					forCasesAtomicFormulas = atomicFormulasToHashmap(forCasesAtomicFormulaNodes);        						
        				}

        				//writeToLog(showType+" " + popIdString + " " + atomicFormulas + " --- " + forCasesAtomicFormulas);

        				// Get the result and return it
        				if (showType.equalsIgnoreCase("REALIZED")) {
        					theResponse = MEnvironment.showRealized(popId, atomicFormulas, forCasesAtomicFormulas);
        				}
        				else {
        					theResponse = MEnvironment.showUnrealized(popId, atomicFormulas, forCasesAtomicFormulas);
        				}        					

        			} // end pop/unpop


        		} // end show
        		else if (type.equalsIgnoreCase("COUNT")) {
        			String id = getCountId(margraveCommandNode);
        			theResponse = MEnvironment.countModels(id);

        			Node sizeNode = getSizeNode(margraveCommandNode);
        			if (sizeNode != null) {
        				Integer countSize = Integer.parseInt(getCountSize(sizeNode));
        				theResponse = MEnvironment.countModels(id, countSize);
        			}
        			else {
        				theResponse = MEnvironment.countModels(id);
        			}
        		}

        		else if (type.equalsIgnoreCase("GET-INFO"))
        		{
        			// n is the margrave-command node.
        			String getType = getGetInfoType(margraveCommandNode); 
        			Node getNode = getChildNode(margraveCommandNode, "GET-INFO");
        			String pname = getPolicyName(getNode);
        			String decname = getDecisionName(getNode);
        			
        			// decname will be null, if the user doesn't want to limit by decision

        			writeToLog("\n");
        			writeToLog("GET-INFO: "+getType+" "+getNode + " " + pname +" "+decname);

        			String rname = "";

        			if (getType.equalsIgnoreCase("HIGHER-PRIORITY-THAN")) {
        				theResponse = MEnvironment.getHigherPriorityThan(pname, rname);
        			}
        			else if (getType.equalsIgnoreCase("RULES")) {
        				theResponse = MEnvironment.getRulesIn(pname, false, decname);
        			}
        			else if (getType.equalsIgnoreCase("QUALIFIED-RULES")) {
        				theResponse = MEnvironment.getRulesIn(pname, true, decname);
        			}
        		}        			        		

        		//Add Statement
        		else if (type.equalsIgnoreCase("ADD")) {
        			
        			Node childNode = margraveCommandNode.getFirstChild();
        			
        			if (childNode.getNodeName().equalsIgnoreCase("VOCAB-IDENTIFIER"))
        			{
        				theResponse = handleAddVocabFact(margraveCommandNode, childNode);
        			}
        			else if (childNode.getNodeName().equalsIgnoreCase("POLICY-IDENTIFIER"))
        			{
        				String pname = getPolicyName(margraveCommandNode);

        				Node secondChildNode = childNode.getNextSibling(); // WORRY Shouldn't be hardcoded in!!
        				if(secondChildNode.getNodeName().equalsIgnoreCase("RULE"))
        				{        					
        					String rname= getRuleName(margraveCommandNode);

        					// WORRY This should be changed to be made more generic, because it assumes too much
        					Node ruleNode = childNode.getNextSibling();

        					// Decision
        					String decName = getDecisionType(ruleNode);        						

        					List<String> varOrdering = new ArrayList<String>();        						
        					varOrdering = getListElements(ruleNode, "DECISION-TYPE", "id");

        					// Target fmla        					
        					Node targetNode = getChildNode(ruleNode, "TARGET");
        					assert(targetNode != null);
        					MExploreCondition targetCondition = exploreHelper(targetNode.getFirstChild());
        					Formula target = targetCondition.fmla;

        					// condition is used exclusively for XACML, so it's ignored here.
        					Formula condition = Formula.TRUE;        					        					
        					theResponse = MEnvironment.addRule(pname, rname, decName, varOrdering, target, condition, targetCondition);
        				}
        				else if(secondChildNode.getNodeName().equalsIgnoreCase("VARIABLE-DECLARATION"))
        				{
        					String varname = getAttributeOfChildNodeOrNode(secondChildNode, "VARIABLE-DECLARATION", "varname");
        					String vartype = getAttributeOfChildNodeOrNode(secondChildNode, "VARIABLE-DECLARATION", "sort");

        					theResponse = MEnvironment.addVarDec(pname, varname, vartype);
        				}

        			}
        			else if (childNode.getNodeName().equalsIgnoreCase("PARENT")) {
        				String parent = getParentName(childNode);
        				String child = getChildName(childNode);
        				theResponse = MEnvironment.addChild(parent, child);
        			}
        		}
        		else
        		{
        			theResponse = MEnvironment.errorResponse(MEnvironment.sUnknown, MEnvironment.sCommand, "");
        		}
        	}
        	else
        	{
        		theResponse = MEnvironment.errorResponse(MEnvironment.sNotDocument, "", "");
        	}


        	// Finally -- did we assign a proper response? If not, none of the above cases was matched.
        	// _SHOULD_ have elses everywhere. This is just in case.
        	if(theResponse == null)
        		theResponse = MEnvironment.errorResponse(MEnvironment.sFailure, "", 
        				"Fatal error: No matching case in java engine. (If you receive this error, please notify the Margrave developers.)");
        		
        	return theResponse;
        }

		private static Document handleAddVocabFact(Node margraveCommandNode, Node childNode) {
			String vname = getVocabName(margraveCommandNode);
			Node secondChildNode = childNode.getNextSibling(); // WORRY Shouldn't be hardcoded in!!
			String addType = secondChildNode.getNodeName();
			writeToLog("addType: " + addType +"\n");

			if (addType.equalsIgnoreCase("SUBSORT")) {
				String parent = getSubSortParent(margraveCommandNode);
				String child = getSubSortChild(margraveCommandNode);
				return MEnvironment.addSubsort(vname, parent, child);
			}
			else if (addType.equalsIgnoreCase("SORT")) {
				String sortName = getSortName(margraveCommandNode);
				return MEnvironment.addSort(vname, sortName);
			}
			else if (addType.equalsIgnoreCase("SORT-WITH-CHILDREN")) {
				String sortName = getAttributeOfChildNodeOrNode(margraveCommandNode, "SORT-WITH-CHILDREN", "name");

				List<String> childnames = getListElements(margraveCommandNode, "SORT-WITH-CHILDREN", "name");        						        						
				return MEnvironment.addSortWithSubs(vname, sortName, childnames);				
			}        					
			else if (addType.equalsIgnoreCase("PREDICATE")) {
				String sName = getPredicateName(margraveCommandNode);
				List<String> constr = getRelationsList(margraveCommandNode);
				writeToLog("Adding Predicate\n");
				return MEnvironment.addPredicate(vname, sName, constr);
			}

			else if (addType.equalsIgnoreCase("CONSTANT")) {
				String sName = getAttributeOfChildNodeOrNode(secondChildNode, "CONSTANT", "name");
				String sSort = getAttributeOfChildNodeOrNode(secondChildNode, "CONSTANT", "type");     
				writeToLog("Adding constant "+sName+" : "+sSort+"\n");
				return MEnvironment.addConstant(vname, sName, sSort);
			}
			else if (addType.equalsIgnoreCase("FUNCTION"))
			{
				String sName = getAttributeOfChildNodeOrNode(secondChildNode, "FUNCTION", "name");   
				List<String> constr = getListElements(secondChildNode, "RELATIONS", "name");        						
				writeToLog("Adding function "+sName+" : "+constr+"\n");
				//System.err.println("Adding function "+sName+" : "+constr+"\n");
				return MEnvironment.addFunction(vname, sName, constr);
			}

			else if (addType.equalsIgnoreCase("CONSTRAINT")) {
				Node constraintNode = secondChildNode; //Just for clarity

				String constraintType = getConstraintType(constraintNode);

				// FORMULA is special.
				if(constraintType.equalsIgnoreCase("FORMULA"))
				{
					return MEnvironment.addConstraintFormula(vname, 
							exploreHelper(constraintNode.getFirstChild()).fmla);
				}
				

				List<String> relations = getRelationsList(constraintNode);
				
				assert(relations != null);
				String firstRelation = relations.get(0);

				if (constraintType.equalsIgnoreCase("SINGLETON")) {
					return MEnvironment.addConstraintSingleton(vname, firstRelation);
				}
				else if (constraintType.equalsIgnoreCase("SINGLETON-ALL")) {
					return MEnvironment.addConstraintSingletonAll(vname, firstRelation);
				}
				else if (constraintType.equalsIgnoreCase("ATMOSTONE")) {
					return MEnvironment.addConstraintAtMostOne(vname, firstRelation);
				}
				else if (constraintType.equalsIgnoreCase("DISJOINT")) {
					assert(relations.size() == 2);
					String secondRelation = relations.get(1);
					return MEnvironment.addConstraintDisjoint(vname, firstRelation, secondRelation);
				}
				else if (constraintType.equalsIgnoreCase("SUBSET")) {
					assert(relations.size() == 2);
					String secondRelation = relations.get(1);
					return MEnvironment.addConstraintSubset(vname, firstRelation, secondRelation);
				}
				else if (constraintType.equalsIgnoreCase("CONSTANTS-NEQ")) {
					assert(relations.size() == 2);
					String secondRelation = relations.get(1);
					return MEnvironment.addConstraintConstantsNeq(vname, firstRelation, secondRelation);
				}		
				else if (constraintType.equalsIgnoreCase("CONSTANTS-COVER")) {
					return MEnvironment.addConstraintConstantsCover(vname, firstRelation);
				}
				else if (constraintType.equalsIgnoreCase("CONSTANTS-NEQ-ALL")) {
					MCommunicator.writeToLog("\nAdding constraint constants-neq-all: "+firstRelation);
					return MEnvironment.addConstraintConstantsNeqAll(vname, firstRelation);
				}				
				else if (constraintType.equalsIgnoreCase("ATMOSTONE-ALL")) {
					return MEnvironment.addConstraintAtMostOneAll(vname, firstRelation);
				}
				else if (constraintType.equalsIgnoreCase("NONEMPTY")) {
					return MEnvironment.addConstraintNonempty(vname, firstRelation);
				}
				else if (constraintType.equalsIgnoreCase("NONEMPTY-ALL")) {
					return MEnvironment.addConstraintNonemptyAll(vname, firstRelation);
				}
				else if (constraintType.equalsIgnoreCase("ABSTRACT")) {
					return MEnvironment.addConstraintAbstract(vname, firstRelation);
				}
				else if (constraintType.equalsIgnoreCase("ABSTRACT-ALL")) {
					return MEnvironment.addConstraintAbstractAll(vname, firstRelation);
				}
				else if (constraintType.equalsIgnoreCase("TOTAL-FUNCTION")) {
					return MEnvironment.addConstraintTotalFunction(vname, firstRelation);
				}
				else if (constraintType.equalsIgnoreCase("TOTAL-RELATION")) {
					return MEnvironment.addConstraintTotalRelation(vname, firstRelation);
				}
				else if (constraintType.equalsIgnoreCase("PARTIAL-FUNCTION")) {
					return MEnvironment.addConstraintPartialFunction(vname, firstRelation);
				}
				else {
					// Unknown constraint type; throw an exception
					return MEnvironment.errorResponse(MEnvironment.sUnknown, MEnvironment.sConstraint, constraintType);
				}        						
			}
			else {
				return MEnvironment.errorResponse(MEnvironment.sCommand, MEnvironment.sNotExpected, addType);
			}
		}

		private static Document handleExplore(String originalXMLText, Node n) 
		{
			MExploreCondition exploreCondition;
			
			// Catch and re-throw any exception, because if EXPLORE fails,
			// need to reset lastResult to -1.
		
			try
			{        				        			
				n = n.getFirstChild();
				String name = n.getNodeName();
				if (name.equalsIgnoreCase("EXPLORE"))
				{
					//Explore should only have one child - "Condition". exploreHelper takes the node one down from condition
					String queryID = getAttributeOfChildNodeOrNode(n, "EXPLORE", "id");

					if(MEnvironment.isQueryIDUsed(queryID))
					{
						// Don't allow ID re-use.
						return MEnvironment.errorResponse(MEnvironment.sQuery, MEnvironment.sFailure, "The query identifier "+queryID+" is already in use.");
					}

					exploreCondition = exploreHelper(n.getFirstChild().getFirstChild()); 
					if (exploreCondition == null)
						MEnvironment.errorWriter.println("explore condition is null!");
					MQuery result = null;
					
					//Default Values                                     					
					List<MIDBCollection> under = new LinkedList<MIDBCollection>();
					List<String> publ = new ArrayList<String>();
					Map<String, String> publSorts = new HashMap<String, String>();
					HashMap<String, Set<List<MTerm>>> idbOut = new HashMap<String, Set<List<MTerm>>>();
					Boolean tupling = false;
					Integer debugLevel = 0;

					Node underNode = getUnderNode(n);
					Node publishNode = getPublishNode(n);
					//Node tuplingNode = getTuplingNode(n);
					Node debugNode = getDebugNode(n);
					Node ceilingsNode = getCeilingsNode(n);

					// Now called INCLUDE, and 
					// used to alert tupling that there are EDB indexings it needs
					// to include, even if they don't appear.
					//Node includeNode = getIncludeNode(n);

					if (underNode != null)
					{ 
						under = namesToIDBCollections(getUnderList(n));

					}
					if (publishNode != null)
					{
						// <PUBLISH><VARIABLE-DECLARATION sort=\"B\"><VARIABLE-TERM id=\"y\" /></VARIABLE-DECLARATION>
						//          <VARIABLE-DECLARATION sort=\"C\"><VARIABLE-TERM id=\"x\" /></VARIABLE-DECLARATION></PUBLISH>

						List<Node> decls = getElementChildren(publishNode);
						for(Node varDeclNode : decls)
						{	        							  	        							
							String varname = getAttributeOfChildNodeOrNode(varDeclNode, "VARIABLE-DECLARATION", "varname");
							String vartype = getAttributeOfChildNodeOrNode(varDeclNode, "VARIABLE-DECLARATION", "sort");
							publ.add(varname);
							publSorts.put(varname, vartype);
						}

					}
					//if (includeNode != null) {
					//	List<Node> idbChildNodes = getElementChildren(includeNode);	        						
					//	idbOut = atomicFormulasToHashmap(idbChildNodes);
					//}
					//if (tuplingNode != null) { //For now if the node exists just set tupling to true
					//	tupling = true;
					//}
					if (debugNode != null) {
						debugLevel = Integer.parseInt(getDebugLevel(debugNode));
					}
					
					Map<String, Integer> ceilingMap = new HashMap<String, Integer>();
					if (ceilingsNode != null) 
					{
	        			List<Node> ceilingNodes = getElementChildren(ceilingsNode);
						
	        			for(Node ceil : ceilingNodes)
	        			{
	        				// LOCAL ceilings (for this query only)
	        	        	String sortname = getNodeAttribute(ceil, "sort");
	        	        	String ceiling = getNodeAttribute(ceil, "value");
	        	        	writeToLog("\nCEILING: "+sortname+" is "+ceiling);
	        	        	
		        			try
	        				{		        	        			        				
	        					int intValue = Integer.parseInt(ceiling);	        					
	            				ceilingMap.put(sortname, intValue);
	        				}
	        				catch(NumberFormatException e)
	        				{
	        					return MEnvironment.errorResponse(MEnvironment.sNotDocument, "Not an integer", ceiling);
	        				}        					        				
	        			}
					}

					// Exception will be thrown and caught by caller to return an EXCEPTION element.
					result = MQuery.createFromExplore(
							queryID,
							exploreCondition.addSeenIDBCollections(under), 
							publ,
							publSorts,
							idbOut, tupling, debugLevel, ceilingMap);


					writeToLog("AT END OF EXPLORE");
					return MEnvironment.returnQueryResponse(result, originalXMLText);

				} // end if explore node
				
				throw new MUserException("Internal error: Command promised a query definition but did not provide it.");

			} 
			catch(MBaseException e)
			{
				MEnvironment.clearLastQuery();
				throw e;
			}			
		}
        
        private static void handleComb(Node n, Set<String> rFA, Map<String, Set<String>> rO)
        {
        	Node combListNode = getChildNode(n, "COMB-LIST");
			List<Node> combNodes = getElementChildren(combListNode);
			for(Node combNode : combNodes)
			{
				if("FA".equalsIgnoreCase(combNode.getNodeName()))
				{
					List<String> names = getListElements(combListNode, "FA", "id");
					rFA.addAll(names);
				}
				else if("OVERRIDES".equalsIgnoreCase(combNode.getNodeName()))
				{
					String under = getAttributeOfChildNodeOrNode(combNode, "OVERRIDES", "decision");
					List<String> overs = getListElements(combListNode, "OVERRIDES", "id");
					if(!rO.containsKey(under))
						rO.put(under, new HashSet<String>());
					rO.get(under).addAll(overs);
				}
				// else do nothing		
			}
			
		}

		private static HashMap<String, Set<List<MTerm>>> atomicFormulasToHashmap(List<Node> childNodes)
        {
        	//HashMap<String, Set<List<String>>> hashMap = new HashMap<String, Set<List<String>>>();
        
        	// Used to be just variables:
        	// R(x, y), P(y, z), R(z, z) --->
        	// [ R->[["x","y"], ["z","z"]] P->[["y","z"]]
        	// Now we have terms as well.
        	
			HashMap<String, Set<List<MTerm>>> hashMap = new HashMap<String, Set<List<MTerm>>>();
			
			for(Node childNode : childNodes)
			{
				if(childNode.getNodeName().equalsIgnoreCase("FORCASES"))
				{
					// Ignore. This is a special child node for SHOW REALIZED.
				}
				else if(childNode.getNodeName().equalsIgnoreCase("ATOMIC-FORMULA")) 
				{
					List<String> relNamePieces = getRelationNameFromAtomicFmla(childNode);
					List<MTerm> terms = getTermsFromAtomicFmla(childNode);
					
					String relName = "";
					for(int ii=0; ii < relNamePieces.size();ii++)
					{
						// Re-compose dotted notation (for now)
						if("".equals(relName))
							relName += relNamePieces.get(ii);
						else
							relName += "."+relNamePieces.get(ii);
					}
					
					if(!hashMap.containsKey(relName))
						hashMap.put(relName, new HashSet<List<MTerm>>());
					hashMap.get(relName).add(terms);
				
				}
				else if(childNode.getNodeName().equalsIgnoreCase("ISA"))
				{
					// Only atomic ISA allowed!
					String relName = getAttributeOfChildNodeOrNode(childNode, "ISA", "sort");
										
					// ISA has a special TERM sub-node. The first (and only) child of that is the variable-term, function-term, etc.
					MTerm theTerm = termHelper(getChildNode(childNode, "TERM").getFirstChild());				
					// ISA has a special FORMULA sub-node. The first (and only) child of that is the actual formula.
					MExploreCondition cond = exploreHelper(getChildNode(childNode, "FORMULA").getFirstChild());
					
					if(theTerm == null || cond == null || !cond.fmla.equals(Formula.TRUE))
					{
						throw new MUnsupportedFormulaException("atomicFormulasToHashmap: Got a non-atomic ISA node. ISAs in this context must be atomic.");
					}
									
					if(!hashMap.containsKey(relName))
						hashMap.put(relName, new HashSet<List<MTerm>>());
					List<MTerm> singletonList = new ArrayList<MTerm>(1);
					singletonList.add(theTerm);
					hashMap.get(relName).add(singletonList);
					
				}
				else if(childNode.getNodeName().equalsIgnoreCase("EQUALS"))
				{
					List<MTerm> terms = getTermsFromEqualsFmla(childNode);
					
					if(!hashMap.containsKey("="))
						hashMap.put("=", new HashSet<List<MTerm>>());
					hashMap.get("=").add(terms);
				}
				else
				{
					throw new MUnsupportedFormulaException("atomicFormulasToHashmap: Expected a list of atomic formulas. Got a node with name: "+childNode.getNodeName());
				}
			}
        							        
        	return hashMap;
        }
        
		private static String getInfoId(Node n) {
			return getAttributeOfChildNodeOrNode(n, "INFO", "id");
		}
		
        //Helper functions for specific parts of commands
        private static String getPolicyName(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "POLICY-IDENTIFIER", "pname");
        }
        
        private static String getVocabName(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "VOCAB-IDENTIFIER", "vname");
        }
        
        private static String getSubSortParent(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "SUBSORT", "parent");
        }
        
        private static String getSubSortChild(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "SUBSORT", "child");
        }
        
        private static String getSortName(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "SORT", "name");
        }
        
        private static String getPredicateName(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "PREDICATE", "name");
        }
              
        private static String getParentName(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "PARENT-IDENTIFIER", "name");
        }
        
        private static String getChildName(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "CHILD-IDENTIFIER", "name");
        }
        
        private static String getRuleName(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "RULE", "name");
        }
        private static String getDecisionName(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "DECISION", "name");
        }
        private static String getDecisionType(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "DECISION-TYPE", "type");
        }
        
        private static String getConstraintType(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "CONSTRAINT", "type");
        }
                
        //Rename
       /* public static String getRenameFirstId(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "RENAME", "id1");
        }
        public static String getRenameSecondId(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "RENAME", "id2");
        }*/
                
        //SHOW
        public static String getShowType(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "SHOW", "type");
        }
        public static Node getForCasesNode(Node n) {
        	return getChildNode(n, "FORCASES");
        }
        
        // <SHOW type="populated" id="0" ... 
        public static String getPopulatedId(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "SHOW", "id");
        }
        public static String getUnpopulatedId(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "SHOW", "id");
        }
        
        //COUNT
        private static String getCountId(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "COUNT", "id");
        }
        private static Node getSizeNode(Node n) {
        	return getChildNode(n, "SIZE");
        }
        private static String getCountSize(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "COUNT", "size");
        }
        
        //GET
        public static String getGetInfoType(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "GET-INFO", "type");
        }
        
        //ATOMIC FORMULAS
        public static List<Node> getAtomicFormulaNodesFromList(Node n) 
        {
        	Node child = getChildNode(n, "ATOMIC-FORMULA-LIST");
        	if(child == null)
        		return null;
        	return getElementChildren(child);
        }
        
        private static String getLoadFileName(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "LOAD", "file-name");
        }
        private static String getLoadSchemaFileName(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "LOAD", "schema-file-name");
        }
        
        public static Node getUnderNode(Node n) {
        	return getChildNode(n, "UNDER");
        }
		public static Node getPublishNode(Node n) {
			return getChildNode(n, "PUBLISH");
		}
		public static Node getIncludeNode(Node n) {
			return getChildNode(n, "INCLUDE");
		}
		public static Node getTuplingNode(Node n) {
			return getChildNode(n, "TUPLING");
		}
		public static Node getDebugNode(Node n) {
			return getChildNode(n, "DEBUG");
        }
		public static Node getCeilingsNode(Node n) {
			return getChildNode(n, "CEILINGS");
		}
        
	
        public static String getDebugLevel(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "DEBUG", "debug-level");
        }
        public static String getCeilingLevel(Node n) {
        	return getAttributeOfChildNodeOrNode(n, "CEILING", "ceiling-level");
        }
        
        //LISTS
        private static List<String> getRelationsList(Node n) {
        	return getListElements(n, "RELATIONS", "name");
        }
                        
        private static List<String> getUnderList(Node n)
        {
			// The under node is the "list" node, so need to be passed the EXPLORE node,
        	// not the UNDER node for getListElements to work.
        	List<String> result = getListElements(n, "UNDER", "pname");     
        	writeToLog("UNDER LIST: "+result.toString());
        	return result;
        }
        
        //Returns the child node of n whose name is nodeName 
        //If no child, returns null
        private static Node getChildNode(Node n, String nodeName)
        {
        	List<Node> childNodes = getElementChildren(n);
        	        	
        	for (Node childNode : childNodes)
        	{
        		if (nodeName.equalsIgnoreCase(childNode.getNodeName()))
        		{
        			return childNode;
        		}
        	}
        	return null; //Didn't find it, error
        }
        
        private static List<Node> getChildNodes(Node n, String nodeName)
        {
        	List<Node> childNodes = getElementChildren(n);
        	List<Node> result = new ArrayList<Node>();
        	
        	for (Node childNode : childNodes)
        	{
        		if (nodeName.equalsIgnoreCase(childNode.getNodeName()))
        		{
        			result.add(childNode);
        		}
        	}
        	return result; 
        }
        
        /**
         * Finds the child node of n whose name is nodeName (unless n's name is nodename), and returns the value of its attribute with attributeName
         * @param n 
         * @param nodeName
         * @param attributeName
         * @return
         */
        private static String getAttributeOfChildNodeOrNode(Node n, String nodeName, String attributeName)
        {
        	if(n == null) {
        		return null;
        	}
        	
        	Node node = null;
        	if (n.getNodeName().equalsIgnoreCase(nodeName))
        	{
        		node = n;
        	}
        	else
        	{
        		node = getChildNode(n, nodeName);
        	}
        	
        	//Return null if we couldn't find the node, or if the node doesn't have the specified attribute
        	if (node == null) {
        		return null;
        	}
        	Node attribute = node.getAttributes().getNamedItem(attributeName);
        	if (attribute == null) {
        		return null;
        	}
        	return attribute.getNodeValue();
        }
        
        
        
        private static String getNodeAttribute(Node n, String attributeName)
        {
        	if(n == null) { 
        		return null;
        	}
        	
        	Node attribute = n.getAttributes().getNamedItem(attributeName);
        	if(attribute == null) {
        		return null;
        	}        		
        	
        	return attribute.getNodeValue();
        }
        
        //Returns a list of the attribute values associated with the attributeName of every childNode of a Node named listName, which is itself a child node of n
        private static List<String> getListElements(Node n, String listName, String attributeName)
        {
        	Node listNode = getChildNode(n, listName);
        	writeToLog("\nIn getListElements. Node: "+n.getNodeName()+"; listName: "+listName+"; attributeName: "+attributeName+"\n");        
        	
        	//Return null if we can't find the node
        	if (listNode == null)
        	{
        		writeToLog("\nNo child node found with that name. Returning null.\n");
        		return null;
        	}
        	
        	LinkedList<String> attributeValues = new LinkedList<String>();
        	
        	List<Node> childNodes = getElementChildren(listNode);
        	for (Node aNode : childNodes) {
        		attributeValues.add(aNode.getAttributes().getNamedItem(attributeName).getNodeValue());
        	}
        	return attributeValues;
        }

        private static List<Node> getElementChildren(Node n)
        {
        	// NodeList does not implement Iterable
        	
        	List<Node> result = new ArrayList<Node>();
        	if(n == null)
        		return result;
        	NodeList nlResult = n.getChildNodes();

        	if(nlResult == null)
        		return result;
        	
        	for(int ii = 0;ii<nlResult.getLength();ii++)
        	{
        		Node aChild = nlResult.item(ii);
        		//if(aChild.getNodeType() != Node.TEXT_NODE)
        		if(aChild.getNodeType() == Node.ELEMENT_NODE)
        			result.add(aChild);        		
        	}
        		        	 
        	return result;
        }
        
        //Expects the node one down from condition node
        private static MExploreCondition exploreHelper(Node n) throws MUserException, MGEManagerException, MGEUnknownIdentifier
        {
        	assert(n != null);
        	
        	List<Node> childNodes = getElementChildren(n);

        	String name = n.getNodeName();
        	
        	//writeToLog("\nIn exploreHelper. Node Name: " + name + "\n");
        	
        	//if(childNodes.getLength() == 0)
        	//	writeToLog("\nNo child nodes.\n");
        	//else
        	//	writeToLog("First child node's name: " + childNodes.item(0).getNodeName() + "\n");

        	if (name.equalsIgnoreCase("AND"))
        	{        		        	
        		assert(getElementChildren(n).size() == 2);
        		return exploreHelper(childNodes.get(0)).and(exploreHelper(childNodes.get(1)));
        	}
        	else if (name.equalsIgnoreCase("OR")) 
        	{
        		assert(getElementChildren(n).size() == 2);
        		return exploreHelper(childNodes.get(0)).or(exploreHelper(childNodes.get(1)));
        	}
        	else if (name.equalsIgnoreCase("IMPLIES"))
        	{
        		// cannot trust XML to preserve node order
        		Node antecedent = getChildNode(n, "ANTE");
        		Node consequent = getChildNode(n, "CONS");
        		
        		// The children of these nodes are the formulas in ante/cons position.
        		MExploreCondition antecedentFmla = exploreHelper(antecedent.getFirstChild());
        		MExploreCondition consequentFmla = exploreHelper(consequent.getFirstChild());
        		        		
        		return antecedentFmla.implies(consequentFmla);
        	}
        	else if(name.equalsIgnoreCase("ISA"))
        	{        		        		
        		String typename = getAttributeOfChildNodeOrNode(n, "ISA", "sort");        		        		
        		Relation theRel = MFormulaManager.makeRelation(typename, 1);

        		// These may be null
        		Node internalFmlaWrapNode = getChildNode(n, "FORMULA");
        		Node internalTermWrapNode = getChildNode(n, "TERM");
        		        		
        		MExploreCondition internalFmlaC;
        		MExploreCondition newFmlaC;

        		// If no fmla passed, this is a sort-as-predicate. Just checking whether the var is in the sort.
        		if(internalFmlaWrapNode == null)
        			internalFmlaC = new MExploreCondition(true);
        		else        			
            		internalFmlaC = exploreHelper(internalFmlaWrapNode.getFirstChild());
        		
        		MTerm theTerm = termHelper(internalTermWrapNode.getFirstChild());        		
       			newFmlaC = internalFmlaC.isaSubstitution(theTerm.expr, theRel);        		
        		        		        
        		writeToLog("\nFormula helper (ISA) got "+theTerm+" : "+theRel+" | "+internalFmlaC);
        		writeToLog("\n  Substituted to: "+newFmlaC);
        		
        		// The term in the ISA _must_ be added to the context, since it isn't necessarily 
        		// part of the formula condition. (For instance, TRUE isas)
        		newFmlaC.termMap.put(theTerm.expr, theTerm);
        		
        		return newFmlaC;
        	}
        	else if(name.equalsIgnoreCase("EQUALS"))
        	{        		
        		return handleEqualsFormula(n);        		
        	}
        	else if (name.equalsIgnoreCase("IFF")) {
        		assert(getElementChildren(n).size() == 2);
        		return exploreHelper(n.getFirstChild()).iff(exploreHelper(n.getChildNodes().item(1)));
        	}
        	else if (name.equalsIgnoreCase("NOT")) {
        		return exploreHelper(n.getFirstChild()).not();
        	}
        	else if(name.equalsIgnoreCase("EXISTS"))
        	{
        		String theVarName = getAttributeOfChildNodeOrNode(n, "EXISTS", "var");
        		String theSortName = getAttributeOfChildNodeOrNode(n, "EXISTS", "sort");
        		
        		Variable theVar = MFormulaManager.makeVariable(theVarName);
        		Relation theSort = MFormulaManager.makeRelation(theSortName, 1);
        		
        		return exploreHelper(n.getFirstChild()).exists(theVar, theSort);
        	}
        	else if(name.equalsIgnoreCase("FORALL"))
        	{
        		String theVarName = getAttributeOfChildNodeOrNode(n, "FORALL", "var");
        		String theSortName = getAttributeOfChildNodeOrNode(n, "FORALL", "sort");
        		
        		Variable theVar = MFormulaManager.makeVariable(theVarName);
        		Relation theSort = MFormulaManager.makeRelation(theSortName, 1);
        		
        		return exploreHelper(n.getFirstChild()).forall(theVar, theSort);
        	}
        	else if (name.equalsIgnoreCase("ATOMIC-FORMULA"))
        	{
        		// "<ATOMIC-FORMULA><RELATION-NAME><ID id=\"P\" /><ID id=\"R2\" /></RELATION-NAME><TERMS><VARIABLE-TERM id=\"z\" /><CONSTANT-TERM id=\"c\" /></TERMS></ATOMIC-FORMULA>"
        		return handleAtomicFormula(n);
        	}
        	else if(name.equalsIgnoreCase("TRUE"))
        	{
        		return new MExploreCondition(true);
        	}
        	else if(name.equalsIgnoreCase("FALSE"))
        	{
        		return new MExploreCondition(false);
        	}
        	
        	throw new MUserException("exploreHelper was unable to match node type: "+name);
    }

		private static MExploreCondition handleEqualsFormula(Node n) {
			// Comes in with 2 TERMS now instead of 2 maybe-variables.    
						
			List<MTerm> terms = getTermsFromEqualsFmla(n);
			MTerm term1 = terms.get(0);
			MTerm term2 = terms.get(1);
				
			//String idname1 = getNodeAttribute(n, "EQUALS", "v1");
			//String idname2 = getNodeAttribute(n, "EQUALS", "v2");
			
			writeToLog("\nEQUALS: "+term1+" = "+term2+"\n\n");
			        		
			//Variable v1 = MFormulaManager.makeVariable(idname1);
			//Variable v2 = MFormulaManager.makeVariable(idname2);
			Formula fmla = MFormulaManager.makeEqAtom(term1.expr, term2.expr);
			writeToLog("\nNew Explore condition (equals): "+fmla);
			        		        	
			return new MExploreCondition(fmla, term1, term2, true);
		}

		private static List<MTerm> getTermsFromEqualsFmla(Node n)
		{
			List<Node> childNodes = getElementChildren(n);
			
			List<MTerm> terms = new ArrayList<MTerm>(2);
			
			terms.add(termHelper(childNodes.get(0)));
			terms.add(termHelper(childNodes.get(1)));
			return terms;			
		}

		private static MExploreCondition handleAtomicFormula(Node n)
		{
			
			List<String> relationNameComponents = getRelationNameFromAtomicFmla(n);
			List<MTerm> terms = getTermsFromAtomicFmla(n);
			
			/////////////////////////////////////////////////////
			
			// Could be:
			// (1) compound relation name, e.g. pol.idbname
			// (2) EDB name
			// (3) view name (really an idb!)
			
			writeToLog("\nAtomic-Formula: \nrelationNameComponents: " + relationNameComponents + "\nterms: " + terms.toString());
			
			if(relationNameComponents.size() < 1)
				throw new MUserException("Unable to obtain relation name in atomic formula.");
			
			String relationName = relationNameComponents.get(relationNameComponents.size() - 1);
			
			if(relationNameComponents.size() == 1)
			{        		
				MIDBCollection pol = MEnvironment.getPolicyOrView(relationName);
				
				writeToLog("\n		relationName: " + relationName + "\npol: " + pol + "\n\n");

				/////////////////////////////////////////////////////
				// (3) view?
				if (pol != null)
				{
					Formula idbf = MEnvironment.getOnlyIDB(relationName);
					if(idbf != null)
					{
						// Perform variable substitution
			    		writeToLog("\nView IDB before substitution: "+idbf);
						idbf = performSubstitution(relationName, pol, idbf, terms);
						
						// Assemble MExploreCondition object
			    		writeToLog("\nNew Explore condition (view): "+idbf);
						return new MExploreCondition(idbf, pol, relationName, terms);        		
					}
				}
				
				/////////////////////////////////////////////////////
				// (2) EDB, then!

				// We don't have a vocabulary yet. So just make the relation.
				// The manager will prevent duplicates.
				Relation rel = MFormulaManager.makeRelation(relationName, terms.size());

				Expression termvector;
				Formula f = null;

				termvector = MFormulaManager.makeTermTuple(terms);
				f = MFormulaManager.makeAtom(termvector, rel);

				// No variable substitution needed!
				writeToLog("\nNew Explore condition (EDB): "+f);
				return new MExploreCondition(f, rel, terms);

			}
			
			/////////////////////////////////////////////////////
			// (1) compound relation name (reference to a policy, etc.)
			String collName = "";
			for(int ii=0; ii < relationNameComponents.size()-1;ii++)
			{
				collName += relationNameComponents.get(ii);
			}
			
			MIDBCollection pol = MEnvironment.getPolicyOrView(collName);
			if(pol == null)
				throw new MUserException("Unknown policy: "+collName);
			
			// throws exception rather than returning null
			Formula idbf = validateDBIdentifier(collName, relationName);
			
			// Substitute variables in policy's IDB for terms in query
			writeToLog("\nNon-view IDB before substitution: "+idbf);
			idbf = performSubstitution(relationName, pol, idbf, terms);
			
			MCommunicator.writeToLog("\nSAW TERMS: "+terms);
			

			
			writeToLog("\nNew Explore condition (non-view IDB): "+idbf);
			return new MExploreCondition(idbf, pol, relationName, terms);
		}

		private static List<MTerm> getTermsFromNode(Node termsNode)		
		{
			List<Node> termsNodeComponents = getElementChildren(termsNode);   
			List<MTerm> terms = new ArrayList<MTerm>();
			for(Node theNode : termsNodeComponents)
			{
				MTerm theTerm = termHelper(theNode); 				
				//String nameStr = getNodeAttribute(theNode, "ID", "id");
				terms.add(theTerm);
			}
			return terms;	
		}
		
		private static List<MTerm> getTermsFromAtomicFmla(Node n) {
			/////////////////////////////////////////////////////
			// Terms			
			Node termsNode = getChildNode(n, "TERMS");
			return getTermsFromNode(termsNode);			
		}

		private static List<String> getRelationNameFromAtomicFmla(Node n) {
			/////////////////////////////////////////////////////
			// Relation name
			Node relationNameNode = getChildNode(n, "RELATION-NAME");
			List<Node> relationComponents = getElementChildren(relationNameNode);        		        	
			List<String> relationNameComponents = new ArrayList<String>();        		
			for(Node theNode : relationComponents)
			{
				// <ID id=\"P\"/>
				String nameStr = getAttributeOfChildNodeOrNode(theNode, "ID", "id");
				relationNameComponents.add(nameStr);
			}
			return relationNameComponents;
		}
     


	private static MTerm termHelper(Node n) 
     {
    	 List<Node> childNodes = getElementChildren(n);

    	 String name = n.getNodeName();
     	
     	//writeToLog("\nIn exploreHelper. Node Name: " + name + "\n");
     	
     	//if(childNodes.getLength() == 0)
     	//	writeToLog("\nNo child nodes.\n");
     	//else
     	//	writeToLog("First child node's name: " + childNodes.item(0).getNodeName() + "\n");

     	if (name.equalsIgnoreCase("FUNCTION-TERM"))
     	{
     		String funcName = getAttributeOfChildNodeOrNode(n, "FUNCTION-TERM", "func");
     		
     		List<MTerm> subTerms = new ArrayList<MTerm>();
     		for(Node aNode : childNodes)
     		{
     			MTerm aChildTerm = termHelper(aNode);
     			subTerms.add(aChildTerm);
     		}
     		
     		return new MFunctionTerm(funcName, subTerms);
     		
     		// remember what symbols we saw, so we can catch errors?
     		
     	}	
     	else if (name.equalsIgnoreCase("CONSTANT-TERM"))
     	{
     		String constName = getAttributeOfChildNodeOrNode(n, "CONSTANT-TERM", "id");     		
     		return new MConstantTerm(constName);
     	}
     	else if (name.equalsIgnoreCase("VARIABLE-TERM"))
     	{
     		String varName = getAttributeOfChildNodeOrNode(n, "VARIABLE-TERM", "id");
     		return new MVariableTerm(varName);     
     	}
     	else
     	{     		     		
     		throw new MGEUnsupportedXACML("Unsupported term type: "+name);
     	}
     }

	//Returns a list containing the value of the first attribute of every child node of n
     protected static List<String> getListChildren(Node n) 
     {
         List<Node> childNodes = getElementChildren(n);
         List<String> list = new LinkedList<String>();

          for (Node aNode : childNodes) {
              list.add(aNode.getAttributes().item(0).getNodeValue());
          }
          
          return list;
     }
     
     static void initializeLog()
     {
    	 if(!bDoLogging)
    		 return;
    	     	     	 
    	 // Wipe the log clean every time the engine runs. 
    	 try
    	 {
    		 // This only works if the .class file isn't in a .jar; it's also risky because the place Margrave is
    		 // installed may not be writable. 
    		 
    		 //URL absoluteClassPathNameURL = MCommunicator.class.getClass().getResource("/edu/wpi/margrave/MCommunicator.class");
    		 //URL absoluteLogFileNameURL = new URL(absoluteClassPathNameURL, sLogFileName);
    		 //File logFILE = new File(absoluteLogFileNameURL.getFile());    		    		     		     		 
    		 //String absoluteLogFileName = java.net.URLDecoder.decode(logFILE.toString(), "UTF-8");
    		 
    		 // Instead, default to the system's temp file folder:
    		 String tempFolder = System.getProperty("java.io.tmpdir");
    		 if(!tempFolder.endsWith(File.separator))
    			 tempFolder += File.separator;
    		 String absoluteLogFileName = tempFolder+sLogFileName; 
    		     		 
    		 MCommunicator.outLogStream = new FileWriter(absoluteLogFileName);
    		 MCommunicator.outLog = new BufferedWriter(outLogStream);
    		 MCommunicator.outLog.write("Margrave engine log started at: "+ new Date()+"\n");	   
    		 MCommunicator.outLog.write("======================================================================\n");
    		 MCommunicator.outLog.flush();   
    		 
    		 // Log is initialized and open
    	 }
    	 catch (IOException e)
    	 {
    		 // Couldn't initialize log. try to report back to Racket.
   	      out.println(makeDetailedError("\nError initializing log file: "+ e.getMessage() +" (exception: "+e+")"+"( outLog = "+outLog+")"+"( outLogStream = "+outLogStream+")"));
   	      out.flush();
   	      System.exit(3);
    	 }
    	 

    	 
     }
     
     static void writeToLog(String s)
     {
    	 if(!bDoLogging)
    		 return;

    	 try
    	 {    		
    		 MCommunicator.outLog.write(s);
    		 MCommunicator.outLog.flush();    	        	    
    	 }
    	 catch (Exception e)
    	 {
    	     //Catch exception if any
    		 out.println(makeDetailedError("\nError writing log file: " + e.getMessage() +" (exception: "+e+")"+"( outLog = "+outLog+")"+"( outLogStream = "+outLogStream+")"));
    		 out.flush();
    	     System.exit(3);
    	 }
     }
               
     
     protected static String transformXMLString(Document theResponse) 
     {
    	 try
    	 {
			Transformer transformer = TransformerFactory.newInstance().newTransformer();
			transformer.setOutputProperty(OutputKeys.INDENT, "yes");
			
			//initialize StreamResult with File object to save to file
			StreamResult result = new StreamResult(new StringWriter());
			DOMSource source = new DOMSource(theResponse);
			
			// If this line causes a null pointer exception, there is an empty text element somewhere.
			// For some reason the transformer can't handle text elements with "" in them.
			transformer.transform(source, result);			
			
			
			String xmlString = result.getWriter().toString();
			//xmlString += cEOF;
			return xmlString;
		}
		catch(Exception e)
		{
			// Will hit this if theResponse is null.	
			// don't do this. would go through System.err
			//e.printStackTrace();
			//return (makeDetailedError(e.getLocalizedMessage())+cEOF);
			return (makeDetailedError(e.getLocalizedMessage()));
		}
	}
     
     protected static String transformXMLToString(Document theResponse)
     {
    	 try
    	 {
			Transformer transformer = TransformerFactory.newInstance().newTransformer();
			transformer.setOutputProperty(OutputKeys.INDENT, "yes");
			
			//initialize StreamResult with File object to save to file
			StreamResult result = new StreamResult(new StringWriter());
			DOMSource source = new DOMSource(theResponse);
			
			// If this line causes a null pointer exception, there is an empty text element somewhere.
			// For some reason the transformer can't handle text elements with "" in them.
			transformer.transform(source, result);			
			
			
			String xmlString = result.getWriter().toString();
			return xmlString;
    	 }
		catch(Exception e)
		{
			// Will hit this if theResponse is null.	
			// don't do this. would go through System.err
			//e.printStackTrace();
			return (makeDetailedError(e.getLocalizedMessage()));
		}
    	 
     }
     
     protected static byte[] transformXML(Document theResponse) 
     {
    	return transformXMLToString(theResponse).getBytes(); 
     }
	
	protected static Formula performSubstitution(String collectionIdSymbol, MIDBCollection coll, Formula f, List<MTerm> newterms)
	throws MUserException, MGEUnknownIdentifier
	{		
		// Replace expressions (here, variables) with other expressions
		// e.g. x becomes f(y, c)		
		
		List<Expression> newtermsexprs = new ArrayList<Expression>(newterms.size());
		for(MTerm t : newterms)
			newtermsexprs.add(t.expr);
		
		return MEnvironment.performSubstitution(collectionIdSymbol, coll, f, newtermsexprs);	
	}
	

	
	protected static Formula validateDBIdentifier(String objn, String dbn)
	throws MUserException
	{
		writeToLog("\nMCommunicator.validateDBIdentifier invoked for: "+objn+", "+dbn);
		
		// Is objn a policy name? If not, error.
		MIDBCollection pol = MEnvironment.getPolicyOrView(objn);
		 		
		if(pol == null)
			throw new MGEUnknownIdentifier("Unknown IDB Collection: "+objn);			   					
		
		writeToLog("\n  Collection found. Self reported name="+pol.name+" ContainsIDB = "+pol.containsIDB(dbn));
		
		if(pol.containsIDB(dbn))
		{
			writeToLog("\n  Returning: "+pol.getIDB(dbn));
			return pol.getIDB(dbn);
		}
		else
			throw new MGEUnknownIdentifier("Unknown IDB: "+dbn+" in collection: "+objn);		 
	}
	
	private static List<MIDBCollection> namesToIDBCollections(List<String> names) throws MUserException
	{
		if(names == null)
			return new ArrayList<MIDBCollection>();
		
		List<MIDBCollection> result = new ArrayList<MIDBCollection>(names.size());
		
		for(String n : names)
		{
			if(MEnvironment.getPolicyOrView(n) == null)
				throw new MGEUnknownIdentifier("Unknown symbol in UNDER clause: "+n);
			result.add(MEnvironment.getPolicyOrView(n));
		}
		
		return result;
	}
	
	public static void unitTests()
	{
		MEnvironment.writeErrLine("----- Begin MCommunicator Tests (No messages is good.) -----");
		
		// Test XML handling.
		// handleXMLCommand
		
	//	String testInfo = "<MARGRAVE-COMMAND type=\"INFO\"><INFO /></MARGRAVE-COMMAND> ";
	//	String testInfoWithID = "<MARGRAVE-COMMAND type=\"INFO\"><INFO id=\"Something\" /></MARGRAVE-COMMAND>  ";
	//	String reset = "<MARGRAVE-COMMAND type=\"RESET\"><ID>MyQuery</ID></MARGRAVE-COMMAND>";  // ***
	//	String show = "<MARGRAVE-COMMAND type=\"SHOW\"><SHOW type=\"ONE\" ID=\"MyQuery\" /></MARGRAVE-COMMAND> ";
	//	String count = "<MARGRAVE-COMMAND type=\"COUNT\"><COUNT ID=\"MyQuery\" /></MARGRAVE-COMMAND> ";
	//	String isposs = "<MARGRAVE-COMMAND type=\"IS-POSSIBLE\"><IS-POSSIBLE ID=\"MyQuery\" /></MARGRAVE-COMMAND> ";
	/*	String showUnrealizedForCases = 
"<MARGRAVE-COMMAND type=\"SHOW\"><SHOW type=\"UNREALIZED\" ID=\"Myquery\">"+
"<ATOMIC-FORMULA><RELATION-NAME><ID id=\"P\" /><ID id=\"R\" /></RELATION-NAME><TERMS><VARIABLE-TERM id=\"x\" /><VARIABLE-TERM id=\"y\" /><FUNCTION-TERM func=\"f\"><CONSTANT-TERM id=\"c\" /><VARIABLE-TERM id=\"z\" /></FUNCTION-TERM></TERMS></ATOMIC-FORMULA>"+
"<ATOMIC-FORMULA><RELATION-NAME><ID id=\"P\" /><ID id=\"R2\" /></RELATION-NAME><TERMS><VARIABLE-TERM id=\"z\" /><CONSTANT-TERM id=\"c\" /></TERMS></ATOMIC-FORMULA>" +
"<FORCASES><ATOMIC-FORMULA><RELATION-NAME><ID id=\"IDB\" /></RELATION-NAME><TERMS><VARIABLE-TERM id=\"x\" /></TERMS></ATOMIC-FORMULA>" +
"<ATOMIC-FORMULA><RELATION-NAME><ID id=\"P\" /><ID id=\"R3\" /></RELATION-NAME><TERMS><VARIABLE-TERM id=\"y\" /></TERMS></ATOMIC-FORMULA></FORCASES></SHOW></MARGRAVE-COMMAND> ";
*/
		
		String aQuery = 
"<MARGRAVE-COMMAND type=\"EXPLORE\"><EXPLORE id=\"Myqry\"><CONDITION><OR>" +
"<ATOMIC-FORMULA><RELATION-NAME><ID id=\"P\"/><ID id=\"permit\"/></RELATION-NAME><TERMS><CONSTANT-TERM id=\"c\" /><FUNCTION-TERM func=\"f\"><CONSTANT-TERM id=\"c\" /></FUNCTION-TERM></TERMS></ATOMIC-FORMULA>" +
"<AND><EQUALS><VARIABLE-TERM id=\"x\" /><VARIABLE-TERM id=\"y\" /></EQUALS>" +
"<OR> <ISA sort=\"U\"><TERM><VARIABLE-TERM id=\"x\" /></TERM></ISA> <ISA sort=\"U\"><TERM><VARIABLE-TERM id=\"x\" /></TERM></ISA> " +
                                    "<EQUALS><VARIABLE-TERM id=\"x\" /><VARIABLE-TERM id=\"y\" /></EQUALS></OR>" +
"</AND></OR></CONDITION>" +
"<PUBLISH><VARIABLE-DECLARATION sort=\"B\" varname=\"y\" /><VARIABLE-DECLARATION sort=\"C\" varname=\"x\" /></PUBLISH></EXPLORE></MARGRAVE-COMMAND> ";

// aQuery unsat since publish gives us types of x, y that don't fit the query. 		
		
		String aQuery2 = 
			"<MARGRAVE-COMMAND type=\"EXPLORE\"><EXPLORE id=\"Myqry2\"><CONDITION>" +
			"<AND><NOT><ATOMIC-FORMULA><RELATION-NAME><ID id=\"P\"/><ID id=\"permit\"/></RELATION-NAME><TERMS><VARIABLE-TERM id=\"x\" /><VARIABLE-TERM id=\"y\" /></TERMS></ATOMIC-FORMULA></NOT>" +
			"<EQUALS><VARIABLE-TERM id=\"x\" /><VARIABLE-TERM id=\"y\" /></EQUALS>"+
			"<EQUALS><VARIABLE-TERM id=\"x\" /><FUNCTION-TERM func=\"f\"><CONSTANT-TERM id=\"c\" /></FUNCTION-TERM></EQUALS></AND></CONDITION>" +
			"<PUBLISH><VARIABLE-DECLARATION sort=\"U\" varname=\"y\" /><VARIABLE-DECLARATION sort=\"U\" varname=\"x\" /></PUBLISH></EXPLORE></MARGRAVE-COMMAND> ";
		
// aQuery2 sat
		
		
		List<String> creationCommands = new ArrayList<String>();
		
		creationCommands.add("<MARGRAVE-COMMAND type=\"ADD\"><VOCAB-IDENTIFIER vname=\"Test1\" /><SORT-WITH-CHILDREN name=\"U\"><SORT name=\"A\" /><SORT name=\"B\" /><SORT name=\"C\" /></SORT-WITH-CHILDREN></MARGRAVE-COMMAND> ");		
		creationCommands.add("<MARGRAVE-COMMAND type=\"ADD\"><VOCAB-IDENTIFIER vname=\"Test1\" /><PREDICATE name=\"r\" /><RELATIONS><RELATION name=\"A\"/><RELATION name=\"B\"/><RELATION name=\"C\"/><RELATION name=\"C\"/></RELATIONS></MARGRAVE-COMMAND> ");
		creationCommands.add("<MARGRAVE-COMMAND type=\"ADD\"><VOCAB-IDENTIFIER vname=\"Test1\" /><CONSTANT name=\"c\" type=\"C\" /></MARGRAVE-COMMAND>");
		creationCommands.add("<MARGRAVE-COMMAND type=\"ADD\"><VOCAB-IDENTIFIER vname=\"Test1\" /><FUNCTION name=\"f\"><RELATIONS><RELATION name=\"C\" /><RELATION name=\"A\" /></RELATIONS></FUNCTION></MARGRAVE-COMMAND> ");
		
		creationCommands.add("<MARGRAVE-COMMAND type=\"CREATE POLICY LEAF\"><POLICY-IDENTIFIER pname=\"P\" /><VOCAB-IDENTIFIER vname=\"Test1\" /></MARGRAVE-COMMAND> ");
		creationCommands.add("<MARGRAVE-COMMAND type=\"ADD\"><POLICY-IDENTIFIER pname=\"P\" /><VARIABLE-DECLARATION sort=\"A\" varname=\"x\" /></MARGRAVE-COMMAND>");
		creationCommands.add("<MARGRAVE-COMMAND type=\"ADD\"><POLICY-IDENTIFIER pname=\"P\" /><VARIABLE-DECLARATION sort=\"A\" varname=\"y\" /></MARGRAVE-COMMAND>");
		
		creationCommands.add("<MARGRAVE-COMMAND type=\"ADD\"><POLICY-IDENTIFIER pname=\"P\" /><RULE name=\"Rule1\"><DECISION-TYPE type=\"permit\"><ID id=\"x\" /><ID id=\"y\" /></DECISION-TYPE>" +
				"<TARGET><AND><ATOMIC-FORMULA><RELATION-NAME><ID id=\"r\" /></RELATION-NAME><TERMS><VARIABLE-TERM id=\"x\" /><VARIABLE-TERM id=\"x\" /><VARIABLE-TERM id=\"x\" /><VARIABLE-TERM id=\"x\" /></TERMS></ATOMIC-FORMULA>" +
				"<ISA sort=\"B\" > <TERM><VARIABLE-TERM id=\"x\" /></TERM></ISA>"+
				"</AND></TARGET></RULE></MARGRAVE-COMMAND>");
		
		// FA {Permit, Deny}, but CallPolice overrides both.
		creationCommands.add("<MARGRAVE-COMMAND type=\"SET RCOMBINE FOR POLICY\"><POLICY-IDENTIFIER pname=\"P\" /><COMB-LIST>" +
				"<FA><ID id=\"permit\" /><ID id=\"deny\" /></FA>" +
				"<OVERRIDES decision=\"permit\"><ID id=\"callpolice\" /></OVERRIDES>" +
				"<OVERRIDES decision=\"deny\"><ID id=\"callpolice\" /></OVERRIDES></COMB-LIST></MARGRAVE-COMMAND>"); 
		creationCommands.add("<MARGRAVE-COMMAND type=\"PREPARE\"><POLICY-IDENTIFIER pname=\"P\" /></MARGRAVE-COMMAND>"); 
		
		//creationCommands.add("");
		
		for(String cmd : creationCommands)
		{
			handleXMLCommand(cmd);
		}
		
		String aShow = 
			"<MARGRAVE-COMMAND type=\"SHOW\"><SHOW type=\"NEXT\" id=\"Myqry\" /></MARGRAVE-COMMAND>";
		String aShow2 = 
			"<MARGRAVE-COMMAND type=\"SHOW\"><SHOW type=\"NEXT\" id=\"Myqry2\" /></MARGRAVE-COMMAND>";		
		String aReset = "<MARGRAVE-COMMAND type=\"RESET\"><RESET id=\"Myqry\" /></MARGRAVE-COMMAND>";

		handleXMLCommand(aQuery);
		handleXMLCommand(aShow); // results in a model xml response (or unsat)
		handleXMLCommand(aShow); // test MULTIPLE unsat in a row (don't get iterator exception)
		handleXMLCommand(aReset); // success
		
		handleXMLCommand(aQuery2);
		handleXMLCommand(aShow2); // results in a model xml response (or unsat)
		handleXMLCommand(aShow2); // results in a model xml response (or unsat)
		handleXMLCommand(aShow2); // results in a model xml response (or unsat)
		handleXMLCommand(aShow2); // results in a model xml response (or unsat)

		
		MEnvironment.debug();
		
		MEnvironment.writeErrLine("----- End MCommunicator Tests -----");	
	}
	
}

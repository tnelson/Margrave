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
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.URL;
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
	static final char cEOF = (char)0;
	
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
	 
		// also TODO: re-enable stack printing!

		// Re-direct all System.err input to our custom buffer		
		// Uses Apache Commons IO for WriterOutputStream.
		System.setErr(new PrintStream(new WriterOutputStream(MEnvironment.errorWriter), true));
	
		initializeLog();
		writeToLog("\n\n");
		

		
		readCommands();

		// outLog will be closed as it goes out of scope
	}

        public static void handleXMLCommand(String command)
        {        	
            DocumentBuilder docBuilder = null;
            DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
            try {
                docBuilder = docFactory.newDocumentBuilder();
            } catch (ParserConfigurationException ex) {
                Logger.getLogger(MCommunicator.class.getName()).log(Level.SEVERE, null, ex);
            }

            // Save this command for use in exception messages
    		MEnvironment.lastCommandReceived = command.trim();    		
            
            Document doc = null;

            try {
                doc = docBuilder.parse(new InputSource(new StringReader(command)));
            } catch (SAXException ex) {
                Logger.getLogger(MCommunicator.class.getName()).log(Level.SEVERE, null, ex);
            } catch (IOException ex) {
                Logger.getLogger(MCommunicator.class.getName()).log(Level.SEVERE, null, ex);
            }
            
            Document theResponse;
            try
            {
                // protect against getFirstChild() call            
                if(doc != null)
                	theResponse = xmlHelper(doc.getFirstChild(), command);
                else
                	theResponse = MEnvironment.errorResponse(MEnvironment.sNotDocument, MEnvironment.sCommand, command);
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
        private static Document xmlHelper(Node node, String originalXMLText) throws MBaseException
        {        	
        	NodeList childNodes = node.getChildNodes();
        	String type = node.getAttributes().item(0).getNodeValue();
        	writeToLog("  Type attribute was: " + type + "\n");

        	Node n;
        	MExploreCondition exploreCondition;

        	Document theResponse = null;
        	//for (int i = 0; i < childNodes.getLength(); i++) {
        		n = node;

        		if (n.getNodeType() == Node.ELEMENT_NODE)
        		{

        			/*if (type.equalsIgnoreCase("COMPARE"))
        			{
        				n = n.getFirstChild();
        				// n is now the COMPARE element
        				    					
    					Node tuplingNode = getTuplingNode(n);
    					Node debugNode = getDebugNode(n);
    					Node ceilingNode = getCeilingNode(n);
    					
    					Node pol1 =  getChildNode(n, "POLICY1");
    					Node pol2 =  getChildNode(n, "POLICY2");
    					    					
    					String polname1 = getPolicyName(pol1);
    					String polname2 = getPolicyName(pol2);

    					List<String> publ = null;
    					Boolean tupling = false;
    					Integer debugLevel = 0;
    					Integer ceilingLevel = -1; 

    					// Publish, under make no sense
    					
    					if (tuplingNode != null) 
    						tupling = true;    					
    					if (debugNode != null) 
    						 debugLevel = Integer.parseInt(getDebugLevel(debugNode));    					
    					if (ceilingNode != null) 
    						ceilingLevel = Integer.parseInt(getCeilingLevel(ceilingNode));    					
    					    					
    					writeToLog("\nCOMPARE policies: "+polname1+ ", "+polname2+"\n"); 
    					writeToLog("\nUsing Ceiling Level: " + ceilingLevel + " and DebugLevel: " + debugLevel + "\n");
    			
    					// Exception will be thrown and caught by caller to return an EXCEPTION element.
    					theResponse = MEnvironment.returnCompareQuery(originalXMLText,
    								polname1, polname2,    								    							
    								tupling, debugLevel, ceilingLevel);    			      					    					
        				
        			}*/
        			
        			if (type.equalsIgnoreCase("EXPLORE"))
        			{
        				// Catch and re-throw any exception, because if EXPLORE fails,
        				// need to reset lastResult to -1.
        				try
        				{        				        			
	        				n = n.getFirstChild();
	        				String name = n.getNodeName();
	        				if (name.equalsIgnoreCase("EXPLORE"))
	        				{
	        					//writeToLog("IN EXPLORE" + "\n");
	        					
	        					//Explore should only have one child - "Condition". exploreHelper takes the node one down from condition
	        					exploreCondition = exploreHelper(n.getFirstChild().getFirstChild()); 
	        					if (exploreCondition == null)
	        						MEnvironment.errorWriter.println("explore condition is null!");
	        					MQuery result = null;
	        					
	        					//Default Values                                     					
	        					List<MIDBCollection> under = new LinkedList<MIDBCollection>();
	        					List<String> publ = null;
	                            HashMap<String, Set<List<String>>> idbOut = new HashMap<String, Set<List<String>>>();
	                            Boolean tupling = false;
	        					Integer debugLevel = 0;
	        					Integer ceilingLevel = -1; 
	        					
	        					Node underNode = getUnderNode(n);
	        					Node publishNode = getPublishNode(n);
	        					Node tuplingNode = getTuplingNode(n);
	        					Node debugNode = getDebugNode(n);
	        					Node ceilingNode = getCeilingNode(n);
	        					
	        					// Now called INCLUDE, and 
	        					// used to alert tupling that there are EDB indexings it needs
	        					// to include, even if they don't appear.
	        					Node idbOutputNode = getIdbNode(n);
	        					
	        					if (underNode != null)
	        					{ 
	        						under = namesToIDBCollections(getUnderList(n));
	        						
	        					}
	        					if (publishNode != null) {
	        						publ = getExplorePublishVars(publishNode);
	        					}
	        					if (idbOutputNode != null) {
	        						NodeList idbChildNodes = idbOutputNode.getChildNodes();
	        						
	        						idbOut = atomicFormulasToHashmap(idbChildNodes);
	        					}
	        					if (tuplingNode != null) { //For now if the node exists just set tupling to true
	        						tupling = true;
	        					}
	        					if (debugNode != null) {
	        						 debugLevel = Integer.parseInt(getDebugLevel(debugNode));
	        					}
	        					if (ceilingNode != null) {
	        						ceilingLevel = Integer.parseInt(getCeilingLevel(ceilingNode));
	        					}
	        					
	        					writeToLog("\nUsing Ceiling Level: " + ceilingLevel + " and DebugLevel: " + debugLevel + "\n");
	        					
	        			
	        					// Exception will be thrown and caught by caller to return an EXCEPTION element.
	        					result = MQuery.createFromExplore(
	        							exploreCondition.addSeenIDBCollections(under), 
	        							publ, idbOut, tupling, debugLevel, ceilingLevel);
	        			
	      
	        					writeToLog("AT END OF EXPLORE");
	        					theResponse = MEnvironment.returnQueryResponse(result, originalXMLText);
	        					
	        				}
	  
        				} 
          				catch(MBaseException e)
        				{
          					MEnvironment.lastResult = -1;
          					throw e;
        				}
          				
        			}
        			else if (type.equalsIgnoreCase("INFO")) {
        				writeToLog("In Info");
        				String idString = getInfoId(n);
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
        				String pname = getPolicyName(n);
        				String vname = getVocabName(n);
        				theResponse = MEnvironment.createPolicyLeaf(pname, vname);
        			} 
        			else if (type.equalsIgnoreCase("CREATE POLICY SET")) {
        				String pname = getPolicyName(n);
        				String vname = getVocabName(n);
        				theResponse = MEnvironment.createPolicySet(pname, vname);
        			} 
        			else if (type.equalsIgnoreCase("CREATE VOCABULARY")) {
        				writeToLog("In Create Vocabulary\n");
        				String vname = getVocabName(node);
        				writeToLog("Got Vocab name. It is: " + vname + "\n");
        				theResponse = MEnvironment.createVocabulary(vname);
        				if (theResponse == null) {
        					writeToLog("The result of create vocabulary was null!!\n");
        				}
        				writeToLog("Finished Create Vocabulary\n");
        			} 
        			else if (type.equalsIgnoreCase("LOAD XACML POLICY")) {
        				String fileName = getLoadFileName(n);
        				String schemaFileName = getLoadSchemaFileName(n);
        				theResponse = MEnvironment.loadXACML(fileName, schemaFileName);
        			}
        			else if (type.equalsIgnoreCase("LOAD SQS POLICY")) {
        				String fileName = getLoadFileName(n);
        				theResponse = MEnvironment.loadSQS(fileName);
        			}
        			
        			else if (type.equalsIgnoreCase("PREPARE")) {
        				String pname = getPolicyName(n);
        				theResponse = MEnvironment.preparePolicy(pname);
        			} 
        			else if (type.equalsIgnoreCase("DELETE VOCABULARY")) {
        				String vname = getVocabName(n);
        				theResponse = MEnvironment.deleteVocabulary(vname);
        			} 
        			/* Ignoring for now
        			else if (type.equalsIgnoreCase("LOAD XACML POLICY")) {
        				String fname = n.getAttributes().item(0).getNodeValue();
        				String sfname = n.getAttributes().item(1).getNodeValue();
        				theResponse = MEnvironment.loadXACML(fname, sfname);
        			} 
        			else if (type.equalsIgnoreCase("LOAD SQS POLICY")) {
        				String fname = n.getAttributes().item(0).getNodeValue();
        				theResponse = MEnvironment.loadSQS(fname);
        			} */
        			//Set Statement
        			
        			else if (type.equalsIgnoreCase("SET TARGET FOR POLICY")) {
        				String pname = getPolicyName(n);
        				List<String> conjuctChain = getConjunctChainList(n);
        				theResponse = MEnvironment.setPolicyTarget(pname, conjuctChain);
        			}	
        			else if (type.equalsIgnoreCase("SET RCOMBINE FOR POLICY")) {
        				String pname = getPolicyName(n);
        				List<String> idl = getIdentifierList(n);
        				theResponse = MEnvironment.setRCombine(pname, idl);
        			}	
        			else if (type.equalsIgnoreCase("SET PCOMBINE FOR POLICY")) {
        				String pname = getPolicyName(n);
        				List<String> idl = getIdentifierList(n);
        				theResponse = MEnvironment.setPCombine(pname, idl);
        			}
        			else if (type.equalsIgnoreCase("QUIT"))
        			{
        				 MEnvironment.quitMargrave();
        			}
        			else if (type.equalsIgnoreCase("RENAME")) {
        				String id1 = getRenameFirstId(n);
        				String id2 = getRenameSecondId(n);
        				theResponse = MEnvironment.renameIDBCollection(id1, id2);
        			}
        			else if (type.equalsIgnoreCase("IS-POSSIBLE")) {
        				Integer id = Integer.parseInt(getIsPossibleId(n));
        				theResponse = MEnvironment.isPoss(id);
        				writeToLog("Returning from IS-POSSIBLE");
        			}
        			else if (type.equalsIgnoreCase("IS-GUARANTEED")) {
        				Integer id = Integer.parseInt(getIsGuaranteedId(n));
        				theResponse = MEnvironment.isGuar(id);
        			}
        			else if (type.equalsIgnoreCase("SHOW")) {
        				writeToLog("In show");
        				String showType = getShowType(n);
        				writeToLog("In show");
        				Integer id = Integer.parseInt(getShowId(n));
        				
        				writeToLog("\nshowtype: " + showType + "\n");
        				if (showType.equalsIgnoreCase("ONE"))
        				{
        					writeToLog("In Show One");
        					try
        					{
        						writeToLog("In Show One");
								theResponse = MEnvironment.getFirstModel(id);
							} catch (MBaseException e) {
								theResponse = MEnvironment.exceptionResponse(e);						
							}
							//MEnvironment.showFirstModel(id);
        				}
        				else if (showType.equalsIgnoreCase("NEXT"))
        				{
        					try
        					{
								theResponse = MEnvironment.getNextModel(id);
							} catch (MBaseException e) {
								theResponse = MEnvironment.exceptionResponse(e);						
							}
							//MEnvironment.showNextModel(id);
        				}
        				else if (showType.equalsIgnoreCase("NEXTCOLLAPSE")) {
        					theResponse = MEnvironment.showNextCollapse(id);
        				}
        				else if (showType.equalsIgnoreCase("CEILING")) {
        					theResponse = MEnvironment.showCeiling(id);
        				}
        				else if (showType.equalsIgnoreCase("POPULATED") |
        						showType.equalsIgnoreCase("UNPOPULATED")) 
        				{
        					String popIdString;
        					if (showType.equalsIgnoreCase("POPULATED")) {
        						popIdString = getPopulatedId(n); // command
        					}
        					else {
        						popIdString = getUnpopulatedId(n); // command
        					}
        					Integer popId = Integer.parseInt(popIdString);        					
        					Node showNode = getChildNode(n, "SHOW");
        					Node forCasesNode = getForCasesNode(showNode);
        					NodeList atomicFormulaNodes = showNode.getChildNodes();
        					
        					// not this call
        					//NodeList atomicFormulaNodes = getAtomicFormulaNodesFromList(n);
        					
        					// atomicFormulasToHashmap will ignore the FOR-CASES element.
        					Map<String, Set<List<String>>> atomicFormulas = atomicFormulasToHashmap(atomicFormulaNodes);
        					    
        					
        					// Default map is empty. If FOR CASES, populate it.
        					Map<String, Set<List<String>>> forCasesAtomicFormulas = new HashMap<String, Set<List<String>>>();
        					if (forCasesNode != null)
        					{
        						//NodeList forCasesAtomicFormulaNodes = getAtomicFormulaNodesFromList(forCasesNode);
        						NodeList forCasesAtomicFormulaNodes = forCasesNode.getChildNodes();
        						forCasesAtomicFormulas = atomicFormulasToHashmap(forCasesAtomicFormulaNodes);        						
        					}
        					        			
        					//writeToLog(showType+" " + popIdString + " " + atomicFormulas + " --- " + forCasesAtomicFormulas);
        					
        					// Get the result and return it
        					if (showType.equalsIgnoreCase("POPULATED")) {
    							theResponse = MEnvironment.showPopulated(popId, atomicFormulas, forCasesAtomicFormulas);
    						}
    						else {
    							theResponse = MEnvironment.showUnpopulated(popId, atomicFormulas, forCasesAtomicFormulas);
    						}        					
        					
        				} // end pop/unpop


        			} // end show
        			else if (type.equalsIgnoreCase("COUNT")) {
        				String idString = getCountId(n);
        				Integer id = Integer.parseInt(idString);
        				theResponse = MEnvironment.countModels(id);
        				
        				Node sizeNode = getSizeNode(n);
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
        				String getType = getGetInfoType(n); 
        				Node getNode = getChildNode(n, "GET-INFO");
        				String pname = getPolicyName(getNode);
        				String decname = getDecisionName(getNode);
        				// decname will be null, if the user doesn't want to limit by decision
        				
        				writeToLog("\n");
        				writeToLog("GET-INFO: "+getType+" "+getNode + " " + pname +" "+decname);
        				
        				String rname = "";
        				//if (getType.equalsIgnoreCase("DECISION")) {
        				//	theResponse = MEnvironment.getDecisionFor(pname, rname);;
        				//}
        				
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
        				Node childNode = n.getFirstChild();
        				if (childNode.getNodeName().equalsIgnoreCase("VOCAB-IDENTIFIER")) {
        					String vname = getVocabName(n);
        					Node secondChildNode = childNode.getNextSibling(); //TODO:Shouldn't be hardcoded in!!
        					String addType = secondChildNode.getNodeName();
        					writeToLog("addType: " + addType +"\n");
        					
        					if (addType.equalsIgnoreCase("SUBSORT")) {
        						String parent = getSubSortParent(n);
        						String child = getSubSortChild(n);
        						theResponse = MEnvironment.addSubsort(vname, parent, child);
        					}
        					else if (addType.equalsIgnoreCase("SORT")) {
        						String sortName = getSortName(n);
        						theResponse = MEnvironment.addSort(vname, sortName);
        						writeToLog("Added Sort\n");
        					}
        					else if (addType.equalsIgnoreCase("DECISION")) {
        						writeToLog("In Decision");
        						String decName = getDecisionName(n);
        						writeToLog("Adding Decision: " + decName + "\n");
        						theResponse = MEnvironment.addDecision(vname, decName);
        						writeToLog("Added Decision: " + decName + "\n");
        					}
        					else if (addType.equalsIgnoreCase("PREDICATE")) {
        						String sName = getPredicateName(n);
        						List<String> constr = getRelationsList(n);
        						writeToLog("Adding Predicate\n");
        						theResponse = MEnvironment.addPredicate(vname, sName, constr);
        					}
        					else if (addType.equalsIgnoreCase("REQUESTVAR")) {
        						String varName = getRequestVar(n);
        						String domainSort = getRequestSort(n);
        						theResponse = MEnvironment.addRequestVariable(vname, varName, domainSort);
        					}
        					else if (addType.equalsIgnoreCase("OTHERVAR")) {
        						String varName = getOtherVarName(n);
        						String domainSort = getOtherVarSort(n);
        						theResponse = MEnvironment.addOtherVariable(vname, varName, domainSort);
        					}
        					else if (addType.equalsIgnoreCase("CONSTRAINT")) {
        						Node constraintNode = secondChildNode; //Just for clarity
        						        						        						
        						String constraintType = getConstraintType(constraintNode);
        						
        						//Special case for subset. Something similar has to be done for disjoint
        						if (constraintType.equalsIgnoreCase("SUBSET")) {
        							String parent = getParentName(constraintNode);
        							String child = getChildName(constraintNode);
        							theResponse = MEnvironment.addConstraintSubset(vname, child, parent);
        						}
        						else {

        							List<String> relations = getRelationsList(constraintNode); 
        							String firstRelation = relations.get(0);

        							if (constraintType.equalsIgnoreCase("DISJOINT")) {
        								theResponse = MEnvironment.addConstraintDisjoint(vname, firstRelation, relations.get(1));
        							}
        							else if (constraintType.equalsIgnoreCase("DISJOINT-ALL")) {
        								theResponse = MEnvironment.addConstraintDisjointAll(vname, firstRelation);
        							}
        							else if (constraintType.equalsIgnoreCase("SINGLETON")) {
        								theResponse = MEnvironment.addConstraintSingleton(vname, firstRelation);
        							}
        							else if (constraintType.equalsIgnoreCase("SINGLETON-ALL")) {
        								theResponse = MEnvironment.addConstraintSingletonAll(vname, firstRelation);
        							}
        							else if (constraintType.equalsIgnoreCase("ATMOSTONE")) {
        								theResponse = MEnvironment.addConstraintAtMostOne(vname, firstRelation);
        							}
        							else if (constraintType.equalsIgnoreCase("ATMOSTONE-ALL")) {
        								theResponse = MEnvironment.addConstraintAtMostOneAll(vname, firstRelation);
        							}
        							else if (constraintType.equalsIgnoreCase("NONEMPTY")) {
        								theResponse = MEnvironment.addConstraintNonempty(vname, firstRelation);
        							}
        							else if (constraintType.equalsIgnoreCase("NONEMPTY-ALL")) {
        								theResponse = MEnvironment.addConstraintNonemptyAll(vname, firstRelation);
        							}
        							else if (constraintType.equalsIgnoreCase("ABSTRACT")) {
        								theResponse = MEnvironment.addConstraintAbstract(vname, firstRelation);
        							}
        							else if (constraintType.equalsIgnoreCase("ABSTRACT-ALL")) {
        								theResponse = MEnvironment.addConstraintAbstractAll(vname, firstRelation);
        							}
        							else if (constraintType.equalsIgnoreCase("TOTAL-FUNCTION")) {
        								theResponse = MEnvironment.addConstraintTotalFunction(vname, firstRelation);
        							}
        							else if (constraintType.equalsIgnoreCase("TOTAL-RELATION")) {
        								theResponse = MEnvironment.addConstraintTotalRelation(vname, firstRelation);
        							}
        							else if (constraintType.equalsIgnoreCase("PARTIAL-FUNCTION")) {
        								theResponse = MEnvironment.addConstraintPartialFunction(vname, firstRelation);
        							}
        							else if (constraintType.equalsIgnoreCase("SUBSET")) 
        							{
        								theResponse = MEnvironment.addConstraintSubset(vname, firstRelation, relations.get(1));	
        							}
        							else
        							{
        								// Unknown constraint type; throw an exception
        								theResponse = MEnvironment.errorResponse(MEnvironment.sUnknown, MEnvironment.sConstraint, constraintType);
        							}
        						}
        					}
        				}
        				else if (childNode.getNodeName().equalsIgnoreCase("POLICY-IDENTIFIER")) {
        					String pname = getPolicyName(n);
        					String rname= getRuleName(n);

        					// IMPORTANT
        					// Assume ADD -> POLICY-IDENTIFIER is always adding a RULE.
        					
        					Node ruleNode = childNode.getNextSibling();//This should be changed to be made more generic, because it assumes too much
        					String decName = getDecisionType(ruleNode); 
        					List<Node> relationNodes = getListOfRelationNodes(ruleNode);
        					List<String> relationsList = new LinkedList<String>();
        					String relationName;
        					String sign;
        					String exclamationPoint = "";
            				List<String> variables;
            				String variableListString;
            				
            				for (Node relNode : relationNodes) {
            					
            					relationName = getRelationName(relNode);
            					sign = getRelationSign(relNode);
            					//writeToLog("\n\n\n*******************************************\nSIGN IS: " + sign + "\n*****************");
            					if (sign.equalsIgnoreCase("true")) {
            						//writeToLog("No exclamation point");
            						exclamationPoint = "";
            					}
            					else {
            						//writeToLog("Added exclamation point");
            						exclamationPoint = "!";
            					}
            					
            					variables = getIdentifierList(relNode);
            					variableListString = "";
            					for (String v : variables) {
            						//Avoid beginning space
            						if (variableListString.length() < 1) {
            							variableListString = v;
            						}
            						else {
            							variableListString = variableListString + " " + v;
            						}
            					}
            					relationsList.add(exclamationPoint + relationName + " " + variableListString);
            				}
            				writeToLog("\nRELATIONSLIST: " + relationsList.toString() + "\n");
            				theResponse = MEnvironment.addRule(pname, rname, decName, relationsList);
            				
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

        	//}
        		
        	// Finally -- did we assign a proper response? If not, none of the above cases was matched.
        	// _SHOULD_ have elses everywhere. This is just in case.
        	if(theResponse == null)
        		theResponse = MEnvironment.errorResponse(MEnvironment.sFailure, "", 
        				"Fatal error: No matching case in java engine. (If you receive this error, please notify the Margrave developers.)");
        		
        	return theResponse;
        }
        
        private static HashMap<String, Set<List<String>>> atomicFormulasToHashmap(NodeList childNodes) {
        	HashMap<String, Set<List<String>>> hashMap = new HashMap<String, Set<List<String>>>();
        	// default of empty map is set above. Just populate it.
			        	        
        	
			String collectionName;
			String relationName;
			List<String> identifiers;
			
			// For each atomic formula sent
			for (int i = 0; i < childNodes.getLength(); i++)
			{
				Node childNode = childNodes.item(i);
				
				//writeToLog("\nnodename: "+childNode.getNodeName());
				//writeToLog("\nnodetype: "+childNode.getNodeType());
				
				
				// If this is not an atomic formula, pass over it
				if(childNode.getNodeName().equalsIgnoreCase("ATOMIC-FORMULA-Y") ||
						childNode.getNodeName().equalsIgnoreCase("ATOMIC-FORMULA-N")) 
				{
				
					collectionName = getAtomicFormulaYCollection(childNode); // may be null!
					if(collectionName != null)				
						relationName = getAtomicFormulaYRelation(childNode);
					else
						relationName = getAtomicFormulaNRelation(childNode);
					
					identifiers = getIdentifierList(childNode); //could be empty!
	
					writeToLog("\natomicFormulasToHashmap (ATOMIC-FMLA): "+collectionName+", "+relationName+", "+identifiers+"\n");
					
					// TODO: Should make a class to help with this eventually; right now
					// there is too much string processing going on in the engine.
					
					String idbName;
					if(collectionName != null && collectionName.length() > 0)
						idbName = collectionName + ":" + relationName;
					else
						idbName = relationName;
					
					// initialize if needed
					if(!hashMap.containsKey(idbName))
						hashMap.put(idbName, new HashSet<List<String>>());
					
					if (identifiers.size() > 0)
					{
						// indexed: need to add a variable vector to the entry's value
						hashMap.get(idbName).add(identifiers);
					}
				}
				else if(childNode.getNodeName().equalsIgnoreCase("EQUALS"))
				{
					String idname1 = getNodeAttribute(childNode, "EQUALS", "v1");
	        		String idname2 = getNodeAttribute(childNode, "EQUALS", "v2");
	        		
	        		writeToLog("\natomicFormulasToHashmap (EQUALS): "+idname1+", "+idname2+"\n");
	        		
	        		if(!hashMap.containsKey("="))
	        			hashMap.put("=", new HashSet<List<String>>());
	        		List<String> params = new ArrayList<String>(2);
	        		params.add(idname1);
	        		params.add(idname2);
	        		hashMap.get("=").add(params);	        			        		
				}
				
			}
        	return hashMap;
        }
        
		private static String getInfoId(Node n) {
			return getNodeAttribute(n, "INFO", "id");
		}
		
        //Helper functions for specific parts of commands
        private static String getPolicyName(Node n) {
        	return getNodeAttribute(n, "POLICY-IDENTIFIER", "pname");
        }
        
        private static String getVocabName(Node n) {
        	return getNodeAttribute(n, "VOCAB-IDENTIFIER", "vname");
        }
        
        private static String getSubSortParent(Node n) {
        	return getNodeAttribute(n, "SUBSORT", "parent");
        }
        
        private static String getSubSortChild(Node n) {
        	return getNodeAttribute(n, "SUBSORT", "child");
        }
        
        private static String getSortName(Node n) {
        	return getNodeAttribute(n, "SORT", "name");
        }
        
        private static String getPredicateName(Node n) {
        	return getNodeAttribute(n, "PREDICATE", "name");
        }
        
        private static String getRequestVar(Node n) {
        	return getNodeAttribute(n, "REQUESTVAR", "name");
        }
		
        private static String getRequestSort(Node n) {
        	return getNodeAttribute(n, "REQUESTVAR", "sort");
        }
        
        private static String getParentName(Node n) {
        	return getNodeAttribute(n, "PARENT-IDENTIFIER", "name");
        }
        
        private static String getChildName(Node n) {
        	return getNodeAttribute(n, "CHILD-IDENTIFIER", "name");
        }
        
        private static String getRuleName(Node n) {
        	return getNodeAttribute(n, "RULE", "name");
        }
        private static String getDecisionName(Node n) {
        	return getNodeAttribute(n, "DECISION", "name");
        }
        private static String getDecisionType(Node n) {
        	return getNodeAttribute(n, "DECISION-TYPE", "type");
        }
        
        private static String getConstraintType(Node n) {
        	return getNodeAttribute(n, "CONSTRAINT", "type");
        }

        //Relations in a rule
        private static String getRelationName(Node n) {
        	return getNodeAttribute(n, "RELATION", "name");
        }
        private static String getRelationSign(Node n) {
        	return getNodeAttribute(n, "RELATION", "sign");
        }
        private static List<Node> getListOfRelationNodes(Node n) {
        	Node relationsNode = getChildNode(n, "RELATIONS");
        	List<Node> relationNodes = new LinkedList<Node>();
        	NodeList childNodes = relationsNode.getChildNodes();

        	for (int i = 0; i < childNodes.getLength(); i++) {
        		relationNodes.add(childNodes.item(i));
        	}

        	return relationNodes;
        } 
        
        //Othervar
        public static String getOtherVarName(Node n) {
        	return getNodeAttribute(n, "OTHERVAR", "name");
        }
        public static String getOtherVarSort(Node n) {
        	return getNodeAttribute(n, "OTHERVAR", "sort");
        }
        
        //Rename
        public static String getRenameFirstId(Node n) {
        	return getNodeAttribute(n, "RENAME", "id1");
        }
        public static String getRenameSecondId(Node n) {
        	return getNodeAttribute(n, "RENAME", "id2");
        }
        
        public static String getIsPossibleId(Node n) {
        	return getNodeAttribute(n, "IS-POSSIBLE", "id");
        }
        public static String getIsGuaranteedId(Node n) {
        	return getNodeAttribute(n, "IS-GUARANTEED", "id");
        }
        
        //SHOW
        public static String getShowType(Node n) {
        	return getNodeAttribute(n, "SHOW", "type");
        }
        public static String getShowId(Node n) {
        	return getNodeAttribute(n, "SHOW", "id");
        }
        public static Node getForCasesNode(Node n) {
        	return getChildNode(n, "FORCASES");
        }
        
        // <SHOW type="populated" id="0" ... 
        public static String getPopulatedId(Node n) {
        	return getNodeAttribute(n, "SHOW", "id");
        }
        public static String getUnpopulatedId(Node n) {
        	return getNodeAttribute(n, "SHOW", "id");
        }
        
        //COUNT
        private static String getCountId(Node n) {
        	return getNodeAttribute(n, "COUNT", "id");
        }
        private static Node getSizeNode(Node n) {
        	return getChildNode(n, "SIZE");
        }
        private static String getCountSize(Node n) {
        	return getNodeAttribute(n, "COUNT", "size");
        }
        
        //GET
        public static String getGetInfoType(Node n) {
        	return getNodeAttribute(n, "GET-INFO", "type");
        }
        
        //ATOMIC FORMULAS
        public static String getAtomicFormulaYCollection(Node n) {
        	return getNodeAttribute(n, "ATOMIC-FORMULA-Y", "collection-name");
        }
        public static String getAtomicFormulaYRelation(Node n) {
        	return getNodeAttribute(n, "ATOMIC-FORMULA-Y", "relation-name");
        }
        public static String getAtomicFormulaNRelation(Node n) {
        	return getNodeAttribute(n, "ATOMIC-FORMULA-N", "relation-name");
        }
        public static NodeList getAtomicFormulaNodesFromList(Node n) {
        	Node child = getChildNode(n, "ATOMIC-FORMULA-LIST");
        	if(child == null)
        		return null;
        	return child.getChildNodes();
        }
        
        private static String getLoadFileName(Node n) {
        	return getNodeAttribute(n, "LOAD", "file-name");
        }
        private static String getLoadSchemaFileName(Node n) {
        	return getNodeAttribute(n, "LOAD", "schema-file-name");
        }
        
        public static Node getUnderNode(Node n) {
        	return getChildNode(n, "UNDER");
        }
		public static Node getPublishNode(Node n) {
			return getChildNode(n, "PUBLISH");
		}
		public static Node getIdbNode(Node n) {
			return getChildNode(n, "IDBOUTPUT");
		}
		public static Node getTuplingNode(Node n) {
			return getChildNode(n, "TUPLING");
		}
		public static Node getDebugNode(Node n) {
			return getChildNode(n, "DEBUG");
        }
		public static Node getCeilingNode(Node n) {
			return getChildNode(n, "CEILING");
		}
        
		public static List<String> getExplorePublishVars(Node n) {
			return getIdentifierList(n);
		}
        public static String getDebugLevel(Node n) {
        	return getNodeAttribute(n, "DEBUG", "debug-level");
        }
        public static String getCeilingLevel(Node n) {
        	return getNodeAttribute(n, "CEILING", "ceiling-level");
        }
        
        //LISTS
        private static List<String> getRelationsList(Node n) {
        	return getListElements(n, "RELATIONS", "name");
        }
        
        private static List<String> getConjunctChainList(Node n) {        	
			return getListElements(n, "CONJUNCTCHAIN", "name");
		}
        
        private static List<String> getIdentifierList(Node n)
        {
        	List<String> result = getListElements(n, "IDENTIFIERS", "name");
        	if(result != null)
        		return result;
        	return new ArrayList<String>();
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
        private static Node getChildNode(Node n, String nodeName) {
        	NodeList childNodes = n.getChildNodes();
        	
        	Node childNode;
        	for (int i = 0; i < childNodes.getLength(); i++) {
        		childNode = childNodes.item(i);
        		if (nodeName.equalsIgnoreCase(childNode.getNodeName())) {
        			return childNode;
        		}
        	}
        	return null; //Didn't find it, error
        }
        
        //Finds the child node of n whose name is nodeName (unless n's name is nodename), and returns the value of its attribute with attributeName
        private static String getNodeAttribute(Node n, String nodeName, String attributeName)
        {
        	if(n == null)
        		return null;
        	
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
        	return node.getAttributes().getNamedItem(attributeName).getNodeValue().toLowerCase();
        }
        
        //Returns a list of the attribute values associated with the attributeName of every childNode of a Node named listName, which is itself a child node of n
        private static List<String> getListElements(Node n, String listName, String attributeName) {
        	Node listNode = getChildNode(n, listName);
        	
        	writeToLog("\nIn getListElements. Node: "+n.getNodeName()+"; listName: "+listName+"; attributeName: "+attributeName+"\n");
        	
        	//Return null if we can't find the node
        	if (listNode == null) {
        		writeToLog("\nNo child node found with that name. Returning null.\n");
        		return null;
        	}
        	
        	LinkedList<String> attributeValues = new LinkedList<String>();
        	
        	NodeList childNodes = listNode.getChildNodes();
        	for (int i = 0; i < childNodes.getLength(); i++) {
        		attributeValues.add(childNodes.item(i).getAttributes().getNamedItem(attributeName).getNodeValue().toLowerCase());
        	}
        	return attributeValues;
        }

        //Expects the node one down from condition node
        private static MExploreCondition exploreHelper(Node n) throws MUserException, MGEManagerException, MGEUnknownIdentifier
        {
        	NodeList childNodes = n.getChildNodes();

        	String name = n.getNodeName();
        	
        	//writeToLog("\nIn exploreHelper. Node Name: " + name + "\n");
        	
        	//if(childNodes.getLength() == 0)
        	//	writeToLog("\nNo child nodes.\n");
        	//else
        	//	writeToLog("First child node's name: " + childNodes.item(0).getNodeName() + "\n");

        	if (name.equalsIgnoreCase("AND")) {
        		return exploreHelper(childNodes.item(0)).and(exploreHelper(childNodes.item(1)));
        	}
        	else if (name.equalsIgnoreCase("OR")) {
        		return exploreHelper(n.getFirstChild()).or(exploreHelper(n.getChildNodes().item(1)));
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
        	else if(name.equalsIgnoreCase("EQUALS"))
        	{        		
        		// Comes in with 2 TERMS now instead of 2 maybe-variables.    
        		MTerm term1 = termHelper(childNodes.item(0));
        		MTerm term2 = termHelper(childNodes.item(1));
        			
        		//String idname1 = getNodeAttribute(n, "EQUALS", "v1");
        		//String idname2 = getNodeAttribute(n, "EQUALS", "v2");
        		
        		writeToLog("\nEQUALS: "+term1+" = "+term2+"\n\n");
        		        		
        		        		
        		//Variable v1 = MFormulaManager.makeVariable(idname1);
        		//Variable v2 = MFormulaManager.makeVariable(idname2);
        		Formula fmla = MFormulaManager.makeEqAtom(term1.expr, term2.expr);
        		        		        	
        		return new MExploreCondition(fmla, term1, term2, true);        		
        	}
        	else if (name.equalsIgnoreCase("IFF")) {
        		return exploreHelper(n.getFirstChild()).iff(exploreHelper(n.getChildNodes().item(1)));
        	}
        	else if (name.equalsIgnoreCase("NOT")) {
        		return exploreHelper(n.getFirstChild()).not();
        	}
        	else if(name.equalsIgnoreCase("EXISTS"))
        	{
        		String theVarName = getNodeAttribute(n, "EXISTS", "var");
        		String theSortName = getNodeAttribute(n, "EXISTS", "sort");
        		
        		Variable theVar = MFormulaManager.makeVariable(theVarName);
        		Relation theSort = MFormulaManager.makeRelation(theSortName, 1);
        		
        		return exploreHelper(n.getFirstChild()).exists(theVar, theSort);
        	}
        	else if(name.equalsIgnoreCase("FORALL"))
        	{
        		// TODO
        	}
        	else if (name.equalsIgnoreCase("ATOMIC-FORMULA"))
        	{
        		Node relationName = getChildNode(n, "RELATION-NAME");
        		NodeList relationComponents = relationName.getChildNodes();
        		
        		// TODO
        		
        		String relationName = getAtomicFormulaNRelation(n);//n.getAttributes().item(0).getNodeValue();
        		
        		List<String> vl = getIdentifierList(n);

        		// Could be a view or an EDB. If EDB, must
        		// remember the Relation we created so that we can check
        		// for validity later.

        		//validateDBIdentifier(relationName);

        		MIDBCollection pol = MEnvironment.getPolicyOrView(relationName);

        		writeToLog("\nAtomic-Formula-N: \nrelationName: " + relationName + "\nvl: " + vl.toString() + "\npol: " + pol + "\n\n");
        		if (pol != null)
        		{
        			Formula idbf = MEnvironment.getOnlyIDB(relationName);
        			if(idbf != null)
        			{
            			// Perform variable substitution
           				idbf = performSubstitution(relationName, pol, idbf, vl);
            			
            			// Assemble MExploreCondition object
            			return new MExploreCondition(idbf, pol, vl);        		
        			}
        			
        			// Otherwise, must be an EDB named the same as a policy? 
        			// (Or someone forgot their :idbname suffix.)
        		}

        		// EDB, then!

        		// We don't have a vocabulary yet. So just make the relation.
        		// The manager will prevent duplicates.
        		Relation rel = MFormulaManager.makeRelation(relationName, vl.size());

        		Expression varvector;
        		Formula f = null;
        		
        		varvector = MFormulaManager.makeVarTuple(vl);
        		f = MFormulaManager.makeAtom(varvector, rel);
        		
        		// No variable substitution needed!
        		return new MExploreCondition(f, rel, vl);
        	}
        	else if(name.equalsIgnoreCase("TRUE"))
        	{
        		return new MExploreCondition(true);
        	}
        	else if (name.equalsIgnoreCase("ATOMIC-FORMULA-Y")) 
        	{
        		String collectionName = getAtomicFormulaYCollection(n);
        		String relationName = getAtomicFormulaYRelation(n);

        		//Variables
        		List<String> vl = getIdentifierList(n);
        		
        		Formula idbf = null;
        		
        		// Will throw an exception rather than return null
        		idbf = validateDBIdentifier(collectionName, relationName);
        		
        		// Perform variable substitution
        		MIDBCollection pol = MEnvironment.getPolicyOrView(collectionName);
        		        		        		
        		writeToLog("\nAtomic-Formula-Y: \nCollection Name: " + collectionName + "\nRelation Name: " + 
        				relationName + "\nPolicy: " + pol + "\nvl: " + vl.toString() + "\n\n");
       			writeToLog("\nBefore substitution: "+idbf);
       			
       			idbf = performSubstitution(collectionName, pol, idbf, vl);

       			writeToLog("\nAfter substitution: "+idbf+"\n");
       			
        		// Assemble MExploreCondition object	
        		//writeToLog("Returning from atomic-formula-y");
        		return new MExploreCondition(idbf, pol, vl);

        	}
        	
        	MEnvironment.errorWriter.println("exploreHelper returning null! error! name was: "+name);
        	
        return null;
    }
     
     private static MTerm termHelper(Node n) 
     {
    	 NodeList childNodes = n.getChildNodes();

    	 String name = n.getNodeName();
     	
     	//writeToLog("\nIn exploreHelper. Node Name: " + name + "\n");
     	
     	//if(childNodes.getLength() == 0)
     	//	writeToLog("\nNo child nodes.\n");
     	//else
     	//	writeToLog("First child node's name: " + childNodes.item(0).getNodeName() + "\n");

     	if (name.equalsIgnoreCase("FUNCTION-TERM"))
     	{
     		String funcName = getNodeAttribute(n, "FUNCTION-TERM", "func");
     		
     		List<MTerm> subTerms = new ArrayList<MTerm>();
     		for(int ii=0;ii<childNodes.getLength();ii++)
     		{
     			subTerms.add(termHelper(childNodes.item(ii)));
     		}
     		
     		return new MFunctionTerm(funcName, subTerms);
     		
     		// remember what symbols we saw, so we can catch errors?
     		
     	}	
     	else if (name.equalsIgnoreCase("CONSTANT-TERM"))
     	{
     		String constName = getNodeAttribute(n, "CONSTANT-TERM", "id");     		
     		return new MConstantTerm(constName);
     	}
     	else
     	{
     		String varName = getNodeAttribute(n, "VARIABLE-TERM", "id");
     		return new MVariableTerm(varName);
     	}
     }

	//Returns a list containing the value of the first attribute of every child node of n
     protected static List<String> getListChildren(Node n) {
         NodeList childNodes = n.getChildNodes();
         List<String> list = new LinkedList<String>();

          for (int i = 0; i < childNodes.getLength(); i++) {
              list.add(childNodes.item(i).getAttributes().item(0).getNodeValue());
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
    		 // Put the logfile with the .class files for now (avoids having location change depending on where 
    		 //   the current directory is.
    		 
    		 URL absoluteClassPathNameURL = MCommunicator.class.getClass().getResource("/edu/wpi/margrave/MCommunicator.class");
    		 URL absoluteLogFileNameURL = new URL(absoluteClassPathNameURL, sLogFileName);
    		 File logFILE = new File(absoluteLogFileNameURL.getFile());
    		 
    		 String absoluteLogFileName = java.net.URLDecoder.decode(logFILE.toString(), "UTF-8"); 
    		 
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
   	      out.println(makeDetailedError("\nError initializing log file: "+ e.getMessage() +" (exception: "+e+")"+"( outLog = "+outLog+")"+"( outLogStream = "+outLogStream+")")+cEOF);
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
     
     protected static void readCommands()
     {
    	   StringBuffer theCommand = new StringBuffer();
   		try
   		{
   			while(true)
   			{
   				int theChar = in.read();
   				if(theChar == semicolon)
   				{
   					writeToLog("========================================\n========================================\n");
   					writeToLog((new Date()).toString());
   					writeToLog("\nExecuting command: " + theCommand.toString() + "\n");
   					
   					
   					// deal with the command (including returning the result)
   					handleXMLCommand(theCommand.toString());

   					theCommand = new StringBuffer();
   				}
   				else if(theChar == -1)
   				{
   					// 	keep waiting for a semicolon. the command is still coming.
   				}
   				else
   				{
   					// Need to cast, because otherwise it will append the integer as a string.
   					theCommand.append((char)theChar);
   				}
   			} // end loop while(true)
   		}
   		catch(IOException e)
   		{
   			out.println(setupError+cEOF);
   			out.flush();
   			// don't need to flush System.err since we never use it anymore.
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
			xmlString += cEOF;
			return xmlString;
		}
		catch(Exception e)
		{
			// Will hit this if theResponse is null.	
			// don't do this. would go through System.err
			//e.printStackTrace();
			return (makeDetailedError(e.getLocalizedMessage())+cEOF);
		}
	}
     
     protected static byte[] transformXML(Document theResponse) 
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
			xmlString += cEOF;
			return xmlString.getBytes();
		}
		catch(Exception e)
		{
			// Will hit this if theResponse is null.	
			// don't do this. would go through System.err
			//e.printStackTrace();
			return (makeDetailedError(e.getLocalizedMessage())+cEOF).getBytes();
		}
	}
	
	private static Formula performSubstitution(Object collectionIdSymbol, MIDBCollection coll, Formula f, List<String> newvarnames)
	throws MUserException, MGEUnknownIdentifier
	{		
		if(f == null)
			throw new MGEUnknownIdentifier("Did not have a formula for IDB: "+collectionIdSymbol);
		if(newvarnames.size() != coll.varOrdering.size())
			report_arity_error(collectionIdSymbol, newvarnames, coll);
		
		HashMap<Variable, Variable> toReplace = new HashMap<Variable, Variable>();
			
		// coll knows what its idbs free variable vector is.
		int ii = 0;
		writeToLog("\nsubs found coll.varOrdering = "+coll.varOrdering);
		for(Variable oldv : coll.varOrdering)		
		{
			Variable v = MFormulaManager.makeVariable(newvarnames.get(ii));
			toReplace.put(oldv, v);
			
			writeToLog("\nsubs will replace var: "+oldv+" with new var: "+v);
			ii ++;	
		}
		
		return f.accept(new RelationAndVariableReplacementV(new HashMap<Relation, Relation>(), toReplace));		
	}
	
	protected static Formula validateDBIdentifier(String objn, String dbn)
	throws MUserException
	{
		
		// Is objn a policy name? If not, error.
		 MIDBCollection pol = MEnvironment.getPolicyOrView(objn);
		 
		 if(pol == null)
			 throw new MGEUnknownIdentifier("Unknown IDB Collection: "+objn);
			   	
		 // Is idb an idb in objn? If not, error
		 Formula idbf = MEnvironment.getIDB(objn, dbn);
		 if(idbf == null)
			 throw new MGEUnknownIdentifier("Unknown IDB: "+dbn+" in collection: "+objn);
		 
		 return idbf;
	}
	
	private static void report_arity_error(Object idbSymbol, List<String> varlist, MIDBCollection coll) throws MUserException
	{
		throw new MGEArityMismatch("Arity Mismatch. Vector given was: "+varlist+", but collection expects arity "+coll.varOrdering.size()+".");
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
	
}

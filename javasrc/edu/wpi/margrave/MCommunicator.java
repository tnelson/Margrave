package edu.wpi.margrave;

import java_cup.*;
import kodkod.ast.*;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
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

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.sun.java_cup.internal.runtime.Symbol;

public class MCommunicator 
{
	static final InputStream in = System.in;
	static final PrintStream out = System.out;
	static final char semicolon = ';';
	
	static final String setupError = "<MARGRAVE-RESPONSE type=\"fatal-error\"><ERROR>Unable to send XML reply.</ERROR></MARGRAVE-RESPONSE>";
	static final char cEOF = (char)0;
	
	static boolean xmlCommand;
	
	static String makeLastResortError(Document theResponse)
	{
		return "<MARGRAVE-RESPONSE type=\"fatal-error\"><ERROR>Unable to produce XML document</ERROR></MARGRAVE-RESPONSE>";
	}
	
	public static void main(String[] args) 
	{
		writeToLog("\n\n\n");
		if(args.length > 0 && args[0].toLowerCase().equals("debug"))
		{
			MEnvironment.debugParser = true;
		}						

		//if(args.length > 1) {
			xmlCommand = true;
		//}
		//else {
		//	xmlCommand = false;;
		//}
		
		readCommands();
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
            	theResponse = xmlHelper(doc.getFirstChild(), command);
            }
            catch(Exception e)
            {
            	// Construct an exception response;
            	theResponse = MEnvironment.exceptionResponse(e);
            }
            try {
        		writeToLog("Returning: " + transformXMLString(theResponse) + "\n");
        		out.write(transformXML(theResponse));
        	} catch (IOException e) {
        		// TODO Auto-generated catch block
        		e.printStackTrace();
        	}
        	out.flush(); // ALWAYS FLUSH!
        	System.err.flush(); // just in case
        }

        //Takes a MARGRAVE-COMMAND node
        private static Document xmlHelper(Node node, String originalXMLText) throws MSemanticException, MGException
        {
        	writeToLog("In XMLHelper\n");
        	NodeList childNodes = node.getChildNodes();
        	String type = node.getAttributes().item(0).getNodeValue();
        	writeToLog("type: " + type + "\n");

        	Node n;
        	MExploreCondition exploreCondition;

        	Document theResponse = null;
        	//for (int i = 0; i < childNodes.getLength(); i++) {
        		n = node;

        		if (n.getNodeType() == Node.ELEMENT_NODE)
        		{

        			if (type.equalsIgnoreCase("EXPLORE"))
        			{
        				n = n.getFirstChild();
        				String name = n.getNodeName();
        				if (name.equalsIgnoreCase("EXPLORE")) {
        					writeToLog("IN EXPLORE" + "\n");
        					
        					//Explore should only have one child - "Condition". exploreHelper takes the node one down from condition
        					exploreCondition = exploreHelper(n.getFirstChild().getFirstChild()); 
        					if (exploreCondition == null)
        						System.out.println("explore condition is null!");
        					MQuery result = null;
        					
        					//Default Values                                     					
        					List<MIDBCollection> under = new LinkedList<MIDBCollection>();
        					List<String> publ = null;
                            HashMap<String, Set<List<String>>> idbOut = new HashMap<String, Set<List<String>>>();
                            Boolean tupling = false;
        					Integer debugLevel = 0;
        					Integer ceilingLevel = 6; 
        					
        					Node underNode = getExploreUnderNode(n);
        					Node publishNode = getExplorePublishNode(n);
        					Node idbOutputNode = getExploreIdbNode(n);
        					Node tuplingNode = getExploreTuplingNode(n);
        					Node debugNode = getExploreDebugNode(n);
        					Node ceilingNode = getExploreCeilingNode(n);
        					
        					
        					if (underNode != null)
        					{ 
        						under = namesToIDBCollections(getUnderList(n));
        						
        					}
        					if (publishNode != null) {
        						publ = getExplorePublishVars(publishNode);
        					}
        					if (idbOutputNode != null) {
        						NodeList idbChildNodes = idbOutputNode.getChildNodes();
        						
        						// default of empty map is set above. Just populate it.
        						
        						String collectionName;
        						String relationName;
        						List<String> identifiers;
        						
        						// For each atomic formula sent
        						for (int i = 0; i < childNodes.getLength(); i++) {
        							Node childNode = idbChildNodes.item(i);
        							
        							collectionName = getAtomicFormulaYCollection(childNode);
        							relationName = getAtomicFormulaYRelation(childNode);
        							identifiers = getIdentifierList(childNode);
        							
        							// Identifiers could be empty (but won't be null)
        							
        							// TODO: Should make a class to help with this eventually; right now
        							// there is too much string processing going on in the engine.
        							String idbName = collectionName + ":" + relationName;
        							    
        							// initialize if needed
        							if(!idbOut.containsKey(idbName))
        								idbOut.put(idbName, new HashSet<List<String>>());
        							
        							if (identifiers.size() > 0)
        							{
        								// indexed: need to add a variable vector to the entry's value
        								idbOut.get(idbName).add(identifiers);
        							}
        							
        						}
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
        			else if (type.equalsIgnoreCase("INFO")) {
        				writeToLog("In Info");
        				String idString = getInfoId(n);
        				writeToLog("\nPast getting id info");
        				if (idString != null) {
        					//Integer id = Integer.parseInt(idString);
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
							} catch (MGException e) {
								theResponse = MEnvironment.exceptionResponse(e);						
							}
							//MEnvironment.showFirstModel(id);
        				}
        				else if (showType.equalsIgnoreCase("NEXT"))
        				{
        					try
        					{
								theResponse = MEnvironment.getNextModel(id);
							} catch (MGException e) {
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
        				else if (showType.equalsIgnoreCase("POPULATED")) {
        					List<String> rlist = getIdentifierList(n);
        					Node forCasesNode = getForCasesNode(n);
        					if (forCasesNode != null) {
        						//theResponse = MEnvironment.showPopulated(id, rlist);
        					}
        					else {
        						//theResponse = MEnvironment.showPopulated(id, rlist, clist); 
        					}
        				}
        				else if (showType.equalsIgnoreCase("UNPOPULATED")) {
        					List<String> rlist = getIdentifierList(n);
        					Node forCasesNode = getForCasesNode(n);
        					if (forCasesNode != null) {
        						//theResponse = MEnvironment.showUnpopulated(id, rlist);
        					}
        					else {

        						//theResponse = MEnvironment.showUnpopulated(id, rlist, clist);
        					}
        					
        				}


        			}
        			
        			else if (type.equalsIgnoreCase("GET")) {
        				String getType = getGetType(n);
        				String pname = getPolicyName(n);
        				String rname = "";
        				if (getType == "DECISION") {
        					theResponse = MEnvironment.getDecisionFor(pname, rname);;
        				}
        				else if (getType == "HIGHER-PRIORITY-THAN") {
        					theResponse = MEnvironment.getHigherPriorityThan(pname, rname);
        				}
        				else if (getType == "RULES") {
        					theResponse = MEnvironment.getRulesIn(pname, false);
        				}
        				else if (getType == "QUALIFIED-RULES") {
        					theResponse = MEnvironment.getRulesIn(pname, true);
        				}
        			}
        			
        			else if (type.equalsIgnoreCase("COMPARE")) {
        				
        			}
        			
        			//Add Statement
        			else if (type.equalsIgnoreCase("ADD")) {
        				Node childNode = n.getFirstChild();
        				if (childNode.getNodeName().equalsIgnoreCase("VOCAB-IDENTIFIER")) {
        					String vname = getVocabName(n);
        					Node secondChildNode = childNode.getNextSibling(); //Probably shouldn't be hardcoded in
        					String addType = secondChildNode.getNodeName();
        					writeToLog("addType: " + addType +"\n");
        					if (addType == "SUBSORT") {
        						String parent = getSubSortParent(n);
        						String child = getSubSortChild(n);
        						theResponse = MEnvironment.addSubsort(vname, parent, child);
        					}
        					else if (addType == "SORT") {
        						String sortName = getSortName(n);
        						theResponse = MEnvironment.addSort(vname, sortName);
        						writeToLog("Added Sort\n");
        					}
        					else if (addType == "DECISION") {
        						writeToLog("In Decision");
        						String decName = getDecisionName(n);
        						writeToLog("Adding Decision: " + decName + "\n");
        						theResponse = MEnvironment.addDecision(vname, decName);
        						writeToLog("Added Decision: " + decName + "\n");
        					}
        					else if (addType == "PREDICATE") {
        						String sName = getPredicateName(n);
        						List<String> constr = getRelationsList(n);
        						writeToLog("Adding Predicate\n");
        						theResponse = MEnvironment.addPredicate(vname, sName, constr);
        					}
        					else if (addType == "REQUESTVAR") {
        						String varName = getRequestVar(n);
        						String domainSort = getRequestSort(n);
        						theResponse = MEnvironment.addRequestVariable(vname, varName, domainSort);
        					}
        					else if (addType == "OTHERVAR") {
        						String varName = getOtherVarName(n);
        						String domainSort = getOtherVarSort(n);
        						theResponse = MEnvironment.addOtherVariable(vname, varName, domainSort);
        					}
        					else if (addType == "CONSTRAINT") {
        						Node constraintNode = secondChildNode; //Just for clarity
        						
        						String constraintType = getConstraintType(constraintNode);
        						List<String> relations = getRelationsList(constraintNode);
        						
        						String firstRelation = relations.get(0);
        						
        						if (constraintType == "DISJOINT") {
        							theResponse = MEnvironment.addConstraintDisjoint(vname, firstRelation, relations.get(1));
        						}
        						else if (constraintType == "DISJOINT-ALL") {
        							theResponse = MEnvironment.addConstraintDisjointAll(vname, firstRelation);
        						}
        						else if (constraintType == "SINGLETON") {
        							theResponse = MEnvironment.addConstraintSingleton(vname, firstRelation);
        						}
        						else if (constraintType == "SINGLETON-ALL") {
        							theResponse = MEnvironment.addConstraintSingletonAll(vname, firstRelation);
        						}
        						else if (constraintType == "ATMOSTONE") {
        							theResponse = MEnvironment.addConstraintAtMostOne(vname, firstRelation);
        						}
        						else if (constraintType == "NONEMPTY") {
        							theResponse = MEnvironment.addConstraintNonempty(vname, firstRelation);
        						}
        						else if (constraintType == "NONEMPTY-ALL") {
        							theResponse = MEnvironment.addConstraintNonemptyAll(vname, firstRelation);
        						}
        						else if (constraintType == "ABSTRACT") {
        							theResponse = MEnvironment.addConstraintAbstract(vname, firstRelation);
        						}
        						else if (constraintType == "ABSTRACT-ALL") {
        							theResponse = MEnvironment.addConstraintAbstractAll(vname, firstRelation);
        						}
        						else if (constraintType == "TOTAL-FUNCTION") {
        							theResponse = MEnvironment.addConstraintTotalFunction(vname, firstRelation);
        						}
        						else if (constraintType == "PARTIAL-FUNCTION") {
        							theResponse = MEnvironment.addConstraintPartialFunction(vname, firstRelation);
        						}
        						else if (constraintType == "SUBSET") {
        							theResponse = MEnvironment.addConstraintSubset(vname, firstRelation, relations.get(1));
        						}
        					}
        				}
        				else if (childNode.getNodeName().equalsIgnoreCase("POLICY-IDENTIFIER")) {
        					String pname = getPolicyName(n);
        					String rname= getRuleName(n);

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
            					writeToLog("\n\n\n*******************************************\nSIGN IS: " + sign + "\n*****************");
            					if (sign.equalsIgnoreCase("true")) {
            						writeToLog("No exclamation point");
            						exclamationPoint = "";
            					}
            					else {
            						writeToLog("Added exclamation point");
            						exclamationPoint = "!";
            					}
            					variables = getIdentifierList(relNode);
            					variableListString = "";
            					for (String v : variables) {
            						//Avoid beginning space
            						if (variableListString == "") {
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
        					String parent = getParentName(n);
        					String child = getChildName(n);
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
        	return theResponse;
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
        
        //GET
        public static String getGetType(Node n) {
        	return getNodeAttribute(n, "GET", "type");
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
        
        
        
        public static Node getExploreUnderNode(Node n) {
        	return getChildNode(n, "UNDER");
        }
		public static Node getExplorePublishNode(Node n) {
			return getChildNode(n, "PUBLISH");
		}
		public static Node getExploreIdbNode(Node n) {
			return getChildNode(n, "IDBOUTPUT");
		}
		public static Node getExploreTuplingNode(Node n) {
			return getChildNode(n, "TUPLING");
		}
		public static Node getExploreDebugNode(Node n) {
			return getChildNode(n, "DEBUG");
        }
		public static Node getExploreCeilingNode(Node n) {
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
			return getListElements(n, "CONJUCTCHAIN", "name");
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
        		if (nodeName == childNode.getNodeName()) {
        			return childNode;
        		}
        	}
        	return null; //Didn't find it, error
        }
        
        //Finds the child node of n whose name is nodeName (unless n's name is nodename), and returns the value of its attribute with attributeName
        private static String getNodeAttribute(Node n, String nodeName, String attributeName) {
        	Node node = null;
        	if (n.getNodeName() == nodeName) {
        		node = n;
        	}
        	else {
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
        	
        	//Return null if we can't find the node
        	if (listNode == null) {
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
        private static MExploreCondition exploreHelper(Node n) throws MSemanticException, MGEManagerException {
        	NodeList childNodes = n.getChildNodes();

        	String name;

        	//Node n;
        	//for (int i = 0; i < childNodes.getLength(); i++) {
        	//    n = childNodes.item(i);
        	name = n.getNodeName();
        	writeToLog("Name: " + name + "\n");
        	writeToLog("First node's name: " + childNodes.item(0).getNodeName() + "\n");

        	if (name.equalsIgnoreCase("AND")) {
        		return exploreHelper(childNodes.item(0)).and(
        				exploreHelper(childNodes.item(1)));
        	}
        	else if (name.equalsIgnoreCase("OR")) {
        		return exploreHelper(n.getFirstChild()).or(exploreHelper(n.getChildNodes().item(1)));
        	}
        	else if (name.equalsIgnoreCase("IMPLIES")) {
        		return exploreHelper(n.getFirstChild()).implies(exploreHelper(n.getChildNodes().item(1)));
        	}
        	else if (name.equalsIgnoreCase("IFF")) {
        		return exploreHelper(n.getFirstChild()).iff(exploreHelper(n.getChildNodes().item(1)));
        	}
        	else if (name.equalsIgnoreCase("NOT")) {
        		return exploreHelper(n.getFirstChild()).not();
        	}
        	else if (name.equalsIgnoreCase("ATOMIC-FORMULA-N")) {
        		String relationName = getAtomicFormulaNRelation(n);//n.getAttributes().item(0).getNodeValue();
        		
        		List<String> vl = getIdentifierList(n);

        		// Could be a view or an EDB. If EDB, must
        		// remember the Relation we created so that we can check
        		// for validity later.

        		//validateDBIdentifier(relationName);

        		MIDBCollection pol = MEnvironment.getPolicyOrView(relationName);

        		writeToLog("\nAtomic-Formula-N: \nrelationName: " + relationName + "\nvl: " + vl.toString() + "\npol: " + pol + "\n");
        		if (pol != null)
        		{
        			Formula idbf = MEnvironment.getOnlyIDB(relationName);
        			// Perform variable substitution
       				idbf = performSubstitution(relationName, pol, idbf, vl);
        			
        			// Assemble MExploreCondition object
        			return new MExploreCondition(idbf, pol, vl);
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
        		        		        		
        		writeToLog("\nAtomic-Formula-Y: \nCollection Name: " + collectionName + "\nRelation Name: " + relationName + "\nPolicy: " + pol + "\nvl: " + vl.toString() + "\n");
        		
       			idbf = performSubstitution(collectionName, pol, idbf, vl);

        		// Assemble MExploreCondition object	
        		writeToLog("Returning from atomic-formula-y");
        		return new MExploreCondition(idbf, pol, vl);

        	}
         System.out.println("returning null! error!");
        return null;
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
     
     static void writeToLog(String s) {
    	 try{
    		    // Create file 
    		    FileWriter fstream = new FileWriter("log.txt", true);
    		        BufferedWriter out = new BufferedWriter(fstream);
    		    out.write(s);
    		    //Close the output stream
    		    out.close();
    		    }catch (Exception e)
    		    {
    		      //Catch exception if any
    		      System.err.println("Error: " + e.getMessage());
    		    }
     }
     
     protected static void readCommands() {
    	   StringBuffer theCommand = new StringBuffer();
   		try
   		{
   			while(true)
   			{
   				int theChar = in.read();
   				if(theChar == semicolon)
   				{
   					writeToLog("Executing command: " + theCommand.toString() + "\n");
   					executeCommand(theCommand.toString());

   					theCommand = new StringBuffer();
   				}
   				else if(theChar == -1)
   				{
   					// 	keep waiting for a semicolon.
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
   			System.out.println(setupError+cEOF);
   			out.flush();
   			System.err.flush();
   		}
     }
     
     protected static void executeCommand(String command) {
    	 if(xmlCommand) {
    		 handleXMLCommand(command);
    	 }
    	 else {
    		 //handleTextCommand(command);
    	 }
     }


    /* protected static void handleTextCommand(String command) {
    	 // Command is complete. Deal with it.
    	 Document theResponse = MEnvironment.commandSilent(command);

    	 try {
    		 out.write(transformXML(theResponse));
    	 } catch (IOException e) {
    		 // TODO Auto-generated catch block
    		 e.printStackTrace();
    	 }
    	 out.flush(); // ALWAYS FLUSH!
    	 System.err.flush(); // just in case

     }*/
     
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
			System.err.println(e.getLocalizedMessage());
			//e.printStackTrace();
			return (makeLastResortError(theResponse)+cEOF);
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
			System.err.println(e.getLocalizedMessage());
			//e.printStackTrace();
			return (makeLastResortError(theResponse)+cEOF).getBytes();
		}
	}
	
	private static Formula performSubstitution(Object collectionIdSymbol, MIDBCollection coll, Formula f, List<String> newvarnames)
	throws MSemanticException
	{		
		
		if(newvarnames.size() != coll.varOrdering.size())
			report_arity_error(collectionIdSymbol, newvarnames, coll);
		
		HashMap<Variable, Variable> toReplace = new HashMap<Variable, Variable>();
			
		// coll knows what its idbs free variable vector is.
		int ii = 0;
		for(Variable oldv : coll.varOrdering)		
		{
			Variable v = MFormulaManager.makeVariable(newvarnames.get(ii));
			toReplace.put(oldv, v);
			ii ++;	
		}
		
		return f.accept(new RelationAndVariableReplacementV(new HashMap<Relation, Relation>(), toReplace));		
	}
	
	protected static Formula validateDBIdentifier(String objn, String dbn)
	throws MSemanticException
	{
		
		// Is objn a policy name? If not, error.
		 MIDBCollection pol = MEnvironment.getPolicyOrView(objn);
		 
		 if(pol == null)
			 throw new MSemanticException("Unknown IDB Collection: "+objn);
			   	
		 // Is idb an idb in objn? If not, error
		 Formula idbf = MEnvironment.getIDB(objn, dbn);
		 if(idbf == null)
			 throw new MSemanticException("Unknown IDB: "+dbn+" in collection: "+objn);
		 
		 return idbf;
	}
	
	private static void report_arity_error(Object idbSymbol, List<String> varlist, MIDBCollection coll) throws MSemanticException
	{
		throw new MSemanticException("Arity Mismatch. Vector given was: "+varlist+", but collection expects arity "+coll.varOrdering.size()+".");
	}

	private static List<MIDBCollection> namesToIDBCollections(List<String> names) throws MSemanticException
	{
		if(names == null)
			return new ArrayList<MIDBCollection>();
		
		List<MIDBCollection> result = new ArrayList<MIDBCollection>(names.size());
		
		for(String n : names)
		{
			if(MEnvironment.getPolicyOrView(n) == null)
				throw new MSemanticException("Unknown symbol in UNDER clause: "+n);
			result.add(MEnvironment.getPolicyOrView(n));
		}
		
		return result;
	}
	
}

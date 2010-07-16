package edu.wpi.margrave;

import java_cup.*;
import kodkod.ast.*;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
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
		System.out.println("a");
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

        public static void handleXMLCommand(String command) {
            DocumentBuilder docBuilder = null;
            DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
            try {
                docBuilder = docFactory.newDocumentBuilder();
            } catch (ParserConfigurationException ex) {
                Logger.getLogger(MCommunicator.class.getName()).log(Level.SEVERE, null, ex);
            }

            Document doc = null;

            try {
            	System.out.println("B");
                doc = docBuilder.parse(new InputSource(new StringReader(command)));
                System.out.println("C");
            } catch (SAXException ex) {
                Logger.getLogger(MCommunicator.class.getName()).log(Level.SEVERE, null, ex);
            } catch (IOException ex) {
                Logger.getLogger(MCommunicator.class.getName()).log(Level.SEVERE, null, ex);
            }

            xmlHelper(doc.getFirstChild());
        }

     private static void xmlHelper(Node node) {
        NodeList nodeList = node.getChildNodes();

        Node n;
        MExploreCondition exploreCondition;
        for (int i = 0; i < nodeList.getLength(); i++) {
            n = nodeList.item(i);

            if (n.getNodeType() == Node.ELEMENT_NODE) {
                String name = n.getNodeName();

                if (name.equalsIgnoreCase("EXPLORE")) {
                    exploreCondition = exploreHelper(n.getFirstChild().getFirstChild()); //Explore should only have one child - "Condition"
                } else if (name.equalsIgnoreCase("PUBLISH")) {
                }
            } else {
                System.out.println("error!");
            }

        }
    }

     //Expects the node one down from condition node
     private static MExploreCondition exploreHelper(Node n) {
        NodeList childNodes = n.getChildNodes();

        String name;
        
        //Node n;
        //for (int i = 0; i < childNodes.getLength(); i++) {
        //    n = childNodes.item(i);
            name = n.getNodeName();
            System.out.println("Name: " + name);
            System.out.println("First node's name: " + childNodes.item(0).getNodeName());
            
            if (name.equalsIgnoreCase("AND")) {
                exploreHelper(childNodes.item(0)).and(
                		exploreHelper(n.getChildNodes().item(1)));
            }
            else if (name.equalsIgnoreCase("OR")) {
                exploreHelper(n.getFirstChild()).or(exploreHelper(n.getChildNodes().item(1)));
            }
            else if (name.equalsIgnoreCase("IMPLIES")) {
                exploreHelper(n.getFirstChild()).implies(exploreHelper(n.getChildNodes().item(1)));
            }
            else if (name.equalsIgnoreCase("IFF")) {
                exploreHelper(n.getFirstChild()).iff(exploreHelper(n.getChildNodes().item(1)));
            }
            else if (name.equalsIgnoreCase("NOT")) {
                exploreHelper(n.getFirstChild()).not();
            }
            else if (name.equalsIgnoreCase("ATOMIC-FORMULA-N")) {
                String relationName = n.getAttributes().item(0).getNodeValue();

                Node variableVector = n.getFirstChild();
                NodeList variableNodes = variableVector.getChildNodes();
                List<String> vl = new LinkedList<String>();

                 for (int j = 0; j < variableNodes.getLength(); j++) {
                     vl.add(variableNodes.item(j).getAttributes().item(0).getNodeValue());
                 }

                 // Could be a view or an EDB. If EDB, must
                 // remember the Relation we created so that we can check
                 // for validity later.

                 //validateDBIdentifier(relationName);

                 MIDBCollection pol = MEnvironment.getPolicyOrView(relationName);

                 if (pol != null) {
                     Formula idbf = MEnvironment.getOnlyIDB(relationName);
                     // Perform variable substitution
                     try {
						idbf = performSubstitution(relationName, pol, idbf, vl);
					} catch (MSemanticException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					

                     // Assemble MExploreCondition object
                     return new MExploreCondition(idbf, pol, vl);
                 }

                 // EDB, then!

                 // We don't have a vocabulary yet. So just make the relation.
                 // The manager will prevent duplicates.
                 Relation rel = MFormulaManager.makeRelation(relationName, vl.size());

                 Expression varvector;
                 Formula f = null;
				try {
					varvector = MFormulaManager.makeVarTuple(vl);
					f = MFormulaManager.makeAtom(varvector, rel);
				} catch (MGEManagerException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				
                 // No variable substitution needed!
                 return new MExploreCondition(f, rel, vl);
             }
         //}
        return null;
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
    		 handleTextCommand(command);
    	 }
     }


     protected static void handleTextCommand(String command) {
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
	
	protected Formula validateDBIdentifier(Object collectionSymbol, Object dbSymbol)
	throws MSemanticException
	{
		String objn = (String)((java_cup.runtime.Symbol) collectionSymbol).value;
		String dbn = (String)((java_cup.runtime.Symbol) dbSymbol).value;
		
		// Is objn a policy name? If not, error.
		 MIDBCollection pol = MEnvironment.getPolicyOrView(objn);
		 if(pol == null)
		 	report_unknown_identifier(collectionSymbol);
			   	
		 // Is idb an idb in objn? If not, error
		 Formula idbf = MEnvironment.getIDB(objn, dbn);
		 if(idbf == null)
		 	report_unknown_idb(collectionSymbol, dbSymbol);
		 
		 return idbf;
	}
	
	private void report_unknown_identifier(Object idSymbol) throws MSemanticException
	{	
		Symbol tok = (Symbol)idSymbol;	
		throw new MSemanticException("Unknown identifier", tok.left, tok.right, idSymbol);
	}
	
	private void report_unknown_idb(Object collSymbol, Object idbSymbol) throws MSemanticException
	{
		Symbol tok = (Symbol)idbSymbol;	
		Symbol collTok = (Symbol)collSymbol;
		throw new MSemanticException("Unknown IDB for the collection "+collTok.value, tok.left, tok.right, idbSymbol);
	}
	
	private static void report_arity_error(Object idbSymbol, List<String> varlist, MIDBCollection coll) throws MSemanticException
	{
		Symbol tok = (Symbol)idbSymbol;	
		throw new MSemanticException("Arity Mismatch. Vector given was: "+varlist+", but collection expects arity "+coll.varOrdering.size()+".", tok.left, tok.right, idbSymbol);
	}

}

package edu.wpi.margrave;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.StringWriter;
import java.util.Arrays;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;

public class MCommunicator 
{
	static final InputStream in = System.in;
	static final PrintStream out = System.out;
	static final char semicolon = ';';
	
	static final String setupError = "<MARGRAVE-RESPONSE type=\"fatal-error\"><ERROR>Unable to send XML reply.</ERROR></MARGRAVE-RESPONSE>";
	static final char cEOF = (char)0;
	
	static String makeLastResortError(Document theResponse)
	{
		return "<MARGRAVE-RESPONSE type=\"fatal-error\"><ERROR>Unable to produce XML document</ERROR></MARGRAVE-RESPONSE>";
	}
	
	public static void main(String[] args) 
	{
		if(args.length > 0 && args[0].toLowerCase().equals("debug"))
		{
			MEnvironment.debugParser = true;
		}
								
		StringBuffer theCommand = new StringBuffer();
		try
		{
			while(true)
			{
				int theChar = in.read();
				if(theChar == semicolon)
				{
					// Command is complete. Deal with it.
					Document theResponse = MEnvironment.commandSilent(theCommand.toString());					

					out.write(transformXML(theResponse));	
					out.flush(); // ALWAYS FLUSH!	
					System.err.flush(); // just in case
					
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
}

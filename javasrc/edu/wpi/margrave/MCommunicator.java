package edu.wpi.margrave;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.Arrays;

public class MCommunicator 
{
	static final InputStream in = System.in;
	static final PrintStream out = System.out;
	static final char semicolon = ';';

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
					String theResponse = MEnvironment.commandSilent(theCommand.toString());
					char aNul = 0;
					theResponse += aNul;
					
					//System.err.println(Arrays.toString(theResponse.getBytes()));
					//System.err.println(theResponse);
					out.write(theResponse.getBytes());	
					out.flush(); // ALWAYS FLUSH!	
					
					theCommand = new StringBuffer(); 				
				}
				else if(theChar == -1)
				{
					// 	keep waiting for a semicolon.
				}
				else
				{
					// Need to cast 
					// because otherwise it will append the integer as a string.
					theCommand.append((char)theChar);
					//System.err.println((char)theChar);
				}					
			} // end loop while(true)
		}
		catch(IOException e)
		{
			System.err.println(e.getLocalizedMessage());
			//System.err.println(e.getStackTrace());
		}
	}
}

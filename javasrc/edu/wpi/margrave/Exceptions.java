package edu.wpi.margrave;

import java.io.IOException;

//***********************************************
//Margrave Exceptions
//***********************************************

class MGException extends Exception
{
	private static final long serialVersionUID = 100; 
	
	MGException(String s)
	{
		super(s);
	}
}

//class MSemanticException extends IOException
class MSemanticException extends MGException
{	
	String problem;

	MSemanticException(String problem)
	{
		super("Margrave could not understand..."); // don't have a null message
		this.problem = problem;
	}
}


class MGECombineVocabs extends MGException
{
	private static final long serialVersionUID = 100; 
	
	MGECombineVocabs(String s)
	{
		super(s);
	}
}

class MGEUnsortedVariable extends MGException
{
	private static final long serialVersionUID = 100;
	
	MGEUnsortedVariable(String s)
	{
		super(s);
	}
}

class MGEBadQueryString extends MGException
{
	private static final long serialVersionUID = 100;
	
	MGEBadQueryString(String s)
	{
		super(s);
	}
}

class MGEUnknownIdentifier extends MGException
{
	private static final long serialVersionUID = 100;
	
	public MGEUnknownIdentifier(String message)
	{         
		super(message);
	}
}

class MGEArityMismatch extends MGException
{
	private static final long serialVersionUID = 100;
	
public MGEArityMismatch(String message)
{         
  super(message);
}
}

class MGEBadCombinator extends MGException
{
	private static final long serialVersionUID = 100;
	
	public MGEBadCombinator(String message)
	{         
		super(message);
	}
}

class MGEBadIdentifierName extends MGException
{
	private static final long serialVersionUID = 100;
	
public MGEBadIdentifierName(String message)
{         
  super(message);
}
}

class MGENoMoreSolutions extends MGException
{
	private static final long serialVersionUID = 100;
	
public MGENoMoreSolutions(String message)
{         
  super(message);
}
}

class MGEUnsupportedXACML extends MGException
{
	private static final long serialVersionUID = 100;
	
	MGEUnsupportedXACML(String s)
	{ 
		super(s);
	}
}


/**
 * Indicates an error given by the Formula Manager:
 * Unknown Relation name, etc.
 * @author Tim
 *
 */
class MGEManagerException extends MGException
{
	private static final long serialVersionUID = 100; 
	
	MGEManagerException(String s)
	{
		super(s);
	}
}
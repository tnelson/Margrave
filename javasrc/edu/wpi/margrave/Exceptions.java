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

import kodkod.ast.Variable;


//***********************************************
//Margrave Exceptions
//***********************************************


// All MBaseExceptions MUST BE CAUGHT in the MCommunicator.
// They inherit RuntimeException so they can be thrown by the 
// many visitors (extending Kodkod visitors) Margrave uses.
abstract class MBaseException extends RuntimeException
{
	private static final long serialVersionUID = 100; 
	
	MBaseException(String s)
	{
		super(s);
	}
}

/* Should be displayed nicely to the user. e.g.
 * "Pardon me, but I do not recognize that identifier. Please pass the crumpets."
 */
// TODO should be abstract; will want some new exception types first
class MUserException extends MBaseException
{	
	private static final long serialVersionUID = 100; 
	
	MUserException(String problem)
	{
		super(problem);
	}
}

/* Should be displayed as a nasty error. e.g.
 * "Argh! Here is a stacktrace."
 */
abstract class MFatalException extends MBaseException
{	
	private static final long serialVersionUID = 100; 
	
	MFatalException(String problem)
	{
		super(problem);
	}
}

// ------------------------------------------------------------------

/* Thrown when the formula given cannot be tupled, or the 
 *  tupling process gets confused.
 */
class MGETuplingFailure extends MUserException
{
	private static final long serialVersionUID = 100; 
	
	MGETuplingFailure(String s)
	{
		super(s);
	}
}

/* Thrown if a pair of vocabularies used in the same query cannot be combined.
 * (e.g., they may have different request vectors)
 */
class MGECombineVocabs extends MUserException
{
	private static final long serialVersionUID = 100; 
	
	MGECombineVocabs(String s)
	{
		super(s);
	}
}

//Thrown by FormulaSigInfo if a given Formula contains 
//unsupported features or quantification over a non-sort.
class MUnsupportedFormulaException extends MUserException
{
	private static final long serialVersionUID = 100; 
	
	MUnsupportedFormulaException(String str)
	{
		super(str);
	}
}

//Thrown by FormulaSigInfo if the user asks about an 
//Expression which is not a sort w/r/t the given Sig.
class MNotASortException extends MUserException
{
	private static final long serialVersionUID = 100; 
	
	MNotASortException(String str)
	{
		super(str);
	}
}

class MGEBadQueryString extends MUserException
{
	private static final long serialVersionUID = 100;
	
	MGEBadQueryString(String s)
	{
		super(s);
	}
}

class MGEUnknownIdentifier extends MUserException
{
	private static final long serialVersionUID = 100;
	
	public MGEUnknownIdentifier(String message)
	{         
		super(message);
	}
}

class MGEArityMismatch extends MUserException
{
	private static final long serialVersionUID = 100;
	
	public MGEArityMismatch(String message)
	{         
		super(message);
	}
}

class MGEBadCombinator extends MUserException
{
	private static final long serialVersionUID = 100;
	
	public MGEBadCombinator(String message)
	{         
		super(message);
	}
}

class MGEBadIdentifierName extends MUserException
{
	private static final long serialVersionUID = 100;
	
	public MGEBadIdentifierName(String message)
	{         
		super(message);
	}
}

class MGEVariableAlreadyBound extends MUserException
{
	private static final long serialVersionUID = 100;
	Object varOrSetOfVars;	
	
	public MGEVariableAlreadyBound(Object varOrSetOfVars, String message)
	{    
		super(message);    
		this.varOrSetOfVars = varOrSetOfVars;
	}
}

/* Thrown if a solution iterator's next() method is called
 * but the iterator is empty.
 */
class MGENoMoreSolutions extends MUserException
{
	private static final long serialVersionUID = 100;
	
	public MGENoMoreSolutions(String message)
	{         
		super(message);
	}
}

/*
 * Thrown by the XACML parser if the document contains
 * unsupported XACML features
 */
class MGEUnsupportedXACML extends MUserException
{
	private static final long serialVersionUID = 100;
	
	MGEUnsupportedXACML(String s)
	{ 
		super(s);
	}
}
class MGEUnsupportedSQS extends MUserException
{
	private static final long serialVersionUID = 100;
	
	MGEUnsupportedSQS(String s)
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
class MGEManagerException extends MUserException
{
	private static final long serialVersionUID = 100; 
	
	MGEManagerException(String s)
	{
		super(s);
	}
}
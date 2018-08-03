
//----------------------------------------------------
// The following code was generated by CUP v0.11a beta 20060608
// Wed Jun 09 14:31:50 EDT 2010
//----------------------------------------------------

package edu.wpi.margrave;

import java_cup.runtime.*;
import java.io.*;

/** CUP v0.11a beta 20060608 generated parser.
  * @version Wed Jun 09 14:31:50 EDT 2010
  */
public class MJunosParser extends java_cup.runtime.lr_parser {

  /** Default constructor. */
  public MJunosParser() {super();}

  /** Constructor which sets the default scanner. */
  public MJunosParser(java_cup.runtime.Scanner s) {super(s);}

  /** Constructor which sets the default scanner. */
  public MJunosParser(java_cup.runtime.Scanner s, java_cup.runtime.SymbolFactory sf) {super(s,sf);}

  /** Production table. */
  protected static final short _production_table[][] = 
    unpackFromStrings(new String[] {
    "\000\002\000\002\002\003\000\002\002\004" });

  /** Access to production table. */
  public short[][] production_table() {return _production_table;}

  /** Parse-action table. */
  protected static final short[][] _action_table = 
    unpackFromStrings(new String[] {
    "\000\004\000\004\004\005\001\002\000\004\002\006\001" +
    "\002\000\004\002\001\001\002\000\004\002\000\001\002" +
    "" });

  /** Access to parse-action table. */
  public short[][] action_table() {return _action_table;}

  /** <code>reduce_goto</code> table. */
  protected static final short[][] _reduce_table = 
    unpackFromStrings(new String[] {
    "\000\004\000\004\002\003\001\001\000\002\001\001\000" +
    "\002\001\001\000\002\001\001" });

  /** Access to <code>reduce_goto</code> table. */
  public short[][] reduce_table() {return _reduce_table;}

  /** Instance of action encapsulation class. */
  protected CUP$MJunosParser$actions action_obj;

  /** Action encapsulation object initializer. */
  protected void init_actions()
    {
      action_obj = new CUP$MJunosParser$actions(this);
    }

  /** Invoke a user supplied parse action. */
  public java_cup.runtime.Symbol do_action(
    int                        act_num,
    java_cup.runtime.lr_parser parser,
    java.util.Stack            stack,
    int                        top)
    throws java.lang.Exception
  {
    /* call code in generated class */
    return action_obj.CUP$MJunosParser$do_action(act_num, parser, stack, top);
  }

  /** Indicates start state. */
  public int start_state() {return 0;}
  /** Indicates start production. */
  public int start_production() {return 1;}

  /** <code>EOF</code> Symbol index. */
  public int EOF_sym() {return 0;}

  /** <code>error</code> Symbol index. */
  public int error_sym() {return 1;}


  /** User initialization code. */
  public void user_init() throws java.lang.Exception
    {
 /* No longer an init method? */           
    }

  /** Scan to get the next Symbol. */
  public java_cup.runtime.Symbol scan()
    throws java.lang.Exception
    {
	  return getScanner().next_token();
    }




public void report_fatal_error(String message, Object info)
throws Exception
{
	System.err.println(message);
	if(info instanceof Symbol)
	{
		Symbol tok = (Symbol)info;

		// Don't try to recover. Just report the error to the user.
		// Docs say line# = cur_token.left 
		//          col# = cur_token.right

		throw new MParserException(tok.left,
				tok.right, tok.value);
	}

	
	super.report_fatal_error(message, info);
}

protected PrintStream errorStream = System.err;
protected MEnvironment environment = null;

public void setEnvironment(MEnvironment environment)
{
	this.environment = environment;
}

public void setErrorStream(PrintStream errorStream)
{
	this.errorStream = errorStream;
}


}

/** Cup generated class to encapsulate user supplied action code.*/
class CUP$MJunosParser$actions {
  private final MJunosParser parser;

  /** Constructor */
  CUP$MJunosParser$actions(MJunosParser parser) {
    this.parser = parser;
  }

  /** Method with the actual generated action code. */
  public final java_cup.runtime.Symbol CUP$MJunosParser$do_action(
    int                        CUP$MJunosParser$act_num,
    java_cup.runtime.lr_parser CUP$MJunosParser$parser,
    java.util.Stack            CUP$MJunosParser$stack,
    int                        CUP$MJunosParser$top)
    throws java.lang.Exception
    {
      /* Symbol object for return from actions */
      java_cup.runtime.Symbol CUP$MJunosParser$result;

      /* select the action based on the action number */
      switch (CUP$MJunosParser$act_num)
        {
          /*. . . . . . . . . . . . . . . . . . . .*/
          case 1: // $START ::= JUNOSCONFIG EOF 
            {
              Object RESULT =null;
		int start_valleft = ((java_cup.runtime.Symbol)CUP$MJunosParser$stack.elementAt(CUP$MJunosParser$top-1)).left;
		int start_valright = ((java_cup.runtime.Symbol)CUP$MJunosParser$stack.elementAt(CUP$MJunosParser$top-1)).right;
		Object start_val = (Object)((java_cup.runtime.Symbol) CUP$MJunosParser$stack.elementAt(CUP$MJunosParser$top-1)).value;
		RESULT = start_val;
              CUP$MJunosParser$result = parser.getSymbolFactory().newSymbol("$START",0, ((java_cup.runtime.Symbol)CUP$MJunosParser$stack.elementAt(CUP$MJunosParser$top-1)), ((java_cup.runtime.Symbol)CUP$MJunosParser$stack.peek()), RESULT);
            }
          /* ACCEPT */
          CUP$MJunosParser$parser.done_parsing();
          return CUP$MJunosParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 0: // JUNOSCONFIG ::= VERSION 
            {
              Object RESULT =null;
		 RESULT = new Object(); 
              CUP$MJunosParser$result = parser.getSymbolFactory().newSymbol("JUNOSCONFIG",0, ((java_cup.runtime.Symbol)CUP$MJunosParser$stack.peek()), ((java_cup.runtime.Symbol)CUP$MJunosParser$stack.peek()), RESULT);
            }
          return CUP$MJunosParser$result;

          /* . . . . . .*/
          default:
            throw new Exception(
               "Invalid action number found in internal parse table");

        }
    }
}

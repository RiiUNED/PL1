
//----------------------------------------------------
// The following code was generated by CUP v0.11a beta 20060608
// Tue Oct 24 23:11:57 CEST 2023
//----------------------------------------------------

package compiler.syntax;

import java_cup.runtime.Symbol;
import java.util.*;
import es.uned.lsi.compiler.lexical.*;
import es.uned.lsi.compiler.code.*;
import es.uned.lsi.compiler.intermediate.*;
import es.uned.lsi.compiler.semantic.*;
import es.uned.lsi.compiler.semantic.symbol.*;
import es.uned.lsi.compiler.semantic.type.*;
import es.uned.lsi.compiler.syntax.*;
import compiler.CompilerContext;
import compiler.lexical.*;
import compiler.syntax.nonTerminal.*;
import compiler.semantic.symbol.*;
import compiler.semantic.type.*;
import compiler.intermediate.*;
import compiler.code.*;

/** CUP v0.11a beta 20060608 generated parser.
  * @version Tue Oct 24 23:11:57 CEST 2023
  */
public class parser extends java_cup.runtime.lr_parser {

  /** Default constructor. */
  public parser() {super();}

  /** Constructor which sets the default scanner. */
  public parser(java_cup.runtime.Scanner s) {super(s);}

  /** Constructor which sets the default scanner. */
  public parser(java_cup.runtime.Scanner s, java_cup.runtime.SymbolFactory sf) {super(s,sf);}

  /** Production table. */
  protected static final short _production_table[][] = 
    unpackFromStrings(new String[] {
    "\000\004\000\002\002\004\000\002\004\002\000\002\002" +
    "\004\000\002\003\002" });

  /** Access to production table. */
  public short[][] production_table() {return _production_table;}

  /** Parse-action table. */
  protected static final short[][] _action_table = 
    unpackFromStrings(new String[] {
    "\000\005\000\004\002\000\001\002\000\004\002\007\001" +
    "\002\000\004\002\ufffe\001\002\000\004\002\uffff\001\002" +
    "\000\004\002\001\001\002" });

  /** Access to parse-action table. */
  public short[][] action_table() {return _action_table;}

  /** <code>reduce_goto</code> table. */
  protected static final short[][] _reduce_table = 
    unpackFromStrings(new String[] {
    "\000\005\000\006\002\003\004\004\001\001\000\002\001" +
    "\001\000\004\003\005\001\001\000\002\001\001\000\002" +
    "\001\001" });

  /** Access to <code>reduce_goto</code> table. */
  public short[][] reduce_table() {return _reduce_table;}

  /** Instance of action encapsulation class. */
  protected CUP$parser$actions action_obj;

  /** Action encapsulation object initializer. */
  protected void init_actions()
    {
      action_obj = new CUP$parser$actions(this);
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
    return action_obj.CUP$parser$do_action(act_num, parser, stack, top);
  }

  /** Indicates start state. */
  public int start_state() {return 0;}
  /** Indicates start production. */
  public int start_production() {return 0;}

  /** <code>EOF</code> Symbol index. */
  public int EOF_sym() {return 0;}

  /** <code>error</code> Symbol index. */
  public int error_sym() {return 1;}



	SyntaxErrorManager syntaxErrorManager = CompilerContext.getSyntaxErrorManager();
	
	public void syntax_error(Symbol symbol)
	{ 
	    Token token = (Token) symbol.value;
	    syntaxErrorManager.syntaxError ("Error sintactico", token);	    
	}
		
	public void unrecovered_syntax_error(java_cup.runtime.Symbol symbol)
	{	
	    Token token = (Token) symbol.value;
	    syntaxErrorManager.syntaxFatalError ("Error fatal", token);
	}

}

/** Cup generated class to encapsulate user supplied action code.*/
class CUP$parser$actions {


	
	SyntaxErrorManager   syntaxErrorManager   = CompilerContext.getSyntaxErrorManager();
	SemanticErrorManager semanticErrorManager = CompilerContext.getSemanticErrorManager ();
	ScopeManagerIF       scopeManager         = CompilerContext.getScopeManager ();
	FinalCodeFactoryIF   finalCodeFactory     = CompilerContext.getFinalCodeFactory ();


  private final parser parser;

  /** Constructor */
  CUP$parser$actions(parser parser) {
    this.parser = parser;
  }

  /** Method with the actual generated action code. */
  public final java_cup.runtime.Symbol CUP$parser$do_action(
    int                        CUP$parser$act_num,
    java_cup.runtime.lr_parser CUP$parser$parser,
    java.util.Stack            CUP$parser$stack,
    int                        CUP$parser$top)
    throws java.lang.Exception
    {
      /* Symbol object for return from actions */
      java_cup.runtime.Symbol CUP$parser$result;

      /* select the action based on the action number */
      switch (CUP$parser$act_num)
        {
          /*. . . . . . . . . . . . . . . . . . . .*/
          case 3: // axiom ::= 
            {
              Axiom RESULT =null;

              CUP$parser$result = parser.getSymbolFactory().newSymbol("axiom",1, ((java_cup.runtime.Symbol)CUP$parser$stack.peek()), ((java_cup.runtime.Symbol)CUP$parser$stack.peek()), RESULT);
            }
          return CUP$parser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 2: // program ::= NT$0 axiom 
            {
              Object RESULT =null;
              // propagate RESULT from NT$0
                RESULT = (Object) ((java_cup.runtime.Symbol) CUP$parser$stack.elementAt(CUP$parser$top-1)).value;
		int axleft = ((java_cup.runtime.Symbol)CUP$parser$stack.peek()).left;
		int axright = ((java_cup.runtime.Symbol)CUP$parser$stack.peek()).right;
		Axiom ax = (Axiom)((java_cup.runtime.Symbol) CUP$parser$stack.peek()).value;
		   		
  		
  		syntaxErrorManager.syntaxInfo ("Parsing process ended.");
   
              CUP$parser$result = parser.getSymbolFactory().newSymbol("program",0, ((java_cup.runtime.Symbol)CUP$parser$stack.elementAt(CUP$parser$top-1)), ((java_cup.runtime.Symbol)CUP$parser$stack.peek()), RESULT);
            }
          return CUP$parser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 1: // NT$0 ::= 
            {
              Object RESULT =null;
 
        syntaxErrorManager.syntaxInfo ("Starting parsing..."); 
   
              CUP$parser$result = parser.getSymbolFactory().newSymbol("NT$0",2, ((java_cup.runtime.Symbol)CUP$parser$stack.peek()), ((java_cup.runtime.Symbol)CUP$parser$stack.peek()), RESULT);
            }
          return CUP$parser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 0: // $START ::= program EOF 
            {
              Object RESULT =null;
		int start_valleft = ((java_cup.runtime.Symbol)CUP$parser$stack.elementAt(CUP$parser$top-1)).left;
		int start_valright = ((java_cup.runtime.Symbol)CUP$parser$stack.elementAt(CUP$parser$top-1)).right;
		Object start_val = (Object)((java_cup.runtime.Symbol) CUP$parser$stack.elementAt(CUP$parser$top-1)).value;
		RESULT = start_val;
              CUP$parser$result = parser.getSymbolFactory().newSymbol("$START",0, ((java_cup.runtime.Symbol)CUP$parser$stack.elementAt(CUP$parser$top-1)), ((java_cup.runtime.Symbol)CUP$parser$stack.peek()), RESULT);
            }
          /* ACCEPT */
          CUP$parser$parser.done_parsing();
          return CUP$parser$result;

          /* . . . . . .*/
          default:
            throw new Exception(
               "Invalid action number found in internal parse table");

        }
    }
}


package compiler.lexical;

import compiler.syntax.sym;
import compiler.lexical.Token;
import es.uned.lsi.compiler.lexical.ScannerIF;
import es.uned.lsi.compiler.lexical.LexicalError;
import es.uned.lsi.compiler.lexical.LexicalErrorManager;

// incluir aqui, si es necesario otras importaciones

%%
 
%public
%class Scanner
%char
%line
%column
%cup
%unicode


%implements ScannerIF
%scanerror LexicalError

// incluir aqui, si es necesario otras directivas

%{
  LexicalErrorManager lexicalErrorManager = new LexicalErrorManager ();
  private int commentCount = 0;
  
  public Token nuevoToken(int tipo){
  	Token token = new Token(tipo);
  	token.setLine(yyline + 1);
  	token.setColumn(yycolumn + 1);
  	token.setLexema(yytext());
  	return token;
  }
  
  public void nuevoError(String msg){
  	LexicalError error = new LexicalError ();
    error.setLine (yyline + 1);
    error.setColumn (yycolumn + 1);
    error.setLexema (yytext ());
    lexicalErrorManager.lexicalError (error);
    lexicalErrorManager.lexicalDebug (msg);
  }
%}  
  

ESPACIO_BLANCO=[ \t\r\n\f]
fin = "fin"{ESPACIO_BLANCO}
SALTO_LINEA = [\r\n]
LETTER = [A-Za-z]
DIGIT = [0-9]
NUMBER = {DIGIT}|{DIGIT}{DIGIT}*
IDENTIFIER = {LETTER}({LETTER}|{DIGIT})*
COMMENT = "--" ~{SALTO_LINEA}
STRING = "\"" ~"\""
BAD_IDENTIFIER = {NUMBER}{LETTER}|{NUMBER}{LETTER}{LETTER}*

%%

<YYINITIAL> 
{
           			       
    "+"                {  
                           Token token = new Token (sym.PLUS);
                           token.setLine (yyline + 1);
                           token.setColumn (yycolumn + 1);
                           token.setLexema (yytext ());
           			       return token;
                        }
    
    // incluir aqui el resto de las reglas patron - accion

	//Palabras reservadas
	"and" { return nuevoToken(sym.AND); }
	"begin" { return nuevoToken(sym.BEGIN); }
	"Boolean" { return nuevoToken(sym.BOOLEAN); }
	"constant" { return nuevoToken(sym.CONSTANT); }
	"else" { return nuevoToken(sym.ELSE); }
	"end" { return nuevoToken(sym.END); }
	"False" { return nuevoToken(sym.FALSE); }
	"function" { return nuevoToken(sym.FUNCTION); }
	"if" { return nuevoToken(sym.IF); }
	"Integer" { return nuevoToken(sym.INTEGER); }
	"is" { return nuevoToken(sym.IS); }
	"loop" { return nuevoToken(sym.LOOP); }
	"out" { return nuevoToken(sym.OUT); }
	"procedure" { return nuevoToken(sym.PROCEDURE); }
	"Put_line" { return nuevoToken(sym.PUT_LINE); }
	"record" { return nuevoToken(sym.RECORD); }
	"return" { return nuevoToken(sym.RETURN); }
	"then" { return nuevoToken(sym.THEN); }
	"True" { return nuevoToken(sym.TRUE); }
	"type" { return nuevoToken(sym.TYPE); }
	"while" { return nuevoToken(sym.WHILE); }
	// Fin palabras reservadas
	
	//Delimitadores
	"(" { return nuevoToken(sym.RBRACKET); }
	")" { return nuevoToken(sym.LBRACKET); }
	"," { return nuevoToken(sym.COMMA); }
	";" { return nuevoToken(sym.SEMICOLON); }
	":" { return nuevoToken(sym.COLON); }
	//Fin Delimitadores
	
	//Operadores
	"*" { return nuevoToken(sym.MULTIPLICATION); }
	"-" { return nuevoToken(sym.MINUS); }
	">" { return nuevoToken(sym.GT); }
	"/=" { return nuevoToken(sym.NEQ); }
	":=" { return nuevoToken(sym.ASSIGNMENT); }
	"." { return nuevoToken(sym.DOT); }
	//Fin Operadores

	//ExReg
   {ESPACIO_BLANCO}	{}
   {NUMBER} { return nuevoToken(sym.NUMBER); }
   {IDENTIFIER} { return nuevoToken(sym.IDENTIFIER); }
   {COMMENT} {}
   {STRING} { return nuevoToken(sym.STRING); }
	
{fin} {}

	//Fin ExReg
    
    // error en caso de coincidir con ningun patron
	[^] {nuevoError ("Error sin identificar");}
    {BAD_IDENTIFIER} {nuevoError("Identificador invalido");}
}


                         



/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       parser1                                                            */
/*                                                                          */
/*                                                                          */
/*       Group Members:          ID number                                  */
/*                                                                          */
/*           Ji Won Min           14201895                                  */                                
/*                                                                          */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       parser1                                                            */
/*                                                                          */
/*       Pure parser that includes full syntax error detection for the CPL  */
/*       language. It accepts all syntactically valid CPL programs by       */
/*       printing "VALID". On a syntax error, it uses the crash and burn    */
/*       method, where it stops and prints an error message.                */
/*                                                                          */
/*       To run, this requires <inputfile> <listfile>                       */
/*       in the command line.                                               */
/*                                                                          */
/*                                                                          */
/*--------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "global.h"
#include "scanner.h"
#include "line.h"


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Global variables used by this parser.                                   */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE FILE *InputFile;           /*  CPL source comes from here.          */
PRIVATE FILE *ListFile;            /*  For nicely-formatted syntax errors.  */

PRIVATE TOKEN  CurrentToken;       /*  Parser lookahead token.  Updated by  */
                                   /*  routine Accept (below).  Must be     */
                                   /*  initialised before parser starts.    */


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Function prototypes                                                     */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE int  OpenFiles(int argc, char *argv[]);

PRIVATE void ParseProgram(void);
PRIVATE void ParseDeclarations(void);
PRIVATE void ParseProcDeclaration(void);
PRIVATE void ParseBlock(void);
PRIVATE void ParseParameterList(void);
PRIVATE void ParseFormalParameter(void);
PRIVATE void ParseStatement(void); 
PRIVATE void ParseSimpleStatement(void);
PRIVATE void ParseWhileStatement(void);
PRIVATE void ParseIfStatement(void);
PRIVATE void ParseReadStatement(void);
PRIVATE void ParseWriteStatement(void);
PRIVATE void ParseRestOfStatement(void);
PRIVATE void ParseBooleanExpression(void);
PRIVATE void ParseProcCallList(void);
PRIVATE void ParseAssignment(void);
PRIVATE void ParseExpression(void);
PRIVATE void ParseRelOp(void);
PRIVATE void ParseActualParameter(void);
PRIVATE void ParseCompoundTerm(void);
PRIVATE void ParseAddOp(void);
PRIVATE void ParseMultOp(void);
PRIVATE void ParseTerm(void);
PRIVATE void ParseSubTerm(void);

PRIVATE void Accept(int code);
PRIVATE void ReadToEndOfFile(void);


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Main: Entry point. Sets up parser globals (opens input and output files */
/*         and sets for error recovery, initialises current lookahead),     */
/*        then calls "ParseProgram" to start the parse.                     */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PUBLIC int main ( int argc, char *argv[] )
{
    if ( OpenFiles( argc, argv ) )  {
        InitCharProcessor( InputFile, ListFile);
        CurrentToken = GetToken();
        ParseProgram();
        fclose( InputFile);
        fclose( ListFile);
        printf("VALID\n");
        return  EXIT_SUCCESS;
    }
    else 
        return EXIT_FAILURE;
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseProgram implements:                                                */
/*                                                                          */
/*       <Program>     :== "BEGIN" { <Statement> ";" } "END" "."            */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseProgram(void) {
    Accept(PROGRAM);
    Accept(IDENTIFIER);
    Accept(SEMICOLON);
    if(CurrentToken.code == VAR) {
        ParseDeclarations();
    }
    while (CurrentToken.code == PROCEDURE) {
        ParseProcDeclaration();
    }
    ParseBlock();
    Accept(ENDOFPROGRAM);     /* Token "." has name ENDOFPROGRAM          */
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseDeclarations implements:                                           */
/*                                                                          */
/*       <Declarations>     :==  “VAR” <Variable> { “,” <Variable> } “;”    */
/*                                                                          */
/*                                                                          */
/*    Inputs:       1) Integer acting as boolean to differentiate between   */
/*                    local/global variables                                */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      1) Number of declarations for INC instruction           */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseDeclarations(void) {
    Accept(VAR);
    Accept(IDENTIFIER);
    while(CurrentToken.code != SEMICOLON) {
        Accept(COMMA);
        Accept(IDENTIFIER);
    }
    Accept(SEMICOLON);
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseProcDeclarations implements:                                       */
/*                                                                          */
/* <ProcDeclarations>  :== “PROCEDURE” <Identifier> [ <ParameterList> ] “;” */
/*                    [ <Declarations> ] { <ProcDeclaration> } <Block> “;”  */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                  Synchronisation.                                        */
/*                  BackPatching.                                           */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseProcDeclaration(void) {
    Accept(PROCEDURE);
    Accept(IDENTIFIER);
    if(CurrentToken.code == LEFTPARENTHESIS) {
        ParseParameterList();
    }
    Accept(SEMICOLON);
    if(CurrentToken.code == VAR) {
        ParseDeclarations();
    }
    while (CurrentToken.code == PROCEDURE) {
        ParseProcDeclaration();
    }
    ParseBlock();
    Accept(SEMICOLON);
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseBlock implements:                                                  */
/*                                                                          */
/*       <Block>    :==     “BEGIN” { <Statement> “;” } “END”               */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseBlock(void){
    Accept(BEGIN);
    while(CurrentToken.code != END) {
        ParseStatement();
        Accept(SEMICOLON);
    }
    Accept(END);
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseParameterList implements:                                          */
/*                                                                          */
/* <ParameterList> :== “(” <FormalParameter> { “,” <FormalParameter> } “)”  */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseParameterList(void) {
    Accept(LEFTPARENTHESIS);
    ParseFormalParameter();
    while(CurrentToken.code != RIGHTPARENTHESIS) {
        Accept(COMMA);
        ParseFormalParameter();
    }
    Accept(RIGHTPARENTHESIS);
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseFormalParameter implements:                                        */
/*                                                                          */
/*    <FormalParameter>     :==      [ “REF” ] <Variable>                   */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Pointer of the symbol created in symbol table           */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseFormalParameter(void) {
    if(CurrentToken.code == REF) {
        Accept(REF);
    }
    Accept(IDENTIFIER);
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseStatement implements:                                              */
/*                                                                          */
/*  <Statement>  :== <SimpleStatement> | <WhileStatement> | <IfStatement> | */
/*                   <ReadStatement> | <WriteStatement>                     */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseStatement(void) {
    switch(CurrentToken.code){
        case IDENTIFIER:
        default:
            ParseSimpleStatement();
            break;
        case WHILE:
            ParseWhileStatement();
            break;
        case IF:
            ParseIfStatement();
            break;
        case READ:
            ParseReadStatement();
            break;
        case WRITE:
            ParseWriteStatement();
            break;
    }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseSimpleStatement implements:                                        */
/*                                                                          */
/*    <SimpleStatement>     :==      <Variable> <RestOfStatement>           */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseSimpleStatement(void) {
    Accept(IDENTIFIER);
    ParseRestOfStatement(); 
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseWhileStatement implements:                                         */
/*                                                                          */
/*  <WhileStatement>     :==      “WHILE” <BooleanExpression> “DO” <Block>  */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseWhileStatement(void) {
    Accept(WHILE);
    ParseBooleanExpression();
    Accept(DO);
    ParseBlock();
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseIfStatement implements:                                            */
/*                                                                          */
/*  <IfStatement> :== “IF”<BooleanExpression>“THEN”<Block>[“ELSE”<Block>}   */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseIfStatement(void) {
    Accept(IF);
    ParseBooleanExpression(); 
    Accept(THEN);
    ParseBlock();
    if(CurrentToken.code == ELSE) {
        Accept(ELSE);
        ParseBlock();
    }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseReadStatement implements:                                          */
/*                                                                          */
/*    <ReadStatement>     :==     “READ” <ProcCallList>                     */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseReadStatement(void) {
    Accept(READ);
    ParseProcCallList();
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseWriteStatement implements:                                         */
/*                                                                          */
/*    <WriteStatment>     :==     “WRITE” <ProcCallList>                    */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseWriteStatement(void) {
    Accept(WRITE);
    ParseProcCallList();

}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseRestOfStatement implements:                                        */
/*                                                                          */
/*    <RestOfStatement>     :==    <ProcCallList> | <Assignment> | ε        */
/*                                                                          */
/*                                                                          */
/*    Inputs:       Symbol pointer that is the result of LookupSymbol       */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseRestOfStatement(void) {
    if(CurrentToken.code == LEFTPARENTHESIS) {
        ParseProcCallList();
    }
    else if(CurrentToken.code == ASSIGNMENT) {
        ParseAssignment();
    }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseBooleanExpression implements:                                      */
/*                                                                          */
/*    <BooleanExpression>  :==    <Expression> <RelOp> <Expression>         */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      The forward addressing location that later needs to     */
/*                  be backpached.                                          */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseBooleanExpression(void) {
    ParseExpression();
    ParseRelOp();
    ParseExpression();
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseProcCallList implements:                                           */
/*                                                                          */
/* <ProcCallList>  :== “(” <ActualParameter> { “,” <ActualParameter> } “)”  */
/*                                                                          */
/*                                                                          */
/*    Inputs:       The instruction to differentiate READ, WRITE, and       */
/*                  a procedure call.                                       */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseProcCallList(void) {
    Accept(LEFTPARENTHESIS);
    ParseActualParameter();
    while(CurrentToken.code == COMMA) {
        Accept(COMMA);
        ParseActualParameter();
    }
    Accept(RIGHTPARENTHESIS);
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseAssignment implements:                                             */
/*                                                                          */
/*    <Assignment>     :==    “:=” <Expression>                             */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseAssignment(void) {
    Accept(ASSIGNMENT);
    ParseExpression();
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseExpression implements:                                             */
/*                                                                          */
/*    <Expression>     :==    <CompoundTerm> { <AddOp> <CompoundTerm> }     */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseExpression(void) {
    ParseCompoundTerm();
    while(CurrentToken.code == ADD || CurrentToken.code == SUBTRACT) {
        ParseAddOp();
        ParseCompoundTerm();
    }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseRelOp implements:                                                  */
/*                                                                          */
/*    <RelOp>     :==    “=” | “<=” | “>=” | “<” | “>”                      */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseRelOp(void) {
    switch(CurrentToken.code) {
        case EQUALITY:
        default:
            Accept(EQUALITY);
            break;
        case LESSEQUAL:
            Accept(LESSEQUAL);
            break;
        case GREATEREQUAL:
            Accept(GREATEREQUAL);
            break;
        case LESS:
            Accept(LESS);
            break;
        case GREATER:
            Accept(GREATER);
            break;
    }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseActualParameter implements:                                        */
/*                                                                          */
/*    <ActualParameter>     :==   <Variable> | <Expression>                 */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: None                                                    */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseActualParameter(void) {
    ParseExpression(); /* a single variable is also an expression */
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseCompoundTerm implements:                                           */
/*                                                                          */
/*    <CompoundTerm>     :==   <Term> { <MultOp> <Term> }                   */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseCompoundTerm(void) {
    ParseTerm();
    while(CurrentToken.code == MULTIPLY || CurrentToken.code == DIVIDE) {
        ParseMultOp();
        ParseTerm();
    }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseAddOp implements:                                                  */
/*                                                                          */
/*    <AddOp>     :==    “+” | “−”                                          */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseAddOp(void) {
    if(CurrentToken.code == ADD) {
        Accept(ADD);
    }
    else{
        Accept(SUBTRACT);
    }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseMultOp implements:                                                 */
/*                                                                          */
/*    <MultOp>     :==    “∗” | “/”                                         */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseMultOp(void) {
    if(CurrentToken.code == MULTIPLY) {
        Accept(MULTIPLY);
    }
    else{
        Accept(DIVIDE);
    }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseTerm implements:                                                   */
/*                                                                          */
/*    <Term>     :==    [ “−” ] <SubTerm>                                   */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseTerm(void) {
    if(CurrentToken.code == SUBTRACT) {
        Accept(SUBTRACT);
    }
    ParseSubTerm();
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseSubTerm implements:                                                */
/*                                                                          */
/*    <SubTerm>     :==    <Variable> | <IntConst> | “(” <Expression> “)”   */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseSubTerm(void) {
    if(CurrentToken.code == IDENTIFIER) {
        Accept(IDENTIFIER);
    }
    else if(CurrentToken.code == INTCONST) {
        Accept(INTCONST);
    }
    else{
        Accept(LEFTPARENTHESIS);
        ParseExpression();
        Accept(RIGHTPARENTHESIS);
    }
}




/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  End of parser.  Support routines follow.                                */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Accept:  Takes an expected token name as argument, and if the current   */
/*           lookahead matches this, advances the lookahead and returns.    */
/*                                                                          */
/*           If the expected token fails to match the current lookahead,    */
/*           this routine reports a syntax error and exits ("crash & burn"  */
/*           parsing).  Note the use of routine "SyntaxError"               */
/*           (from "scanner.h") which puts the error message on the         */
/*           standard output and on the listing file, and the helper        */
/*           "ReadToEndOfFile" which just ensures that the listing file is  */
/*           completely generated.                                          */
/*                                                                          */
/*                                                                          */
/*    Inputs:       Integer code of expected token                          */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: If successful, advances the current lookahead token     */
/*                  "CurrentToken".                                         */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void Accept(int ExpectedToken )
{
    if ( CurrentToken.code != ExpectedToken )  {
        SyntaxError( ExpectedToken, CurrentToken);
        ReadToEndOfFile();
        fclose( InputFile);
        fclose( ListFile);
        exit( EXIT_FAILURE);
    }
    else  CurrentToken = GetToken();
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  OpenFiles:  Reads strings from the command-line and opens the           */
/*              associated input and listing files.                         */
/*                                                                          */
/*    Note that this routine mmodifies the globals "InputFile" and          */
/*    "ListingFile".  It returns 1 ("true" in C-speak) if the input and     */
/*    listing files are successfully opened, 0 if not, allowing the caller  */
/*    to make a graceful exit if the opening process failed.                */
/*                                                                          */
/*                                                                          */
/*    Inputs:       1) Integer argument count (standard C "argc").          */
/*                  2) Array of pointers to C-strings containing arguments  */
/*                  (standard C "argv").                                    */
/*                                                                          */
/*    Outputs:      No direct outputs, but note side effects.               */
/*                                                                          */
/*    Returns:      Boolean success flag (i.e., an "int":  1 or 0)          */
/*                                                                          */
/*    Side Effects: If successful, modifies globals "InputFile" and         */
/*                  "ListingFile".                                          */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE int  OpenFiles( int argc, char *argv[] )
{

    if ( argc != 3 )  {
        fprintf( stderr, "%s <inputfile> <listfile>\n", argv[0]);
        return 0;
    }

    if ( NULL == ( InputFile = fopen( argv[1], "r" ) ) )  {
        fprintf( stderr, "cannot open \"%s\" for input\n", argv[1]);
        return 0;
    }

    if ( NULL == ( ListFile = fopen( argv[2], "w" ) ) )  {
        fprintf( stderr, "cannot open \"%s\" for output\n", argv[2]);
        fclose( InputFile);
        return 0;
    }

    return 1;
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ReadToEndOfFile:  Reads all remaining tokens from the input file.       */
/*              associated input and listing files.                         */
/*                                                                          */
/*    This is used to ensure that the listing file refects the entire       */
/*    input, even after a syntax error (because of crash & burn parsing,    */
/*    if a routine like this is not used, the listing file will not be      */
/*    complete.  Note that this routine also reports in the listing file    */
/*    exactly where the parsing stopped.  Note that this routine is         */
/*    superfluous in a parser that performs error-recovery.                 */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Reads all remaining tokens from the input.  There won't */
/*                  be any more available input after this routine returns. */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ReadToEndOfFile( void )
{
    if ( CurrentToken.code != ENDOFINPUT )  {
        Error( "Parsing ends here in this program\n", CurrentToken.pos);
        while ( CurrentToken.code != ENDOFINPUT )  CurrentToken = GetToken();
    }
}

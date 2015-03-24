/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       parser1                                                            */
/*                                                                          */
/*                                                                          */
/*       Group Members:          ID number                                  */
/*                                                                          */
/*           Ji Won Min           14201895                                  */                                
/*                                                                          */
/*       Currently just a copy of "smallparser.c".  To create "parser1.c",  */
/*       modify this source to reflect the CPL grammar.                     */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       smallparser                                                        */
/*                                                                          */
/*       An illustration of the use of the character handler and scanner    */
/*       in a parser for the language                                       */
/*                                                                          */
/*       <Program>     :== "BEGIN" { <Statement> ";" } "END" "."            */
/*       <Statement>   :== <Identifier> ":=" <Expression>                   */
/*       <Expression>  :== <Identifier> | <IntConst>                        */
/*                                                                          */
/*                                                                          */
/*       Note - <Identifier> and <IntConst> are provided by the scanner     */
/*       as tokens IDENTIFIER and INTCONST respectively.                    */
/*                                                                          */
/*       Although the listing file generator has to be initialised in       */
/*       this program, full listing files cannot be generated in the        */
/*       presence of errors because of the "crash and burn" error-          */
/*       handling policy adopted. Only the first error is reported, the     */
/*       remainder of the input is simply copied to the output (using       */
/*       the routine "ReadToEndOfFile") without further comment.            */
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
/*  Main: Smallparser entry point.  Sets up parser globals (opens input and */
/*        output files, initialises current lookahead), then calls          */
/*        "ParseProgram" to start the parse.                                */
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



PRIVATE void ParseDeclarations(void) {
    Accept(VAR);
    Accept(IDENTIFIER);
    while(CurrentToken.code != SEMICOLON) {
        Accept(COMMA);
        Accept(IDENTIFIER);
    }
    Accept(SEMICOLON);
}



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

PRIVATE void ParseBlock(void){
    Accept(BEGIN);
    while(CurrentToken.code != END) {
        ParseStatement();
        Accept(SEMICOLON);
    }
    Accept(END);
}

PRIVATE void ParseParameterList(void) {
    Accept(LEFTPARENTHESIS);
    ParseFormalParameter();
    while(CurrentToken.code != RIGHTPARENTHESIS) {
        Accept(COMMA);
        ParseFormalParameter();
    }
    Accept(RIGHTPARENTHESIS);
}

PRIVATE void ParseFormalParameter(void) {
    if(CurrentToken.code == REF) {
        Accept(REF);
    }
    Accept(IDENTIFIER);
}

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

PRIVATE void ParseSimpleStatement(void) {
    Accept(IDENTIFIER);
    ParseRestOfStatement(); 
}

PRIVATE void ParseWhileStatement(void) {
    Accept(WHILE);
    ParseBooleanExpression();
    Accept(DO);
    ParseBlock();
}

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

PRIVATE void ParseReadStatement(void) {
    Accept(READ);
    ParseProcCallList();
}

PRIVATE void ParseWriteStatement(void) {
    Accept(WRITE);
    ParseProcCallList();

}

PRIVATE void ParseRestOfStatement(void) {
    if(CurrentToken.code == LEFTPARENTHESIS) {
        ParseProcCallList();
    }
    else if(CurrentToken.code == ASSIGNMENT) {
        ParseAssignment();
    }
    /*do nothing for epsilon*/
}

PRIVATE void ParseBooleanExpression(void) {
    ParseExpression();
    ParseRelOp();
    ParseExpression();
}

PRIVATE void ParseProcCallList(void) {
    Accept(LEFTPARENTHESIS);
    ParseActualParameter();
    while(CurrentToken.code == COMMA) {
        Accept(COMMA);
        ParseActualParameter();
    }
    Accept(RIGHTPARENTHESIS);
}

PRIVATE void ParseAssignment(void) {
    Accept(ASSIGNMENT);
    ParseExpression();
}

PRIVATE void ParseExpression(void) {
    ParseCompoundTerm();
    while(CurrentToken.code == ADD || CurrentToken.code == SUBTRACT) {
        ParseAddOp();
        ParseCompoundTerm();
    }
}

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
        /*default? don't know how to handle it. current one should handle error ok.*/
    }
}

PRIVATE void ParseActualParameter(void) {
    /*this is wrong: can't differentiate between x and x-1 in params*/
    /*
    if(CurrentToken.code != IDENTIFIER) {
        ParseExpression(); 
    }
    else{
        ParseExpression();
    }
    */
    ParseExpression();
}

PRIVATE void ParseCompoundTerm(void) {
    ParseTerm();
    while(CurrentToken.code == MULTIPLY || CurrentToken.code == DIVIDE) {
        ParseMultOp();
        ParseTerm();
    }
}

PRIVATE void ParseAddOp(void) {
    if(CurrentToken.code == ADD) {
        Accept(ADD);
    }
    else{
        Accept(SUBTRACT);
    }
}

PRIVATE void ParseMultOp(void) {
    if(CurrentToken.code == MULTIPLY) {
        Accept(MULTIPLY);
    }
    else{
        Accept(DIVIDE);
    }
}

PRIVATE void ParseTerm(void) {
    if(CurrentToken.code == SUBTRACT) {
        Accept(SUBTRACT);
    }
    ParseSubTerm();
}

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

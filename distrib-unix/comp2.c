/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       comp2.c                                                            */
/*                                                                          */
/*                                                                          */
/*       Group Members:          ID number                                  */
/*                                                                          */
/*           Ji Won Min          14201895                                   */                                
/*                                                                          */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       comp2                                                              */
/*                                                                          */
/*       Full compiler that performs syntax and semantic error detection    */
/*       along with code generation for all parts of the CPL language       */
/*       including procedure definitions with parameters.                   */
/*                                                                          */
/*       An extention of comp1.c, which has built upon parser1.c and        */
/*       parser2.c.                                                         */
/*                                                                          */
/*       To run, this requires <inputfile> <listfile> <outputfile>          */
/*       in the command line.                                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "global.h"
#include "scanner.h"
#include "line.h"
#include "sets.h"
#include "symbol.h"
#include "strtab.h"
#include "code.h"


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Global variables used by this parser.                                   */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE FILE *InputFile;           /*  CPL source comes from here.          */
PRIVATE FILE *ListFile;            /*  For nicely-formatted syntax errors.  */
PRIVATE FILE *CodeFile;

PRIVATE TOKEN  CurrentToken;       /*  Parser lookahead token.  Updated by  */
                                   /*  routine Accept (below).  Must be     */
                                   /*  initialised before parser starts.    */

PRIVATE SET StatementFS;
PRIVATE SET StatementFS_aug;
PRIVATE SET StatementFBS;
PRIVATE SET DeclarationsFS;
PRIVATE SET DeclarationsFBS;
PRIVATE SET ProcDeclarationsFS;
PRIVATE SET ProcDeclarationsFBS;

PRIVATE int scope;
PRIVATE int varaddress;

PRIVATE int ReadFlag;              /*  For read instruction to emit correct */
PRIVATE SYMBOL * ReadStore;        /*  store operation at ProcCallList.     */


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Function prototypes                                                     */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE int  OpenFiles(int argc, char *argv[]);

PRIVATE void ParseProgram(void);
PRIVATE int ParseDeclarations(int localFlag);
PRIVATE void ParseProcDeclaration(void);
PRIVATE void ParseBlock(void);
PRIVATE void ParseParameterList(void);
PRIVATE SYMBOL * ParseFormalParameter(void);
PRIVATE void ParseStatement(void); 
PRIVATE void ParseSimpleStatement(void);
PRIVATE void ParseWhileStatement(void);
PRIVATE void ParseIfStatement(void);
PRIVATE void ParseReadStatement(void);
PRIVATE void ParseWriteStatement(void);
PRIVATE void ParseRestOfStatement( SYMBOL *target );
PRIVATE int  ParseBooleanExpression(void);
PRIVATE void ParseProcCallList(int instr);
PRIVATE void ParseAssignment(void);
PRIVATE void ParseExpression(void);
PRIVATE int  ParseRelOp(void);
PRIVATE void ParseActualParameter(void);
PRIVATE void ParseCompoundTerm(void);
PRIVATE void ParseAddOp(void);
PRIVATE void ParseMultOp(void);
PRIVATE void ParseTerm(void);
PRIVATE void ParseSubTerm(void);

PRIVATE void Synchronise( SET *F, SET *FB );
PRIVATE void SetupSets(void);

PRIVATE void Accept(int code);

PRIVATE SYMBOL * MakeSymbolTableEntry( int symtype, int *varaddress);
PRIVATE SYMBOL *LookupSymbol( void );


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

        ReadFlag = 0;
        ReadStore = NULL;
        
        InitCharProcessor( InputFile, ListFile);
        InitCodeGenerator(CodeFile);
        SetupSets();
        CurrentToken = GetToken();
        scope = 1;
        varaddress = 0;
        ParseProgram();
        WriteCodeFile();
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
/*                  Synchronisation.                                        */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseProgram (void) {
    int counter = 0;
    Accept(PROGRAM);
    Accept(IDENTIFIER);
    Accept(SEMICOLON);
    Synchronise(&DeclarationsFS, &DeclarationsFBS);
    if(CurrentToken.code == VAR) {
        /*global varibales*/
        counter = ParseDeclarations(0);
        

    }
    Synchronise(&ProcDeclarationsFS, &ProcDeclarationsFBS);
    while (CurrentToken.code == PROCEDURE) {
        ParseProcDeclaration();
        Synchronise(&ProcDeclarationsFS, &ProcDeclarationsFBS);
    }
    if(counter > 0)
        Emit(I_INC, counter);
    ParseBlock();
    Accept(ENDOFPROGRAM);     /* Token "." has name ENDOFPROGRAM          */
    Accept(ENDOFINPUT); 
    _Emit(I_HALT);
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseDeclarations implements:                                           */
/*                                                                          */
/*       <Program>     :==  “VAR” <Variable> { “,” <Variable> } “;”         */
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

PRIVATE int ParseDeclarations(int localFlag) {
    int counter = 1;
    int localVar = 1;
    Accept(VAR);
    if(localFlag)
        MakeSymbolTableEntry(STYPE_LOCALVAR, &localVar);
    else
        MakeSymbolTableEntry(STYPE_VARIABLE, &varaddress);
    Accept(IDENTIFIER);
    while(CurrentToken.code == COMMA) {
        Accept(COMMA);
        if(localFlag)
            MakeSymbolTableEntry(STYPE_LOCALVAR, &localVar);
        else
            MakeSymbolTableEntry(STYPE_VARIABLE, &varaddress);
        Accept(IDENTIFIER);
        counter++;
    }
    Accept(SEMICOLON);
    return counter;
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseProcDeclarations implements:                                       */
/*                                                                          */
/*       <Program>  :== “PROCEDURE” <Identifier> [ <ParameterList> ] “;”    */
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
    int backpatch_addr, counter, incFlag;
    SYMBOL *procedure; 

    incFlag = 0;
    Accept(PROCEDURE);
    procedure = MakeSymbolTableEntry( STYPE_PROCEDURE, NULL );
    Accept(IDENTIFIER);
    backpatch_addr = CurrentCodeAddress();
    Emit( I_BR, 0 );
    procedure->address = CurrentCodeAddress();
    scope++;
    if(CurrentToken.code == LEFTPARENTHESIS) {
        ParseParameterList();
    }
    Accept(SEMICOLON);
    Synchronise(&DeclarationsFS, &DeclarationsFBS);

    if(CurrentToken.code == VAR) {
        counter = ParseDeclarations(1);
        incFlag = 1;
        Emit(I_INC, counter);

    }
    Synchronise(&ProcDeclarationsFS, &ProcDeclarationsFBS);
    while (CurrentToken.code == PROCEDURE) {
        ParseProcDeclaration();
        Synchronise(&ProcDeclarationsFS, &ProcDeclarationsFBS);
    }
    ParseBlock();
    Accept(SEMICOLON);
    if(incFlag)
        Emit(I_DEC, counter);
    _Emit( I_RET );
    BackPatch( backpatch_addr, CurrentCodeAddress() );
    RemoveSymbols(scope);
    scope--;
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseBlock implements:                                                  */
/*                                                                          */
/*       <Program>  :==     “BEGIN” { <Statement> “;” } “END”               */
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
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseBlock(void){
    Accept(BEGIN);
    Synchronise(&StatementFS_aug, &StatementFBS);
    while(InSet(&StatementFS, CurrentToken.code)) {
        ParseStatement();
        Accept(SEMICOLON);
        Synchronise(&StatementFS_aug, &StatementFBS);
    }
    Accept(END);
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseParameterList implements:                                          */
/*                                                                          */
/*    <Program> :== “(” <FormalParameter> { “,” <FormalParameter> } “)”     */
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
    int counter, i;
    SYMBOL * pointerArray[20]; /*buffer of 10*/
    counter = 0;
    Accept(LEFTPARENTHESIS);
    pointerArray[counter] = ParseFormalParameter();

    while(CurrentToken.code != RIGHTPARENTHESIS) {
        Accept(COMMA);
        counter++;
        if(counter < 20)
            pointerArray[counter] = ParseFormalParameter();
    }
    
    /*assign addresses to formal parameters*/
    /*done outside of MakeSymbolTableEntry because of unknown counter*/
    for(i=0; i<=counter; ++i){
        pointerArray[i]->address=(i-(counter+1));
    }
    Accept(RIGHTPARENTHESIS);
    
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseFormalParameter implements:                                        */
/*                                                                          */
/*    <Program>     :==      [ “REF” ] <Variable>                           */
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

PRIVATE SYMBOL * ParseFormalParameter(void) {
    int type = STYPE_VALUEPAR;
    SYMBOL * temp;
    if(CurrentToken.code == REF) {
        Accept(REF);
        type = STYPE_REFPAR;
    }
    temp = MakeSymbolTableEntry(type, NULL);
    Accept(IDENTIFIER);
    return temp;


}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseStatement implements:                                              */
/*                                                                          */
/*    <Program>  :== <SimpleStatement> | <WhileStatement> | <IfStatement> | */
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
/*    <Program>     :==      <Variable> <RestOfStatement>                   */
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
    SYMBOL *target;
    target = LookupSymbol(); /* Look up IDENTIFIER in lookahead. */
    Accept( IDENTIFIER );
    ParseRestOfStatement( target ); 
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseWhileStatement implements:                                         */
/*                                                                          */
/*    <Program>     :==      “WHILE” <BooleanExpression> “DO” <Block>       */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                  Backpatching.                                           */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseWhileStatement(void) {
    int Label1, Label2, L2BackPatchLoc;

    Accept( WHILE );
    Label1 = CurrentCodeAddress( );
    L2BackPatchLoc = ParseBooleanExpression( );
    Accept( DO );
    ParseBlock( );
    Emit( I_BR, Label1 );
    Label2 = CurrentCodeAddress( );
    BackPatch( L2BackPatchLoc, Label2 ); 
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseIfStatement implements:                                            */
/*                                                                          */
/*  <Program> :== “IF” <BooleanExpression> “THEN” <Block> [“ELSE” <Block>}  */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                  Backpatching.                                           */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseIfStatement(void) {
    int Label1, Label2, L1BackPatchLoc, L2BackPatchLoc;

    Accept(IF);
    L1BackPatchLoc = ParseBooleanExpression();
    Accept(THEN);
    ParseBlock();
    
    L2BackPatchLoc = CurrentCodeAddress(); 
    Emit(I_BR, 0);
    
    if(CurrentToken.code == ELSE) {
        Accept(ELSE);
        Label1 = CurrentCodeAddress();
        BackPatch(L1BackPatchLoc, Label1);
        ParseBlock();
    }else{
        Label1 = CurrentCodeAddress();
        BackPatch(L1BackPatchLoc, Label1);
    }
    Label2 = CurrentCodeAddress();
    BackPatch(L2BackPatchLoc, Label2);
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseReadStatement implements:                                          */
/*                                                                          */
/*    <Program>     :==     “READ” <ProcCallList>                           */
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
    ReadFlag = 1;
    ParseProcCallList(I_READ);
    ReadFlag = 0;
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseWriteStatement implements:                                         */
/*                                                                          */
/*    <Program>     :==     “WRITE” <ProcCallList>                          */
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
    ParseProcCallList(I_WRITE);
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseRestOfStatement implements:                                        */
/*                                                                          */
/*    <Program>     :==    <ProcCallList> | <Assignment> | ε                */
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

PRIVATE void ParseRestOfStatement( SYMBOL *target )
{
    switch ( CurrentToken.code ) {
        case LEFTPARENTHESIS:
            ParseProcCallList(I_CALL);
            if(target->type == STYPE_PROCEDURE)
                ParseRestOfStatement(target);
            break; 
        case SEMICOLON:
            if ( target != NULL ){
                if ( target->type == STYPE_PROCEDURE ){
                    
                    _Emit(I_PUSHFP);
                    _Emit(I_BSF);
                    Emit( I_CALL, target->address );
                    _Emit(I_RSF);
                }
                else {
                    Error( "Error in fetching procedure", CurrentToken.pos );
                    KillCodeGeneration();
                }
            }
            break; 
        case ASSIGNMENT:
        default:
            ParseAssignment();
            if ( target != NULL ){
                if ( target->type == STYPE_VARIABLE){
                    Emit( I_STOREA, target->address );
                }
                else if(target->type == STYPE_LOCALVAR || target->type == STYPE_VALUEPAR){
                    Emit( I_STOREFP, target->address);
                }
                else {
                    Error( "Error in fetching assignment", CurrentToken.pos );
                    KillCodeGeneration();
                }
            }
            break;
    }
} 


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseBooleanExpression implements:                                      */
/*                                                                          */
/*    <Program>     :==    <Expression> <RelOp> <Expression>                */
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

PRIVATE int ParseBooleanExpression(void) {
    int BackPatchAddr, RelOpInstruction;
    ParseExpression( ); /* ⟨Expression⟩1 */
    RelOpInstruction = ParseRelOp( );
    ParseExpression( ); /* ⟨Expression⟩2 */
    _Emit( I_SUB );
    BackPatchAddr = CurrentCodeAddress( );
    Emit( RelOpInstruction, 0 );
    return BackPatchAddr; 
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseProcCallList implements:                                           */
/*                                                                          */
/*    <Program>  :== “(” <ActualParameter> { “,” <ActualParameter> } “)”    */
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

PRIVATE void ParseProcCallList(int instr) {
    Accept(LEFTPARENTHESIS);
    ParseActualParameter();
    if(instr == I_READ || instr == I_WRITE)
        _Emit(instr);
    if(instr == I_READ && ReadStore != NULL && ReadFlag){
        Emit(I_STOREA, ReadStore->address);
        ReadStore = NULL;
    }
    while(CurrentToken.code == COMMA) {
        Accept(COMMA);
        ParseActualParameter();
        if(instr == I_READ || instr == I_WRITE)
            _Emit(instr);
        if(instr == I_READ && ReadStore != NULL && ReadFlag){
            Emit(I_STOREA, ReadStore->address);
            ReadStore = NULL;
        }
    }
    Accept(RIGHTPARENTHESIS);
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseAssignment implements:                                             */
/*                                                                          */
/*    <Program>     :==    “:=” <Expression>                                */
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
    Accept( ASSIGNMENT );
    ParseExpression(); 
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseExpression implements:                                             */
/*                                                                          */
/*    <Program>     :==    <CompoundTerm> { <AddOp> <CompoundTerm> }        */
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
    int op;
    ParseCompoundTerm();
    while ( ( op = CurrentToken.code ) == ADD || op == SUBTRACT ) {
        ParseAddOp();
        ParseCompoundTerm();
        if ( op == ADD ) 
            _Emit( I_ADD ); 
        else 
            _Emit( I_SUB );
    }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseRelOp implements:                                                  */
/*                                                                          */
/*    <Program>     :==    “=” | “<=” | “>=” | “<” | “>”                    */
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

PRIVATE int ParseRelOp(void) {
    int RelOpInstruction;
    switch(CurrentToken.code) {
        case EQUALITY:
        default:
            RelOpInstruction = I_BNZ;
            Accept(EQUALITY);
            break;
        case LESSEQUAL:
            RelOpInstruction = I_BG;
            Accept(LESSEQUAL);
            break;
        case GREATEREQUAL:
            RelOpInstruction = I_BL;
            Accept(GREATEREQUAL);
            break;
        case LESS:
            RelOpInstruction = I_BGZ;
            Accept(LESS);
            break;
        case GREATER:
            RelOpInstruction = I_BLZ;
            Accept(GREATER);
            break;
    }
    return RelOpInstruction;
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseActualParameter implements:                                        */
/*                                                                          */
/*    <Program>     :==   <Variable> | <Expression>                         */
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
/*    <Program>     :==   <Term> { <MultOp> <Term> }                        */
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
    int op;
    ParseTerm();
    while ( ( op = CurrentToken.code ) == MULTIPLY || op == DIVIDE ) {
        ParseMultOp();
        ParseTerm();
        if ( op == MULTIPLY ) 
            _Emit( I_MULT ); 
        else 
            _Emit( I_DIV );
    } 
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseAddOp implements:                                                  */
/*                                                                          */
/*    <Program>     :==    “+” | “−”                                        */
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
/*    <Program>     :==    “∗” | “/”                                        */
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
/*    <Program>     :==    [ “−” ] <SubTerm>                                */
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
    int negateflag = 0;
    if ( CurrentToken.code == SUBTRACT ) {
        negateflag = 1;
        Accept( SUBTRACT );
    }
    ParseSubTerm();
    if ( negateflag ) 
        _Emit( I_NEG ); 
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseSubTerm implements:                                                */
/*                                                                          */
/*    <Program>     :==    <Variable> | <IntConst> | “(” <Expression> “)”   */
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
    SYMBOL *var;
    int i, dS;
    switch(CurrentToken.code){
        case IDENTIFIER:
        default:
            var = LookupSymbol();
            if ( var != NULL){
                if(var->type==STYPE_VARIABLE){
                    if(ReadFlag)
                        ReadStore =  var;
                    else
                        Emit( I_LOADA, var->address );
                }
                else if(var->type == STYPE_LOCALVAR || var->type == STYPE_VALUEPAR) {
                    dS = scope - var->scope;
                    if ( dS == 0 ){
                        Emit( I_LOADFP, var->address );
                    }
                    else {
                        _Emit( I_LOADFP );
                        for ( i = 0; i < dS - 1; i++)
                            _Emit( I_LOADSP );
                        Emit( I_LOADSP, var->address ); 
                    }
                }
            }
            else{
                Error( "Identifier not declared", CurrentToken.pos );
                KillCodeGeneration();
            }
            Accept( IDENTIFIER );
            break; 
            
        case INTCONST:
            Emit( I_LOADI, CurrentToken.value );
            Accept( INTCONST );
            break;

        case LEFTPARENTHESIS:
            Accept(LEFTPARENTHESIS);
            ParseExpression();
            Accept(RIGHTPARENTHESIS);
            break;
    }
    
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  End of parser.  Support routines follow.                                */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    Synchronise: For Augmented S-Algol Recovery. Essentially S-Algol with */
/*                 key points at ParseBlock, ParseProcDeclarations and      */
/*                 ParseProgram to use First and Follow sets with beacons   */
/*                 for recovery.                                            */
/*                                                                          */
/*                                                                          */
/*    Inputs:      1) First set of Synchronisation point                    */
/*                 2) Follow sets + beacons of Synchronisation point        */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Lookahead token advanced if not in set.                 */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void Synchronise( SET *F, SET *FB ) {
    SET S;
    S = Union( 2, F, FB );
    if ( !InSet( F, CurrentToken.code ) ) {
        SyntaxError2( *F, CurrentToken );
        while ( !InSet( &S, CurrentToken.code ) )
            CurrentToken = GetToken();
    }
} 


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    SetupSets:   Setup for Augmented S-Algol Recovery of first and follow */
/*                 + beacons sets for each primary error recovery statement.*/
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void SetupSets(void) {

    /* <Block> */
    InitSet(&StatementFS, 5, IDENTIFIER, WHILE, IF, READ, WRITE);
    InitSet(&StatementFS_aug, 6, IDENTIFIER, WHILE, IF, READ, WRITE, END);
    InitSet(&StatementFBS, 4, ENDOFPROGRAM, SEMICOLON, ELSE, ENDOFINPUT);

    /* <Program> */
    InitSet(&DeclarationsFS, 3, VAR, PROCEDURE, BEGIN);
    InitSet(&DeclarationsFBS, 3, ENDOFPROGRAM, ENDOFINPUT, END);

    /* <ProcDeclarations> */
    InitSet(&ProcDeclarationsFS, 2, PROCEDURE, BEGIN);
    InitSet(&ProcDeclarationsFBS, 3, ENDOFINPUT, ENDOFPROGRAM, END);
}


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
    static int recovering = 0;

    if(recovering) {
        while(CurrentToken.code != ExpectedToken &&
            CurrentToken.code != ENDOFINPUT) {
            CurrentToken = GetToken(); 
        }    
        recovering = 0;
    }
    if(CurrentToken.code != ExpectedToken) {
        SyntaxError(ExpectedToken, CurrentToken);
        recovering = 1;
    }
    else {
        CurrentToken = GetToken();
    }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    MakeSymbolTableEntry: Creates an entry in the symbol table for        */
/*                  semantic error processing and code generation. Uses     */
/*                  SYMBOL type declared in symbol.h.                       */
/*                                                                          */
/*    Inputs:       1) To declare what type the entry is. (see symbol.h)    */
/*                  2) Address assigned for local and global variables.     */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      The pointer to the symbol created.                      */
/*                                                                          */
/*    Side Effects: Throws error if identifier already declared.            */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE SYMBOL * MakeSymbolTableEntry( int symtype, int *varaddress )
{

    SYMBOL * oldsptr;
    SYMBOL * newsptr = NULL;
    int hashindex;
    char * cptr;

    if ( CurrentToken.code == IDENTIFIER ) {
        if ( NULL == ( oldsptr = Probe( CurrentToken.s, &hashindex )) || oldsptr->scope < scope ) {
            if ( oldsptr == NULL ) cptr = CurrentToken.s; else cptr = oldsptr->s;
            if ( NULL == ( newsptr = EnterSymbol( cptr, hashindex ))) {
                /*〈Fatal internal error in EnterSymbol, compiler must exit: code for this goes here〉 */
                Error( "Fatal internal error in EnterSymbol", CurrentToken.pos );
            KillCodeGeneration();
            }
            else {
                if ( oldsptr == NULL ) PreserveString();
                newsptr->scope = scope;
                newsptr->type = symtype;
                if ( symtype == STYPE_VARIABLE || symtype == STYPE_LOCALVAR) {
                    newsptr->address = *varaddress; (*varaddress)++;
                }
                else newsptr->address = -1;
            }
        }
        else { 
            /*〈Error, variable already declared: code for this goes here〉*/
            Error( "Variable already declared", CurrentToken.pos );
            KillCodeGeneration();

        }
    }
    return newsptr;
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    LookupSymbol: Variable lookup for semantic processing in the code     */
/*                  generator. Looks at symbol table for already declared   */
/*                  identifiers.                                            */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      None                                                    */
/*                                                                          */
/*    Side Effects: Throws error if identifier not found.                   */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE SYMBOL *LookupSymbol( void )
{
    SYMBOL *sptr;
    if ( CurrentToken.code == IDENTIFIER ) {
        sptr = Probe( CurrentToken.s, NULL );
        if ( sptr == NULL ) {
            Error( "Identifier not declared", CurrentToken.pos );
            KillCodeGeneration();
        }
    }
    else sptr = NULL;
    return sptr;
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

    if ( argc != 4 )  {
        fprintf( stderr, "%s <inputfile> <listfile> <outputfile>\n", argv[0]);
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

    if ( NULL == ( CodeFile = fopen( argv[3], "w" ) ) )  {
        fprintf( stderr, "cannot open \"%s\" for output\n", argv[3]);
        fclose( InputFile);
        fclose( ListFile);
        return 0;
    }

    return 1;
}



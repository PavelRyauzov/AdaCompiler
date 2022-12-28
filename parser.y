%{
#include <stdio.h>
#include <stdlib.h>
#include "tree_nodes.h"
#include "print_tree.h"

extern int yylex();
extern int yyparse();
extern FILE* yyin;

void yyerror(const char* s);

ProgramList *root;

// ----- declarations -----
Expression *createExpression(ExprType type, Expression *left, Expression *right);
Expression *createSimpleExpression(ExprType type, Value val);
Expression *createExpressionWithList(ExprType type, Value val, ExpressionList *exprList);
ExpressionList *createExpressionList(Expression *expr);
ExpressionList *appendExpressionToList(ExpressionList *list, Expression *expr);

Statement *createStatement(StmtType type, StmtValue value);
WhileStatement *createWhile(Expression *condition, StatementList *whileBlock);
ForStatement *createFor(char *iterID, Range *countIterations, StatementList *forBlock);
CallFunctionStatement *createCallFunction(char *funcId, ExpressionList *args);
IfStatement *createIf(Expression *condition, StatementList *stmtList, ElseIfStatementList *elseIfStmtList, ElseStatement *elseStmt);
ElseStatement *createElse(StatementList *stmtList);
ElseIfStatement *createElseIf(Expression *condition, StatementList *stmtList);
ElseIfStatementList *createElseIfStatementList(ElseIfStatement *stmt);
ElseIfStatementList *appendElseIfToList(ElseIfStatementList *list, ElseIfStatement *stmt);
AssigmentStatement *createAssigmentStmt(Expression *left, Expression *right);
StatementList *createStatementList(Statement *stmt);
StatementList *appendStatementToList(StatementList *list, Statement *stmt);

Range *createRange(Expression *startIndex, Expression *lastIndex, char* id);
DeclarationStatement *createDeclarationStatement(DeclarationType type, DeclarationStmtValue stmt);
DeclarationStatementList *createDeclarationStatementList(DeclarationStatement *stmt);
DeclarationStatementList *appendDeclarationStatementToList(DeclarationStatementList *list, DeclarationStatement *stmt);
VariableDeclaration *createVariableDeclaration(VarType varType, VariableList *varList, Range *range, char *typeId);
VariableDeclarationList *createVariableDeclarationList(VariableDeclaration *varDecl);
VariableDeclarationList *appendVariableDeclarationList(VariableDeclarationList *list, VariableDeclaration *varDecl);
VariableList *createVariableList(char* id);
VariableList *appendToVariableList(VariableList *list, char* id);
TypeDeclaration *createTypeDeclaration(char *typeId, Range *range, VarType varType);
FuncReturnType *createFuncReturnType(char *typeId, VarType varType);

ProgramBlock *createProgramBlock(char *id, DeclarationStatementList *declarationSection, StatementList *performSection, VariableDeclarationList *args, FuncReturnType *returnType);
ProgramList *createProgramList(ProgramBlock *progBlock);
ProgramList *appendProgramToList(ProgramList *list, ProgramBlock *progBlock);
%}

%union { 
	int int_const;
	float float_const;
	char *string_const;
	char *id_const;
	char char_const;
	VarType vt;

	Expression *expr;
	ExpressionList *exprList;
	Statement *stmt;
	StatementList *stmtList;
	WhileStatement *whileStmt;
	Range *rangeStmt;
	AssigmentStatement *assigStmt;
	ForStatement *forStmt;
	IfStatement *ifStmt;
	CallFunctionStatement *callFuncStmt;
	ElseStatement *elseStmt;
	ElseIfStatement *elseIfStmt;
	ElseIfStatementList *elseIfStmtList;
	VariableList *varList;
	VariableDeclaration *varDecl;
	VariableDeclarationList *varDeclList;
	TypeDeclaration *typeDeclaration;
	FuncReturnType *funcReturnType;
	DeclarationStatement *declStmt;
	DeclarationStatementList *declStmtList;
	ProgramBlock *progBlock;
	ProgramList *progList;
}

%type <expr> expression
%type <stmt> empty_statement return_statement
%type <exprList> expression_list
%type <exprList> expression_listE
%type <varList> variable_list
%type <rangeStmt> range
%type <assigStmt> assigment_statement
%type <elseIfStmt> elsif_statement
%type <elseStmt> else_statement
%type <callFuncStmt> call_function_statement
%type <ifStmt> if_statement
%type <elseIfStmtList> elsif_statement_list
%type <forStmt> for_statement
%type <whileStmt> while_statement
%type <stmtList> statement_list
%type <stmt> statement
%type <vt> variable_type
%type <funcReturnType> function_return_type
%type <varDecl> variable_declaration
%type <varDeclList> variable_declaration_list
%type <typeDeclaration> type_declaration
%type <declStmt> declaration_statement
%type <declStmtList> declaration_statement_list
%type <progBlock> program_block
%type <progList> program_list
%type <progList> program


%token INTEGER BOOLEAN FLOAT CHARACTER STRING
%token ARRAY
%token TYPE

%token <id_const> ID
%token <string_const> CONST_STRING
%token <int_const> CONST_INTEGER
%token <float_const> CONST_FLOAT
%token <char_const> CONST_CHARACTER
%token <int_const> CONST_BOOL

%token RANGE
%token LENGTH

%token NIL

%token WHILE
%token FOR
%token IN
%token OF
%token LOOP

%token IF
%token THEN
%token ELSIF
%token ELSE

%token FUNCTION
%token PROCEDURE
%token IS
%token RETURN
%token BEGIN_TOKEN END


%token ';'
%token ','
%token DOUBLE_DOT
%token ASSIGNMENT
%token  ':'
%token '['
%token ']'

%left OR AND
%left '=' NOT_EQUAL '<' LESSER_EQUAL '>' GREATER_EQUAL
%left '+' '-' '&'
%right UMINUS UPLUS
%left '*' '/'
%right NOT
%left '\''
%left '[' ']'
%nonassoc ')'

%start program

%%
program : program_list {$$ = root = $1;}  
		;

program_list : program_block { $$ = createProgramList($1); }
			 | program_list program_block { $$ = appendProgramToList($1,$2); }
		     ;

program_block : PROCEDURE ID IS BEGIN_TOKEN statement_list END ID ';' {$$ = createProgramBlock($2,0,$5,0,0);} 
			  | PROCEDURE ID IS declaration_statement_list BEGIN_TOKEN statement_list END ID ';' {$$ = createProgramBlock($2,$4,$6,0,0);}  
			  | PROCEDURE ID '(' variable_declaration_list ')' IS BEGIN_TOKEN statement_list END ID ';' {$$ = createProgramBlock($2,0,$8,$4,0);} 
			  | PROCEDURE ID '(' variable_declaration_list ')' IS declaration_statement_list BEGIN_TOKEN statement_list END ID ';' {$$ = createProgramBlock($2,$7,$9,$4,0);}

			  | FUNCTION ID RETURN function_return_type IS BEGIN_TOKEN statement_list END ID ';' {$$ = createProgramBlock($2,0,$7,0,$4);}
			  | FUNCTION ID RETURN function_return_type IS declaration_statement_list BEGIN_TOKEN statement_list END ID ';' {$$ = createProgramBlock($2,$6,$8,0,$4);}
			  | FUNCTION ID '(' variable_declaration_list ')' RETURN function_return_type IS BEGIN_TOKEN statement_list END ID ';' {$$ = createProgramBlock($2,0,$10,$4,$7);}
			  | FUNCTION ID '(' variable_declaration_list ')' RETURN function_return_type IS declaration_statement_list BEGIN_TOKEN statement_list END ID ';' {$$ = createProgramBlock($2,$9,$11,$4,$7);} 

			  | error  {$$ = NULL;}
			  ;

statement_list : statement {$$ = createStatementList($1);}  					
			   | statement_list statement {$$ = appendStatementToList($1,$2);}  
			   ;

statement : call_function_statement {$$ = createStatement(ST_CALL_FUNC, (StmtValue){.callFuncStmt=$1});}  	
		  | while_statement {$$ = createStatement(ST_WHILE, (StmtValue){.whileStmt=$1});}
		  | for_statement {$$ = createStatement(ST_FOR, (StmtValue){.forStmt=$1});}  			
		  | if_statement {$$ = createStatement(ST_IF, (StmtValue){.ifStmt=$1});} 			
		  | return_statement {$$ = $1;} 		
		  | empty_statement {$$ = $1;}  			
		  | assigment_statement {$$ = createStatement(ST_ASSIGN, (StmtValue){.assignStmt=$1});} 		
		  ;

/* expression_statement : expression ';'
					 ; */

declaration_statement_list : declaration_statement {$$ = createDeclarationStatementList($1);}
						   | declaration_statement_list declaration_statement {$$ = appendDeclarationStatementToList($1, $2);}
						   ;

declaration_statement:	variable_declaration ';'  {$$ = createDeclarationStatement(DT_VARIABLE, (DeclarationStmtValue){.varDeclaration=$1});}
					 |  type_declaration ';'  {$$ = createDeclarationStatement(DT_TYPE, (DeclarationStmtValue){.typeDeclaration=$1});}
					 |	program_block  {$$ = createDeclarationStatement(DT_FUNCTION, (DeclarationStmtValue){.progBlock=$1});}
					 ;

variable_declaration_list : variable_declaration  {$$ = createVariableDeclarationList($1);}
						  | variable_declaration_list ';' variable_declaration {$$ = appendVariableDeclarationList($1,$3);}
						  ;

variable_declaration : variable_list ':' variable_type {$$ = createVariableDeclaration($3,$1,0,0);} 	
					 | variable_list ':' ID	 {$$ = createVariableDeclaration(0,$1,0,$3);}
					 | variable_list ':' ARRAY '(' range ')' OF variable_type  {$$ = createVariableDeclaration($8,$1,$5,0);}
					 ;

type_declaration : TYPE ID IS ARRAY '(' range ')' OF variable_type  {$$ = createTypeDeclaration($2, $6, $9);}
				 ;

function_return_type : variable_type  {$$ = createFuncReturnType(0, $1);}
					 | ID  {$$ = createFuncReturnType($1, VT_VOID);}
					 ;

variable_list : ID {$$ = createVariableList($1);}
			  | variable_list ',' ID {$$ = appendToVariableList($1, $3);}
			  ;

variable_type : INTEGER    {$$ = VT_INTEGER;}
			  | FLOAT      {$$ = VT_FLOAT;}	
			  | STRING     {$$ = VT_STRING;}
			  | CHARACTER  {$$ = VT_CHARACTER;}
			  | BOOLEAN	   {$$ = VT_BOOLEAN;}
			  ;

while_statement : WHILE expression LOOP statement_list END LOOP ';'	 {$$ = createWhile($2, $4);}
				;

for_statement : FOR ID IN range LOOP statement_list END LOOP ';' {$$ = createFor($2,$4,$6);} 
			  ;

range : expression DOUBLE_DOT expression {$$ = createRange($1, $3, 0);}
	  |	ID '\'' RANGE {$$ = createRange(0, 0, $1);} 			
	  ;

call_function_statement : ID '(' expression_list ')' ';' {$$ = createCallFunction($1, $3);}

if_statement : IF expression THEN statement_list elsif_statement_list else_statement END IF ';'	 {$$ = createIf($2, $4, $5, $6);}
			 | IF expression THEN statement_list else_statement END IF ';' {$$ = createIf($2, $4, 0, $5);}	 
			 ;

else_statement : 	/* empty */ 		  {$$ = 0;}
			   | ELSE statement_list	  {$$ = createElse($2);}
			   ;

elsif_statement : ELSIF expression THEN statement_list {$$ = createElseIf($2, $4);}  	
				;

elsif_statement_list : elsif_statement {$$ = createElseIfStatementList($1);}		
				     | elsif_statement_list elsif_statement {$$ = appendElseIfToList($1, $2);}	 
				     ;

return_statement : RETURN expression ';' {$$ = createStatement(ST_RETURN, (StmtValue){.exprStmt=$2});}
				 | RETURN ';'            {$$ = createStatement(ST_RETURN, (StmtValue){});}
			     ;

assigment_statement : expression ASSIGNMENT expression ';'  {$$ = createAssigmentStmt($1, $3);}
			        ;

empty_statement : NIL ';' {$$ = createStatement(ST_NULL, (StmtValue){});}
				;

expression : expression '+' expression {$$ = createExpression(ET_PLUS, $1, $3); }
		   | expression '-' expression {$$ = createExpression(ET_MINUS, $1, $3); }
		   | expression '*' expression {$$ = createExpression(ET_MULT, $1, $3);}
	       | expression '/' expression {$$ = createExpression(ET_DIV, $1, $3);}
		   | expression '&' expression {$$ = createExpression(ET_CONCAT, $1, $3);}
		   | '+' expression %prec UPLUS	{$$ = createExpression(ET_PLUS, 0, $2);}
		   | '-' expression %prec UMINUS {$$ = createExpression(ET_MINUS, 0, $2);}
		   | NOT expression {$$ = createExpression(ET_NOT, 0, $2);}
		   | expression '<' expression {$$ = createExpression(ET_LESSER, $1, $3);}
		   | expression '>' expression {$$ = createExpression(ET_GREATER, $1, $3);}
		   | expression LESSER_EQUAL expression	{$$ = createExpression(ET_LESSER_EQUAL, $1, $3);} 
		   | expression GREATER_EQUAL expression {$$ = createExpression(ET_GREATER_EQUAL, $1, $3);}
		   | expression '=' expression {$$ = createExpression(ET_EQUAL, $1, $3);}
	       | expression NOT_EQUAL expression {$$ = createExpression(ET_NOT_EQUAL, $1, $3);}
		   | expression OR expression {$$ = createExpression(ET_LOGIC_OR, $1, $3);}
		   | expression AND expression {$$ = createExpression(ET_LOGIC_AND, $1, $3);}
		   | ID '(' expression_list ')' {$$ = createExpressionWithList(ET_ARRAY_OR_FUNC, (Value){.string_val=$1}, $3);}
		   | '(' expression ')'	 {$$ = $2;}
		   | expression '[' expression ']' {$$ = createExpression(ET_INDEXER, $1, $3);}
		   | CONST_INTEGER {$$ = createSimpleExpression(ET_INTEGER, (Value){.int_val = $1});}
		   | CONST_FLOAT  {$$ = createSimpleExpression(ET_FLOAT, (Value){.float_val=$1});}
		   | CONST_STRING {$$ = createSimpleExpression(ET_STRING, (Value){.string_val=$1});}
		   | CONST_CHARACTER {$$ = createSimpleExpression(ET_CHARACTER, (Value){.char_val=$1});}
		   | CONST_BOOL	{$$ = createSimpleExpression(ET_BOOL, (Value){.int_val=$1});}	 
		   | ID	{$$ = createSimpleExpression(ET_ID, (Value){.string_val=$1});}
		   | ID '\'' LENGTH {$$ = createSimpleExpression(ET_LENGTH_ARR_ATTR, (Value){.string_val=$1});}
		   ;

expression_list : 	/* empty */  	{$$ = 0;}
				| expression_listE {$$ = $1;}
				;

expression_listE : expression {$$ = createExpressionList($1);}
				 | expression_list ',' expression {$$ = appendExpressionToList($1,$3);}
				 ;
%%

int main(int argc, char* argv[]) {

	printf("argv[0] = %s, argv[1] = %s\n", argv[0], argv[1]); 
	if(argc>1) {
		yyin=fopen(argv[1], "r");
    	yyparse();

		freopen("./build/tree_img.txt", "w", stdout);
		printTree(root);
		fclose(stdout);
		fclose (yyin);
	}
  
	return 0;
}

void yyerror(const char* s) {
	fprintf(stderr, "Parse error: %s\n", s);
	exit(1);
}

// ------------------------------  Expression ------------------------------ 

Expression *createExpression(ExprType type, Expression *left, Expression *right)
{
	Expression *result = (Expression *)malloc(sizeof(Expression));

	result->type = type;

	result->left = left;
	result->right = right;

	result->exprList = 0;
	result->nextInList = 0;

	return result;
}

Expression *createSimpleExpression(ExprType type, Value value)
{
	Expression *result = (Expression *)malloc(sizeof(Expression));

	result->type = type;
	result->value = value;

	result->exprList = 0;
	result->right = 0;
	result->left = 0;
	result->nextInList = 0;

	return result;
}

Expression *createExpressionWithList(ExprType type, Value value, ExpressionList *exprList)
{
	Expression *result = (Expression *)malloc(sizeof(Expression));

	result->type = type;
	result->value = value;
	result->exprList = exprList;

	result->right = 0;
	result->left = 0;
	result->nextInList = 0;

	return result;
}

ExpressionList *createExpressionList(Expression *expr)
{
	ExpressionList *result = (ExpressionList *)malloc(sizeof(ExpressionList));

	result->begin = expr;
	result->end = expr;

	return result;
}

ExpressionList *appendExpressionToList(ExpressionList *list, Expression *expr)
{
	list->end->nextInList = expr;
	list->end = expr;

	return list;
}

// ------------------------------  Statement ------------------------------ 
Statement *createStatement(StmtType type, StmtValue value)
{
	Statement *result = (Statement *)malloc(sizeof(Statement));

	result->type = type;
	result->stmtValue = value;
	result->nextInList = 0;

	return result;
}

WhileStatement *createWhile(Expression *condition, StatementList *whileBlock)
{
	WhileStatement *result = (WhileStatement *)malloc(sizeof(WhileStatement));

	result->condition = condition;
	result->whileBlock = whileBlock;

	return result;
}

ForStatement *createFor(char *iterID, Range *countIterations, StatementList *forBlock)
{
	ForStatement *result = (ForStatement *)malloc(sizeof(ForStatement));

	result->iterID = iterID;
	result->range = countIterations;
	result->stmtList = forBlock;

	return result;
}

IfStatement *createIf(Expression *condition, StatementList *stmtList, ElseIfStatementList *elseIfStmtList, ElseStatement *elseStmt)
{
	IfStatement *result = (IfStatement *)malloc(sizeof(IfStatement));

	result->condition = condition;
	result->stmtList = stmtList;
	result->elseIfStmtList = elseIfStmtList;
	result->elseStmt = elseStmt;

	return result;
}

ElseStatement *createElse(StatementList *stmtList)
{
	ElseStatement *result = (ElseStatement *)malloc(sizeof(ElseStatement));

	result->stmtList = stmtList;

	return result;
}

ElseIfStatement *createElseIf(Expression *condition, StatementList *stmtList)
{
	ElseIfStatement *result = (ElseIfStatement *)malloc(sizeof(ElseIfStatement));

	result->condition = condition;
	result->stmtList = stmtList;
	result->nextInList = 0;

	return result;
}

ElseIfStatementList *createElseIfStatementList(ElseIfStatement *stmt)
{
	ElseIfStatementList *result = (ElseIfStatementList *)malloc(sizeof(ElseIfStatementList));

	result->begin = stmt;
	result->end = stmt;

	return result;
}

ElseIfStatementList *appendElseIfToList(ElseIfStatementList *list, ElseIfStatement *stmt)
{
	list->end->nextInList = stmt;
	list->end = stmt;
	return list;
}

AssigmentStatement *createAssigmentStmt(Expression *left, Expression *right)
{
	AssigmentStatement *result = (AssigmentStatement *)malloc(sizeof(AssigmentStatement));

	result->left = left;
	result->right = right;

	return result;
}

CallFunctionStatement *createCallFunction(char *funcId, ExpressionList *args) 
{
	CallFunctionStatement *result = (CallFunctionStatement *)malloc(sizeof(CallFunctionStatement));
	result->funcId = funcId;
	result->args = args;
	
	return result;
}

StatementList *createStatementList(Statement *stmt)
{
	StatementList *result = (StatementList *)malloc(sizeof(StatementList));

	result->begin = stmt;
	result->end = stmt;
	return result;
}

StatementList *appendStatementToList(StatementList *list, Statement *stmt)
{
	list->end->nextInList = stmt;
	list->end = stmt;
	return list;
}

// ------------------------------  Declaration statements ------------------------------ 
Range *createRange(Expression *startIndex, Expression *lastIndex, char* id)
{
	struct Range *result = (Range *)malloc(sizeof(Range));

	result->startIndex = startIndex;
	result->lastIndex = lastIndex;
	result->id = id;
	return result;
}

DeclarationStatement *createDeclarationStatement(DeclarationType type, DeclarationStmtValue stmt)
{
	DeclarationStatement *result = (DeclarationStatement *)malloc(sizeof(DeclarationStatement));

	result->type = type;
	result->stmt = stmt;
	result->nextInList = 0;

	return result;
}

DeclarationStatementList *createDeclarationStatementList(DeclarationStatement *stmt)
{
	DeclarationStatementList *result = (DeclarationStatementList *)malloc(sizeof(DeclarationStatementList));

	result->begin = stmt;
	result->end = stmt;

	return result;
}

DeclarationStatementList *appendDeclarationStatementToList(DeclarationStatementList *list, DeclarationStatement *stmt)
{
	list->end->nextInList = stmt;
	list->end = stmt;
	return list;
}

VariableDeclaration *createVariableDeclaration(VarType varType, VariableList *varList, Range *range, char *typeId)
{
	VariableDeclaration *result = (VariableDeclaration *)malloc(sizeof(VariableDeclaration));

	result->varType = varType;
	result->varList = varList;

	result->isArray = range != 0;
	result->range=range;

	result->isType = typeId != 0;
	result->typeId = typeId;

	result->nextInList = 0;

	return result;
}

VariableDeclarationList *createVariableDeclarationList(VariableDeclaration *varDecl)
{
	VariableDeclarationList *result = (VariableDeclarationList *)malloc(sizeof(VariableDeclarationList));

	result->begin = varDecl;
	result->end = varDecl;

	return result;
}

VariableDeclarationList *appendVariableDeclarationList(VariableDeclarationList *list, VariableDeclaration *varDecl)
{
	list->end->nextInList = varDecl;
	list->end = varDecl;

	return list;
}

VariableList *createVariableList(char* id)
{
	VariableList *result = (VariableList *)malloc(sizeof(VariableList));

	result->id = id;
	result->end = result;
	result->nextInList = 0;

	return result;
}

VariableList *appendToVariableList(VariableList *list, char* id)
{
	VariableList *result = (VariableList *)malloc(sizeof(VariableList));

	result->id = id;
	result->nextInList = 0;

	list->end->nextInList = result;
	list->end = result;

	return list;
}

TypeDeclaration *createTypeDeclaration(char *typeId, Range *range, VarType varType)
{
	TypeDeclaration *result = (TypeDeclaration *)malloc(sizeof(TypeDeclaration));

	result->id = typeId;
	result->range = range;
	result->varType = varType;

	return result;
}

FuncReturnType *createFuncReturnType(char *typeId, VarType varType)
{
	FuncReturnType *result = (FuncReturnType *)malloc(sizeof(FuncReturnType));

	result->isType = typeId != 0;
	result->typeId = typeId;

	result->isVarType = varType != VT_VOID;
	result->varType=varType;

	return result;
}


// ------------------------------  Program block ------------------------------ 

ProgramBlock *createProgramBlock(char *id, DeclarationStatementList *declarationSection, StatementList *performSection, VariableDeclarationList *args, FuncReturnType *returnType)
{
	ProgramBlock *result = (ProgramBlock *)malloc(sizeof(ProgramBlock));

	result->id = id;
	result->declarationSection = declarationSection;
	result->performSection = performSection;
	result->funcArgs = args;
	result->returnType = returnType;

	result->nextInList = NULL;

	return result;
}

ProgramList *createProgramList(ProgramBlock *progBlock)
{
	ProgramList *result = (ProgramList *)malloc(sizeof(ProgramList));

	result->begin = progBlock;
	result->end = progBlock;

	return result;
}

ProgramList *appendProgramToList(ProgramList *list, ProgramBlock *progBlock)
{
	list->end->nextInList = progBlock;
	list->end = progBlock;

	return list;
}

%{
#include <stdio.h>
#include <stdlib.h>

#include "tree_nodes.h"

extern int yylex();
extern int yyparse();
extern FILE* yyin;

void yyerror(const char* s);

ProgramList *root;

// ----- declarations -----
Expression *createExpression(ExprType type, Expression *left, Expression *right);
Expression *createSimpleExpression(ExprType type, Value val);
Expression *createExpressionWithList(ExprType type, Value val, ExpressionList *exprList);


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
program : program_list  
		;

program_list : program_block  
			 | program_list program_block  
		     ;

program_block : PROCEDURE ID IS BEGIN_TOKEN statement_list END ID ';'  
			  | PROCEDURE ID IS declaration_statement_list BEGIN_TOKEN statement_list END ID ';'  
			  | PROCEDURE ID '(' variable_declaration_list ')' IS BEGIN_TOKEN statement_list END ID ';'  
			  | PROCEDURE ID '(' variable_declaration_list ')' IS declaration_statement_list BEGIN_TOKEN statement_list END ID ';'  

			  | FUNCTION ID RETURN function_return_type IS BEGIN_TOKEN statement_list END ID ';'  
			  | FUNCTION ID RETURN function_return_type IS declaration_statement_list BEGIN_TOKEN statement_list END ID ';'  
			  | FUNCTION ID '(' variable_declaration_list ')' RETURN function_return_type IS BEGIN_TOKEN statement_list END ID ';'  
			  | FUNCTION ID '(' variable_declaration_list ')' RETURN function_return_type IS declaration_statement_list BEGIN_TOKEN statement_list END ID ';'  

			  | error  
			  ;

statement_list : statement  					
			   | statement_list statement  
			   ;

statement : ID '(' expression_list ')' ';'	 	
		  | while_statement	 		
		  | for_statement  			
		  | if_statement  			
		  | return_statement  		
		  | empty_statement  			
		  | assigment_statement  		
		  ;

/* expression_statement : expression ';'
					 ; */

declaration_statement_list : declaration_statement  
						   | declaration_statement_list declaration_statement  
						   ;

declaration_statement:	variable_declaration ';'  
					 |  type_declaration ';'  
					 |	program_block  
					 ;

variable_declaration_list : variable_declaration  
						  | variable_declaration_list ';' variable_declaration  
						  ;

variable_declaration : variable_list ':' variable_type   	
					 | variable_list ':' ID	 
					 | variable_list ':' ARRAY '(' range ')' OF variable_type  
					 ;

type_declaration : TYPE ID IS ARRAY '(' range ')' OF variable_type  
				 ;

function_return_type : variable_type  
					 | ID  
					 ;

variable_list : ID  
			  | variable_list ',' ID  
			  ;

variable_type : INTEGER	 
			  | FLOAT  		
			  | STRING  
			  | CHARACTER  
			  | BOOLEAN	 
			  ;

while_statement : WHILE expression LOOP statement_list END LOOP ';'	 
				;

for_statement : FOR ID IN range LOOP statement_list END LOOP ';'  
			  ;

range : expression DOUBLE_DOT expression  
	  |	ID '\'' RANGE  			
	  ;

if_statement : IF expression THEN statement_list elsif_statement_list else_statement END IF ';'	 
			 | IF expression THEN statement_list else_statement END IF ';'	 
			 ;

else_statement : 	/* empty */ 		 
			   | ELSE statement_list	 
			   ;

elsif_statement : ELSIF expression THEN statement_list  	
				;

elsif_statement_list : elsif_statement  			
				     | elsif_statement_list elsif_statement	 
				     ;

return_statement : RETURN expression ';'  
				 | RETURN ';'  
			     ;

assigment_statement : expression ASSIGNMENT expression ';'  
			        ;

empty_statement : NIL ';'  
				;

expression : expression '+' expression {$$ = createExpression(ET_PLUS, $1, $3); }
		   | expression '-' expression {$$ = createExpression(ET_MINUS, $1, $3); }
		   | expression '*' expression {$$ = createExpression(ET_MULT, $1, $3);}
	       | expression '/' expression {$$ = createExpression(ET_DIV, $1, $3);}
		   | expression '&' expression {$$ = createExpression(ET_CONCAT, $1, $3);}
		   | '+' expression %prec UPLUS	{$$ = createExpression(ET_PLUS, NULL, $2);}
		   | '-' expression %prec UMINUS {$$ = createExpression(ET_MINUS, NULL, $2);}
		   | NOT expression {$$ = createExpression(ET_NOT, NULL, $2);}
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

expression_list : 	/* empty */  	
				| expression_listE		
				;

expression_listE : expression  		
				 | expression_list ',' expression  
				 ;
%%

int main(int argc, char* argv[]) {

	printf("argv[0] = %s, argv[1] = %s\n", argv[0], argv[1]); 
	if(argc>1) {
		yyin=fopen(argv[1], "r");
    	yyparse();
		fclose (yyin);
	}
  
	return 0;
}

void yyerror(const char* s) {
	fprintf(stderr, "Parse error: %s\n", s);
	exit(1);
}

Expression *createExpression(ExprType type, Expression *left, Expression *right)
{
	Expression *result = (Expression *)malloc(sizeof(Expression));

	result->type = type;

	result->left = left;
	result->right = right;

	result->exprList = NULL;
	result->nextInList = NULL;

	return result;
}

Expression *createSimpleExpression(ExprType type, Value value)
{
	Expression *result = (Expression *)malloc(sizeof(Expression));

	result->type = type;
	result->value = value;

	result->exprList = NULL;
	result->right = NULL;
	result->left = NULL;
	result->nextInList = NULL;

	return result;
}

Expression *createExpressionWithList(ExprType type, Value value, ExpressionList *exprList)
{
	Expression *result = (Expression *)malloc(sizeof(Expression));

	result->type = type;
	result->value = value;
	result->exprList = exprList;

	result->right = NULL;
	result->left = NULL;
	result->nextInList = NULL;

	return result;
}
%{
#include <stdio.h>
#include <stdlib.h>

#include "tree_nodes.h"

extern int yylex();
extern int yyparse();
extern FILE* yyin;

void yyerror(const char* s);

ProgramList *root;
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

expression : expression '+' expression 
		   | expression '-' expression	 
		   | expression '*' expression	 
	       | expression '/' expression	 
		   | expression '&' expression	 
		   | '+' expression %prec UPLUS	 
		   | '-' expression %prec UMINUS  
		   | NOT expression  
		   | expression '<' expression  
		   | expression '>' expression  
		   | expression LESSER_EQUAL expression	 
		   | expression GREATER_EQUAL expression  	
		   | expression '=' expression  
	       | expression NOT_EQUAL expression  
		   | expression OR expression  
		   | expression AND expression  
		   | ID '(' expression_list ')'	 
		   | '(' expression ')'	 
		   | expression '[' expression ']'	 
		   | CONST_INTEGER	 
		   | CONST_FLOAT  
		   | CONST_STRING	
		   | CONST_CHARACTER	 
		   | CONST_BOOL		 
		   | ID				 
		   | ID '\'' LENGTH	 
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


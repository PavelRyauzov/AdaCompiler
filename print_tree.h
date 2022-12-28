#include "tree_nodes.h"

typedef struct TreeUnit TreeUnit;
typedef struct Tree Tree;

struct TreeUnit
{
    char *label;
	char *edgeLabel;
    int num;
    int parentNum;
    TreeUnit *next;
};

struct Tree
{
    TreeUnit * begin;
    TreeUnit * end;
};

void printTree(ProgramList *pr);
void programParse(ProgramBlock *prog, Tree *tree, int parentNum);
void parseFuncReturnType(FuncReturnType *frt, Tree *tree, int parentNum);

void parseExpression(Expression *expr, Tree *tree, int parentNum);
void parseExpressionList(ExpressionList *exprList, Tree *tree, int parentNum);

void parseStatement(Statement *stmt, Tree *tree, int parentNum);
void parseWhileStatement(WhileStatement *whileStmt, Tree *tree, int parentNum);
void parseForStatement(ForStatement *forStmt, Tree *tree, int parentNum);
void parseIfStatement(IfStatement *ifStmt, Tree *tree, int parentNum);
void parseElse(ElseStatement *elseStmt, Tree *tree, int parentNum);
void parseElseIfStatementList(ElseIfStatementList *elseIfStmt, Tree *tree, int parentNum);
void parseStatementList(StatementList *stmtList, Tree *tree, int parentNum);

void parseRange(Range *range, Tree *tree, int parentNum);
void parseDeclarationStatement(DeclarationStatement *declStmt, Tree *tree, int parentNum);
void parseDeclarationStatementList(DeclarationStatementList *declStmtList, Tree *tree, int parentNum);
void parseVariableDeclaration(VariableDeclaration *varDecl, Tree *tree, int parentNum);
void parseVariableDeclarationList(VariableDeclarationList *varDeclList, Tree *tree, int parentNum);
void parseVariableList(VariableList *varList, Tree *tree, int parentNum);
void parseTypeDeclaration(TypeDeclaration *typeDecl, Tree *tree, int parentNum);





#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "print_tree.h"


TreeUnit *newTreeUnit(int parentNum, const char *label, const char *edgeLabel)
{
    if(label!=NULL && edgeLabel!=NULL && parentNum>=0)
    {
        TreeUnit *unit = (TreeUnit *)malloc(sizeof(TreeUnit));
        unit->label = (char *)malloc( sizeof(char)*(strlen(label)+1) );
        strcpy(unit->label,label);
		unit->edgeLabel = (char *)malloc( sizeof(char)*(strlen(edgeLabel)+1) );
        strcpy(unit->edgeLabel,edgeLabel);
        unit->parentNum = parentNum;
		unit->next = NULL;
        return unit;
    }
    return NULL;
}

void addTreeUnit(Tree *tree, TreeUnit *element)
{
    if(tree!=NULL && element!=NULL )
    {
        if(tree->begin==NULL)
        {
            tree->begin = element;
            tree->end = element;
            element->num = 1;
        }
        else
        {
            tree->end->next = element;
            element->num = tree->end->num+1;
            tree->end = element;
        }
    }
}

char *expr_type_str(ExprType et)
{
    if(et==ET_INTEGER) return "ET_INTEGER";
    if(et==ET_FLOAT) return "ET_FLOAT";
    if(et==ET_STRING) return "ET_STRING";
    if(et==ET_CHARACTER) return "ET_CHARACTER";
    if(et==ET_ID) return "ET_ID";
    if(et==ET_ARRAY_OR_FUNC) return "ET_ARRAY_OR_FUNC";
    if(et==ET_BOOL) return "ET_BOOL";
    if(et==ET_EQUAL) return "=";
    if(et==ET_NOT_EQUAL) return "/=";
    if(et==ET_LESSER) return "<";
    if(et==ET_GREATER) return ">";
    if(et==ET_LESSER_EQUAL) return "<=";
    if(et==ET_GREATER_EQUAL) return ">=";
    if(et==ET_PLUS) return "+";
    if(et==ET_MINUS) return "-";
    if(et==ET_CONCAT) return "&";
    if(et==ET_MULT) return "*";
    if(et==ET_DIV) return "/";
    if(et==ET_LOGIC_OR) return "OR";
    if(et==ET_LOGIC_AND) return "AND";
    if(et==ET_ASSIGN) return ":=";
    if(et==ET_NOT) return "NOT";
    if(et==ET_RANGE_ARR_ATTR) return "ET_RANGE_ARR_ATTR";
    if(et==ET_LENGTH_ARR_ATTR) return "ET_LENGTH_ARR_ATTR";
    return "";
}

char *stmt_type_str(StmtType et)
{
    if(et==ST_CALL_FUNC) return "ST_CALL_FUNC";
    if(et==ST_RETURN) return "ST_RETURN";
    if(et==ST_NULL) return "ST_NULL";
    if(et==ST_WHILE) return "ST_WHILE";
    if(et==ST_FOR) return "ST_FOR";
    if(et==ST_IF) return "ST_IF";
    if(et==ST_ASSIGN) return ":=";
    return "";
}

char *variable_type_str(VarType et)
{
    if(et==VT_INTEGER) return "INTEGER";
    if(et==VT_FLOAT) return "FLOAT";
    if(et==VT_BOOLEAN) return "BOOLEAN";
    if(et==VT_STRING) return "STRING";
    if(et==VT_CHARACTER) return "CHARACTER";
    if(et==VT_ARRAY) return "ARRAY";
    return "";
}

void print_node(int nodeNum, char *nodeName)
{
    printf("%d  [label=\"%s\"];\n",nodeNum,nodeName);
}

void print_edge(int numNode1, int numNode2, char* name)
{
    printf("%d->%d[label=\"%s\"];\n",numNode1,numNode2,name);
}

void printTree(ProgramList *pr)
{
    if(pr!=NULL)
    {
        Tree *tree = (Tree *)malloc(sizeof(Tree));
        tree->begin = NULL;
        tree->end = NULL;

        programParse(pr->end, tree, 0);

		printf("digraph Program {\n");

			TreeUnit *i_units = tree->begin;
			while(i_units!=NULL)
			{
				print_node(i_units->num,i_units->label);
				i_units = i_units->next;
			}
			printf("\n");
			TreeUnit *i_parent = tree->begin;
			while(i_parent!=NULL)
			{
				TreeUnit *i_child = tree->begin;
				while(i_child!=NULL)
				{
					if(i_parent->num==i_child->parentNum)
					{
						print_edge(i_parent->num, i_child->num,i_child->edgeLabel);
					}

					i_child = i_child->next;
				}
				i_parent = i_parent->next;
			}

		printf("\n}");
    }
}

void programParse(ProgramBlock *prog, Tree *tree, int parentNum)
{
    if(prog!=NULL)
    {
        //main title
        addTreeUnit(tree, newTreeUnit(parentNum,prog->id,"Function"));
		// int currentIter = tree->end->num;

        // //parts of program
        // if(prog->declarationSection!=NULL)
        // {
        //     parseDeclarationStatementList(prog->declarationSection,tree,currentIter);
        // }

        // if(prog->performSection!=NULL)
        // {
        //     parseStatementList(prog->performSection,tree,currentIter);
        // }

        // if(prog->funcArgs!=NULL)
        // {
		// 	parseVariableDeclarationList(prog->funcArgs,tree,currentIter);
        // }

        // //type of function
        // parseFuncReturnType(prog->returnType,tree,currentIter);
    }
}

// void parseFuncReturnType(FuncReturnType *frt, Tree *tree, int parentNum)
// {
//     addTreeUnit(tree, newTreeUnit(parentNum, "Return type", ""));
//     int currentIter = tree->end->num;

//     if (frt!=0 ) {
//         if (frt->isType) 
//         {
//             addTreeUnit(tree, newTreeUnit(currentIter, frt->typeId, ""));
//         }
//         else if (frt->isVarType)
//         {
//             addTreeUnit(tree, newTreeUnit(currentIter, variable_type_str(frt->varType), ""));
//         }
//     } 
//     else {
//         addTreeUnit(tree, newTreeUnit(currentIter, "void", ""));
//     }
// }

// void parseDeclarationStatementList(DeclarationStatementList *declStmtList, Tree *tree, int parentNum)
// {
// 	addTreeUnit(tree, newTreeUnit(parentNum,"DeclarationStatementList",""));
// 	int currentIter = tree->end->num;

//     if(declStmtList!=NULL)
//     {
// 		DeclarationStatement *ds = declStmtList->begin;
//         for(; ds!=NULL; ds = ds->nextInList)
//         {
//             parseDeclarationStatement(ds,tree,currentIter);
//         }
//     }
// }

// void parseDeclarationStatement(DeclarationStatement *declStmt, Tree *tree, int parentNum)
// {
// 	addTreeUnit(tree, newTreeUnit(parentNum,"DeclarationStatement",""));
// 	int currentIter = tree->end->num;

// 	if(declStmt!=NULL)
// 	{
// 		switch(declStmt->type)
// 		{
// 			case DT_FUNCTION:
// 				programParse(prog->stmt.declStmt, tree, currentIter);
// 			break;

// 			case DT_VARIABLE:
// 				parseVariableDeclaration(declStmt->stmt.varDecl, tree, currentIter);
// 			break;
// 		}
// 	}
// }

// void parseVariableDeclarationList(VariableDeclarationList *varDeclList, Tree *tree, int parentNum)
// {
// 	if(varDeclList!=NULL)
//     {
// 		addTreeUnit(tree, newTreeUnit(parentNum,"VariableDeclarationList",""));
// 		int currentIter = tree->end->num;

// 		VariableDeclaration *ds = varDeclList->begin;
// 		for(; ds!=NULL; ds = ds->nextInList)
//         {
// 			parseVariableDeclaration(ds, tree, currentIter);
//         }
//     }
// }

// void parseVariableDeclaration(VariableDeclaration *varDecl, Tree *tree, int parentNum)
// {
//     if(varDecl!=NULL)
//     {
// 		addTreeUnit(tree, newTreeUnit(parentNum,"VariableDeclaration",""));
// 		int currentIter = tree->end->num;

// 		addTreeUnit(tree, newTreeUnit(currentIter,variable_type_str(varDecl->type),""));

//         if(varDecl->isArray)
//         {
// 			addTreeUnit(tree, newTreeUnit(currentIter,"Array",""));

//             parseRange(varDecl->range, tree, currentIter);
//         }

// 		if(varDecl->varList!=NULL)
// 		{
// 			parseVariableList(varDecl->varList, tree, currentIter);
// 		}
//     }
// }


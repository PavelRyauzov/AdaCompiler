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
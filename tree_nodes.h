typedef union StmtValue StmtValue;
typedef union DeclarationStmtValue DeclarationStmtValue;
typedef union Value Value;

typedef enum ExprType ExprType;
typedef enum VarType VarType;
typedef enum DeclarationType DeclarationType;
typedef enum StmtType StmtType;

typedef struct ProgramList ProgramList;
typedef struct ProgramBlock ProgramBlock;
typedef struct FuncReturnType FuncReturnType;
typedef struct DeclarationStatementList DeclarationStatementList;
typedef struct DeclarationStatement DeclarationStatement;
typedef struct VariableDeclarationList VariableDeclarationList;
typedef struct VariableDeclaration VariableDeclaration;
typedef struct TypeDeclaration TypeDeclaration;
typedef struct VariableList VariableList;
typedef struct IfStatement IfStatement;
typedef struct ElseStatement ElseStatement;
typedef struct ElseIfStatement ElseIfStatement;
typedef struct ElseIfStatementList ElseIfStatementList;
typedef struct ForStatement ForStatement;
typedef struct Range Range;
typedef struct WhileStatement WhileStatement;
typedef struct AssigmentStatement AssigmentStatement;
typedef struct StatementList StatementList;
typedef struct Statement Statement;
typedef struct ExpressionList ExpressionList;
typedef struct Expression Expression;

union Value
{

	int int_val;
	float float_val;
	char char_val;
	char *string_val;
};

union StmtValue 
{
	WhileStatement *whileStmt;
	AssigmentStatement *assignStmt;
	ForStatement *forStmt;
	IfStatement *ifStmt;
	Expression *exprStmt;
};

union DeclarationStmtValue 
{
	VariableDeclaration *varDeclaration;
	TypeDeclaration *typeDeclaration;
	ProgramBlock *progBlock;
};

enum ExprType
{
	ET_INTEGER,
	ET_FLOAT,
	ET_STRING,
	ET_CHARACTER,
	ET_ID,
	ET_ARRAY_OR_FUNC,
	ET_BOOL,
	ET_EQUAL,
	ET_NOT_EQUAL,
	ET_LESSER,
	ET_GREATER,
	ET_LESSER_EQUAL,
	ET_GREATER_EQUAL,
	ET_PLUS,
	ET_MINUS,
	ET_CONCAT,
	ET_MULT,
	ET_DIV,
	ET_LOGIC_OR,
	ET_LOGIC_AND,
	ET_ASSIGN,
	ET_NOT,
	ET_RANGE_ARR_ATTR,
	ET_LENGTH_ARR_ATTR,
	ET_INDEXER
};

enum VarType
{
	VT_INTEGER,
	VT_FLOAT,
	VT_BOOLEAN,
	VT_STRING,
	VT_CHARACTER,
	VT_ARRAY,
	VT_VOID
};

enum DeclarationType
{
	DT_VARIABLE,
	DT_FUNCTION,
	DT_TYPE
};

enum StmtType
{
	ST_EXPRESSION,
	ST_RETURN,
	ST_NULL,
	ST_WHILE,
	ST_FOR,
	ST_IF,
	ST_ASSIGN
};

struct ProgramList
{
	ProgramBlock *begin;
	ProgramBlock *end;
};

struct ProgramBlock
{
	char *id;

	DeclarationStatementList *declarationSection;
	StatementList *performSection;
	VariableDeclarationList *funcArgs;
	FuncReturnType *returnType;

	ProgramBlock *nextInList;
};

struct FuncReturnType {
	char* typeId;
	enum VarType varType;
};

struct DeclarationStatementList
{
	DeclarationStatement *begin;
	DeclarationStatement *end;
};

struct DeclarationStatement
{
	DeclarationType type;

	DeclarationStmtValue declarationStmtValue;

	DeclarationStatement *nextInList;
};

struct VariableDeclarationList
{
	VariableDeclaration *begin;
	VariableDeclaration *end;
};

struct VariableDeclaration
{
	enum VarType type;
	VariableList *varList;

	// ====== //
	int isType;
    char* typeId;
	// ====== //

	int isArray;
	Range *range;

	VariableDeclaration *nextInList;
};

struct TypeDeclaration 
{
	char *id;
	Range *range;
	enum VarType type;
};

struct VariableList
{
	char *id;

	VariableList *nextInList;
	VariableList *end;
};

struct IfStatement
{
	Expression *condition;
	StatementList *stmtList;
	ElseIfStatementList *elseIfStmtList;
	ElseStatement *elseStmt;
};

struct ElseStatement
{
	StatementList *stmtList;
};

struct ElseIfStatement
{
	Expression *condition;
	StatementList *stmtList;

	ElseIfStatement *nextInList; // statement is part of list
};

struct ElseIfStatementList
{
	ElseIfStatement *begin;
	ElseIfStatement *end;
};

struct ForStatement
{
	char *iterID;
	Range *range;
	StatementList *stmtList;
};

struct Range
{
	Expression *startIndex;
	Expression *lastIndex;
	char *id;
};

struct WhileStatement
{
	Expression *condition;
	StatementList *whileBlock;
};

struct AssigmentStatement
{
	Expression *left;
	Expression *right;
};

struct StatementList
{
	Statement *begin;
	Statement *end;
};

struct Statement
{
	enum StmtType type;

	union StmtValue stmtValue;

	// ----- fix
	////Expression *exprStmt;
	// =======
	char *callFuncId;
	ExpressionList *callFuncArgs;
	// --------

	Statement *nextInList; // statement is part of list
};

struct ExpressionList
{
	Expression *begin;
	Expression *end;
};

struct Expression
{
	enum ExprType type;

	Value value;

	Expression *left;
	Expression *right;

	ExpressionList *exprList; // expression has expression list

	Expression *nextInList; // expression is part of list
};

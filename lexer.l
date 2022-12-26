
%{
#include <stdio.h>

#define YY_DECL int yylex()

#include "tree_nodes.h"
#include "parser.tab.h"
    
%}

%option noyywrap
%option case-insensitive

ID				[a-z]+([_]?[a-z0-9]+)*
LINE_COMMENT	"--".* 

CHARACTER		\'.\'
INTEGER			[0-9]+
FLOAT          	[0-9]*\.{INTEGER}
EXPONENT	 	{FLOAT}[eE][+-]?{INTEGER}
SPACE           [[:space:]]*


%x CONST_STR
%%

%{
	int s = 0;
	char symbol[2];
	char buf[100000];
	memset(buf, '\0', 100000);
%}

integer				{ yylval.vt = VT_INTEGER; return INTEGER; }
boolean				{ yylval.vt = VT_BOOLEAN; return BOOLEAN; }
float				{ yylval.vt = VT_FLOAT; return FLOAT; }
character			{ yylval.vt = VT_CHARACTER; return CHARACTER; }
string				{ yylval.vt = VT_STRING; return STRING; }
array				{ return ARRAY; }
type                { return TYPE; }

procedure           { return PROCEDURE; }
function            { return FUNCTION; }
begin               { return BEGIN_TOKEN; }
end                 { return END; }
is                  { return IS; }
return              { return RETURN; } 
if                  { return IF; }
then                { return THEN; }
elsif               { return ELSIF; }
else				{ return ELSE; }
while               { return WHILE; }
loop                { return LOOP; }
null                { return NIL; }
for                 { return FOR; }
in                  { return IN; } 
and                 { return AND; }
or                  { return OR; }
not                 { return NOT; }
of                  { return OF; } 

Range           { return RANGE; }
Length          { return LENGTH; }

"+"                 { return '+'; }
"-"                 { return '-'; }
"*"                 { return '*'; }
"/"                 { return '/'; }

"="                 { return '='; }
"/="                { return NOT_EQUAL; }
"<"                 { return '<'; }
">"                 { return '>'; }
"<="                { return LESSER_EQUAL; }
">="                { return GREATER_EQUAL; }

":="                { return ASSIGNMENT; }
"&"                 { return '&'; } 
".."                { return DOUBLE_DOT; }

"(" { return '('; }
")" { return ')'; }
"," { return ','; }
";" { return ';'; }
":" { return ':'; }
"'" { return '\''; }

\" { buf[0] = 0; BEGIN(CONST_STR); }
<CONST_STR>[^\"\n]+  { strcat(buf, yytext); }
<CONST_STR>\n  	    { printf("Found error string '%s'\n", buf); BEGIN(INITIAL); }
<CONST_STR>\"\"      { strcat(buf, "\""); }
<CONST_STR><<EOF>>   { printf("Found error string '%s'\n", buf); BEGIN(INITIAL); }
<CONST_STR>\"        { 
						  printf("Found a CONST_STRING - '%s'\n", buf); BEGIN(INITIAL); 
						  yylval.string_const=(char *)malloc(strlen(buf)+1);
						  strcpy(yylval.string_const, buf);
						  return CONST_STRING;
						} 
					
{CHARACTER} 	{ yylval.char_const = *(yytext+1); return CONST_CHARACTER; }

true			{ yylval.int_const = 1; return CONST_BOOL; }
false			{ yylval.int_const = 0; return CONST_BOOL; }

{INTEGER} 		{ yylval.int_const = atoi(yytext); return CONST_INTEGER; }
{FLOAT} 		{ yylval.float_const = atof(yytext); return CONST_FLOAT; }
{EXPONENT} 		{ yylval.float_const = atof(yytext); return CONST_FLOAT; }

{ID} 			{ 
				  yylval.id_const = (char *) malloc(strlen(yytext)+1);
				  strcpy(yylval.id_const, yytext);
				  return ID;
				}

{LINE_COMMENT}	{ }

\n				{ }
{SPACE}			{ }

%%

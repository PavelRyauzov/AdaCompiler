all: parser

parser.tab.c parser.tab.h:	parser.y
	bison -t -v -d parser.y

lex.yy.c: lexer.l parser.tab.h
	flex lexer.l

parser: lex.yy.c parser.tab.c parser.tab.h print_tree.c 
	gcc -o parser print_tree.c parser.tab.c lex.yy.c

clean:
	rm  parser.tab.c lex.yy.c parser.tab.h parser.output

build:
	./parser ./tests/test0.adb  

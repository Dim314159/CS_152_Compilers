#!/bin/bash
flex phase_2.lex
bison -v -d --file-prefix=y phase_2.y
gcc -o parser y.tab.c lex.yy.c -lfl

rm -f lex.yy.c y.tab.* y.output

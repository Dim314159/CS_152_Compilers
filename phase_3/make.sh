#!/bin/bash
flex phase_3.lex
bison -v -d --file-prefix=parser phase_3.yy
g++ -std=c++11 -o parser parser.tab.cc lex.yy.c


gpp_lexer.out:	gpp_lexer.c
		yacc -d gpp_interpreter.y
		lex -o gpp_lexer.c gpp_lexer.l
		cc -g gpp_lexer.c y.tab.c -ll -o gpp_interpreter.out

clean:
		rm gpp_interpreter.out
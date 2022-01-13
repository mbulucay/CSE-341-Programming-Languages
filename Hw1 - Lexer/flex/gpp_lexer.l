/* Global Variables */
%{
    #include<stdio.h>
    #include<stdlib.h>
    #define true 1
    #define false 0
    FILE *fptr;
%}

/*** KeyWords ***/

KW_AND      "and"
KW_OR       "or"
KW_NOT      "not"
KW_EQUAL    "equal"
KW_LESS     "less"
KW_NIL      "nil"
KW_LIST     "list"
KW_APPEND   "append"
KW_CONCAT   "concat"
KW_SET      "set"
KW_DEFFUN   "deffun"
KW_FOR      "for"
KW_IF       "if"
KW_EXIT     "exit"
KW_LOAD     "load"
KW_DISP     "disp"
KW_TRUE     "true"
KW_FALSE    "false"


/*** Operators ***/

OP_PLUS			"+"
OP_MINUS		"-"
OP_DIV			"/"
OP_MULT			"*"
OP_OP 			"("
OP_CP			")"
OP_DBLMULT		"**"
OP_OC 			"""
OP_CC 			"”"
OP_COMMA		","


/*** Types ***/

VALUE 			[-]?[0-9]|[-]?[1-9][0-9]*
ID				[a-zA-Z][a-zA-Z0-9]*
COMMENT			";;".*

%%

(\r\n|\r|\n) {return 0;}
{KW_AND}	{printf("KW_AND\n");}
{KW_OR}		{printf("KW_OR\n");}
{KW_NOT}	{printf("KW_NOT\n");}
{KW_EQUAL}	{printf("KW_EQUAL\n");}
{KW_LESS}	{printf("KW_LESS\n");}
{KW_NIL}	{printf("KW_NIL\n");}
{KW_LIST}	{printf("KW_LIST\n");}
{KW_APPEND}	{printf("KW_APPEND\n");}
{KW_CONCAT}	{printf("KW_CONCAT\n");}
{KW_SET}	{printf("KW_SET\n");}
{KW_DEFFUN}	{printf("KW_DEFFUN\n");}
{KW_FOR}	{printf("KW_FOR\n");}
{KW_IF}		{printf("KW_IF\n");}
{KW_EXIT}	{printf("KW_EXIT\n");}
{KW_LOAD}	{printf("KW_LOAD\n");}
{KW_DISP}	{printf("KW_DISP\n");}
{KW_TRUE}	{printf("KW_TRUE\n");}
{KW_FALSE}	{printf("KW_FALSE\n");}

{OP_PLUS}		{printf("OP_PLUS\n");}
{OP_MINUS}		{printf("OP_MINUS\n");}
{OP_DIV}		{printf("OP_DIV\n");}
{OP_MULT}		{printf("OP_MULT\n");}
{OP_CP}			{printf("OP_CP\n");}
{OP_OP}			{printf("OP_OP\n");}
{OP_DBLMULT}	{printf("OP_DBLMULT\n");}
["] 		{printf("OP_OC\n");}
{OP_CC} 		{printf("OP_CC\n");}
{OP_COMMA}		{printf("OP_COMMA\n");}
[[:space:]]+ 		 /*** ignore space ***/
{COMMENT}		{printf("COMMENT\n");}


[_]?[[:alpha:]][[:alnum:]]*   {printf("IDENTIFIER\n");}
[[1-9][:digit:]]+         {printf("VALUE\n");}
{VALUE}					  {printf("VALUE\n");}


[a-zA-Z]*[,+-/*]	 	  	  {fprintf(stdout,"SYNTAX ERROR %s \n", yytext); } 
[+][A-zA-Z0-9,-/*+]*	 	  {fprintf(stdout,"SYNTAX ERROR %s \n", yytext); }	
[,][A-zA-Z0-9+-/*,]*	 	  {fprintf(stdout,"SYNTAX ERROR %s \n", yytext); }
[-][A-zA-Z0-9+,/*-]*	 	  {fprintf(stdout,"SYNTAX ERROR %s \n", yytext); }	
[/][A-zA-Z0-9+-,*/]*	 	  {fprintf(stdout,"SYNTAX ERROR %s \n", yytext); }
[*][A-zA-Z0-9+-,/*]*	 	  {fprintf(stdout,"SYNTAX ERROR %s \n", yytext); }
[[:digit:]]+[[:alpha:]]  	  {fprintf(stdout,"SYNTAX ERROR %s \n", yytext); }
[[:digit:]]+[,+-/*]   	 	  {fprintf(stdout,"SYNTAX ERROR %s \n", yytext); }
[0][0-9]*				 	  {fprintf(stdout,"SYNTAX ERROR %s \n", yytext); }                       	
(.) 	 				 	  {fprintf(stdout,"SYNTAX ERROR %s \n", yytext); }	


%%

int yywrap(){}

int main(int argc, char **argv){

	argv++, argc--;

	if(argc > 0){ 
		yyin = fopen(argv[0], "r");
        if(yyin == NULL){
            perror("Error opening file: ");
            return 0;
        }

		yylex();
	}
	else{	
		yyin = stdin;
        fprintf(stdout,"$ ");
		while(true) {
			yylex();
		} 
	}
	
	return 0;
}

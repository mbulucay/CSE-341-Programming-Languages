#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define maxEntry 30

int yyerror (char *s);
int yylex();

/*input symbol*/
typedef struct {
	char idntfr[maxEntry]; // key.
	int value;
}input;

/*	Symbol Table */
typedef struct{
	input symbol[maxEntry];
	int inputCount;
}symbols;

/*	Symbol Table */
symbols *allSymbols;

/*
	Creating a new symbols struct to store all defined variable.
*/
void createSymbols(){
	allSymbols = (symbols*) malloc(sizeof(symbols));
	allSymbols->inputCount = 0;						//intial value of valid entry is 0.
}

/*
	Return the value of the symbol.
*/
int get(char id[30]){
	/*
	Searching for the id in the symbol table.
	*/
	for(int i=0;i < (allSymbols->inputCount); ++i){
		int cond = strcmp(allSymbols->symbol[i].idntfr, id);
		if(cond == 0) 	//if found get the value
			return allSymbols->symbol[i].value;
	}

	printf("Symbol not found in the among symbols!\n Exiting...");
	exit(-1);
}

/*
	Add the symbol to the symbol table or update it.
*/
void put(char identifier[30], int value){

	/* create new entry for checking */
	input *entry = (input*) malloc(sizeof(input));
	entry->value = value;

	/* if the entry is already in the table, update it. */
	strcpy(entry->idntfr, identifier);
	for(int i = 0;i<allSymbols->inputCount; ++i){
		int cond = strcmp(allSymbols->symbol[i].idntfr, entry->idntfr);
		if( cond == 0 ){ 								// update variable value
			allSymbols->symbol[i].value = entry->value; 
			return;
		}
	}

	/*
	If not found add the new variable to the symbol table.Insert new variable to the table
	*/
	int index = allSymbols->inputCount;	
	strcpy(allSymbols->symbol[index].idntfr, entry->idntfr); 

	/* add the new variable to the table */
	allSymbols->symbol[index].value = entry->value; 
	++(allSymbols->inputCount);
	free(entry);
}

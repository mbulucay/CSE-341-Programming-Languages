%{

#include <stdio.h>
#include <string.h>
#include "gpp_interpreter.h"

int* add(int*, int);
int* concatenate(int*, int*);
int powerOf(int, int);
void print(int*);

%}

%union{
    int idValue;
    int *idtfvalues;
    char id[50];
};

%start INPUT

/* Defined tokens in lexer */
%token COMMENT OP_PLUS OP_MINUS OP_DIV OP_MULT OP_DBLMULT 
%token OP_OP OP_CP OP_OC OP_CC OP_COMMA KW_TRUE KW_FALSE
%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS
%token KW_NIL KW_LIST KW_APPEND KW_CONCAT 
%token KW_SET KW_DEFFUN KW_FOR KW_IF KW_EXIT
%token KW_LOAD KW_DISP NEWLINE STRING 


/* Defined tokens in parser */
%token <idValue> VALUE
%token <id> ID

%type <idValue> INPUT
%type <idValue> EXPI
%type <idValue> EXPB
%type <idtfvalues> VALUES
%type <idtfvalues> EXPLISTI
%type <idtfvalues> LISTVALUE

%%
/*  
    •START -> INPUT
    •INPUT -> EXPI| EXPLISTI 
*/
INPUT: 
    EXPI {printf("SYNTAX OK. \nResult = %d\n", $1);}
    |
    EXPLISTI {printf("SYNTAX OK. \nResult = "); print($1);}
    ;

/* 
    CFG grammer for the gpp programming language
    as in pdf file

    EXPI-> (set Id EXPI) 
    EXPI -> (+ EXPI EXPI) | (-EXPI EXPI) | (* EXPI EXPI) |(/ EXPI EXPI) | Id | IntegerValue| (Id EXPLISTI) 
*/
EXPI:
    OP_OP OP_MULT EXPI EXPI OP_CP  {$$=$3*$4;}  // (* EXPI EXPI) 
    |
    OP_OP OP_DIV EXPI EXPI OP_CP   {$$=$3/$4;}  // (/ EXPI EXPI) 
    |
    OP_OP OP_PLUS EXPI EXPI OP_CP  {$$=$3+$4;}  // (+ EXPI EXPI) 
    |
    OP_OP OP_MINUS EXPI EXPI OP_CP {$$=$3-$4;}  // (- EXPI EXPI) 
    |
    OP_OP OP_DBLMULT EXPI EXPI OP_CP {$$ = powerOf($3, $4);}
    |
    VALUE {$$ = $1;}
    |
    ID {$$ = get($1);}
    |
    OP_OP KW_SET ID EXPI OP_CP {$$ = $4; put($3, $4);}              // (set Id EXPI)
    |
    OP_OP KW_IF EXPB EXPI OP_CP {$$ = (1 == $3) ? $4: 0;}           // (if EXPB EXPI) 
    |
    OP_OP KW_IF EXPB EXPI EXPI OP_CP {$$ = (1 == $3) ? $4: $5;}     // (if EXPB EXPI EXPI)
    |
    OP_OP KW_FOR EXPB EXPI OP_CP { $$ = (1 == $3) ? $4 : 0; }       // (for EXPB EXPI)
    |
    OP_OP KW_DISP EXPI OP_CP { $$ = $3; printf("Disp: %d\n", $3);} 
    ;

/* 
    EXPB -> (and EXPB EXPB) | (or EXPB EXPB) | (not EXPB) | (equal EXPB EXPB) | (equal EXPI EXPI) |  BinaryValue 
*/
EXPB:
    KW_TRUE  { $$ = 1; }   // true
    |
    KW_FALSE   { $$ = 0; } // false
    | 
    OP_OP KW_AND EXPB EXPB OP_CP {$$ = $3 && $4;}   // (and EXPB EXPB) 
    |
    OP_OP KW_OR EXPB EXPB OP_CP  {$$ = $3 || $4;}    // (or EXPB EXPB) 
    |
    OP_OP KW_NOT EXPB OP_CP  {$$ = ! ($3);}         // (not EXPB)
    |
    OP_OP KW_LESS EXPI EXPI OP_CP { $$ = $3 < $4; } // (less EXPI EXPI) 
    |
    OP_OP KW_EQUAL EXPB EXPB OP_CP {$$ = ($3 == $4);}  // (equal EXPB EXPB) 
    |
    OP_OP KW_EQUAL EXPI EXPI OP_CP {$$ = ($3 == $4);}  // (equal EXPI EXPI)
    |
    OP_OP KW_DISP EXPB OP_CP { $$ = $3; printf("Disp: %s\n", ($3 ? "True":"False"));}
    ;

/* 
    EXPLISTI-> (concatEXPLISTIEXPLISTI) | (append EXPIEXPLISTI) | LISTVALUE| null 
*/
EXPLISTI:
    OP_OP KW_APPEND EXPI EXPLISTI OP_CP {$$ = add($4, $3);}
    |
    OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP {$$ = concatenate($3, $4);}
    |
    OP_OP KW_LIST VALUES OP_CP {$$ = $3;}
    |
    LISTVALUE  {$$ = $1;}
    |
    OP_OP KW_DISP LISTVALUE OP_CP { $$ = $3; printf("Disp: "); print($3);}
    ;

/* 
    LISTVALUE -> ( VALUES ) | () | nil 
*/
LISTVALUE:  
    OP_OP VALUES OP_CP {$$ = $2;}
    |
    OP_OP OP_CP { $$= NULL; }
    |
    KW_NIL { $$ = NULL;}
    ;

/* 
    VALUES -> VALUES IntegerValue | IntegerValue 
*/
VALUES: 
    VALUES VALUE  {$$ = add($1, $2);}
    |
    VALUE {$$ = NULL; $$ = add($$, $1);}
    ;

%%

/* ERROR messages */
int yyerror(char *s) {
    printf("SYNTAX ERROR. Expression not recognized\n");
    exit(-1);
}

/* Result */
void print(int *darray){

    printf("( ");
    for(int i=0; *(darray+i)!=-1; ++i)
        printf("%d ", *(darray+i));
    printf(")\n");
}

/* Function for adding new elements to the list */ 
int* add(int *darray, int num){    
    int *temp = darray;
    int size = 0, newSize = 0;     
    
    /* If the list is empty */
    if(temp == NULL){
        darray = (int *) malloc(sizeof(int)*2);
        *darray = num;
        *(darray+1) = -1;
        return darray;     
    } 
    
    /* If the list is not empty */
    while(*temp != -1)
        ++temp, ++size;

    /* If the list is full */
    temp = darray;
    darray = (int*)(malloc(sizeof(int) * (size+2)));
    for(int idx = 0 ; idx<size; ++idx)
        darray[idx] = temp[idx];
                  
    /* Adding the new element */
    darray[size] = num;                   
    darray[size+1] = -1;                  

    return darray;     
}

/*
 * This function is used to concatenate two lists.
 * It returns the new list.
 */
int* concatenate(int *darray1, int *darray2){
    int* tmp;
    int length_1=0, 
        length_2=0,
        idx = 0;
    /* Find the length of the first array */
    tmp = darray1;
    while(*tmp != -1)
        length_1++, tmp++;

    /* Find the length of the second array */
    tmp = darray2;
    while(*tmp != -1)
        length_2++, tmp++;

    /* Open new space for the concatenated array */
    tmp = (int *) malloc(sizeof(int) * (length_1 + length_2) + 3);

    /* Copy the first array into the new array */
    for(idx;idx<length_1;++idx)          
        tmp[idx] = darray1[idx];
    /* Copy the second array into the new array */
    for(int j = 0;j<length_2; ++j) 
        tmp[idx++] = darray2[j]; 

    /* sign for the end of the array */
    tmp[idx] = -1;

    return tmp;
}

/* Function to calculate power of a number */
int powerOf(int b, int power) {
    if (power == 0)
        return 1;        
    return (b * powerOf(b, power - 1));
}

int main(int argc, char **argv)
{
    printf("Enter the expression: ");
    ++argv, --argc;
    createSymbols();

    while(1){
        yyparse();
    }

    return 0;
}
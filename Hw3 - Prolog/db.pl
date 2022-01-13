/*
    Facts
*/

/* % trace. */


loves('romeo', 'julia').
loves('julia','martin').

/*julia romeo yu seviyor ancak romeo julia yi sevmeli
    yani
    romeo julia yi sevdigi surece julia da romeoyu seviyor
*/
loves('julia', 'romeo') :- loves('romeo', 'julia').  

loves('julia', 'hatir') :- loves('romeo', 'hatir').  

/*========================================*/

/*
    listing(male)
    male(X), female(Y). ;;
*/

/*========================================*/

/*
male(ali).
male(veli).
male(osman).

female(ayse).
female(zeynep).
female(rabia).


wife(ali,rabia).
parent(osman,ali).
parent(ali, veli).
parent(ali, ayse).
parent(rabia, veli).
parent(rabia, ayse).
parent(veli, zeynep).

get_grandparent(X, Y) :-
    parent(X, Z),
    parent(Z, Y),
    format("~w ~w ~w ", [X, Z, Y]).

get_grandchild :-
    parent(osman, X), parent(X, Y),
    write('osman '), write(Y), write('\' nin dedesi'),nl.

get_parent :-
    parent(X,veli), parent(X, ayse),
    format("~w ~s ~n", [X, "ayse ve velinin velisi"]).

*/

/*

?- parent(X,veli).
X = ali ;
X = rabia.

?- parent(X,veli), male(X).
X = ali ;
false

?- parent(Y, veli) , parent(X, Y).
Y = ali,
X = osman ;
false.

?- parent(Y, veli) , parent(X, Y), female(Y).
false.

?- parent(Y, veli) , parent(X, Y), male(Y).
Y = ali,
X = osman ;
false.

?- parent(osman, X), parent(X, Y).
X = ali,
Y = veli ;
X = ali,
Y = ayse.

?- parent(X,veli), parent(X,ayse), female(X).
X = rabia

?- get_grandparent(A, veli).
A = osman ;
false.

*/

/*========================================*/


/*
    Rules
    eldeki factlerin birlesmesi veya kosula baglanmasi ile yeni kosul olusturma 
*/

hates(smith,X) :- happy(X).

happy(albert).
happy(alice).
happy(bob).
happy(bill).
with_albert(alice).
near_water(john).

runs(albert) :- happy(albert).

dance(alice) :-
    happy(alice), happy(albert),
    with_albert(alice).

does_alice_dance(alice) :- dance(alice),
    write('evet albert ile dans ediyor ikiside mutlu').

/*

?- hates(smith,A).
A = albert ;
A = alice ;
A = bob ;
A = bill.
*/

/*and ile baglar*/
swims(bob) :-
    happy(bob),
    near_water(bob).

swims(john) :-
    happy(john).

swims(john) :-
    near_water(john).
    
/*========================================*/

person(X) :- male(X).
person(X) :- female(X).

/*========================================*/

what_grade(5) :-
  write('Go to kindergarten').
what_grade(6) :-
  write('Go to first grade').
what_grade(Other) :-
  Other > 6,
  Grade is Other - 5,
  format('Go to grade ~w', [Grade]).

/*========================================*/



has(alice, pet(cat, olive)).

customer(tom, smith, 20.55).
customer(sally, smith, 120.55).

get_customer_bal(FName,LName) :-
    customer(FName, LName, Bal),
    format("~w ~w has ~w dolars", [FName, LName, Bal]).

/*
?- has(alice, pet(cat,X)).
X = olive.

?- customer(sally, _, Bal).
Bal = 120.55

?- get_customer_bal(tom,smith).
tom smith has 20.55 dolars
true.

؛?- get_customer_bal(sally,smith).
sally smith has 120.55 dolars
true

*/

vertical(line(point(X, Y), point(X, Y2))).
horizontal(line(point(X, Y), point(X2, Y))).

/*
?- vertical(line(point(5,13), point(5, 21))).
true.

?- vertical(line(point(5,13), point(6, 21))).
false.

?- vertical(line(point(5,13), point(X, 21))).
X = 5.

?- vertical(line(point(5,5), point(5, 5))).
true.

̀?- horizontal(line(point(5,5), point(5, 5))).
true.

?- horizontal(line(point(5,5), point(21, Y))).
Y = 5.

?- horizontal(line(point(13,5), X)).
X = point(_, 5).

?- vertical(line(point(13,5), X)).
X = point(13, _).

*/

/*
https://stackoverflow.com/questions/8951321/compound-boolean-expressions-in-prolog

As noted by others, your original example

test(A, B, C, D) :- cond(A), cond(B); cond(C), cond(D).
is perfectly valid (assuming the parse is as you intended it). Didn't you try it?

A Primer

Logical AND

foo :- a , b .
Logical OR

foo :- a ; b .
Combined

foo :- a , b ; c , d .
The above parses as:

foo :- ( a , b ) ; ( c , d ) .
Use parentheses to indicate a different desired binding:

foo :- a , ( b ; c ) , d .
Even better, eschew the ; OR operator and break alternatives up into separate clauses. Sequence is much easier for people to comprehend than branching tree structures. Breaking alternation up into multiple clauses simplifies testing/debugging and improves comprehension. Therefore, prefer

foo :- a , b .
foo :- c , d .
over

foo :- a , b ; c , d .
and prefer

foo :- a , bar , d .

bar :- b .
bar :- c .
over

foo :- a , ( b ; c ) , d .
Possibly most importantly, breaking things up into multiple clauses like this makes later maintenance easier. With a structure like:

foo :- a , b ; c , d .
what do you do when you add another case? What about when it expands to 50 alternatives?

Each additional alternative increases the number of code paths through the clause, thus making testing and comprehension more difficult. To get complete code coverage in testing, a lot of alternative paths must be individually tested.

With the equivalent structure

foo :- a , b .
foo :- c , d .
Adding alternatives is simply a matter of adding additional clause or clauses, each of which can be tested in isolation.

A professional programmer writes first for the people who will, several years down the line, need to comprehend, change and fix that code (hint: that person might be yourself).


5*5 =:= 25;12 = 5 + 4.
true ;
false

*/


get_male(X) :-
    male(X),
    write(X).

/*

% alice = alice. = yes
 
% 'alice' = alice. = yes (Prolog considers these to be the same)
 
% \+ (alice = albert). = yes (How to check for not equal)
 
 
% 3 > 15. = no
 
% 3 >= 15. = no
 
% 3 =< 15. = yes
 
 
% W = alice. = yes
 
% This says that we can assign the value of alice to W and not that
 
% W is equal to alice
 
 
% Rand1 = Rand2. = yes
 
% This says that any variable can be assigned anything and one of
 
% those things is another variable
 
 
% If variables can be matched up between 2 complex terms and the
 
% functors are equal then the complex terms are equal
 
% rich(money, X) = rich(Y, no_debt).

?- a(X) = b(Y).
false.

?- rich(a,_) = rich(a,b).
true.

?- rich(a,_) = rich(y,n).
false.

?- rich(a,X) = rich(y,n).
false.

?- rich(a,X) = rich(Y,n).
X = n,
Y = a.

?- rich(_,X) = rich(Y,n).
X = n.

?- rich(_,_) = rich(Y,n).
true.

?- rich(a,_) = rich(Y,n).
Y = a.

?- \+ (5 = 5).
false.

?- \+5 = 5.
false.

?- 5 \= 5.
false

*/


/* Recursive */


parent(albert, bob).
parent(albert, betsy).
parent(albert, bill).


parent(alice, bob).
parent(alice, betsy).
parent(alice, bill).


parent(bob, carl).
parent(bob, charlie).


related(X, Y) :-
    parent(X, Y).

related_grand(X, Y) :-
    parent(X, Z),
    related(Z, Y).

related_sib(X, Y) :-
    parent(A, X) , parent(A, Y).

/*
MATH


% ---------- MATH ----------
 
% Prolog provides 'is' to evaluate mathematical expressions
 
% X is 2 + 2. = X = 4
 
 
% You can use parenthese
 
% X is 3 + (2 * 10). =  X = 23
 
 
% You can also make comparisons
 
% 50 > 30. = yes
 
% (3*10) >= (50/2). = yes
 
% \+ (3 = 10). = yes (How to check for not equal)
 
% 5+4 =:= 4+5. = yes (Check for equality between expressions)
 
% 5+4 =\= 4+5. = yes (Check for non-equality between expressions)
 
% 5 > 10 ; 10 < 100. (Checks if 1 OR the other is true)
 
 
% X is mod(7,2). = X = 1 (Modulus)
 
 
double_digit(X,Y) :- Y is X*2.
% double_digit(4,Y). = Y = 8
 
% Take the 1st argument, multiply it times 2 and return it as the
 
% 2nd argument
 
 
% Get random value between 0 and 10
 
% random(0,10,X).
 
 
% Get all values between 0 and 10
 
% between(0,10,X).
 
 
% Add 1 and assign it to X
 
% succ(2,X).
 
 
% Get absolute value of -8
 
% X is abs(-8).
 
 
% Get largest value
 
% X is max(10,5).
 
 
% Get smallest value
 
% X is min(10,5).
 
 
% Round a value
 
% X is round(10.56).
 
 
% Convert float to integer
 
% X is truncate(10.56).
 
 
% Round down
 
% X is floor(10.56).
 
 
% Round up
 
% X is ceiling(10.56).
 
 
% 2^3
 
% X is 2** 3.
 
 
% Check if a number is even
 
% 10//2 = 5 (is 10 = 2 * 5)
 
is_even(X) :- Y is X//2, X =:= 2 * Y.
 
% sqrt, sin, cos, tan, asin, acos, atan, atan2, sinh, cosh, tanh,
 
% asinh, acosh, atanh, log, log10, exp, pi, e

*/


say_my_name :-
    write("enter your name: "),
    read(Name),
    write("Your name"),
    write(Name).


fav_char :-
    write("enter fav char: "),
    get(C),
    format("it is ascii val ~w ", [C]),
    put(C).
/*
# write_file(File, Text) :-
#     open(File, write, Stream),
#     write(Stream, Text), nl,
#     close(Stream).
*/


opp(A,B) :-
    write([A|B]).


before(X, Y, L):-
  append(_, [X|Tail], L),
  append(_, [Y|_], Tail).


count_10(10) :- nl.

count_10(X) :-
    Y is X + 1,
    write(Y), nl,
    count_10(Y).




% Use recursion to loop
 
count_to_10(10) :- write(10), nl.
 
count_to_10(X) :-
  write(X),nl,
  Y is X + 1,
  count_to_10(Y).
/* Receives Low (lowest value) and High (highest value) */
 
count_down(Low, High) :-
  % Assigns values between Low and High to Y
  between(Low, High, Y),
  % Assigns the difference to Z
  Z is High - Y,
  write(Z),nl,
  % Continue looping until Y = 10
  Y = 10.
 
count_up(Low, High) :-
  between(Low, High, Y),
  Z is Y + Low,
  write(Z), nl,
  Y = 10.
 
% Loop until they guess a number
 
% start is a dummy value used to start the looping
 
guess_num :- loop(start).
 
% When they guess 15 they execute this message and exit
 
loop(15) :- write('You guessed it!').
 
loop(X) :-
  x \= 15,
  write('Guess Number '),
  read(Guess),
  write(Guess),
  write(' is not the number'), nl,
  loop(Guess).




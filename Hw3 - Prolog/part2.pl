
/* Begin Facts */

flight(gaziantep, van, 3).

flight(van, gaziantep, 3).
flight(van, ankara, 4).

flight(ankara, rize, 5).
flight(ankara, van, 4).
flight(ankara, diyarbakir, 8).
flight(ankara, izmir, 6).
flight(ankara, istanbul, 1).

flight(diyarbakir, antalya, 4).
flight(diyarbakir, ankara, 8).

flight(antalya, diyarbakir, 4).
flight(antalya, izmir, 2).
flight(antalya, erzincan, 3).

flight(erzincan, antalya, 3).
flight(erzincan, canakkale, 6).

flight(canakkale, erzincan, 6).

flight(izmir, antalya, 2).
flight(izmir, ankara, 6).
flight(izmir, istanbul, 2).

flight(istanbul, izmir, 2).
flight(istanbul, ankara, 1).
flight(istanbul, rize, 4).

flight(rize, ankara, 5).
flight(rize, istanbul, 4).

/* End Facts */

/* Begin Rules */

%
route(X , Y , C) :-  
    total_cost(X , Y , C , []) ; flight(X, Y, Z).

%Making recursive operation like newrote = currentrote + flight to make all possible 
%routes departure to arrived location
total_cost(Dept , Arr , Cost , _) :- 
    flight(Dept , Arr , Cost).

%Calculating cost off all routes from one city to another
%checking location we passed it or not passed it and calcualting cost as recursively
total_cost(Dept , Arr , Cost, Locs) :-  
    \+ member(Dept , Locs)  , 
    flight(Dept , TP , NewCost) ,
    total_cost(TP , Arr , Sum , [Dept | Locs]) , 
    Dept \= Arr,  Cost is (NewCost + Sum).

/* End Rules */

% Define the set of all natural numbers.
nat(zero).
nat(succ(X)) :- nat(X).

% Define some named predicates to make referring to specific numbers easier.
one(succ(zero)).
two(succ(X))   :- one(X).
three(succ(X)) :- two(X).
four(succ(X))  :- three(X).
five(succ(X))  :- four(X).
six(succ(X))   :- five(X).

% Less than or equal to.
lte(zero,Y) :- nat(Y).
lte(succ(X),succ(Y)) :- lte(X,Y).

% Addition.
plus(zero,Y,Y) :- nat(Y).
plus(succ(X),Y,succ(Z)) :- plus(X,Y,Z).

% Subtraction.
minus(X,Y,Z) :- plus(Y,Z,X).

% Multiplication.
times(zero,_,zero).
times(succ(X),Y,Z) :- plus(T,Y,Z), times(X,Y,T).

% Division.
divide(X,Y,Z) :- times(Y,Z,X).

% Sum of the numbers in a list.
sum([],zero).
sum([H|T],Sum) :- plus(H,SumT,Sum), sum(T,SumT).

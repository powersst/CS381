% Christian Mello (melloc), Steven Powers (powersst), Erik Watterson (watterse)

% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X,Y) :- parent(Y,X).

% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- female(X),parent(X,_).
isFather(X) :- male(X),parent(X,_).

% 3. Define a predicate `grandparent/2`.
grandparent(X,Z) :- parent(X,Y), parent(Y,Z).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X,Y) :- parent(P,Y), parent(P,X), X\=Y.

% 5. Define two predicates `brother/2` and `sister/2`.
brother(X,Y) :- male(X), sibling(X,Y).
sister(X,Y) :- female(X), sibling(X,Y).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(X,Z) :- sibling(X,Y), married(Y,Z).
siblingInLaw(X,Z) :- married(X,Y), sibling(Y,Z).

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt(X,Z) :- female(X), sibling(X,Y), parent(Y,Z).
aunt(X,Z) :- female(X), siblingInLaw(X,Y), parent(Y,Z).
uncle(X,Z) :- male(X), sibling(X,Y), parent(Y,Z).
uncle(X,Z) :- male(X), siblingInLaw(X,Y), parent(Y,Z).

% 8. Define the predicate `cousin/2`.
cousin(X,Z) :- aunt(Y,X), parent(Y,Z).
cousin(X,Z) :- uncle(Y,X), parent(Y,Z).

% 9. Define the predicate `ancestor/2`.
 ancestor(X,Y) :- parent(X,Y).
 ancestor(X,Y) :- parent(P,Y), ancestor(X,P).

% Extra credit: Define the predicate `related/2`.
related(X, Y) :- parent(Z, X), married(Z, Y);
                 parent(Y, X);
                 parent(Z, Y), married(Z, X);
                 parent(X, Y);

                 (sibling(X, Y); sibling(Y, X));

                 (siblingInLaw(X, Y); siblingInLaw(Y, X));
                 (siblingInLaw(X, Z), parent(Y, Z));
                 (siblingInLaw(Z, Y), parent(X, Z));

                 (siblingInLaw(X, Z), parent(GP, Z), parent(GP, Y));
                 (siblingInLaw(X, Z), parent(GP, Z), parent(GP, C), parent(C, Y));
                 (siblingInLaw(Z, Y), parent(GP, Z), parent(GP, X));
                 (siblingInLaw(Z, Y), parent(GP, Z), parent(GP, C), parent(C, X));

                 (uncle(X, Y); uncle(Y, X));
                 (aunt(X, Y); aunt(Y, X));

                 cousin(X, Y);
                 cousin(Y, X);

                 (grandparent(Y, X); grandparent(X, Y));
                 cousin(GP, Y), grandparent(GP, X);
                 cousin(X, GP), grandparent(Y, GP);
                 cousin(Y, GP), grandparent(X, GP).

%%
% Part 2. Language implementation
%%

% 1. Define the predicate `cmd/3`, which describes the effect of executing a
%    command on the stack.
checkType(X) :- number(X).
checkType(X) :- string(X).
checkType(X) :- bool(X).

bool(t).
bool(f).


cmd(C,S1,S2) :- checkType(C), S2 = [C|S1].
cmd(C, [H,H2|T], S2):- C == add, D is H+H2, S2 = [D|T].
cmd(C, [H,H2|T], S2)   :- C == lte, H =< H2, D = t, S2 = [D|T].
cmd(C, [H,H2|T], S2)   :- C == lte, H > H2, D = f, S2 = [D|T].
cmd(if(P1,_),[H1|T],S2) :- H1 == t, S3 = P1,  prog(S3,T,S2).
cmd(if(_,P2),[H1|T],S2) :- H1 == f, S3 = P2,  prog(S3,T,S2).

% 2. Define the predicate `prog/3`, which describes the effect of executing a
%    program on the stack
prog([], S1, S2) :- S1 = S2.
prog([H|T], S1, S2) :- cmd(H, S1, X), prog(T, X, S2).

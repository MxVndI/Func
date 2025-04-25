father(X, Y)  :- man(X), dite(Y, X).
fathers(X) :- dite(X, Y), man(Y), write(Y). 

parent(X, Y) :- dite(Y, X).

sister(X, Y) :- woman(X), X\=Y, dite(X,Z), dite(Y,Z).
sister(X) :-  dite(X,Z), dite(Y,Z), Y\=X, woman(Y), write(Y).

brother(X, Y) :- man(X), X \= Y, dite(X, P), dite(Y, P).

grand_so(X, Y) :- dite(X, Z), dite(Z,Y).
grand_so(X) :- dite(Y, X), dite(Z, Y), write(Z).

grand_pa_and_son(X, Y) :- grand_so(X, Y); grand_so(Y, X).

plemyanik(X, Y) :- man(X), dite(X, Z), (sister(Z, Y); brother(Z, Y)), X \= Y.
plemyanik(X) :- man(X), dite(X, Z), (sister(Z, Y); brother(Z, Y)), X \= Y, write(Y).
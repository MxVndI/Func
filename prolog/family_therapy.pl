father(X, Y)  :- man(X), dite(Y, X), false.
fathers(X) :- dite(X, Y), man(Y), write(Y), false.

sister(X, Y) :- woman(X), X\=Y, dite(X,Z), dite(Y,Z).
sister(X) :-  dite(X,Z), dite(Y,Z), Y\=X, woman(Y), write(Y).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Ejercicio 1
% Duplica
% Recibe dos listas, y es true si los elementos de la primera 
% estan en la segunda duplicados

% Caso base : dos listas vacías son iguales 
duplica([],[]).

% Solamente le pasamos el primer elemento dos veces a la
% función, llamándola con el resto de la lista y hasta que se vacíe
duplica([X|L],[X, X|L1]) :- duplica(L, L1).

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejercicio 2
% Invierte
% Recibe dos listas y se satisface cuando la segunda contiene los
% elementos de la primera en orden inverso
% Hacemos uso del predicado concatena
concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :- concatena(L1, L2, L3).

% Caso base : dos listas vacías
invierte([], []).

% Invertimos el resto de la primera lista y lo concatenamos
% en L con su primer elemento dado la vuelta
invierte([H|T], L) :- invierte(T, R), concatena(R, [H], L).

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejercicio 3
% Palíndromo
% Recibe una lista y se satisface cuando es un palindromo, es
% decir, se lee de la misma manera de izquierda a derecha que
% de derecha a izquierda
% 
% Solamente hay que comprobar que L coincide con ella misma invertida
palindromo(L) :- invierte(L, L).

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejercicio 4
% Divide
%  Recibe 4 argumentos:
%  		L : una lista de elementos
%  		N : un numero
%  		L1 : lista que debe contener los N primeros elem de L
%  		L2 : lista que contiene el resto de elem de L
% Se satisface cuando la lista L1 contiene los primeros N 
% elementos de L y L2 contiene el resto.
% 
% La longitud de L1 debe ser N y L es la concatenación de L1 y L2
divide(L, N, L1, L2) :- length(L1, N), concatena(L1, L2, L).

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejercicio 5
% Aplasta
% Recibe dos listas y se satisface cuando la segunda lista es 
% una versión aplastada de la primera

% Caso base : dos listas vacías ya no se pueden aplastar más 
aplasta([], []) :- !.

% Si T es una lista, aplastamos T en L2 y H en L3, concatenándolas
% después en L1
aplasta([T|H], L1) :- is_list(T), aplasta(T, L2), 
    				  aplasta(H, L3), concatena(L2, L3, L1).

% Si T no es una lista, es decir, es un elemento, y es el primer
% elemento de L, solamente aplastamos H en L 
aplasta([T|H], [T|L]) :- \+ is_list(T), aplasta(H, L). 
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejercicio 6
% Primos
% Recibe un número N y una lista L
% Se satisface cuando L contiene los factores primos de N
next_factor(_, 2, 3) :- !.
next_factor(N, F, NF) :-  F < N, NF is F + 2.

% Caso base : cuando N es 1 ya no podemos dividirlo más
divisores(1, [], _) :- !.

% Si F es divisor de N, lo mete en L, y calcula los divisores
% de N/F
divisores(N, [F|L], F) :- 0 is mod(N, F),
    				  N2 is N/F, divisores(N2, L, F).

% Si F no es divisor de N, calcula el siguiente factor despues 
% de F (NF) y mira si es divisor de N
divisores(N, L, F) :- \+ 0 is mod(N, F), next_factor(N, F, NF),
    				  divisores(N, L, NF).

% Función principal que empieza probando el 2 como divisor
primos(N, L) :- N > 0, divisores(N, L, 2). 


 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejercicio 7.1
% cod_primero(X, L, Lrem, Lfront)
% 
% Recibe un número X, repetido o no en la lista L, de modo
% que Lfront contiene X y sus repeticiones en L, y Lrem 
% contiene el resto de L

% Caso base : L y Lrem están vacías y Lfront solo contiene X
cod_primero(X, [], [], [X]).

% Si X es igual que P lo metemos en Lfront y llamamos a la función
% recursivamente con el resto de L (F)
cod_primero(X, [P|F], Lrem, [T|Lfront]) :- X = P, T = P,
    									cod_primero(X, F, Lrem, Lfront).

% El otro caso posible es que X sea distinto de P, entonces hemos 
% acabado. Llamamos a Lrem lo que quede de L y metemos la X en Lfront
cod_primero(X, [P|F], Lrem, [T]) :- \+ X = P , Lrem = [P|F], T = X.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejercicio 7.2
% cod_all(L, L1)
% Recibe dos listas : 
% 	- L es una lista de números repetidos o no, por ejemplo [1, 2, 2, 3]
% 	- L1 es una lista de listas, en la que cada sublista se forma por 
% 	  la repetición de los números, por ejemplo [[1], [2,2],[3]]

% Caso base : dos listas vacías son iguales 
cod_all([],[]).

% Con cada elemento de la primera lista, llamamos a cod_primero, metiendo
% cada lista Lfront en L1
cod_all([P|L],[T|L1]) :- cod_primero(P, L, Lrem, Lfront), T = Lfront, cod_all(Lrem, L1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 7.3
% run_length(L, L1)
% Recibe dos listas :
% 	- L es una lista de números repetidos o no, por ejemplo [1, 2, 2, 3]
% 	- L1 es la lista de sublistas que contiene cuántas veces aparece
% 	  cada dígito de la lista, por ejemplo, [[1,1], [2,2], [1,3]]

% Caso base : dos listas vacías
aux([], []).

% Función auxiliar que descompone una lista de listas en el tamaño y 
% el elemento de la lista
aux([[P|R]|F], [P1|F1]) :- length([P|R], N), P1 = [N, P], aux(F, F1).

% Función principal que llama a cod_all pasándole su resultado
% a la función auxiliar que la descompone
run_length(L, L1) :- cod_all(L, L2), aux(L2, L1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 8
% 
% build_tree(L, T)
% Construye el árbol T a partir de la lista L ordenada
% siendo L (letra-numero, ...)

% Caso Base : solo queda un elemento en el arbol
build_tree([P-_], tree(P, nil, nil)).

% Función Principal : realiza un build_tree de la lista sin el primer elemento
% y asocia T a un árbol donde el primer elemento es el de la izquierda y el 
% elemento devuelto por la función anterior es el de la derecha
build_tree([P-_|F], T) :- build_tree(F, T2), T = tree(1, tree(P, nil, nil), T2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 8.1
% 
% encode_elem(x1, x2, Tree)
% Codifica el elemento x1 en el elemento x2 a partir de la
% información que aporta el árbol

% Caso Base : Si el elemento x1 es el de más a la izquierda añadimos un 0 a la lista
encode_elem(X1, X2, tree(1, tree(X1, nil, nil), _ )) :- concatena([0], [], X2).

% Caso Base : Si el elemento x1 es el de más a la derecha concatenamos un 1
encode_elem(X1, X2, tree(1, _ , tree(X1, nil, nil))) :- concatena([1], [], X2).

% Función Principal : recursivamente vamos codificando el elemento x1 en uno auxiliar y
% vamos guardando el resultado en el elemento x2
encode_elem(X1, X2, tree(1, _ , T)) :- encode_elem(X1, X3, T), concatena([1], X3, X2).
            

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 8.2
% 
% encode_list(L1, L2, Tree)
% Codifica la lista L1 en la lista L2 a partir de la información 
% que aporta el árbol

% Caso Base : las dos listas están vacías luego no hay más que codificar
encode_list([], [], _).
            
% Función Principal : recursivamente vamos codificando el primer elemento de L1 
% en el primer elemento de L2 hasta que estén vacías
encode_list([P|F], [P2|F2], T) :- encode_elem(P, P2, T), encode_list(F, F2, T).
            
            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 8.3
% 
% encode(L1, L2)
% Codifica la lista L1 en la lista L2 a partir de un diccionario
dictionary([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FUNCION ORDENAR(L1, L2) -> ordena L1 en L2 de mayor a menor
% Caso Base : las dos listas están vacías
ordenar([],[]):- !.

% Función Principal : insertamos de mayor a menor el primer elemento
% de L1 y ordenamos recursivamente el resto de la lista
ordenar([P|F], L):- ordenar(F, L1), mayor(P, L1, L), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FUNCION MAYOR(P, L, L1) -> inserta P en la lista L ordenado de mayor en L1
% Caso Base : Solo hay un elemento en L1 y L está vacía
mayor(P, [], [P]):- !.

% Si P es menor que P1 se inserta en L y L2 y se llama de nuevo al mayor
% con el resto de la lista L1
mayor(P, [P1|L1], [P1|L2]):- P @< P1, !, mayor(P, L1, L2).

% Se concatena P y L en L1
mayor(P, F, [P|F]):- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FUNCION DECODIFICA(L1, L2) -> decodifica L1 en L2, es decir, sea N un número
% y sea L una letra, las dos listas serán:
% L1 = [[N, L], [N1, L1]...]
% L2 = [L-N, L1-N1...]

% Caso Base : ambas listas están vacías 
decodifica([], []).

% Función Principal : Se decodifica el primer par de ambas listas y se llama
% recursivamente a la función hasta que estén vacías
decodifica([[Num, Let] | F], [Let-Num | F2]) :- decodifica(F, F2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FUNCION PERTENECE(L, D) -> es true si todos los elementos de L pertenecen a D
% Caso Base : L está vacía
pertenece([], _).

% Función Principal : recursivamente comprobamos que el primer elemento de L es
% miembro de D y llamamos a pertenece con el resto de la lista
pertenece([P|F], D) :- member(P, D), pertenece(F, D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Los pasos que sigue la función son : si L1 = [a,b]
% 1. Sea Dic el diccionario
% 2. Comprobamos que todos los elementos de L1 pertenecen al diccionario
% 3. Ordenamos L1 en O tal que O queda en orden alfabético ( O = [b, a])
% 4. Llamamos a run_lenght con O para que nos devuelva las veces que aparece
%    cada elemento tal que queda RL = [[1,a], [1,b]]
% 5. Ordenamos otra vez RL de mayor a menor para construir el árbol
% 6. Construimos el árbol en T
% 7. Llamamos a encode_list con el árbol, L1 y L2 
encode(L1, L2) :- dictionary(Dic), pertenece(L1, Dic), ordenar(L1, O), 
    			  run_length(O, RL), ordenar(RL, O2),
    			  decodifica(O2, D), build_tree(D, T), encode_list(L1, L2, T).



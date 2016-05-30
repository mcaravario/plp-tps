:- dynamic(diccionario/1).

% Dado un nombre de archivo que contiene todas las palabras que se quieren
% agregar al diccionario (una por linea), vacia diccionario/1 y agrega
% las definiciones nuevas

cargar(NombreDeArchivo) :-
  retractall(diccionario(_)),
  atom_codes(NombreDeArchivo, Arch),
  open(Arch, read, Str),
  read_file(Str,_),
  close(Str).

read_file(Stream,[]) :- at_end_of_stream(Stream).
read_file(Stream,[X|L]) :-
    not(at_end_of_stream(Stream)),
    read_line_to_codes(Stream,Codes),
    string_codes(X, Codes),
    assertz(diccionario(X)),
    read_file(Stream,L), !.


% listar mensajes secretos de ejemplo.
ej(1, [rombo, cuadrado, espacio, perro, cuadrado, sol, cuadrado]).
% solo debería ser "la cosa" porque cuadrado != triangulo
ej(2, [rombo, cuadrado, espacio, perro, triangulo, sol, cuadrado]).

ej(3, [rombo, cuadrado, perro, cuadrado, sol, luna, triangulo, estrella, arbol, gato]).


% Ejercicio 1
% 
% string_codes/2(?string, ?listaDeCods) necesita que alguno de los dos argumentos debe estar
% instanciado.
%
% diccionario_lista(?L) es reversible, ya que si le pasas una palabra (como
% lista de ascii), devolvera true si pertenece al diccionario cargado
% previamente y false en caso contrario. En caso de pasarle un parametro sin
% instanciar devolvera las palabras como lista de ascii. 

diccionario_lista(L) :- diccionario(X), string_codes(X,L). 

% Ejercicio 2
% juntar_con(?Xs, ?J, ?R)
juntar_con1([J],_,J).
juntar_con1([H|T], J, R) :- append(H,[J],R1), juntar_con1(T,J,R2), append(R1,R2,R).

juntar_con2([[]],_,[]).
juntar_con2([[]|Yss],J,[J|Xs]) :- juntar_con2(Yss,J,Xs).
juntar_con2([Ys|Yss],J,[X|Xs]) :- juntar_con2([Zs|Yss],J,Xs), append([X],Zs,Ys).

juntar_con(X,J,Y) :- var(X), juntar_con2(X,J,Y).
juntar_con(X,J,Y) :- juntar_con1(X,J,Y).


% Ejercicio 3
% palabras(+S,?P)
palabras(S,P) :- juntar_con(P,espacio,S), !.

% Ejercicio 4
% asignar_var(?A,?L,-M)
asignar_var(A,[],[(A,_)]).
asignar_var(A,[(A,Z)|Xs],[(A,Z)|Xs]).
asignar_var(A,[X|Xs],[X|Zs]) :- asignar_var(A,Xs,Zs).

% Ejercicio 5

% asignar_lista_var(+Xs,-Zs)
%    Instancia en Zs el mapa de variables libres por cada atomos en Xs
asignar_lista_var([], []).
asignar_lista_var([X|Xs], Zs) :- asignar_lista_var(Xs, Ps), asignar_var(X, Ps, Zs).


% variables_libres(+Xs,+As,-Ys)
%    Instancia en Yss la lista (en el mismo orden que en Xs) que por cada atomo 
%    la variable libre que le corresponde en el mapeo As
variables_libres([],_,[]).
variables_libres([X|Xs], As, [Z|Ys]) :- member((X,Z),As), variables_libres(Xs,As,Ys).

% variables_libres2(+Xss,+As,-Yss)
%    Idem variables_libres2 pero con lista de listas
variables_libres2([],_,[]).
variables_libres2([Xs|Xss], As, [Rs|Rss]) :- variables_libres(Xs, As, Rs), variables_libres2(Xss,As,Rss).

% palabras_con_variables(+Xss,-Vss)
palabras_con_variables(Xss,Vss) :- juntar_con1(Xss,espacio,L), asignar_lista_var(L, As), variables_libres2(Xss,As,Vss), !.


% Ejercicio 7

% sinRepetidos(+Ls,-Ss)
sinRepetidos([],[]).
sinRepetidos([X|Xss],[X|Zs]) :- sinRepetidos(Xss,Rs), quitar(X,Rs,Zs).

% cantDistintos(+L,-N)
cant_distintos(L,N) :- sinRepetidos(L,L2), length(L2,N).

% Ejercicio 8

%incluido_en_dicc_ascii genera todas las posibles listas que contengan palabras del diccionario (su secuencia de numeros ascii) y
% que sean de longitud N.
% incluido(+n,?L)
incluido_en_dicc_ascii(0, []).
incluido_en_dicc_ascii(N, [M|Ms]) :- N >= 1, diccionario_lista(M), N2 is N-1, 
									 incluido_en_dicc_ascii(N2,Ms).

% descifrar(+S, ?M)
descifrar(S,M) :- palabras(S,P), palabras_con_variables(P,V), length(P,Z),
				  incluido_en_dicc_ascii(Z,R), V=R,
				  juntar_con(R,32,R2), string_codes(M,R2).

% Ejercicio 9
% Genera todas posibles formas de insertar espacios en la primer lista
% agregar_espacios(+L, ?R)
agregar_espacios([],[]).
agregar_espacios([L|Ls],R) :- append([L],[espacio],L1), agregar_espacios(Ls,R1), append(L1,R1,R).
agregar_espacios([L|Ls],R) :- agregar_espacios(Ls,R1), append([L],R1,R).

% descifrar_sin_espacios(+S,?M)
descifrar_sin_espacios(S,M) :- agregar_espacios(S,R), descifrar(R,M).

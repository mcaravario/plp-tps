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
% Reversibilidad:
%  *) Las 3 no pueden estar instanciadas a la vez o solo J instanciada. 
%     No se cuelga en ningun caso, pero no enumera todas las posibles soluciones.
juntar_con([[]],_,[]).
juntar_con([[]|Yss],J,[J|Xs]) :- juntar_con(Yss,J,Xs).
juntar_con([[X|Zs]|Yss],J,[X|Xs]) :- X \== J, juntar_con([Zs|Yss],J,Xs).


% Ejercicio 3
% palabras(?S,?P)
% 
% Reversibilidad: 
%   *) Ambas no pueden estar instancias porque depende de juntar_con (Ver juntar_con)
palabras(S,P) :- juntar_con(P,espacio,S).

% Ejercicio 4
% asignar_var(?A,?L,-M)
%
% Reversibilidad:
%	*) al menos A ó L deben instanciarse.
asignar_var(A, L, L) :- member((A,_),L).
asignar_var(A, L, [(A,X)|L]) :- not(member((A,X),L)).

% Ejercicio 5

% asignar_lista_var(+Xs,-Zs)
%    Instancia en Zs el mapa de variables libres por cada atomo en Xs
%    
%    Ejemplos:
%       ?- asignar_lista_var([rombo,cuadrado,circulo],M).
%       M=[(circulo,_G3300),(cuadrado,_G3309),(rombo,_G3321)];
%       false.
%       ?- asignar_lista_var([rombo,cuadrado,rombo],M).
%       M=[(rombo,_G1440),(cuadrado,_G1452)];
%       false.
asignar_lista_var([], []).
asignar_lista_var([X|Xs], Zs) :- asignar_lista_var(Xs, Ps), asignar_var(X, Ps, Zs).


% variables_libres(+Xs,+As,-Ys)
%    Instancia en Yss la lista (en el mismo orden que en Xs) que por cada atomo 
%    la variable libre que le corresponde en el mapeo As
%    
%    Ejemplo: 
%        ?- variables_libres([rombo,cuadrado,rombo],[(rombo,A),(cuadrado,B)],M)
%        M=[A,B,A];
%        false.
variables_libres([],_,[]).
variables_libres([X|Xs], As, [Z|Ys]) :- member((X,Z),As), variables_libres(Xs,As,Ys).

% variables_libres2(+Xss,+As,-Yss)
%    Idem variables_libres2 pero con lista de listas
%    
%    Ejemplo: 
%        ?- variables_libres2([[rombo,cuadrado,rombo],[cuadrado,cuadrado]],[(rombo,A),(cuadrado,B)],M)
%        M=[[A,B,A],[B,B]];
%        false.
variables_libres2([],_,[]).
variables_libres2([Xs|Xss], As, [Rs|Rss]) :- variables_libres(Xs, As, Rs), variables_libres2(Xss,As,Rss).

% palabras_con_variables(+Xss,-Vss)
palabras_con_variables(Xss,Vss) :- palabras(L,Xss), asignar_lista_var(L, As), variables_libres2(Xss,As,Vss).

% Ejercicio 6

% quitar(?X,+L,-R)
% Observación: L debe estar instanciada, pero puede contener variables libres.
quitar(_,[],[]).
quitar(X,[L|Ls],Rs) :- X == L, quitar(X,Ls,Rs).
quitar(X,[L|Ls],[L|Rs]) :- X \== L, quitar(X,Ls,Rs).

% Ejercicio 7

% sinRepetidos(+Ls,?Ss)
%    True si la lista Ss contiene (en el mismo orden de Ls) la lista Ls sin repetidos
% Observación: L debe estar instanciada, pero puede contener variables libres.
%    
%    Ejemplo:
%       ?- sinRepetidos([2,1,2,3,3,4],L);
%       L=[2,1,3,4];
%       false;
sinRepetidos([],[]).
sinRepetidos([X|Xss],[X|Zs]) :- sinRepetidos(Xss,Rs), quitar(X,Rs,Zs).

% cantDistintos(+L,-N)
% Observación: L debe estar instanciada, pero puede contener variables libres.
cant_distintos(L,N) :- sinRepetidos(L,L2), length(L2,N).

% Ejercicio 8

%incluido_en_dicc_ascii genera todas las posibles listas que contengan palabras del diccionario (su secuencia de numeros ascii) y
% que sean de longitud N.
% incluido_en_dicc_ascii(+N,?L)
% 
% Reversibilidad: 
%    N debe estar instanciado, porque se hacen comparaciones y cuentas con is.
incluido_en_dicc_ascii(0, []).
incluido_en_dicc_ascii(N, [M|Ms]) :- N >= 1, diccionario_lista(M), N2 is N-1, 
									 incluido_en_dicc_ascii(N2,Ms).

% descifrar(+S, ?M)
% Reversibilidad: 
%    *) S debe estar instanciado pues
%    de lo contrario, palabras(S,P) explora solo las frases
%    vacias (S=[],S=[espacio],S=[espacio,espacio],etc) por lo tanto
%    nunca llega a unificar con nada.
% 
% Funcionamiento (Generate & Test):
% 1. Separa los simbolos por espacios (en lista de listas), 
% 2. Asigna una variable fresca distinta a cada simbolo distinto.
% 3. Genera todas las posibles frases del diccionario (en forma de lista de listas de numeros ascii)
%    que unifiquen con las variables y filtra las asignaciones 
%    que a dos simbolos diferentes les corresponde la misma letra
% 4. Devuelve el mensaje en forma de string
descifrar(S,M) :- palabras(S,P), palabras_con_variables(P,V), length(P,Z),
				  incluido_en_dicc_ascii(Z,V),
				  palabras(L1,V),
				  cant_distintos(L1,N1), cant_distintos(S,N2),
				  N1 == N2,
				  juntar_con(V,32,R), string_codes(M,R).

% Ejercicio 9

% agregar_espacios(+L, ?R)
% Genera todas posibles formas de insertar espacios en la primer lista
% 
% Ejemplo:
%		?- agregar_espacios([a,b,c],M).
% 		M = [a, espacio, b, espacio, c, espacio] ;
% 		M = [a, espacio, b, espacio, c] ;
% 		M = [a, espacio, b, c, espacio] ;
% 		M = [a, espacio, b, c] ;
% 		M = [a, b, espacio, c, espacio] ;
% 		M = [a, b, espacio, c] ;
% 		M = [a, b, c, espacio] ;
% 		M = [a, b, c].
%
% Reversibilidad:
%     *) L debe estar instanciada ya que si no se cuelga.
%     *) Ambas no pueden no instanciadas, porque no genera
%     todas las posibles soluciones, hay ramas a las que nunca llega a explorar.

agregar_espacios([],[]).
agregar_espacios([L|Ls],[L,espacio|R1]) :- agregar_espacios(Ls,R1).
agregar_espacios([L|Ls],[L|R1]) :- agregar_espacios(Ls,R1).

% descifrar_sin_espacios(+S,?M)
% Reversibilidad: 
%   *) No es reversible pues descifrar no es reversible (ver descifrar).
%      y además por que si S no esta instanciado agregar_espacios(S,R) no termina.
descifrar_sin_espacios(S,M) :- agregar_espacios(S,R), descifrar(R,M).

% Ejercicio 10
% suma_lista(+L,?S).
suma_lista([],0).
suma_lista([L|LS],S) :- suma_lista(LS,S1), S is S1 + L.

% sumar_palabras(+L,?S).
sumar_palabras([],0).
sumar_palabras([L|LSS], S) :- length(L,L1), sumar_palabras(LSS,S1), S is L1 + S1.

% promedio_longitudes(+LSS,?S).
promedio_longitudes(LSS,P) :- length(LSS,L), sumar_palabras(LSS,S), P is S / L.

% calcular_resta_al_cuadrado(+LSS,?S).
calcular_resta_al_cuadrado([],_,[]).
calcular_resta_al_cuadrado([L|LS], P, [R|RS]) :- length(L,L1), RESTA is L1-P, R is RESTA * RESTA,
                                            calcular_resta_al_cuadrado(LS,P,RS).

% desviacion(+LSS,?S).
desviacion(R,D) :- string_codes(R,C), juntar_con(LSS,32,C), promedio_longitudes(LSS,PR), 
                  calcular_resta_al_cuadrado(LSS,PR,L), suma_lista(L,SUM), length(LSS,N), DIV is SUM / N, 
                  D is sqrt(DIV). 


% mensajes_mas_parejos(+S,?M).
% Reversibilidad:
%	S debe estar instanciado, porque descifrar_sin_espacios no puede tener a S no instanciado
% Funcionamiento:
%	1. Genera todos los posibles mensajes M sin espacios
%	2. Calcula su desviación estandar
%	3. Se fija que para todo otro descriframiento M' no puede tener una desviacion menor (no existe instancia en la que haya M' con desviacion menor)
mensajes_mas_parejos(S,M) :- descifrar_sin_espacios(S,M), desviacion(M,D1), not((descifrar_sin_espacios(S,R2),desviacion(R2,D2), D1 > D2)).

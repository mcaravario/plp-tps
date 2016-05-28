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


/*LABORATORIO 2 PROLOG PRIMER COMMIT */
%     set_prolog_flag(answer_write_options,[max_depth(0)]).         write('\e[2J').
/*
TDA RS
una RS es una lista de TDA's
(nombreRS,fechaRS,userOn,cantPubli,list publi,cantUsers, list usuarios,cantComent,list coment) y tiene la siguiente forma.
[ "nombreRS", fechaRS=[1,2,2020], "usuario online",cantPubli(int),[list publicaciones],cantUsers (int), [list usuarios],CantComent,[ListComent]]
sujeto a cambios
*/
%Constructor de una RS
socialNetwork(Name, Date, SOut):-SOut = [Name,Date,"",0,[],0,[],0,[]].
%constructor de una RS vacio
crearRS(Name,Date,UserOn,CantPubli,ListaPreguntas,CantUser,ListaUser,CantComent,ListComent,SOut):-
    SOut = [Name,Date,UserOn,CantPubli,ListaPreguntas,CantUser,ListaUser,CantComent,ListComent].

%selectores del TDA RS
getNombreRS([Name,_,_,_,_,_,_,_,_], Name).
getDateRS([_,Date,_,_,_,_,_,_,_], Date).
getUserOn([_,_,UserOn,_,_,_,_,_,_], UserOn).
getCantPubli([_,_,_,CantPubli,_,_,_,_,_], CantPubli).
getListPubli([_,_,_,_,ListaPubli,_,_,_,_], ListaPreguntas).
getCantUsers([_,_,_,_,_,CantUser,_,_,_], CantUser).
getListUser([_,_,_,_,_,_,ListaUser,_,_], ListaUser).
getCantComent([_,_,_,_,_,_,_,CantComent,_], CantComent).
getListComent([_,_,_,_,_,_,_,_,ListComent], ListComent).
/*FECHA
TDA FECHA
lista con la forma
[DD,MM,YYYY] donde todos sus valores son numeros
*/
%primero revisa sus parametros
fecha(D,M,Y,_):- D>28,M=2,!,false.
fecha(D,M,Y,_):- D>30,(M=2;M=6;M=9;M=11),!,false.
%si no cumple  los anteriores entrara en esta 
fecha(D,M,Y,[D,M,Y]):-
    number(D),number(M),number(Y)
    D>0,(D<31;D=31),M>0,(M<12;M=12),!.

fechaGetD([D,M,Y],D).
fechaGetM([D,M,Y],M).
fechaGetY([D,M,Y],Y).
/*
TDA PUBLICACION
una publicacion una lista con la siguiente forma
[ id_Pregunta(entero),"autor","Fecha","Cuerpo de la pregunta",destinatarios,likes]
*/
%CONSTRUCTOR PREGUNTA
crearPregunta(ID,Autor,Texto,Fecha,Cuerpo,Destinatarios,ListComent,Likes,NOMBRE):-
NOMBRE=[ID,Autor,Texto,Fecha,Cuerpo,Destinatarios,Likes].
%selectores preguntas
getIDPubli([ID,_,_,_,_,_,_,_],ID).
getAutorPubli([_,Autor,_,_,_,_,_,_],Autor ). 
getTextoPubli([_,_,Texto,_,_,_,_,_],Texto ).
getFechaPubli([_,_,_,Fecha,_,_,_,_], Fecha).
getCuerpoPubli([_,_,_,_,Cuerpo,_,_,_],Cuerpo).
getDestiPubli([_,_,_,_,_,Desti,_,_], Desti).
getComentPubli([_,_,_,_,ListComent,_,_,_],ListComent).
getLikesPubli([_,_,_,_,_,_,_,Likes], Likes).

/*
TDA USUARIOS
un usuario es una lista con la siguiente forma
[ ID,"username","password","fecha registro",[lista ids publicaciones realizadas], [lista ids comentarios realizadas] ]
*/
%constructores usuario (uno para crear un usuario nuevo y otro que crea usuarios con parametros que yo elijo)
crearUser(ID,Username,Password,FechaR,ListaPubli,ListaComent,NuevoUsuario):-
    NuevoUsuario=[ID,Username,Password,FechaR,ListaPubli,ListaComent].
%selectores TDA usuario
getIDUser([ID,_,_,_,_,_],ID ).
getNAMEUser([_,Username,_,_,_,_],Username ).
getPASSUser([_,_,Password,_,_,_],Password ).
getFECHARUser([_,_,_,FechaR,_,_],FechaR ).
getLISTPUBLIUser([_,_,_,_,ListaPubli,_],ListaPubli ).
getLISTCOMENTUser([_,_,_,_,_,ListaComent],ListaComent ).


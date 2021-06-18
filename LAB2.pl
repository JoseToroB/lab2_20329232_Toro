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
%[Name,Date,UserOn,CantPubli,ListaPreguntas,CantUser,ListaUser,CantComent,ListComent]
%selectores del TDA RS
getNombreRS([Name,_,_,_,_,_,_,_,_], Name).
getDateRS([_,Date,_,_,_,_,_,_,_], Date).
getUserOn([_,_,UserOn,_,_,_,_,_,_], UserOn).
getCantPubli([_,_,_,CantPubli,_,_,_,_,_], CantPubli).
getListPubli([_,_,_,_,ListaPubli,_,_,_,_], ListaPubli).
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
fecha(D,M,_,_):- D>28,M=2,!,false.
fecha(D,M,_,_):- D>30,(M=2;M=6;M=9;M=11),!,false.
%si no cumple  los anteriores entrara en esta 
fecha(D,M,Y,[D,M,Y]):-
    number(D),number(M),number(Y),
    D>0,(D<31;D=31), M>0 ,( M<12 ; M=12),!.

fechaGetD([D,_,_],D).
fechaGetM([_,M,_],M).
fechaGetY([_,_,Y],Y).

esFecha([D.M,Y]):-  D>28,M=2,!,false.
esFecha([D.M,Y]):-  D>30,(M=2;M=6;M=9;M=11),!,false.
esFecha([D,M,Y]):-
    number(D),number(M),number(Y),
    D>0,(D<31;D=31), M>0 ,( M<12 ; M=12),!,true.


/*
TDA PUBLICACION
una publicacion una lista con la siguiente forma
[ id_Pregunta(entero),"autor","Fecha","Cuerpo de la pregunta",destinatarios,likes]
*/
%CONSTRUCTOR PREGUNTA
crearPregunta(ID,Autor,Texto,Fecha,Cuerpo,Destinatarios,ListComent,Likes,NOMBRE):-
NOMBRE=[ID,Autor,Texto,Fecha,Cuerpo,Destinatarios,ListComent,Likes].
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

%este "selector" obtiene la lista del usuario buscado dentro de la lista de usuarios
getUserLista([[ID,Username,Password,FechaR,ListaPubli,ListaComent]|RestoUsers],UserBuscado,Lista):-  
    (Username = UserBuscado, Lista=[ID,Username,Password,FechaR,ListaPubli,ListaComent]); getUserLista(RestoUsers,UserBuscado,Lista).
getUserLista([_|[ID,Username,Password,FechaR,ListaPubli,ListaComent]],UserBuscado,Lista):- 
    (Username = UserBuscado, Lista=[ID,Username,Password,FechaR,ListaPubli,ListaComent]).

/*
TDA COMENTARIO
un comentario es una respuesta a la lista
[ ID,"autor","Fecha","Cuerpo de la respuesta",listaComentarios,IDPublicacion,likes,]
*/
%Constructor Respuesta
crearRespuesta(ID,Autor,Fecha,CuerpoTexto,ListComent,IDpubli,Likes,NOMBRERespuesta):-
    NOMBRERespuesta=[ID,Autor,Fecha,CuerpoTexto,ListComent,IDpubli,Likes].

%selectores TDA comentario
getIDResp([ID,_,_,_,_,_,_],ID).
getAUTORResp([_,Autor,_,_,_,_,_],Autor).
getFECHAResp([_,_,Fecha,_,_,_,_],Fecha).
getCUERPOTXTResp([_,_,_,CuerpoTexto,_,_,_],CuerpoTexto).
getLISTCOMENTResp([_,_,_,_,ListComent,_,_],ListComent).
getIDPUBLIResp([_,_,_,_,_,IDpubli,_],IDpubli).
getLIKESResp([_,_,_,_,_,_,Likes],Likes).

%%%%%%%%%%%%%%%%%%%%%%%%
%%FUNCIONES AUXILIARES%%
%%%%%%%%%%%%%%%%%%%%%%%%

%esta funcion agrega al final un elemento en una lista
agregarAlFinal([], E, [E]).
agregarAlFinal([A|Cola], E, [A|NvaCola]) :-
    agregarAlFinal(Cola, E, NvaCola).

%ESTA FUNCION REVISA SI EXISTE UN USUARIO DENTRO DE UNA LISTA
existeUser([H|_],User):-
    getNAMEUser(H,Username ),
    (User = Username).
existeUser([_|T], User) :- existeUser(T, User).

% Dominio: list X string X string
%Predicado que verifica si el usuario y su contraseña coinciden.
buscarUserPass([Cabeza|_],User,Pass):-
    getNAMEUser(Cabeza,Username),
    getPASSUser(Cabeza,Password),
    (User = Username),
    (Pass = Password).
buscarUserPass([_|Cola], User, Pass) :- buscarUserPass(Cola, User, Pass).


%esta funcion revisa una parametro de entrada y entrega un resultado segun su valor
revisarBool(A,_,true,A).
revisarBool(_,B,false,B).
%%%%%%%%%%%
%FUNCIONES%
%%%%%%%%%%%
/*
(0.5 pts) socialNetworkRegister: 
Predicado que permite consultar el valor que toma un TDA SocialNetwork al ocurrir el registro de un nuevo usuario. 
Para esto se ingresa un SocialNetwork inicial, nombre del usuario, contraseña y el SocialNetwork resultante luego del registro. 
El retorno del predicado es true en caso que se pudo satisfacer el registro del usuario.
*/
socialNetworkRegister([Name,Date,UserOn,CantPubli,ListaPreguntas,CantUser,ListaUser,CantComent,ListComent], Fecha, Username, Password,Sn2 ):- 
    string(Username),%los username solo pueden ser string al igual que las contraseñas
    string(Password),
    %creo el user con estos datos entregados
    IDuser is CantUser + 1,
    crearUser( IDuser ,Username,Password,Fecha,[],[],NuevoUsuario),
    %reviso que el usuario no exista, si me da false entonces no hago backtracing
    %y retorno false
    not(existeUser(ListaUser,Username)),!,
    %creo una lista de usuarios nueva con el usuario agregado al final
    agregarAlFinal(ListaUser, NuevoUsuario, ListaUsersRSV2),
    crearRS(Name,Date,UserOn,CantPubli,ListaPreguntas,IDuser,ListaUsersRSV2,CantComent,ListComent,Sn2).

/*
(0.5 pts) socialNetworkLogin: 
Predicado que permite autenticar a un usuario registrado. 
Si la autenticación es válida (i.e., que el usuario existe y su contraseña es correcta)e l retorno es true. 
La actualización del stack incorpora al usuario autenticado en sesión activa (fundamental para que los predicados siguientes puedan funcionar).
*/
socialNetworkLogin([Name,Date,UserOn,CantPubli,ListaPreguntas,CantUser,ListaUser,CantComent,ListComent],Username, Password,Sn2 ):- 
    %compruebo nombre y pass sean strings
    string(Username),
    string(Password),
    %reviso que username y pass sean correctos 
    buscarUserPass(Usuarios,Username,Password),!,
    %si da true entonces retorno la SN con el user logeado
    crearRS(Name,Date,Username,CantPubli,ListaPreguntas,CantUser,ListaUser,CantComent,ListComent,Sn2).

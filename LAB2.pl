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
crearPregunta(ID,Autor,Fecha,Cuerpo,Destinatarios,ListComent,Likes,NOMBRE):-
NOMBRE=[ID,Autor,"formato texto",Fecha,Cuerpo,Destinatarios,ListComent,Likes].
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
crearUser(ID,Username,Password,FechaR,ListaPubli,ListaComent,Amigos,Compartidas,NuevoUsuario):-
    NuevoUsuario=[ID,Username,Password,FechaR,ListaPubli,ListaComent,Amigos,Compartidas].
%selectores TDA usuario
getIDUser([ID,_,_,_,_,_,_,_],ID ).
getNAMEUser([_,Username,_,_,_,_,_,_],Username ).
getPASSUser([_,_,Password,_,_,_,_,_],Password ).
getFECHARUser([_,_,_,FechaR,_,_,_,_],FechaR ).
getLISTPUBLIUser([_,_,_,_,ListaPubli,_,_,_],ListaPubli ).
getLISTCOMENTUser([_,_,_,_,_,ListaComent,_,_],ListaComent ).
getAmigosUser([_,_,_,_,_,_,Amigos,_],Amigos).
getCompartidasUser([_,_,_,_,_,_,_,Compartidas],Compartidas).

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

% BUSCA UN USUARIO Y LO ENTRAGA SI SU NICK Y PASS SON CORRECTOS
buscarUserPass([Cabeza|_],User,Pass):-
    getNAMEUser(Cabeza,Username),
    getPASSUser(Cabeza,Password),
    (User = Username),
    (Pass = Password).
buscarUserPass([_|Cola], User, Pass) :- buscarUserPass(Cola, User, Pass).

%busca un usuario por su nick y retorna sus datos
buscarUsuarioNick([Cabeza|_],User,Lista):-
    getNAMEUser(Cabeza,Username),
    (User = Username),
    (Cabeza = Lista).
buscarUsuarioNick([_|Cola], User,Preguntas) :- buscarUsuarioNick(Cola,User,Preguntas).

%retorno un usuario "nuevo" con un usuario como base
agregarPubliUser([ID,Username,Password,FechaR,ListaPubli,ListaComent,Amigos,Comp],IDPUBLI,NuevoUsuario):-
    append(ListaPubli,[IDPUBLI],NuevaLista),
    crearUser(ID,Username,Password,FechaR,NuevaLista,ListaComent,Amigos,Comp,NuevoUsuario).

remover(_, [], []).
remover(Y, [Y|Xs], Zs):-
    remover(Y, Xs, Zs), !.
remover(X, [Y|Xs], [Y|Zs]):-
    remover(X, Xs, Zs).

estaEN(X,[]):-!,fail.
estaEN(X,[X|_]).
estaEn(X,[H|T]):-estaEN(X,T).

existePost(PostId,[]).
existePost(PostId,[[PostId|_]|T]).
existePost(PostId,[H|T]):-existePost(PostId,T).
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
    string(Username),not(Username=""),!,%los username solo pueden ser string(distintos a "") al igual que las contraseñas
    string(Password),not(Username=""),!,
    %creo el user con estos datos entregados
    IDuser is CantUser + 1,
    crearUser( IDuser ,Username,Password,Fecha,[],[],[],[],NuevoUsuario),
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

/*
(0.5 pts) socialNetworkPost: 
Predicado que permite a un usuario con sesión iniciada en la plataforma realizar una nueva publicación propia o dirigida a otros usuarios. 
Cada publicación registra el autor de la misma (obtenido desde la sesión iniciada con login), 
fecha de publicación (tipo Date), el tipo de publicación (“photo”, “video”, “url”, “text”, “audio”) 
y el contenido de la publicación (solo debe ser un string). 
El retorno es true si se puede satisfacer en “Sn2” el TDA SocialNetwork con la nueva publicación incluida 
y sin la sesión activa del usuario que realizó la publicación.
*/
socialNetworkPost([Name,Date,UserOn,CantPubli,ListaPreguntas,CantUser,ListaUser,CantComent,ListComent], Fecha, Texto, ListaUsernamesDest, Sn2):-
    %reviso que las entradas sean validas
    string(Fecha),
    string(Texto),
    is_list(ListaUsernamesDest),
    %Se verifica si hay un usuario conectado
    not(UserOn=""),!,
    %Se crea la publicacion
    ID is CantPubli+1,
    crearPregunta(ID,UserOn,Fecha,Texto,ListaUsernamesDest,[],0,Publi),
    %se agrega la publicacion a la lista de publicaciones
    append(ListaPreguntas,[Publi],ListaPublisFinales),
    %Se busca al autor de la pregunta, que es el usuario Conectado
    buscarUsuarioNick(ListaUser,UserOn,USUARIOon),
    %Se agrega el id de la pregunta al usuario que la hizo
    %obtengo los datos restantes del usuario para generar uno "nuevo"
    getIDUser(USUARIOon,IDu),
    getPASSUser(USUARIOon,Password ),
    getFECHARUser(USUARIOon,FechaR ),
    getLISTPUBLIUser(USUARIOon,PublicacionesUser),
    getLISTCOMENTUser(USUARIOon,ListaComentu),
    getAmigosUser(USUARIOon,Amigos),
    getCompartidasUser(USUARIOon,Compartidas),
    agregarAlFinal(PublicacionesUser,ID,PublicacionesUserFinales),
    crearUser(IDu,UserOn,Password,FechaR,PublicacionesUserFinales,ListaComentu,Amigos,Compartidas,UserActualizado),
    %se reemplaza al usuario
    remover(USUARIOon,ListaUser,ListaUserActualizada),
    agregarAlFinal(ListaUserActualizada,UserActualizado,UsuariosFinales),
    %creo la SN con el user offline y los datos actualizados
    crearRS(Name,Date,"",ID,ListaPublisFinales,CantUser,UsuariosFinales,CantComent,ListComent,Sn2).

/*
(0.5 pts) socialNetworkFollow: 
Predicado que permite a un usuario con sesión iniciada en la plataforma seguir a otro usuario. 
El retorno es true si se puede satisfacer en “Sn2” el TDA SocialNetwork con algún indicativo de que el usuario que tiene sesión iniciada en “Sn1”
ahora sigue al usuario “U” y sin la sesión activa del usuario.
no se puede seguir a si mismo
*/
socialNetworkFollow([Name,Date,UserOn,CantPubli,ListaPreguntas,CantUser,ListaUser,CantComent,ListComent], Seguir, Sn2):-
    %seguir es string, distinto a ""
    string(Seguir),not(Seguir=""),!,
    %revisamos exista el user Seguir
    existeUser(ListaUser,Seguir),!,
    %revisamos el user On sea distinto a ""
    not(UserOn=""),!,
    %reviso que UserOn != Seguir, reviso que existan.
    not(UserOn=Seguir),!,
    %buscamos los datos del user On para agregar el usuario a seguir
    buscarUsuarioNick(ListaUser,UserOn,UsuarioOn),
    %obtenemos todos los datos restantes del usuario
    getIDUser(UsuarioOn,IDu),
    getPASSUser(UsuarioOn,Passwordu),
    getFECHARUser(UsuarioOn,FechaR),
    getLISTPUBLIUser(UsuarioOn,ListaPubliU),
    getLISTCOMENTUser(UsuarioOn,ListaComentU),
    getAmigosUser(UsuarioOn,Amigos),
    getCompartidasUser(UsuarioOn,Compartidas),
    %revisamos que Seguir no este dentro de amigos
    %en caso de estar tirara false y no avanzara
    not(estaEN(Seguir,Amigos)),!,
    %agregamos el nombre del amigo al final de su lista de amigos
    agregarAlFinal(Amigos,Seguir,AmigosFinal),
    %creamos al usuario nuevamente con su lista actualizada
    crearUser(IDu,UserOn,Passwordu,FechaR,ListaPubliU,ListaComentU,AmigosFinal,Compartidas,UserActualizado),
    %con el user actualizado, removemos su version anterior
    remover(UsuarioOn,ListaUser,ListaUserActualizada),
    agregarAlFinal(ListaUserActualizada,UserActualizado,UsuariosFinales),
    %creo la SN con el user offline y los datos actualizados
    crearRS(Name,Date,"",CantPubli,ListaPreguntas,CantUser,UsuariosFinales,CantComent,ListComent,Sn2).
/*
(0.5 pts) socialNetworkShare: 
Predicado que permite a un usuario con sesión iniciada en la plataforma compartir 
contenido de otro usuario en su propio espacio o dirigido a otros usuarios más. 
El retorno es true si se puede satisfacer en “Sn2” 
el TDA SocialNetwork con un registro de la publicación compartida y sin la sesión activa del usuario.
*/
%como no se niega que alguien pueda compartir varias veces la misma publicacion, dentro de mi codigo si esta permitido
socialNetworkShare([Name,Date,UserOn,CantPubli,ListaPreguntas,CantUser,ListaUser,CantComent,ListComent], Fecha, PostId, ListaUsernamesDest, Sn2):-
    %reviso exista alguien online
    not(UserOn=""),!,
    %fecha sea string
    string(Fecha),!,
    %reviso la lista de usuarios sea valida
    is_list(ListaUsernamesDest),
    %reviso exista un post con tal ID
    existePost(PostId,ListaPreguntas),!,
    %busco al usuario y obtengo el resto de sus valores para luego en la lista de compartidos agregar la nueva publicacion
    buscarUsuarioNick(ListaUser,UserOn,UsuarioOn),
    %obtenemos todos los datos restantes del usuario
    getIDUser(UsuarioOn,IDu),
    getPASSUser(UsuarioOn,Passwordu),
    getFECHARUser(UsuarioOn,FechaR),
    getLISTPUBLIUser(UsuarioOn,ListaPubliU),
    getLISTCOMENTUser(UsuarioOn,ListaComentU),
    getAmigosUser(UsuarioOn,Amigos),
    getCompartidasUser(UsuarioOn,Compartidas),
    %agrego la publicacion compartida con el formato [fecha,postID,ETIQUETADOS]
    agregarAlFinal(Compartidas,[Fecha,PostId,ListaUsernamesDest],CompartidasFinal),
    %creamos al usuario nuevamente con su lista actualizada
    crearUser(IDu,UserOn,Passwordu,FechaR,ListaPubliU,ListaComentU,Amigos,CompartidasFinal,UserActualizado),
    remover(UsuarioOn,ListaUser,ListaUserActualizada),
    agregarAlFinal(ListaUserActualizada,UserActualizado,UsuariosFinales),
    %creo la SN con el user offline y los datos actualizados
    crearRS(Name,Date,"",CantPubli,ListaPreguntas,CantUser,UsuariosFinales,CantComent,ListComent,Sn2).
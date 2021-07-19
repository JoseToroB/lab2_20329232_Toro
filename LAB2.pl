/*LABORATORIO 2 PROLOG PRIMER COMMIT */
%     set_prolog_flag(answer_write_options,[max_depth(0)]),use_module(library(theme/dark)),write('\e[2J'). 
/*
TDA RS
una RS es una lista de TDA's
(nombreRS,fechaRS,userOn,cantPubli,list publi,cantUsers, list usuarios,cantComent,list coment) y tiene la siguiente forma.
[ "nombreRS", fechaRS=[1,2,2020], "usuario online",cantPubli(int),[list publicaciones],cantUsers (int), [list usuarios],CantComent(int),[ListComent]]
sujeto a cambios
*/
/*
socialNetwork(facebook,"1/1/2020",FACE),socialNetworkRegister(FACE,"2/2/2021","toro","123",FACE2),socialNetworkLogin(FACE2,"toro","123",F3),socialNetworkPost(F3,"3/3/2021","pregunta uno ayuda",["pana1","pana2"],F4),socialNetworkRegister(F4,"4/4/2021","jaime","123",F5),socialNetworkLogin(F5,"toro","123",F6),socialnetworkToString(F6,STRING).
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


/*
FECHA
TDA FECHA
con la forma
"d/m/y"
*/
%primero revisa sus parametros
fecha(D,M,_,_):- D>28,M=2,!,false.
fecha(D,M,_,_):- D>30,(M=2;M=6;M=9;M=11),!,false.
%si no cumple  los anteriores entrara en esta 
fecha(D,M,Y,Fecha):-
    number(D),number(M),number(Y),
    D>0,(D<31;D=31), M>0 ,( M<12 ; M=12),!,
    string_concat(D,"/",F1),
    string_concat(M,"/",F2),
    string_concat(F1,F2,F3),
    string_concat(F3,Y,Fecha).

/*
TDA PUBLICACION
una publicacion una lista con la siguiente forma
[ id_Pregunta(entero),"autor",formato,"Fecha","Cuerpo de la pregunta",destinatarios,likes]
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
getComentPubli([_,_,_,_,_,_,ListComent,_],ListComent).
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

/*
TDA COMENTARIO
un comentario es una respuesta a la lista
[ ID,"autor","Fecha","Cuerpo de la respuesta",listaComentarios,IDPublicacion,likes,]
*/
%Constructor Comentario
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

%remover un elemento
remover(_, [], []).
remover(Y, [Y|Xs], Zs):-
    remover(Y, Xs, Zs), !.
remover(X, [Y|Xs], [Y|Zs]):-
    remover(X, Xs, Zs).

%saber si un elemento esta dentro de una lista
estaEN(X,[]):-!,fail.
estaEN(X,[X|_]).
estaEn(X,[H|T]):-estaEN(X,T).

%saber si existe un post
existePost(PostId,[]).
existePost(PostId,[[PostId|_]|T]).
existePost(PostId,[H|T]):-existePost(PostId,T).

%si la id del comentario actual es la buscada, lo entrego
buscarComentario(ID,[[ID|REST]|T],[ID|REST]).
%si la id del comentario actual no es la buscada, sigo buscando
buscarComentario(ID,[H|T],COM):-
    buscarComentario(ID,T,COM).

%esto revisa si cada elemento de una lista esta dentro de otra lista (lo utilizo para los etiquetados asique mi caso base en )
existeET([],L).
existeET([H|T],L):-existeUser(L,H),existeET(T,L).
%%%%%%%%%%%
%FUNCIONES%
%%%%%%%%%%%
/*
(0.5 pts) socialNetworkRegister: 
Predicado que permite consultar el valor que toma un TDA SocialNetwork al ocurrir el registro de un nuevo usuario. 
DOM:socialNetwork X date X string X string X socialNetwork
*/
socialNetworkRegister([Name,Date,UserOn,CantPubli,ListaPreguntas,CantUser,ListaUser,CantComent,ListComent], Fecha, Username, Password,Sn2 ):- 
    string(Username),not(Username=""),!,%los username solo pueden ser string(distintos a "") al igual que las contraseñas
    string(Password),not(Password=""),!,
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
DOM:socialNetwork X string X string X socialNetwork
*/
socialNetworkLogin([Name,Date,UserOn,CantPubli,ListaPreguntas,CantUser,ListaUser,CantComent,ListComent],Username, Password,Sn2 ):- 
    %compruebo nombre y pass sean strings
    string(Username),
    string(Password),
    %reviso que username y pass sean correctos 
    buscarUserPass(ListaUser,Username,Password),!,
    %si da true entonces retorno la SN con el user logeado
    crearRS(Name,Date,Username,CantPubli,ListaPreguntas,CantUser,ListaUser,CantComent,ListComent,Sn2).

/*
(0.5 pts) socialNetworkPost: 
Predicado que permite a un usuario con sesión iniciada en la plataforma realizar una nueva publicación propia o dirigida a otros usuarios. 
DOM:socialNetwork X date X string X string list x socialNetwork
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
DOM:
socialNetwork X string X socialNetwork

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
DOM:
socialNetwork X date X integer X string list X socialNetwork


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FUNCIONES QUE UTILIZA EL TOSTRING%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%lista de amigos a string
amigosToString([],"").

amigosToString([H|T],AmigosStr):-
    string_concat(H,"\n",S1),
    amigosToString(T,S2),
    string_concat(S1,S2,AmigosStr).

%un solo comentario a string
comentarioToString(IDActual,ComentariosTotales,String):-
    %ya que la lista de comentarios almacena la ID de estos, entonces debo encontrar cada comentario ID x ID
    %dentro de la lista de comentariosTotales.
    buscarComentario(IDActual,ComentariosTotales,Comentario),
    %obtenemos todas las partes de un comentario,
    %  [ID,Autor,Fecha,CuerpoTexto,ListComent,IDpubli,Likes]
    getIDResp(Comentario,ID),
    getAUTORResp(Comentario,Autor),
    getFECHAResp(Comentario,Fecha),
    getCUERPOTXTResp(Comentario,CuerpoTexto),
    getLISTCOMENTResp(Comentario,ListComent),
    getIDPUBLIResp(Comentario,IDpubli),
    getLIKESResp(Comentario,Likes),
    %ahora generamos el string del comentario
    atom_string(ID,IDSTR),
    atom_string(Likes,LikesSTR),
    string_concat("  Autor: ",Autor,S0),
    string_concat(S0,"   fecha:",SA),
    string_concat(SA,Fecha,SD),
    string_concat(SD,"\n",Sc),
    string_concat(Sc,"  Comentario ID: ",SB),
    string_concat(SB,IDSTR,S1),
    string_concat(S1,"      Likes: ",S2),
    string_concat(S2,LikesSTR,S3),
    string_concat(S3,"\n  ",S4),
    string_concat(S4,CuerpoTexto,S5),
    string_concat(S5,"\n    ",S6),
    %como un comentario puede tener comentarios, buscamos si este posee alguno
    comentariosToString(ListComent,ComentariosTotales,ComentariosSTR),
    string_concat(S6,ComentariosSTR,S7),
    %en caso de existir comentarios se agregaran con este mismo formato 
    string_concat(S7,"\n",String).
%comentarios a string
comentariosToString([],_,"").
comentariosToString([H|T],ComentariosTotales,ComentariosSTR):-
    comentarioToString(H,ComentariosTotales,S1),
    comentariosToString(T,ComentariosTotales,S2),
    string_concat(S1,S2,ComentariosSTR).

%una sola publicacion a string
preguntaToString(ID,ComentariosTotales,Publicaciones,String):-
    %obtengo la publicacion con la ID  reutilizando la funcion de buscarcomentario
    buscarComentario(ID,Publicaciones,Publi),
    getAutorPubli(Publi,Autor),
    getTextoPubli(Publi,Texto),
    getFechaPubli(Publi,Fecha),
    getCuerpoPubli(Publi,Cuerpo),
    getDestiPubli(Publi,Desti),
    getComentPubli(Publi,ListComent),
    getLikesPubli(Publi,Likes),
    atom_string(ID,IDSTR),
    atom_string(Likes,LikesSTR),
    amigosToString(Desti,EtiquetadosSTR),
    comentariosToString(ListComent,ComentariosTotales,ComentariosSTR),
    %con todo eso listo, creare el string de la publicacion
    string_concat("Autor: ",Autor,S0),
    string_concat(S0,"  Fecha:",S00),
    string_concat(S00,Fecha,S000),
    string_concat(S000,"\n",SA),
    string_concat(SA,"Publicacion ID: ",SB),
    string_concat(SB,IDSTR,S1),
    string_concat(S1,"      Likes: ",S2),
    string_concat(S2,LikesSTR,S3),
    string_concat(S3,"\n",S4),
    string_concat(S4,Cuerpo,S5),
    string_concat(S5,"\n",S6),
    string_concat(S6,"Etiquetados:\n",S7),
    string_concat(S7,EtiquetadosSTR,S8),
    string_concat(S8,"\n",S9),
    string_concat(S9,ComentariosSTR,String).


%lista de preguntas a string con sus comentarios correspondientes.
preguntasYrespuestasToString([],_,_,"").

preguntasYrespuestasToString([H|T],ListComent,Publicaciones,StringSalida):-
    preguntaToString(H,ListComent,Publicaciones,S1),
    preguntasYrespuestasToString(T,ListComent,Publicaciones,S2),
    string_concat(S1,S2,StringSalida).

%una publicaicon compartida a string
compartidaToString([FechaCompartida,IDComp,Etiquetados],ListaPreguntas,String):-
    %ya que las publicaciones y los comentarios tienen la id en la primera posicion, la funcion que busca comentarios la puedo reutilizar en este caso
    buscarComentario(IDComp,ListaPreguntas,PubliComp),
    %ahora obtengo todo lo necesario de la publicacion compartida
    getIDPubli(PubliComp,ID),
    getAutorPubli(PubliComp,Autor),
    getTextoPubli(PubliComp,Texto),
    getFechaPubli(PubliComp,Fecha),
    getCuerpoPubli(PubliComp,Cuerpo),
    getDestiPubli(PubliComp,Desti),
    getComentPubli(PubliComp,ListComent),
    getLikesPubli(PubliComp,Likes),
    %pasando atomos a string
    atom_string(ID,IDSTR),
    atom_string(Likes,LikesSTR),
    string_concat("Fecha compartida: ",FechaCompartida,S1),
    string_concat(S1,"\n",S2),
    string_concat(S2,"Autor Original: ",S3),
    string_concat(S3,Autor,S4),
    string_concat(S4,"     fecha original:",S5),
    string_concat(S5,Fecha,S6),
    string_concat(S6,"   likes originales:",S7),
    string_concat(S7,LikesSTR,S8),
    string_concat(S8,"\n",S9),
    string_concat(S9,Cuerpo,S10),
    string_concat(S10,"\n",String).


%compartidas to string
compartidasToString([],_,"").

compartidasToString([H|T],ListaPreguntas,String):-
    compartidaToString(H,ListaPreguntas,S1),
    compartidasToString(T,ListaPreguntas,S2),
    string_concat(S1,S2,String).

%esta funcion transforma una publicacion a string
%NO MUESTRO LOS ETIQUETADOS POR "PRIVACIDAD"
publicacionAstring([ID,Autor,Formato,Fecha,Cuerpo,Destinatarios,ListComent,Likes],ListaComent,String):-
    atom_string(ID,IDSTR),
    atom_string(Likes,LikesSTR),
    comentariosToString(ListComent,ListaComent,ComentariosSTR),
    %con todo eso listo, creare el string de la publicacion
    string_concat("Autor: ",Autor,S0),
    string_concat(S0,"  Fecha:",S00),
    string_concat(S00,Fecha,S000),
    string_concat(S000,"\n",SA),
    string_concat(SA,"Publicacion ID: ",SB),
    string_concat(SB,IDSTR,S1),
    string_concat(S1,"      Likes: ",S2),
    string_concat(S2,LikesSTR,S3),
    string_concat(S3,"\n",S4),
    string_concat(S4,Cuerpo,S5),
    string_concat(S5,"\n",S6),
    string_concat(S6,ComentariosSTR,String).

%esta funcion transforma todas las publicaciones de la RS a string
todasAstring([],_,"").
todasAstring([H|T],ListaComent,STRING):-
    publicacionAstring(H,ListaComent,S1),
    todasAstring(T,ListaComent,S2),
    string_concat(S1,S2,STRING).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
(0.5 pts) socialNetworkToString: 
Predicado que permite obtener una representación de un TDA socialNetwork como un string posible de visualizar de forma comprensible al usuario. 
DOM:socialNetwork X string
*/
%to string con user offline
socialnetworkToString([Name,Date,"",CantPubli,ListaPreguntas,CantUser,ListaUser,CantComent,ListComent], StrOut):-
    string_concat("La Red Social ",Name,S1),
    string_concat(S1," creada el dia ",S2),
    string_concat(S2,Date,S3),
    atom_string(CantUser,Users),
    string_concat(S3,"\nTiene un total de:",S4),
    string_concat(S4,Users,S5),
    string_concat(S5," usuarios registrados\n",S6),
    atom_string(CantPubli,Publis),
    string_concat(S6,"Con un total de: ",S7),
    string_concat(S7,Publis,S8),
    string_concat(S8," publicaciones\n",S9),
    string_concat(S9,"PUBLICACION/ES EN ESTA RED SOCIAL\n",S10),
    todasAstring(ListaPreguntas,ListComent,STRING),
    string_concat(S10,STRING, StrOut),!.

%to string con user online
socialnetworkToString([Name,Date,UserOn,CantPubli,ListaPreguntas,CantUser,ListaUser,CantComent,ListComent], StrOut):-
    %como existe un user online, obtendre sus datos completos (fecha registro, amigos, publicaciones y las respuestas de estas publicaciones)
    buscarUsuarioNick(ListaUser,UserOn,UsuarioOn),
    %obtenemos todos los datos restantes del usuario
    getIDUser(UsuarioOn,IDu),
    getPASSUser(UsuarioOn,Passwordu),
    getFECHARUser(UsuarioOn,FechaR),
    getLISTPUBLIUser(UsuarioOn,ListaPubliU),
    getLISTCOMENTUser(UsuarioOn,ListaComentU),
    getAmigosUser(UsuarioOn,Amigos),
    getCompartidasUser(UsuarioOn,Compartidas),
    %con todo esto comenzare a crear el string correspondiente
    string_concat(UserOn,"  ",S1),
    string_concat(S1,FechaR,S2),
    string_concat(S2,"\nAmigos\n",S3),
    %agrego los amigos listados 
    amigosToString(Amigos,AmigosStr),
    string_concat(S3,AmigosStr,S4),     
    %ahora agregare las publicaciones y sus comentarios 
    string_concat(S4,"Publicaciones\n",S5),
    preguntasYrespuestasToString(ListaPubliU,ListComent,ListaPreguntas,S6),
    string_concat(S5,S6,S7),
    %ahora agregare las publicaciones compartidas (pero sin sus comentarios)
    string_concat(S7,"Publicaciones Compartidas \n",Sa),
    compartidasToString(Compartidas,ListaPreguntas,S8),
    string_concat(Sa,S8,StrOut),!.

/*
(1 pts) comment: 
Predicado que permite a un usuario con sesión iniciada en la plataforma comentar publicaciones y otros comentarios existentes
DOM:socialNetwork X date X integer X integer X string X socialNetwork
*/
%comment a publicacion 
comment([Name,Date,UserOn,CantPubli,ListaPreguntas,CantUser,ListaUser,CantComent,ListComent],Fecha,PostId,0,TextoComentario,Sn2):-
    %obtengo la publicacion con la ID  reutilizando la funcion de buscarcomentario
    not(UserOn=""),!,%existe user online
    % el comentario es a una publicacion ya que la id del comment es 0
    %existe publicacion
    not(PostId<0),!,%NO EXISTEN POST MENORES A 0
    not(PostId=0),!,%no existe post 0
    not(PostId>CantPubli),!,%la id del post no puede ser mayor a la cantidad de posts
    %reviso que sean strings 
    string(Fecha),string(TextoComentario),
    %ahora creare el comentario
    ID is CantComent + 1,
    crearRespuesta(ID,UserOn,Fecha,TextoComentario,[],PostId,0,Comentario),
    %agrego la publicacion a la lista de publicaciones
    append(ListComent,[Comentario],ComentariosFinales),
    %obtengo al usuario para agregar la publicacion a su lista de publicaciones
    buscarUsuarioNick(ListaUser,UserOn,UsuarioOn),
    %obtenemos todos los datos restantes del usuario
    getIDUser(UsuarioOn,IDu),
    getPASSUser(UsuarioOn,Passwordu),
    getFECHARUser(UsuarioOn,FechaR),
    getAmigosUser(UsuarioOn,Amigos),
    getCompartidasUser(UsuarioOn,CompartidasU),
    getLISTPUBLIUser(UsuarioOn,ListaPubliU),
    %obtengo la lista de comments
    getLISTCOMENTUser(UsuarioOn,ListaComentU),
    %agrego la id a su lista
    append(ListaComentU,[ID],ComentariosUsers),
    %creo el usuario nuevamente pero con la lista nueva
    crearUser(IDu,UserOn,Passwordu,FechaR,ListaPubliU,ComentariosUsers,Amigos,CompartidasU,UserActualizado),
    %elimino el usuario antiguo
    remover(UsuarioOn,ListaUser,ListaUserActualizada),
    %agrego el user actualizado
    agregarAlFinal(ListaUserActualizada,UserActualizado,UsuariosFinales),
    %ahora debo obtener la publicacion
    buscarComentario(PostId,ListaPreguntas,Publicacion),
    %obtengo todo lo de la publicacion
    getAutorPubli(Publicacion,PUBLAutor),
    getTextoPubli(Publicacion,PUBLTexto),
    getFechaPubli(Publicacion,PUBLFecha),
    getCuerpoPubli(Publicacion,PUBLCuerpo),
    getDestiPubli(Publicacion,PUBLDesti),
    getComentPubli(Publicacion,PUBLListComent),
    getLikesPubli(Publicacion,PUBLLikes),
    %elimino la pubicacion vieja
    remover(Publicacion,ListaPreguntas,Publi1),
    %agrego la id del comentario a su lista 
    agregarAlFinal(PUBLListComent,ID,ComentariosPubli),
    display(PUBLListComent),
    %creo la publicacion nueva
    crearPregunta(PostId,PUBLAutor,PUBLFecha,PUBLCuerpo,PUBLDesti,ComentariosPubli,PUBLLikes,PublicacionActualizada),
    %agrego la nueva
    agregarAlFinal(Publi1,PublicacionActualizada,PublisFinal),
    %creo la nueva RS
    crearRS(Name,Date,"",CantPubli,PublisFinal,CantUser,UsuariosFinales,ID,ComentariosFinales,Sn2).

%comment a comentario
comment([Name,Date,UserOn,CantPubli,ListaPreguntas,CantUser,ListaUser,CantComent,ListComent],Fecha,PostId,CommentId,TextoComentario,Sn2):-
    write("\n COMMENT CASO 2\n DEBO HACER LA RECURSION\n"),
    not(UserOn=""),!,%existe user online
    %existe comentario
    not(CommentId=0),!,%si la id del comentario es 0 significa que es un comentario a la publicacion
    not(CommentId>CantComent),!,%la id del comentario no puede ser mayor que la cantidad de comentarios
    %existe publicacion
    not(PostId<0),!,%NO EXISTEN POST MENORES A 0
    not(PostId=0),!,%no existe post 0
    not(PostId>CantPubli),!,%la id del post no puede ser mayor a la cantidad de posts
    %reviso que sean strings 
    string(Fecha),string(TextoComentario),
    %ahora creare el comentario
    ID is CantComent + 1,
    crearRespuesta(ID,UserOn,Fecha,TextoComentario,[],PostId,0,Comentario),
    %agrego la publicacion a la lista de publicaciones
    append(ListComent,[Comentario],ComentariosFinales),
    %obtengo al usuario para agregar la publicacion a su lista de publicaciones
    buscarUsuarioNick(ListaUser,UserOn,UsuarioOn),
    %obtenemos todos los datos restantes del usuario
    getIDUser(UsuarioOn,IDu),
    getPASSUser(UsuarioOn,Passwordu),
    getFECHARUser(UsuarioOn,FechaR),
    getAmigosUser(UsuarioOn,Amigos),
    getCompartidasUser(UsuarioOn,CompartidasU),
    getLISTPUBLIUser(UsuarioOn,ListaPubliU),
    %obtengo la lista de comments
    getLISTCOMENTUser(UsuarioOn,ListaComentU),
    %agrego la id a su lista
    append(ListaComentU,[ID],ComentariosUsers),
    %creo el usuario nuevamente pero con la lista nueva
    crearUser(IDu,UserOn,Passwordu,FechaR,ListaPubliU,ComentariosUsers,Amigos,CompartidasU,UserActualizado),
    %elimino el usuario antiguo
    remover(UsuarioOn,ListaUser,ListaUserActualizada),
    %agrego el user actualizado
    agregarAlFinal(ListaUserActualizada,UserActualizado,UsuariosFinales),

    %ahora debo obtener la publicacion
    buscarComentario(PostId,ListaPreguntas,Publicacion),
    %obtengo la lista de comentarios  de la publicacion para buscar el comentario
    getComentPubli(Publicacionnt,PUBLListComent),

    estaEN(CommentId,PUBLListComent),!,%si no esta dentro de los comentarios entones entregara false y no buscara mas
    
    %ahora si el comentario esta en la lista de comentarios de tal publicacion, buscare el comentario 
    buscarComentario(CommentId,ListComent,ComentarioBuscado),
    
    %ahora obtendre todo lo del comentario al que se le comenta para agregar el nuevo comentario a su lista
    getAUTORResp(ComentarioBuscado,AutorB),
    getFECHAResp(ComentarioBuscado,FechaB),
    getCUERPOTXTResp(ComentarioBuscado,CuerpoTextoB),
    getLISTCOMENTResp(ComentarioBuscado,ListComentB),
    getLIKESResp(ComentarioBuscado,LikesB),

    %agrego la id nueva
    append(ListComentB,[ID],ComentariosComentario),
    %creare el comentario con su lista de comentarios actualizadas
    crearRespuesta(CommentId,AutorB,FechaB,CuerpoTextoB,ComentariosComentario,PostId,LikesB,Comentario1),
    %remuevo el comentario viejo y agrego el actualizado (comentario1)
    remover(ComentarioBuscado,ListComent,ListComent2),
    %agrego los dos nuevos comentarios
    append(ListComent2,[Comentario],ListComent3),
    append(ListComent3,[Comentario1],ListComentFinal),

    %ahora que ya agrege el comentario y el anterior actualizados, creare la RS final
    crearRS(Name,Date,"",CantPubli,ListaPreguntas,CantUser,UsuariosFinales,ID,ListComentFinal,Sn2).

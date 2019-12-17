--Tomás Miguez       094/19
--Fermin Schlottmann 160/19
--Francisco Marziano ???/19
type Set a = [a]
type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, Set Usuario) -- (usuario que publica, texto publicacion, likes)
type RedSocial = (Set Usuario, Set Relacion, Set Publicacion)

-- Funciones basicas

usuarios :: RedSocial -> Set Usuario
usuarios (us, _, _) = us

relaciones :: RedSocial -> Set Relacion
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> Set Publicacion
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> Set Usuario
likesDePublicacion (_, _, us) = us

-- Ejercicios

-- Dada una red social retorna un conjunto con los nombres de todos los usuarios.
nombresDeUsuarios :: RedSocial -> Set String
nombresDeUsuarios ([], _, _) = []
nombresDeUsuarios ((x:xs), rel, ps) = nombreDeUsuario x : nombresDeUsuarios (xs, rel, ps)

-- Dada una red social y un usuario retorna el conjunto de amigos del mismo
amigosDe :: RedSocial -> Usuario -> Set Usuario
amigosDe (_,rs,_) (id, _) = amigosDeAux rs id

-- Dado un conjunto de relaciones y un id retorna el conjunto de usuarios que estan relacionados con el propietario de dicho id.
-- Recorre el conjunto de relaciones verificando si el id aparece en alguno de los dos usuarios de cada tupla
-- En caso afirmativo, agrega al otro usuario de la tupla al conjunto de amigos
amigosDeAux :: Set Relacion -> Integer -> Set Usuario
amigosDeAux [] _ = []
amigosDeAux (((id1, n1), (id2, n2)): r) id
        |id == id1 = (id2, n2): amigosDeAux r id
        |id == id2 = (id1, n1): amigosDeAux r id
        |otherwise = amigosDeAux r id

-- Dada una red social y un usuario retorna la cantidad de amigos de dicho usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs us = length (amigosDe rs us)

-- Dada una red social retorna el usuario con mas amigos. De existir más de uno devuelve cualquiera de ellos.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

-- Dada una red social retorna True si algún usuario tiene más de un millón de amigos
-- Toma al usuario con más amigos de la red y retorna True si la cantidad de amigos que posee cumple la condición
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos rs
        |cantidadDeAmigos rs us > 10^6 = True
        |otherwise = False
         where us = usuarioConMasAmigos rs

-- Dada una red social y un usuario retorna el conjunto de publicaciones del mismo.
publicacionesDe :: RedSocial -> Usuario -> Set Publicacion
publicacionesDe (_, _, ps) us 
                | length ps == 0                       = []
                | us == usuarioDePublicacion (head ps) = (head ps): (publicacionesDe ([], [], tail ps) us)
                | otherwise                            = (publicacionesDe ([], [], tail ps) us)

-- Dada una red social y un usuario retorna el conjunto de publicaciones a las que el usuario les dió like.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> Set Publicacion
publicacionesQueLeGustanA (_, _, []) us = []
publicacionesQueLeGustanA (us, rel, (u, texto, [(id1, n1)]):publ) (id, nom) | id1 == id = (u, texto, [(id1, n1)]) : publicacionesQueLeGustanA (us, rel, publ) (id, nom)
                                                                            | otherwise = publicacionesQueLeGustanA (us, rel, publ) (id, nom)

-- Dada una red social y dos usuarios indica si les gustan las mismas publicaciones
-- Toma los conjuntos de publiaciones que le gustan a cada usuario y retorna True si son iguales
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones r1 u1 u2
        |contenidoP lu1 lu2 == True && contenidoP lu2 lu1 == True = True
        |otherwise = False
         where lu1 = publicacionesQueLeGustanA r1 u1
               lu2 = publicacionesQueLeGustanA r1 u2

-- Dado un usuario y un conjunto de usuarios indica si este pertenece al conjunto
elemU :: Usuario -> Set Usuario -> Bool 
elemU _ [] = False
elemU (id1, n1) ((id2, n2): us)
        |id1 == id2 = True
        |otherwise = elemU (id1, n1) us

-- Dados dos conjuntos de usuarios retorna True si el primero esta contenido en el segundo
contenidoU :: Set Usuario -> Set Usuario -> Bool
contenidoU _ [] = False
contenidoU [] _ = True
contenidoU (x:xs) y
        |elemU x y == True = contenidoU xs y
        |otherwise = False

-- Dados dos conjuntos de usuarios retorna True si estos son iguales
igualU :: Set Usuario -> Set Usuario -> Bool 
igualU u1 u2
        |contenidoU u1 u2 == True && contenidoU u2 u1 == True = True
        |otherwise = False

-- Dada una publicacion y un conjunto de publicaciones indica si este pertenece al conjunto
elemP :: Publicacion -> Set Publicacion -> Bool 
elemP _ [] = False
elemP (u1, t1, l1) ((u2, t2, l2): ps)
        |u1 == u2 && t1 == t2 && igualU l1 l2 == True  = True
        |otherwise = elemP (u1, t1, l1) ps

-- Dados dos conjuntos de publicaciones retorna True si el primero esta contenido en el segundo
contenidoP :: Set Publicacion -> Set Publicacion -> Bool --Falta descripcion
contenidoP _ [] = False
contenidoP [] _ = True
contenidoP (x: xs) y
        |elemP x y == True = contenidoP xs y
        |otherwise = False

-- Dada una red social y un usuario u, indica si existe un usuario que le puso like a todas las publicaciones de u.
esUnSeguidorFiel :: Set Publicacion -> Usuario -> Bool --Verifica si el usuario seg es un seguidor fiel del usuario con conjunto de publicaciones pubDe
esUnSeguidorFiel pubDe seg | length pubDe == 0                          = True
                           | elem seg (likesDePublicacion (head pubDe)) = esUnSeguidorFiel (tail pubDe) seg
                           | otherwise                                  = False

tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel rs u | length (usuarios rs) == 0                                    = False
                         | esUnSeguidorFiel (publicacionesDe rs u) (head (usuarios rs)) = True
                         | otherwise                                                    = tieneUnSeguidorFiel (tail (usuarios rs), [], publicaciones rs) u

-- Dada una red social y dos usuarios, indica si existe una secuencia de usuarios relacionados para llegar del primero al segundo.
dfs :: RedSocial -> Usuario -> Set Usuario -> Set Usuario -> Bool --Hace lo que pide el ej, la otra solo le da los valores iniciales a los parametros
dfs rs usBusc visitados amigosPorVisitar 
    | length amigosPorVisitar == 0 = False
    | usAct == usBusc              = True
    | not(elem usAct visitados)    = dfs rs usBusc (usAct:visitados) ((amigosDe rs usAct) ++ (tail amigosPorVisitar))
    | otherwise                    = dfs rs usBusc visitados (tail amigosPorVisitar)
      where usAct = head amigosPorVisitar

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos rs usOri usBusc = dfs rs usBusc [] [usOri]

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
relacion4_5 = (usuario4, usuario5)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario4])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario4])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_1, publicacion1_2, publicacion1_3, publicacion1_4, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

usuariosC = [usuario1, usuario2, usuario3, usuario4, usuario5]
relacionesC = [relacion1_2,relacion2_3,relacion3_4, relacion4_5]
redC = (usuariosC, relacionesC, [])

{-
Tests:
======

nombresDeUsuarios redA ~~~> {Juan, Natalia, Pedro, Mariela}

amigosDe redA usuario1 ~~~> {usuario2, usuario4}
amigosDe redA usuario2 ~~~> {usuario1, usuario3, usuario4}
amigosDe redA usuario4 ~~~> {usuario2, usuario1, usuario3}

cantidadDeAmigos redA usuario1 ~~~> 2

Notar que para usuarioConMasAmigos podría haber más de un resultado válido.
usuarioConMasAmigos redA == usuario2 || usuarioConMasAmigos redA == usuario4 ~~~> True

estaRobertoCarlos redA ~~~> False

publicacionesDe redA usuario2 ~~~> {publicacion2_1, publicacion2_2}

publicacionesQueLeGustanA redA usuario1 ~~~> {publicacion2_2, publicacion4_1}

lesGustanLasMismasPublicaciones redB usuario1 usuario2 ~~~> False

tieneUnSeguidorFiel redA usuario1 ~~> True
tieneUnSeguidorFiel redA usuario2 ~~> True
tieneUnSeguidorFiel redA usuario3 ~~> False

existeSecuenciaDeAmigos redA usuario1 usuario3 ~~~> True

----

nombresDeUsuarios redB ~~~> {Juan, Pedro, Natalia}

amigosDe redB usuario1 ~~~> {usuario2}
amigosDe redB usuario2 ~~~> {usuario1, usuario3}
amigosDe redB usuario3 ~~~> {usuario2}
amigosDe redB usuario5 ~~~> {}

cantidadDeAmigos redB usuario1 ~~~> 1
cantidadDeAmigos redB usuario2 ~~~> 2
cantidadDeAmigos redB usuario3 ~~~> 1
cantidadDeAmigos redB usuario5 ~~~> 0

usuarioConMasAmigos redB ~~~> usuario2

estaRobertoCarlos redB ~~~> False

publicacionesDe redB usuario1 ~~~> {publicacion1_1, publicacion1_2, publicacion1_3, publicacion1_4}
publicacionesDe redB usuario2 ~~~> {}
publicacionesDe redB usuario3 ~~~> {publicacion3_1, publicacion3_2, publicacion3_3}
publicacionesDe redB usuario5 ~~~> {}

publicacionesQueLeGustanA redB usuario1 ~~~> {}
publicacionesQueLeGustanA redB usuario2 ~~~> {publicacion1_1, publicacion1_3, publicacion3_2, publicacion3_3}
publicacionesQueLeGustanA redB usuario3 ~~~> {}
publicacionesQueLeGustanA redB usuario5 ~~~> {}

lesGustanLasMismasPublicaciones redB usuario1 usuario3 ~~~> True
lesGustanLasMismasPublicaciones redB usuario1 usuario2 ~~~> False

tieneUnSeguidorFiel redB usuario1 ~~> False
tieneUnSeguidorFiel redB usuario2 ~~> False
tieneUnSeguidorFiel redB usuario3 ~~> False
tieneUnSeguidorFiel redB usuario5 ~~> False

existeSecuenciaDeAmigos redB usuario1 usuario3 ~~~> True
existeSecuenciaDeAmigos redB usuario1 usuario5 ~~~> False
existeSecuenciaDeAmigos redC usuario1 usuario5 ~~~> True}

-}

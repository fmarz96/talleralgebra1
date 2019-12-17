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

amigosDeAux :: Set Relacion -> Integer -> Set Usuario
amigosDeAux [] _ = []
amigosDeAux (((id1, n1), (id2, n2)): r) id
        |id == id1 = (id2, n2): amigosDeAux r id
        |id == id2 = (id1, n1): amigosDeAux r id
        |otherwise = amigosDeAux r id

-- Dada una red social y un usuario retorna la cantidad de amigos de dicho usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos = undefined

-- Dada una red social retorna el usuario con mas amigos. De existir más de uno devuelve cualquiera de ellos.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

-- Dada una red social retorna True si algún usuario tiene más de un millón de amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos rs
        |cantidadDeAmigos rs us > 10^6 = True
        |otherwise = False
         where us = usuarioConMasAmigos rs

-- Dada una red social y un usuario retorna el conjunto de publicaciones del mismo.
publicacionesDe :: RedSocial -> Usuario -> Set Publicacion
publicacionesDe = undefined

-- Dada una red social y un usuario retorna el conjunto de publicaciones a las que el usuario les dió like.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> Set Publicacion
publicacionesQueLeGustanA (_, _, []) us = []
publicacionesQueLeGustanA (us, rel, (u, texto, [(id1, n1)]):publ) (id, nom) | id1 == id = (u, texto, [(id1, n1)]) : publicacionesQueLeGustanA (us, rel, publ) (id, nom)
                                                                  	        | otherwise = publicacionesQueLeGustanA (us, rel, publ) (id, nom)

-- Dada una red social y dos usuarios indica si les gustan las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones r1 u1 u2
        |contenidoP lu1 lu2 == True && contenidoP lu2 lu1 == True = True
        |otherwise = False
         where lu1 = publicacionesQueLeGustanA r1 u1
               lu2 = publicacionesQueLeGustanA r1 u2

elemU :: Usuario -> Set Usuario -> Bool
elemU _ [] = False
elemU (id1, n1) ((id2, n2): us)
        |id1 == id2 = True
        |otherwise = elemU (id1, n1) us

contenidoU :: Set Usuario -> Set Usuario -> Bool
contenidoU _ [] = False
contenidoU [] _ = True
contenidoU (x:xs) y
        |elemU x y == True = contenidoU xs y
        |otherwise = False

igualU :: Set Usuario -> Set Usuario -> Bool
igualU u1 u2
        |contenidoU u1 u2 == True && contenidoU u2 u1 == True = True
        |otherwise = False

elemP :: Publicacion -> Set Publicacion -> Bool
elemP _ [] = False
elemP (u1, t1, l1) ((u2, t2, l2): ps)
        |u1 == u2 && t1 == t2 && igualU l1 l2 == True  = True
        |otherwise = elemP (u1, t1, l1) ps

contenidoP :: Set Publicacion -> Set Publicacion -> Bool
contenidoP _ [] = False
contenidoP [] _ = True
contenidoP (x: xs) y
        |elemP x y == True = contenidoP xs y
        |otherwise = False

-- Dada una red social y un usuario u, indica si existe un usuario que le puso like a todas las publicaciones de u.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- Dada una red social y dos usuarios, indica si existe una secuencia de usuarios relacionados para llegar del primero al segundo.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined
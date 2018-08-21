module Backend exposing(..)
import Models exposing(Movie, Preferences)

completaAca = identity

-- **************
-- Requerimiento: filtrar películas por su título a medida que se escribe en el buscador;
-- **************

filtrarPeliculasPorPalabrasClave : String -> List Movie -> List Movie
filtrarPeliculasPorPalabrasClave palabras = List.filter (peliculaTienePalabrasClave palabras)

-- esta función la dejamos casi lista, pero tiene un pequeño bug. ¡Corregilo!
--
-- Además tiene dos problemas, que también deberías corregir:
--
-- * distingue mayúsculas de minúsculas, pero debería encontrar a "Lion King" aunque escriba "kINg"
-- * busca una coincidencia exacta, pero si escribís "Avengers Ultron" debería encontrar a "Avengers: Age Of Ultron"
--
peliculaTienePalabrasClave palabras pelicula = String.contains (mayus palabras) (mayus pelicula.title)

mayus : String -> String
mayus = String.toUpper

-- **************
-- Requerimiento: visualizar las películas según el género elegido en un selector;
-- **************

filtrarPeliculasPorGenero : String -> List Movie -> List Movie
filtrarPeliculasPorGenero genero = List.filter ((List.member genero) << .genre)

-- **************
-- Requerimiento: filtrar las películas que sean aptas para menores de edad,
--                usando un checkbox;
-- **************

filtrarPeliculasPorMenoresDeEdad : Bool -> List Movie -> List Movie
filtrarPeliculasPorMenoresDeEdad mostrarSoloMenores listaPeliculas = if mostrarSoloMenores then List.filter .forKids listaPeliculas else listaPeliculas


-- **************
-- Requerimiento: ordenar las películas por su rating;
-- **************

ordenarPeliculasPorRating : List Movie -> List Movie
ordenarPeliculasPorRating = (List.reverse << List.sortBy .rating)

-- **************
-- Requerimiento: dar like a una película
-- **************

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula id = List.map (sumarLikeAPelicula id)

sumarLikeAPelicula : Int -> Movie -> Movie
sumarLikeAPelicula id peli = if id == peli.id then Movie peli.id peli.poster peli.title peli.rating peli.genre peli.link (peli.likes + 1) peli.matchPercentage peli.forKids peli.actors else peli

-- **************
-- Requerimiento: cargar preferencias a través de un popup modal,
--                calcular índice de coincidencia de cada película y
--                mostrarlo junto a la misma;
-- **************

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias listaPelis = List.map (calcularPorcentaje preferencias) listaPelis

calcularPorcentaje : Preferences -> Movie -> Movie
calcularPorcentaje preferencias peli = Movie peli.id peli.poster peli.title peli.rating peli.genre peli.link peli.likes (sumarPorcentaje preferencias  peli) peli.forKids peli.actors

sumarPorcentaje : Preferences -> Movie -> Int
sumarPorcentaje preferencias peli = (contienePalabraClave preferencias.keywords peli.title) + (tieneActorPreferido preferencias.favoriteActor peli.actors) + (esGeneroPreferido preferencias.genre peli.genre)

contienePalabraClave : String -> String -> Int
contienePalabraClave palabrasClave tituloPeli = List.foldl ((+) << (porcentajeCorrespondiente 20 (String.words tituloPeli))) 0 (String.words palabrasClave)

esGeneroPreferido : String -> List String -> Int
esGeneroPreferido generoPreferido generoPeli = porcentajeCorrespondiente 60 generoPeli generoPreferido 

tieneActorPreferido : String -> List String -> Int
tieneActorPreferido actorPreferido actorPeli = porcentajeCorrespondiente 50 actorPeli actorPreferido 

porcentajeCorrespondiente : Int -> List String ->  String -> Int 
porcentajeCorrespondiente percent listaPalabras palabras = if ((List.member (mayus palabras) (listaAMayus listaPalabras))) then percent else 0

listaAMayus = List.map mayus

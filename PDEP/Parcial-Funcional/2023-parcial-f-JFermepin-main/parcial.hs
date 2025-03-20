import Text.Show.Functions

data Personaje = UnPersonaje {
    nombre :: String,
    dinero :: Int,
    felicidad :: Int -- >=0
} deriving(Show)

type Actividad = Personaje -> Personaje

type Trabajo = String
plantaNuclear, director, mafia, tienda, retiro:: Trabajo

plantaNuclear = "planta nuclear"
director = "escuela elemental"
mafia = "mafia"
tienda = "tienda para zurdos"
retiro = "retiro de ancianos"

type Actividades = [Actividad]
listaActividadesHomero, listaActividadesLisa, listaActividadesFlanders, listaActividadesSkinner, listaActividadesBart :: Actividades

listaActividadesHomero = [comerDonas 12, irATrabajar plantaNuclear, irALaIglesia]
listaActividadesLisa = [irALaEscuela, irALaIglesia]
listaActividadesFlanders = [irATrabajar tienda, irALaIglesia]
listaActividadesSkinner = [irATrabajar director]
listaActividadesBart = [irATrabajar mafia, irALaEscuela]

lisa = UnPersonaje "Lisa" 20 100
homero = UnPersonaje "Homero" 2000001 30
srBurns = UnPersonaje "Sr. Burns" 2000000 0
skinner = UnPersonaje "Sr. Skinner" 100 15
flanders = UnPersonaje "Ned Flanders" 200 300
bart = UnPersonaje "Bart" 6 45

-------Parte 1-------

irALaEscuela :: Actividad
irALaEscuela personaje
    |esLisa personaje = UnPersonaje "Lisa" (dinero personaje) (felicidad personaje + 20)
    |otherwise = restarFelicidad 20 personaje

comerDonas :: Int -> Actividad
comerDonas cantidad personaje
    |tieneDinero 10 personaje = (restarDinero 10 . sumarFelicidad cantidad) personaje 
    |otherwise = personaje

irATrabajar :: Trabajo -> Actividad
irATrabajar trabajo personaje
    |trabajo == director = restarFelicidad 20 personaje
    |otherwise = sumarDinero (length trabajo) personaje

irALaIglesia :: Actividad
irALaIglesia personaje
    |esFlanders personaje = (restarDinero 30 . sumarFelicidad 50) personaje
    |otherwise = (restarDinero 5 . restarFelicidad 35) personaje

hacerActividades :: Actividades -> Personaje -> Personaje
hacerActividades actividades personaje = foldl (flip ($)) personaje actividades


{-

homero = UnPersonaje "Homero" 20 30
listaActividadesHomero = [comerDonas 12]

hacerActividades listaActividadesHomero -> UnPersonaje {nombre = "Homero", dinero = 10, felicidad = 42}

-------

skinner = UnPersonaje "Sr. Skinner" 100 15
listaActividadesSkinner = [irATrabajar director]

hacerActividades listaActividadesSkinner-> UnPersonaje {nombre = "Sr. Skinner", dinero = 100, felicidad = 0}


-------

lisa = UnPersonaje "Lisa" 20 100
listaActividadesLisa = [irALaEscuela, irALaIglesia]

hacerActividades listaActividadesLisa lisa -> UnPersonaje {nombre = "Lisa", dinero = 15, felicidad = 85}

-}

-------Utilidades-------

restarFelicidad :: Int -> Personaje -> Personaje
restarFelicidad valor personaje
    |felicidad personaje >= valor = UnPersonaje (nombre personaje) (dinero personaje) (felicidad personaje - valor)
    |otherwise = UnPersonaje (nombre personaje) (dinero personaje) 0

sumarFelicidad :: Int -> Personaje -> Personaje
sumarFelicidad valor personaje = UnPersonaje (nombre personaje) (dinero personaje) (felicidad personaje + valor)

tieneDinero :: Int -> Personaje -> Bool
tieneDinero monto personaje = dinero personaje >= monto

restarDinero :: Int -> Personaje -> Personaje
restarDinero monto personaje = UnPersonaje (nombre personaje) (dinero personaje - monto) (felicidad personaje) 

sumarDinero :: Int -> Personaje -> Personaje
sumarDinero monto personaje = UnPersonaje (nombre personaje) (dinero personaje + monto) (felicidad personaje) 

esLisa :: Personaje -> Bool
esLisa personaje = nombre personaje == "Lisa"

esFlanders :: Personaje -> Bool
esFlanders personaje = nombre personaje == "Ned Flanders"

-------Parte 2-------

type Logro = Personaje -> Bool

serMillonario :: Logro
serMillonario personaje = dinero personaje > dinero srBurns

alegrarse :: Int -> Logro
alegrarse nivelDeseado personaje = felicidad personaje > nivelDeseado

verAKrosti :: Logro
verAKrosti personaje = dinero personaje >= 10

fueConMoe :: Logro
fueConMoe personaje = dinero personaje < 10 && felicidad personaje > 50

--A--

esDecisiva :: Actividad -> Logro -> Personaje -> Bool
esDecisiva actividad logro personaje
    |not (logro personaje) =  (logro . actividad) personaje
    |otherwise = False

--B--

primeraDecisiva :: Personaje -> Logro -> Actividades -> Personaje
primeraDecisiva personaje logro [] = personaje

primeraDecisiva personaje logro actividades
    |esDecisiva (head actividades) logro personaje =  (head actividades) personaje
    |otherwise = primeraDecisiva personaje logro (tail actividades)

--C--

trabajarSiempre = [irATrabajar plantaNuclear, irATrabajar plantaNuclear]

listaInfinita :: Actividades -> Actividades
listaInfinita actividades = head actividades : listaInfinita actividades

{-

Si se usa:

homero = UnPersonaje "Homero" 1999987 30

primeraDecisiva homero serMillonario (listaInfinita trabajarSiempre)

Va a devolver:

UnPersonaje {nombre = "Homero", dinero = 2000001, felicidad = 30}

Esto es ya que Haskell utiliza "lazy evaluation" y solo utiliza lo que necesita, en este caso, por mas que la lista sea infinita

Ahora si se usa:

UnPersonaje {nombre = "Homero", dinero = 2000001, felicidad = 30}

Ahora si el programa nunca termina, ya que la lista es infinita y nunca va a terminar de recorrerla

-}
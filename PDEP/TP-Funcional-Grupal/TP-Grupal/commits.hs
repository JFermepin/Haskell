----Primera Parte----

import Text.Show.Functions

-----------------------------------------------------------------------------------------------------------------------------------------
--Estructuras----------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

data Archivo = UnArchivo{
    nombre::String,
    contenido::String
} deriving (Show,Eq)

data Carpeta = UnaCarpeta{
    nombreCarpeta::String,
    componentes:: [Archivo]
} deriving(Show,Eq)

data Commit = UnCommit{
    descripcion :: String,
    listaCambios :: [Carpeta -> Carpeta]
} deriving(Show)

data Branch = UnBranch {
    listaCommits :: [Commit]
} deriving(Show)

-----------------------------------------------------------------------------------------------------------------------------------------
--Instancias-----------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

juego = UnArchivo "juego" "LoL"
examen = UnArchivo "apuntes" "resumen"
am1 = UnArchivo "am1" "tabla de derivadas"
--pdep = UnArchivo "pdep" "paradigma funcional"
leeme = UnArchivo "leeme.md" ""
parcial = UnArchivo "parcial.hs" ""

universidad = UnaCarpeta "universidad" [examen,am1]
diversion = UnaCarpeta "diversion" [juego]
trabajo = UnaCarpeta "trabajo" []
pdep = UnaCarpeta "pdep" []

commit1 = UnCommit "hace quilombo" []
commit2 = UnCommit "hace quilombo otra vez" [(agregar juego " - CsGo - Valorant"),(sacar juego 0 3)]
commit3 = UnCommit "commit inicial" [(crearArchivo leeme),(crearArchivo parcial),(agregar leeme "Este es un TP")]

branch1 = UnBranch [commit1,commit2,commit3]

-----------------------------------------------------------------------------------------------------------------------------------------
--Utilidades-----------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

contieneArchivo::Archivo -> Carpeta -> Bool
contieneArchivo archivo carpeta = elem archivo (componentes carpeta)

siEsVacioCarpeta :: Carpeta -> Bool
siEsVacioCarpeta carpeta = length (componentes carpeta) == 0

-----------------------------------------------------------------------------------------------------------------------------------------
--Crear----------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

crearArchivo :: Archivo -> Carpeta -> Carpeta
crearArchivo archivo carpeta 
    | contieneArchivo archivo carpeta = carpeta
    | otherwise = UnaCarpeta (nombreCarpeta carpeta) (archivo:componentes carpeta)

-----------------------------------------------------------------------------------------------------------------------------------------
--Eliminar-------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

eliminar :: Archivo -> Carpeta -> Carpeta
eliminar archivo carpeta 
    | siEsVacioCarpeta carpeta = carpeta
    | otherwise = UnaCarpeta (nombreCarpeta carpeta) (filter (/= archivo) (componentes carpeta))

-----------------------------------------------------------------------------------------------------------------------------------------
--Vaciar---------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

vaciar :: Carpeta -> Carpeta
vaciar carpeta = UnaCarpeta (nombreCarpeta carpeta) []

-----------------------------------------------------------------------------------------------------------------------------------------
--Agregar--------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

agregar :: Archivo -> String -> Carpeta -> Carpeta
agregar archivo texto carpeta
    | siEsVacioCarpeta carpeta = carpeta -- Si la carpeta está vacía, no hay archivos para modificar
    | otherwise = UnaCarpeta (nombreCarpeta carpeta) (map (agregarContenidoArchivo (nombre archivo) texto) (componentes carpeta))

agregarContenidoArchivo :: String -> String -> Archivo -> Archivo
agregarContenidoArchivo nombreArchivo texto archivo
    | nombre archivo == nombreArchivo = archivo {contenido = contenido archivo ++ texto}
    | otherwise = archivo

-----------------------------------------------------------------------------------------------------------------------------------------
--Sacar----------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

sacar :: Archivo -> Int -> Int  -> Carpeta -> Carpeta
sacar archivo desde hasta carpeta
    | siEsVacioCarpeta carpeta = carpeta -- Si la carpeta está vacía, no hay archivos para modificar
    | otherwise = UnaCarpeta (nombreCarpeta carpeta) (map (sacarContenidoArchivo (nombre archivo) desde hasta) (componentes carpeta))

sacarContenidoArchivo :: String -> Int -> Int -> Archivo -> Archivo
sacarContenidoArchivo nombreArchivo desde hasta archivo
    | nombre archivo == nombreArchivo = UnArchivo (nombre archivo) (take desde (contenido archivo) ++ drop hasta (contenido archivo))
    | otherwise = archivo

-----------------------------------------------------------------------------------------------------------------------------------------
--Es Inutil------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

esInutil :: Carpeta -> Commit -> Bool
esInutil carpeta commit = foldl (flip ($)) carpeta (listaCambios commit) == carpeta

esInutilAlReves :: Carpeta -> Commit -> Bool
esInutilAlReves carpeta commit = foldr ($) carpeta (listaCambios commit) == carpeta

-----------------------------------------------------------------------------------------------------------------------------------------
--Commitear------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

commitear :: Carpeta -> Commit -> Carpeta
commitear carpeta commit = foldl (flip ($)) carpeta (listaCambios commit)

----Segunda Parte----

-----------------------------------------------------------------------------------------------------------------------------------------
--Checkout-------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

checkout :: Branch -> Carpeta -> Carpeta
checkout branch carpeta = foldl commitear carpeta (listaCommits branch)

checkoutNuevo :: Branch -> Commit -> Carpeta -> Carpeta
checkoutNuevo branch commit carpeta = foldl commitear carpeta (take (posicionCommit branch commit) (listaCommits branch))

posicionCommit :: Branch -> Commit -> Int
posicionCommit branch commit 
    | descripcion ((listaCommits branch) !! (length (listaCommits branch)-1)) == descripcion commit = (length (listaCommits branch))
    | otherwise = posicionCommit (UnBranch (take (length (listaCommits branch)-1) (listaCommits branch))) commit

-----------------------------------------------------------------------------------------------------------------------------------------
--Log------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

logearArchivo :: Branch -> Archivo -> [String]
logearArchivo branch archivo  = filter (/="") (map (buscaArchivo archivo) (listaCommits branch))

buscaArchivo :: Archivo -> Commit -> String
buscaArchivo archivo commit
    | dejarArchivo (commitear (UnaCarpeta "" [archivo]) commit) archivo == UnaCarpeta "" [archivo] = ""  
    | otherwise = descripcion commit

dejarArchivo :: Carpeta -> Archivo -> Carpeta
dejarArchivo carpeta archivo = UnaCarpeta (nombreCarpeta carpeta) (filter (== archivo) (componentes carpeta))
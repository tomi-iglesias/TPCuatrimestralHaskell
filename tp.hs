import Text.Show.Functions()

main :: IO ()
main = return ()

type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)
type Tecnico = Auto -> Auto
 
-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year
 
data Auto = Auto {
 patente :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm :: Int,
 temperaturaAgua :: Int,
 ultimoArreglo :: Fecha
} deriving Show

auto :: Auto
auto = Auto "DJA123" [0.6,0.4,0.5,0.8] 8500 95 (14,03,1997)

auto2 :: Auto
auto2 = Auto "DJA123" [0.6,0.4,0.6,0.8] 4000 95 (14,03,1997)

-- Punto 1

costoReparacion :: Auto -> Int
costoReparacion unAuto 
    | (length.patente) unAuto == 7 = 12500
    | estaEnRango (take 2 (patente unAuto)) = (calculoPatental.patente) unAuto
    | otherwise = 15000

estaEnRango :: Patente -> Bool
estaEnRango dosCaracteres = dosCaracteres >= "DJ" && dosCaracteres <= "NB"

calculoPatental :: Patente -> Int
calculoPatental unaPatente 
    | last unaPatente == '4' = 3000 * (length unaPatente)
    | otherwise = 20000

-- Punto 2

esAutoPeligroso :: Auto -> Bool
esAutoPeligroso unAuto = (head.desgasteLlantas) unAuto > 0.5

necesitaRevision :: Auto -> Bool
necesitaRevision unAuto = (anio.ultimoArreglo) unAuto <= 2015

-- Punto 3
-- Parte 1

regularRpm :: Auto -> Int
regularRpm unAuto 
    | rpm unAuto >= 2000 = 2000
    | otherwise = rpm unAuto

cambiarCubiertas :: Float -> Float
cambiarCubiertas _ = 0 

temperaturaAguaA90 :: Auto -> Auto
temperaturaAguaA90 unAuto = unAuto { temperaturaAgua = 90 }

alfa :: Tecnico
alfa unAuto = unAuto { rpm = regularRpm unAuto }

bravo :: Tecnico
bravo unAuto = unAuto {desgasteLlantas = map cambiarCubiertas (desgasteLlantas unAuto)}

charly :: Tecnico
charly unAuto = (alfa.bravo) unAuto

-- Parte 2

tango :: Tecnico
tango unAuto = unAuto

lima :: Tecnico
lima unAuto = unAuto {desgasteLlantas = map cambiarCubiertas ((take 2.desgasteLlantas) unAuto) ++ (drop 2.desgasteLlantas) unAuto }

zulu :: Tecnico
zulu unAuto = (lima.temperaturaAguaA90) unAuto

-- Punto 4

estaOrdenadoSegun :: [Auto] -> Bool
estaOrdenadoSegun [] = True
estaOrdenadoSegun [unAuto] = (odd.tipoDesgaste.desgasteLlantas) unAuto 
estaOrdenadoSegun (primero : segundo : cola)
    | (odd.tipoDesgaste.desgasteLlantas) primero && (even.tipoDesgaste.desgasteLlantas) segundo = estaOrdenadoSegun cola
    | otherwise = False

tipoDesgaste :: [Desgaste] -> Int
tipoDesgaste listaDesgaste = (round.(*10).sum) listaDesgaste 


-- Punto 5

cambiarFechaReparacion :: Fecha -> Tecnico
cambiarFechaReparacion unaFecha unAuto = unAuto {ultimoArreglo = unaFecha}

ordenReparacion :: Auto -> Fecha -> [Tecnico] -> Auto
ordenReparacion unAuto unaFecha listaTecnicos = (cambiarFechaReparacion unaFecha.componerListaTecnicos listaTecnicos) unAuto

componerListaTecnicos :: [Tecnico] -> Tecnico
componerListaTecnicos listaTecnicos = foldl1 (.) listaTecnicos

-- Punto 6
-- Parte 1

tecnicosQueDejaAutoEnCondiciones :: [Tecnico] -> Auto -> [Tecnico]
tecnicosQueDejaAutoEnCondiciones listaTecnicos unAuto = filter (repararAuto unAuto) listaTecnicos 

repararAuto :: Auto -> Tecnico -> Bool
repararAuto unAuto unTecnico = (esAutoPeligroso.unTecnico) unAuto 

-- Parte 2

costoRepararAutosQueNecesitanRevision :: [Auto] -> Int
costoRepararAutosQueNecesitanRevision listaAutos = (sum.map costoReparacion.filter necesitaRevision) listaAutos

{-- Parte 7
Parte 1
Si, se podria, dado que como nuestro objetivo es obtener el primer tecnico que deje el auto en condiciones,
utlizando la funcion head, podemos obtener el primer elemento de la lista, sin necesidad de termine de cargar el resto
de los elementos de la lista infinita, ya que dicha funcion utiliza la forma de trabajo "Lazy Evaluation".

Ejemplo : head.(tecnicosQueDejaAutoEnCondiciones [listaInifnita de Tecnicos] unAuto)

Parte 2

En base al punto “Dada una lista de autos, saber cuál es el costo de reparación de los autos que necesitan revisión.”
¿podríamos tener una lista infinita de autos? 

No se podria en este caso, dada que la funcion sum, utiliza la forma de trabajo "Eager Evaluation", es decir, espera a
tener todos los valores de la lista, para luego, sumarlos, por lo tanto, nuestro programa al ejecutar esto quedaria
ejecutandose infinitamente. 

Ejemplo sum [1..] -- no funciona
como tampoco sum [listaInfinitaDeAutosQueNecesitenrevision..]

Y si tomáramos en cuenta los tres primeros autos que necesitan revisión, ¿cómo debería cambiar la función? Por otra parte, 
¿esta versión aceptaría una lista infinita de autos? Modifique la función 6.b con otro nombre y justifique sus respuestas.

En este caso si, aceptaria una lista infinita de autos, dado que usariamos la funcion "take 3" para luego usar la funcion sum, por lo tanto, primero tomariamos
los primeros 3 elementos de la lista infinita que necesitan revision, y luego, sum, con ya esos 3 valores, los sumaria.

La funcion seria sum.take 3 [listaInfinitaDeAutosQueNecesitenrevision..]

costoRepararAutosQueNecesitanRevision :: [Auto] -> Int
costoRepararAutosQueNecesitanRevision listaAutos = (sum.take 3.map costoReparacion.filter necesitaRevision) listaAutos

--}
module Library where
import PdePreludat

type Durabilidad = Number
type Escudo = Number
type Ataque = Number
type Poder = Nave -> Nave

data Nave = UnaNave {
    durabilidad :: Durabilidad,
    escudo :: Escudo,
    ataque :: Ataque,
    poder :: Poder
} deriving (Show, Eq)

--Punto 1

tieFighter, xWing, naveDarthVader, millenniumFalcon :: Nave
tieFighter = UnaNave 200 100 50 turbo
xWing = UnaNave 300 150 100 reparacionDeEmergencia
naveDarthVader = UnaNave 500 300 200 superTurbo
millenniumFalcon = UnaNave 1000 500 50 (reparacionDeEmergencia . incrementarEscudos 100)

--Nueva Nave
--el poder aTodaMarcha aumenta lña velocidad de disparo incrementando el ataque en 100
batiMovil :: Nave
batiMovil = UnaNave 300 250 150 (aTodaMarcha)

aTodaMarcha :: Poder
aTodaMarcha = incrementarAtaque 100

turbo :: Poder
turbo = incrementarAtaque 25

reparacionDeEmergencia :: Poder
reparacionDeEmergencia = incrementarAtaque (-30) . incrementarDurabilidad 50

superTurbo :: Poder
superTurbo = turbo . turbo . turbo . incrementarDurabilidad (-45)

incrementarAtaque :: Number -> Nave -> Nave
incrementarAtaque incremento nave = nave {ataque = ataque nave + incremento}

incrementarEscudos :: Number -> Nave -> Nave
incrementarEscudos incremento nave = nave {escudo = escudo nave + incremento}

incrementarDurabilidad :: Number -> Nave -> Nave
incrementarDurabilidad incremento nave = nave {durabilidad = durabilidad nave + incremento}

--Punto 2

type Flota = [Nave]

calcularDurabilidadTotal :: Flota -> Number
calcularDurabilidadTotal = sum . map durabilidad

--Punto 3

resultadoAtaque :: Nave -> Nave -> Nave
resultadoAtaque atacada atacante = atacar (activarPoder atacada) (activarPoder atacante)

activarPoder :: Nave -> Nave
activarPoder nave = poder nave nave

atacar :: Nave -> Nave -> Nave
atacar atacada atacante = dañar (calcularDaño atacada atacante) atacada

dañar :: Number -> Nave -> Nave
dañar daño atacada
    | daño == 0 = atacada
    | otherwise = atacada {durabilidad = max 0 (durabilidad atacada - daño)}

calcularDaño :: Nave -> Nave -> Number
calcularDaño atacada atacante 
    | escudo atacada > ataque atacante = 0
    | otherwise = escudo atacada - ataque atacante

--Punto 4

fueraDeCombate :: Nave -> Bool
fueraDeCombate nave = durabilidad nave == 0

--Punto 5

type Estrategia = Nave -> Bool

navesDebiles :: Estrategia
navesDebiles nave = escudo nave < 200

navesConCiertaPeligrosidad :: Number -> Estrategia
navesConCiertaPeligrosidad peligrosidad nave = not (ataque nave > peligrosidad)

navesFueraDeCombate :: Nave -> Estrategia
navesFueraDeCombate atacante atacada = fueraDeCombate (resultadoAtaque atacada atacante)

resultadoMision :: Flota -> Nave -> Estrategia -> Flota
resultadoMision flotaEnemiga atacante estrategia = map (ataqueConEstrategia atacante estrategia) flotaEnemiga

ataqueConEstrategia :: Nave -> Estrategia -> Nave -> Nave
ataqueConEstrategia atacante estrategia enemiga 
    | estrategia enemiga = atacar enemiga atacante
    | otherwise = enemiga

--Punto 6

cualEstrategiaEsMejor :: Nave -> Flota -> Estrategia -> Estrategia -> Estrategia
cualEstrategiaEsMejor nave enemigos est1 est2
    |calcularDurabilidadTotal (resultadoMision enemigos nave est1) < calcularDurabilidadTotal (resultadoMision enemigos nave est2) = est1
    |otherwise = est2

realizarMisionMejorEstrategia :: Nave -> Flota -> Estrategia -> Estrategia -> Flota
realizarMisionMejorEstrategia nave enemigos est1 est2 = resultadoMision enemigos nave (cualEstrategiaEsMejor nave enemigos est1 est2)

--Punto 7

flotaInfinita :: Flota
flotaInfinita = repeat xWing

{-No es posible determinar la durabilidad total, ya que es una suma infinita de durabilidad, rompe el programa. Ademas el map nunca cortaria. 
Lo que haria el programa es realizar un ataque sobre cada una de las naves, pero como mi resultado de llevar adelante una mision es una flota,
al ser infinitas naves jamas terminaria de realizar el ataque sobre cada una de ellas, por lo tanto rompe.
-}
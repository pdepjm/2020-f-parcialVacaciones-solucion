-- Esta función ya se la damos
deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

-- 1)
type Idioma = String

data Turista = Turista
  { cansancio :: Int
  , stress    :: Int
  , solitario :: Bool
  , idiomas   :: [Idioma]
  } deriving Show

-- TODO: Cambiar
ana :: Turista
ana =
  Turista { cansancio = 0 , stress = 20, solitario = False, idiomas = ["espaniol"] }

beto :: Turista
beto =
  Turista { cansancio = 15, stress = 15, solitario = True, idiomas = ["aleman"] }

cathi :: Turista
cathi =
  Turista { cansancio = 15, stress = 15, solitario = True, idiomas = ["aleman", "catalan"] }

--
cambiarStress delta turista = turista {stress = stress turista + delta}

cambiarStressPorcentual porciento turista =
  cambiarStress (div (porciento * stress turista) 100) turista

cambiarCansancio delta turista = turista {cansancio = cansancio turista + delta}

aprenderIdioma idioma turista = turista {idiomas = idioma : idiomas turista}

acompaniado turista = turista {solitario = False}

-- 2)
type Excursion = Turista -> Turista

playa :: Excursion
playa turista
  | solitario turista = cambiarCansancio (-5) turista
  | otherwise = cambiarStress (-1) turista

apreciar :: String -> Excursion
apreciar algo = cambiarStress (-length algo)

salirConGente :: Idioma -> Excursion
salirConGente idioma = acompaniado . aprenderIdioma idioma

caminar :: Int -> Excursion
caminar mins = cambiarStress (-intensidad mins) . cambiarCansancio (intensidad mins)

intensidad mins = div mins 4

data Marea
  = Tranquila
  | Moderada
  | Fuerte

paseoEnBarco :: Marea -> Excursion
paseoEnBarco Tranquila = acompaniado
paseoEnBarco Moderada  = id
paseoEnBarco Fuerte    = cambiarCansancio 10 . cambiarStress 6


-- a)
hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion = cambiarStressPorcentual (-10) . excursion

-- b)
deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun f turista excursion =
  deltaSegun f (hacerExcursion excursion turista) turista

-- c)
esEducativa :: Turista -> Excursion -> Bool
esEducativa turista = (> 0) . deltaExcursionSegun (length . idiomas) turista 

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista = filter (esDesestresante turista)

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista = (<= -3) . deltaExcursionSegun stress turista


-- 3)
type Tour = [Excursion]

completo :: Tour
completo = [caminar 20, apreciar "cascada", caminar 40, playa, salidaLocal]

ladoB :: Excursion -> Tour
ladoB excursion = [paseoEnBarco Tranquila, excursion, caminar 120]

islaVecina :: Marea -> Tour
islaVecina mareaVecina = [paseoEnBarco mareaVecina, excursionEnIslaVecina mareaVecina, paseoEnBarco mareaVecina]

--
excursionEnIslaVecina :: Marea -> Excursion
excursionEnIslaVecina Fuerte = apreciar "lago"
excursionEnIslaVecina _  = playa

salidaLocal :: Excursion
salidaLocal = salirConGente "melmacquiano"

-- a)
hacerTour :: Turista -> Tour -> Turista
hacerTour turista tour =
  foldl (flip hacerExcursion) (cambiarStress (length tour) turista) tour

-- b)
propuestaConvincente :: Turista -> [Tour] -> Bool
propuestaConvincente turista = any (esConvincente turista)

esConvincente :: Turista -> Tour -> Bool
esConvincente turista = any (dejaAcompaniado turista) . excursionesDesestresantes turista

dejaAcompaniado :: Turista -> Excursion -> Bool
dejaAcompaniado turista = not . solitario . flip hacerExcursion turista

-- c)
efectividad :: Tour -> [Turista] -> Int
efectividad tour = sum . map (espiritualidadAportada tour) . filter (flip esConvincente tour)

espiritualidadAportada :: Tour -> Turista -> Int
espiritualidadAportada tour = negate . deltaRutina tour

deltaRutina :: Tour -> Turista -> Int
deltaRutina tour turista =
  deltaSegun nivelDeRutina (hacerTour turista tour) turista

nivelDeRutina :: Turista -> Int
nivelDeRutina turista = cansancio turista + stress turista


-- 4)
-- a)
playasEternas :: Tour
playasEternas = salidaLocal : repeat playa

-- b)
{-
Para Ana sí porque la primer actividad ya es desestresante y siempre está acompañada.
Con Beto no se cumple ninguna de las 2 condiciones y el algoritmo diverge.
-}

-- c)
{-
No, solamente funciona para el caso que se consulte con una lista vacía de turista, que dará siempre 0.
-}

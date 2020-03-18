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
  }

anto =
  Turista
    { cansancio = 10
    , stress = 15
    , solitario = True
    , idiomas = ["español", "inglés"]
    }

beto =
  Turista
    {cansancio = 10, stress = 15, solitario = False, idiomas = ["español"]}

cathi =
  Turista
    {cansancio = 10, stress = 15, solitario = False, idiomas = ["italiano"]}

--
cambiarStress delta turista = turista {stress = stress turista + delta}

cambiarStressPorcentual porciento turista =
  cambiarStress ((div porciento 100) * stress turista) turista

cambiarCansancio delta turista = turista {cansancio = cansancio turista + delta}

aprenderIdioma idioma turista = turista {idiomas = idioma : idiomas turista}

-- 2)
type Actividad = Turista -> Turista

surf :: Actividad
surf = cambiarCansancio 10

fistaEnBarco :: Actividad
fistaEnBarco turista = turista {solitario = False}

playa :: Actividad
playa turista
  | solitario turista = cambiarCansancio (-5) turista
  | otherwise = cambiarStress (-1) turista

conocerGente :: Idioma -> Actividad
conocerGente = aprenderIdioma

apreciar :: String -> Actividad
apreciar algo = cambiarStress (-length algo)

caminar :: Int -> Actividad
caminar mins = cambiarCansancio (div mins 4)

data Marea
  = Tranquila
  | Moderada
  | Fuerte

paseoEnBarco :: Marea -> Actividad
paseoEnBarco Tranquila = fistaEnBarco
paseoEnBarco Moderada  = id
paseoEnBarco Fuerte    = cambiarStress 6

-- 3)
type Excursion = Turista -> Turista

playaCercana :: Excursion
playaCercana = playa . caminar 10

cascada :: Excursion
cascada = playa . caminar 20 . apreciar "cascada" . caminar 40

recorridoInternacional :: Idioma -> Excursion
recorridoInternacional idioma =
  apreciar "lago" . caminar 60 . conocerGente idioma

playaEscondida :: Marea -> Excursion
playaEscondida marea = surf . paseoEnBarco marea

islaVecina :: Excursion -> Excursion
islaVecina excursion = excursion . paseoEnBarco Tranquila

-- a)
hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion = cambiarStressPorcentual (-10) . excursion

-- b)
deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun f turista excursion =
  deltaSegun f (hacerExcursion excursion turista) turista

-- c)
leGustaExcursionAAnto :: Excursion -> Bool
leGustaExcursionAAnto = (< 10) . deltaExcursionSegun stress anto

leGustaExcursionABeto :: Excursion -> Bool
leGustaExcursionABeto = (> 0) . deltaExcursionSegun (length . idiomas) beto

-- 4)
type Tour = [Actividad]

completo :: Tour
completo = [playaCercana, cascada, recorridoInternacional "francés"]

playas :: Int -> Tour
playas = flip replicate playaCercana

exterior :: Marea -> Tour
exterior mareaVecina = [islaVecina (excursionSegun mareaVecina), playaCercana]

eternidad :: Tour
eternidad = repeat (recorridoInternacional "francés")

--
excursionSegun :: Marea -> Excursion
excursionSegun Fuerte = cascada
excursionSegun marea  = playaEscondida marea

-- a)
hacerTour :: Turista -> Tour -> Turista
hacerTour turista tour =
  foldl (flip hacerExcursion) (cambiarStress (length tour) turista) tour

-- b)
leGustaTourAAnto :: Tour -> Bool
leGustaTourAAnto = all leGustaExcursionAAnto

leGustaTourABeto :: Tour -> Bool
leGustaTourABeto tour =
  any leGustaExcursionABeto tour || terminaMenosStressado beto tour

--
terminaMenosStressado :: Turista -> Tour -> Bool
terminaMenosStressado turista tour =
  deltaSegun cansancio (hacerTour turista tour) turista < 0

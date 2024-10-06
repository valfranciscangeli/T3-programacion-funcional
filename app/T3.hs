module T3 where

import Control.Arrow (ArrowChoice (left))
import Control.Monad.State
import Data.List (nub)
import Numeric.Probability.Distribution hiding (coin, filter, map)
import Prelude hiding (cycle)

{-------------------------------------------}
{--------------  EJERCICIO 1  --------------}
{-------------------------------------------}

-- First component: # moves to the right
-- Second component: # moves up
type Pos = (Int, Int)

-- Parte (a)
possibleMoves :: Pos -> [Pos]
possibleMoves (x, y) = do
  pos <- [(x + 1, y), (x, y + 1)] -- posiciones posibles
  if abs (uncurry (-) pos) <= 3 -- aplicar condicion a cada Pos
    then return pos
    else []

-- Parte (b)
nSteps :: Int -> Pos -> [Pos]
nSteps 0 pos = [pos] -- caso base
nSteps n pos = nub $ do
  -- se aplica nub al final para eliminar repetidos
  nextPos <- possibleMoves pos
  nub $ nSteps (n - 1) nextPos -- se aplica nub en cada recursion para evitar iterar en elementos repetidos

{-------------------------------------------}
{--------------  EJERCICIO 2  --------------}
{-------------------------------------------}

data Result e g = Fail e | Good g

instance Functor (Result e) where
  -- fmap :: (a->b) -> Result e a -> Result e b
  fmap _ (Fail err) = Fail err -- f no se usa
  fmap f (Good res) = Good (f res)

-- Parte (a)

instance Applicative (Result e) where
  pure = Good
  (Fail err) <*> _ = Fail err
  (Good f) <*> r = fmap f r

instance Monad (Result e) where
  (Fail err) >>= _ = Fail err
  (Good x) >>= f = f x

-- Parte (b)
type Shop = String

cycle :: Shop -> Shop -> Result String ()
cycle = error "cycle undef"

shop :: Shop -> Result String ()
shop = error "shop unfed"

doGroceries :: [Shop] -> Result String ()
doGroceries [] = Fail "No Shops to visit!"
doGroceries [x] = shop x
doGroceries (x1 : x2 : xs) = do
  shop x1
  cycle x1 x2
  doGroceries (x2 : xs)

{-------------------------------------------}
{--------------  EJERCICIO 3  --------------}
{-------------------------------------------}

data Command = Inc | Dec | Mult | Store

-- Primer componente: numero en la hoja
-- Segundo componente: numero en la mano de Bob
type GameState = (Int, Int)

playGame :: [Command] -> State GameState Int
playGame [] = do
  (_, hand) <- get
  return hand
playGame (Inc : cmds) = do
  modify (\(paper, hand) -> (paper, hand + 1))
  playGame cmds
playGame (Dec : cmds) = do
  modify (\(paper, hand) -> (paper, hand - 1))
  playGame cmds
playGame (Mult : cmds) = do
  (paper, hand) <- get
  modify (\(paper, hand) -> (paper, hand * paper))
  playGame cmds
playGame (Store : cmds) = do
  (paper, hand) <- get
  modify (\(_, hand) -> (hand, hand))
  playGame cmds

{-------------------------------------------}
{--------------  EJERCICIO 4  --------------}
{-------------------------------------------}

type Probability = Rational

type Dist a = T Probability a

-- Parte (a.I)
pointDist :: Int -> Dist (Int, Int)
pointDist r = do
  x <- uniform [-r .. r]
  y <- uniform [-r .. r]
  return (x, y)

-- Parte (a.II)
-- resultE3a :: Int -> Probability
-- resultE3a r = 4 * (fromIntegral insideCircle / fromIntegral totalPoints)
--   where
--     totalPoints = (2 * r + 1) ^ 2
--     insideCircle = length [() | (x, y) <- support (pointDist r), x^2 + y^2 <= r^2]

-- Parte (b)
{-
Si le resulta conveniente, puede empezar siguiendo
el hint dado:

data Uni     = Chile | Cato deriving Eq
type Urn     = (Int, Int)
-- 1er componente: #jugadores Chile, 2do componente: #jugadores Cato

pickPlayer :: Urn -> Dist (Uni, Urn)
pickPlayer = undefined
-}

data Uni = Chile | Cato deriving (Eq)

type Urn = (Int, Int) -- (estudiantes de UChile, estudiantes de UC)

pickPlayer :: Urn -> Dist (Uni, Urn)
pickPlayer (chile, cato) = do
  if chile > 0 && cato > 0
    then uniform [(Chile, (chile - 1, cato)), (Cato, (chile, cato - 1))]
    else
      if chile > 0
        then return (Chile, (chile - 1, cato))
        else return (Cato, (chile, cato - 1))

-- resultE3b :: Probability
-- resultE3b = do
--   let initialUrn = (8, 2)  -- 8 estudiantes de UChile, 2 estudiantes de UC
--   players <- sequence [pickPlayer initialUrn, pickPlayer initialUrn]
--   let single1 = take 2 players
--   -- Continúa simulando los otros singles y verifica si los estudiantes de UC no terminan juntos.

{-# LANGUAGE InstanceSigs #-}

module T3 where

import Control.Monad.State
import Data.List (nub) -- para eliminar duplicados de listas
import Numeric.Probability.Distribution ( uniform, T )
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
  newPos <- possibleMoves pos
  nub $ nSteps (n - 1) newPos -- se aplica nub en cada recursion para evitar iterar en elementos repetidos

{-------------------------------------------}
{--------------  EJERCICIO 2  --------------}
{-------------------------------------------}

data Result e g = Fail e | Good g

instance Functor (Result e) where
  -- fmap :: (a->b) -> Result e a -> Result e b
  fmap _ (Fail err) = Fail err -- f no se usa
  fmap f (Good res) = Good (f res)

-- instancia para poder mostrar el resultado:
instance (Show e, Show g) => Show (Result e g) where
  show :: (Show e, Show g) => Result e g -> String
  show (Fail err) = "Fail " ++ show err
  show (Good res) = "Good " ++ show res

-- instancia para comparar resultados:
instance (Eq e, Eq g) => Eq (Result e g) where
  (==) :: (Eq e, Eq g) => Result e g -> Result e g -> Bool
  (Fail err1) == (Fail err2) = err1 == err2
  (Good res1) == (Good res2) = res1 == res2
  _ == _ = False

-- Parte (a)
-- diapo 22 clase 13
instance Applicative (Result e) where
  pure :: a -> Result e a
  pure = Good
  (<*>) :: Result e (a -> b) -> Result e a -> Result e b
  (Fail err) <*> _ = Fail err
  (Good f) <*> res = fmap f res

-- diapo 10 clase 14
-- ya es Applicative, asi que agregamos los op de monada solamente
instance Monad (Result e) where
  (>>=) :: Result e a -> (a -> Result e b) -> Result e b
  (Fail err) >>= _ = Fail err
  (Good res) >>= f = f res

-- Parte (b)
type Shop = String

cycle :: Shop -> Shop -> Result String ()
cycle = error "cycle undef"

shop :: Shop -> Result String ()
shop = error "shop undef"

-- original
{-
doGroceries :: [Shop] -> Result String ()
doGroceries []         = Fail "No Shops to visit!"
doGroceries [x]        = shop x
doGroceries (x1:x2:xs) = case shop x1 of
    Fail e1 -> Fail e1
    Good _  -> case cycle x1 x2 of
        Fail e2 -> Fail e2
        Good _  -> doGroceries (x2:xs)
-}

-- base en diapo 19 clase 14
doGroceries :: [Shop] -> Result String ()
doGroceries [] = Fail "No Shops to visit!" -- se mantiene
doGroceries [x] = shop x -- tambien se mantiene
doGroceries (x1 : x2 : xs) = do
  -- se aplica do-notation para recorrer la lista
  shop x1 -- entra a comprar a la tienda 1
  cycle x1 x2 -- si se pudo comprar, sigue a la tienda 2
  doGroceries (x2 : xs) -- si llega a la tienda 2, se repite

{-------------------------------------------}
{--------------  EJERCICIO 3  --------------}
{-------------------------------------------}

data Command = Inc | Dec | Mult | Store

-- Primer componente: numero en la hoja
-- Segundo componente: numero en la mano de Bob
type GameState = (Int, Int)

playGame :: [Command] -> State GameState Int
------------
playGame [] = do
  -- no quedan comandos
  (_, mano) <- get
  return mano -- termina, entrega el num en mano
playGame (Inc : cmds) = do
  -- comando inicial pide +1
  modify (\(hoja, mano) -> (hoja, mano + 1))
  playGame cmds -- sigue la recursion
playGame (Dec : cmds) = do
  -- comando inicial pide -1
  modify (\(hoja, mano) -> (hoja, mano - 1))
  playGame cmds -- sigue la recursion
playGame (Mult : cmds) = do
  -- comando inicial pide multiplicar por hoja
  (_, _) <- get
  modify (\(hoja, mano) -> (hoja, mano * hoja))
  playGame cmds -- sigue la recursion
playGame (Store : cmds) = do
  -- comando inicial pide sobreescribir hoja
  (_, _) <- get
  modify (\(_, mano) -> (mano, mano))
  playGame cmds -- sigue la recursion

{-------------------------------------------}
{--------------  EJERCICIO 4  --------------}
{-------------------------------------------}

type Probability = Rational

type Dist a = T Probability a

-- Parte (a.I)
pointDist :: Int -> Dist (Int, Int)
-----------
pointDist r = do
  x <- uniform [-r .. r]
  y <- uniform [-r .. r]
  return (x, y)

-- Parte (a.II)
-- resultE3a :: Int -> Probability
-----
-- resultE3a r = 4 * (fromIntegral insideCircle / fromIntegral totalPoints)
--   where
--     totalPoints = (2 * r + 1) ^ 2
--     insideCircle = length [() | (x, y) <- support (pointDist r), x^2 + y^2 <= r^2]

-- Parte (b)
data Uni = Chile | Cato deriving (Eq)

type Urn = (Int, Int)

-- 1er componente: #jugadores Chile, 2do componente: #jugadores Cato

pickPlayer :: Urn -> Dist (Uni, Urn)
---
pickPlayer (chile, cato) = do
  if chile > 0 && cato > 0
    then uniform [(Chile, (chile - 1, cato)), (Cato, (chile, cato - 1))]
    else
      if chile > 0
        then return (Chile, (chile - 1, cato))
        else return (Cato, (chile, cato - 1))

-- resultE3b :: Probability
----
-- resultE3b = do
--    let initialUrn = (8, 2)  -- 8 estudiantes de UChile, 2 estudiantes de UC
--    players <- sequence [pickPlayer initialUrn, pickPlayer initialUrn]
--    let single1 = take 2 players

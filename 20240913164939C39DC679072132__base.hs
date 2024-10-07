module Main where

import Prelude hiding (cycle)
import Control.Monad.State
import Numeric.Probability.Distribution hiding (map,coin,filter)



{-------------------------------------------}
{--------------  EJERCICIO 1  --------------}
{-------------------------------------------}

-- First component: # moves to the right
-- Second component: # moves up
type Pos = (Int,Int)

-- Parte (a)
possibleMoves :: Pos -> [Pos]
possibleMoves = undefined
-- complete la definicion

-- Parte (b)
nSteps :: Int -> Pos -> [Pos]
nSteps = undefined
-- complete la definicion




{-------------------------------------------}
{--------------  EJERCICIO 2  --------------}
{-------------------------------------------}

data Result e g = Fail e | Good g

instance Functor (Result e) where
  -- fmap :: (a->b) -> Result e a -> Result e b
  fmap _ (Fail err) = Fail err
  fmap f (Good res) = Good (f res)

-- Parte (a)




-- Parte (b)
type Shop = String

cycle :: Shop -> Shop -> Result String ()
cycle = error "cycle undef"

shop :: Shop -> Result String ()
shop = error "shop unfed"

doGroceries :: [Shop] -> Result String ()
doGroceries []         = Fail "No Shops to visit!"
doGroceries [x]        = shop x
doGroceries (x1:x2:xs) = case shop x1 of
    Fail e1 -> Fail e1
    Good _  -> case cycle x1 x2 of
        Fail e2 -> Fail e2
        Good _  -> doGroceries (x2:xs)




{-------------------------------------------}
{--------------  EJERCICIO 3  --------------}
{-------------------------------------------}

data Command = Inc | Dec | Mult | Store

-- Primer componente: numero en la hoja
-- Segundo componente: numero en la mano de Bob
type GameState = (Int,Int)

playGame :: [Command] -> State GameState Int
playGame = undefined
-- complete la definicion




{-------------------------------------------}
{--------------  EJERCICIO 4  --------------}
{-------------------------------------------}

type Probability = Rational
type Dist a = T Probability a


-- Parte (a.I)
pointDist :: Int -> Dist (Int, Int)
pointDist = undefined
-- complete la definicion

-- Parte (a.II)
resultE3a :: Int -> Probability
resultE3a = undefined
-- complete la definicion


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


resultE3b :: Probability
resultE3b = undefined
-- complete la definicion

main :: IO()
main = return ()

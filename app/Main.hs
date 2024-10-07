import Control.Monad.State (evalState)
import T3
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

-- Pruebas para EJERCICIO 1 =============================

-- Parte (a):------
testsEjecicio1ParteA :: Spec
testsEjecicio1ParteA =
  describe "EJERCICIO 1.a) possibleMoves" $ do
    it "prueba en (3, 0)" $ possibleMoves (3, 0) `shouldBe` [(3, 1)] -- solo puede subir
    it "prueba en (0, 3)" $ possibleMoves (0, 3) `shouldBe` [(1, 3)] -- solo puede ir a la derecha
    it "prueba en (0,0)" $ possibleMoves (0, 0) `shouldBe` [(1, 0), (0, 1)]
    it "prueba en (2, 1)" $ possibleMoves (2, 1) `shouldBe` [(3, 1), (2, 2)]
    it "prueba en (2, 2)" $ possibleMoves (2, 2) `shouldBe` [(3, 2), (2, 3)]

-- Parte (b):------
testsEjecicio1ParteB :: Spec
testsEjecicio1ParteB = describe "EJERCICIO 1.b) nSteps" $ do
  it "nSteps 0 (0,0)" $ nSteps 0 (0, 0) `shouldBe` [(0, 0)]
  it "nSteps 1 (0,0)" $ nSteps 1 (0, 0) `shouldBe` [(1, 0), (0, 1)]
  it "nSteps 2 (0,0)" $ nSteps 2 (0, 0) `shouldBe` [(2, 0), (1, 1), (0, 2)]
  it "nSteps 3 (0,0)" $ nSteps 3 (0, 0) `shouldBe` [(3, 0), (2, 1), (1, 2), (0, 3)]
  it "nSteps 2 (1,1)" $ nSteps 2 (1, 1) `shouldBe` [(3, 1), (2, 2), (1, 3)]

-- Pruebas para EJERCICIO 2 =============================

-- Parte (a):------
-- hay muchos cast debido a warnings y errores de shoulBe
testsEjecicio2ParteA :: Spec
testsEjecicio2ParteA = describe "EJERCICIO 2.a) Applicative y Monad" $ do
  -- tests para Applicative
  it "pure basico" $ do
    pure (5 :: Int) `shouldBe` (Good (5 :: Int) :: Result String Int)
  it "mantener error" $ do
    (Fail "error" :: Result String (a0 -> Int)) <*> Good (5 :: Int) `shouldBe` (Fail "error" :: Result String Int)
  it "aplicar funcion a Good" $ do
    Good (+ 2) <*> (Good 3 :: Result String Int) `shouldBe` (Good 5 :: Result String Int)

  -- tests para Monad
  it "propagar error" $ do
    (Fail "error" >>= \x -> pure (x + 1) :: Result String Int) `shouldBe` (Fail "error" :: Result String Int)
  it "aplicar funcion a Good" $ do
    (Good 5 >>= \x -> pure (x + 2) :: Result String Int) `shouldBe` (Good 7 :: Result String Int)
  it "funcion que provoca Fail" $ do
    (Good (5 :: Int) >> (Fail "error" :: Result String Int)) `shouldBe` (Fail "error" :: Result String Int)

-- Parte (b):------
testsEjecicio2ParteB :: Spec
testsEjecicio2ParteB = describe "EJERCICIO 2.b) doGroceries" $ do
  it "sin tiendas -> Fail" $ do
    doGroceries [] `shouldBe` Fail "No Shops to visit!"

-- no se puede probar mas porque las funciones estan indefinidas

-- Pruebas para EJERCICIO 3 =============================

-- Ejercicio 3:------
testsEjecicio3 :: Spec
testsEjecicio3 = describe "EJERCICIO 3) playGame" $ do
  it "ejemplo 1" $ do
    evalState (playGame [Inc, Inc]) (0, 0) `shouldBe` 2
  it "ejemplo 2" $ do
    evalState (playGame [Inc, Inc, Store, Mult]) (0, 0) `shouldBe` 4
  it "(playGame [Dec]) (0, 0) " $ do
    evalState (playGame [Dec]) (0, 0) `shouldBe` (-1)
  it "(playGame [Inc, Dec]) (0, 0)" $ do
    evalState (playGame [Inc, Dec]) (0, 0) `shouldBe` 0
  it "(playGame [Inc, Mult]) (2, 2)" $ do
    evalState (playGame [Inc, Mult]) (2, 2) `shouldBe` 6
  it "(playGame [Inc, Store]) (3, 2)" $ do
    evalState (playGame [Inc, Store]) (3, 2) `shouldBe` 3
  it "(playGame [Store]) (5, 0)" $ do
    evalState (playGame [Store]) (5, 0) `shouldBe` 0
  it "(playGame [Inc, Dec, Dec]) (0, 0)" $ do
    evalState (playGame [Inc, Dec, Dec]) (0, 0) `shouldBe` -1

-- Pruebas para EJERCICIO 4 =============================

-- Parte (a):------
testsEjecicio4ParteA :: Spec
testsEjecicio4ParteA = undefined

-- Parte (b):------
testsEjecicio4ParteB :: Spec
testsEjecicio4ParteB = undefined

{-------------------------------------------}
{------------------ MAIN -------------------}
{-------------------------------------------}

main :: IO ()
main = hspec $ do
  -- Ejercicio 1
  testsEjecicio1ParteA
  testsEjecicio1ParteB
  -- Ejercicio 2
  testsEjecicio2ParteA
  testsEjecicio2ParteB
  -- Ejercicio 3
  testsEjecicio3
  -- -- Ejercicio 4
  --testsEjecicio4ParteA
  --testsEjecicio4ParteB
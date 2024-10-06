import Data.List (nub)
import T3
import Test.Hspec

-- Pruebas para EJERCICIO 1 =============================

-- Parte (a):------
testsEjecicio1ParteA :: Spec
testsEjecicio1ParteA =
  describe "EJERCICIO 1.a) possibleMoves" $ do
    it "prueba en (3, 0)" $ do
      possibleMoves (3, 0) `shouldBe` [(3, 1)] -- solo puede subir
    it "prueba en (0, 3)" $ do
      possibleMoves (0, 3) `shouldBe` [(1, 3)] -- solo puede ir a la derecha
    it "prueba en (0,0)" $ do
      possibleMoves (0, 0) `shouldBe` [(1, 0), (0, 1)]
    it "prueba en (2, 1)" $ do
      possibleMoves (2, 1) `shouldBe` [(3, 1), (2, 2)]
    it "prueba en (2, 2)" $ do
      possibleMoves (2, 2) `shouldBe` [(3, 2), (2, 3)]

-- Parte (b):------
testsEjecicio1ParteB :: Spec
testsEjecicio1ParteB = describe "EJERCICIO 1.b) nSteps" $ do
  it "nSteps 0 (0,0)" $ do
    nSteps 0 (0, 0) `shouldBe` [(0, 0)]
  it "nSteps 1 (0,0)" $ do
    nSteps 1 (0, 0) `shouldBe` [(1, 0), (0, 1)]
  it "nSteps 2 (0,0)" $ do
    nSteps 2 (0, 0) `shouldBe` [(2, 0), (1, 1), (0, 2)]
  it "nSteps 3 (0,0)" $ do
    nSteps 3 (0, 0) `shouldBe` [(3, 0), (2, 1), (1, 2), (0, 3)]
  it "nSteps 2 (1,1)" $ do
    nSteps 2 (1, 1) `shouldBe` [(3, 1), (2, 2), (1, 3)]

-- Pruebas para EJERCICIO 2 =============================

-- Parte (a):------
-- testsEjecicio2ParteA :: Spec
-- testsEjecicio2ParteA = describe "EJERCICIO 2.a) posibleMoves" $ do
--   it "pruebas para posibleMoves" $ do
--     evalCF (Simple 5) `shouldBe` (5, 1)

-- Parte (b):------
-- testsEjecicio2ParteB :: Spec
-- testsEjecicio2ParteB = describe "EJERCICIO 2.b) nSteps" $ do
--   it "pruebas para posibleMoves" $ do
--     evalCF (Simple 5) `shouldBe` (5, 1)

-- Pruebas para EJERCICIO 3 =============================

-- Ejercicio 3:------
-- testsEjecicio3 :: Spec
-- testsEjecicio3 = describe "EJERCICIO 3) posibleMoves" $ do
--   it "pruebas para posibleMoves" $ do
--     evalCF (Simple 5) `shouldBe` (5, 1)

-- Pruebas para EJERCICIO 4 =============================

-- Parte (a):------
-- testsEjecicio4ParteA :: Spec
-- testsEjecicio4ParteA = describe "EJERCICIO 4.a) posibleMoves" $ do
--   it "pruebas para posibleMoves" $ do
--     evalCF (Simple 5) `shouldBe` (5, 1)

-- Parte (b):------
-- testsEjecicio4ParteB :: Spec
-- testsEjecicio4ParteB = describe "EJERCICIO 4.b) nSteps" $ do
--   it "pruebas para posibleMoves" $ do
--     evalCF (Simple 5) `shouldBe` (5, 1)

{-------------------------------------------}
{------------------ MAIN -------------------}
{-------------------------------------------}

main :: IO ()
main = hspec $ do
  -- Ejercicio 1
  testsEjecicio1ParteA
  testsEjecicio1ParteB

-- -- Ejercicio 2
-- testsEjecicio2ParteA
-- testsEjecicio2ParteB
-- -- Ejercicio 3
-- testsEjecicio3
-- -- Ejercicio 4
-- testsEjecicio4A
-- testsEjecicio4B
import T3
-- Pruebas para EJERCICIO 1 =============================

-- Parte (a):------
testsEjecicio1ParteA :: Spec
-- testsEjecicio1ParteA = describe "EJERCICIO 1.a) posibleMoves" $ do
--   it "pruebas para posibleMoves" $ do
--     evalCF (Simple 5) `shouldBe` (5, 1)

-- Parte (b):------
testsEjecicio1ParteB :: Spec
-- testsEjecicio1ParteB = describe "EJERCICIO 1.b) nSteps" $ do
--   it "pruebas para posibleMoves" $ do
--     evalCF (Simple 5) `shouldBe` (5, 1)


-- Pruebas para EJERCICIO 2 =============================

-- Parte (a):------
testsEjecicio2ParteA :: Spec
-- testsEjecicio2ParteA = describe "EJERCICIO 2.a) posibleMoves" $ do
--   it "pruebas para posibleMoves" $ do
--     evalCF (Simple 5) `shouldBe` (5, 1)

-- Parte (b):------
testsEjecicio2ParteB :: Spec
-- testsEjecicio2ParteB = describe "EJERCICIO 2.b) nSteps" $ do
--   it "pruebas para posibleMoves" $ do
--     evalCF (Simple 5) `shouldBe` (5, 1)
  

-- Pruebas para EJERCICIO 3 =============================

-- Ejercicio 3:------
testsEjecicio3 :: Spec
-- testsEjecicio3 = describe "EJERCICIO 3) posibleMoves" $ do
--   it "pruebas para posibleMoves" $ do
--     evalCF (Simple 5) `shouldBe` (5, 1)

-- Pruebas para EJERCICIO 4 =============================

-- Parte (a):------
testsEjecicio4ParteA :: Spec
-- testsEjecicio4ParteA = describe "EJERCICIO 4.a) posibleMoves" $ do
--   it "pruebas para posibleMoves" $ do
--     evalCF (Simple 5) `shouldBe` (5, 1)

-- Parte (b):------
testsEjecicio4ParteB :: Spec
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
  -- Ejercicio 2
  testsEjecicio2ParteA
  testsEjecicio2ParteB
  -- Ejercicio 3
  testsEjecicio3
  -- Ejercicio 4
  testsEjecicio4A
  testsEjecicio4B
import T3

{-------------------------------------------}
{------------------ MAIN -------------------}
{-------------------------------------------}

main :: IO ()
main = hspec $ do
  -- Ejercicio 1
  testsEjecicio1ParteB
  testsEjecicio1ParteC
  testsEjecicio1ParteD
  testsEjecicio1ParteE
  testsEjecicio1ParteF
  propiedadEjecicio1
  -- Ejercicio 2
  testsEjecicio2ParteA
  testsEjecicio2ParteB
  testsEjecicio2ParteC
  -- Ejercicio 3
  testsEjecicio3ParteA
  -- Ejercicio 4
  testsEjecicio4
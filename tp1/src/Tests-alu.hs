-- Para correr los tests:
-- ghc Tests-alu.hs && ./Tests-alu

import Tp
import Test.HUnit
import Data.List

-- para unless en assertAlmostEqual, modificado a partir de la definición de
-- assertEqual, sacada de https://hackage.haskell.org/package/HUnit
import Control.Monad

texto_a_normalizar = ["abc c abc abc d", "a b a b", "abc"]
textos_a = ["b=a", "a = 2; a = 4", "C:/DOS C:/DOS/RUN RUN/DOS/RUN"]

almostEq :: Float -> Float -> Bool
almostEq x y = abs (x - y) < 0.0001

assertAlmostEqual :: String -> Float -> Float -> Assertion
assertAlmostEqual preface expected actual =
	unless (almostEq actual expected) (assertFailure msg)
	where msg = (if null preface then "" else preface ++ "\n") ++
	            "expected: " ++ show expected ++ "\n but got: " ++ show actual

infix 1 ||~=
(||~=) :: Float -> Float -> Assertion
actual ||~= expected = assertAlmostEqual "" expected actual

-- evaluar main para correr todos los tests
main = runTestTT allTests


allTests = test [
	"split"                    ~: testsSplit,
	"cuentas"                  ~: testsCuentas,
	"longitudPromedioPalabras" ~: testLongitudPromedio,
	"repeticionesPromedio"     ~: testRepeticionesPromedio,
	"frecuenciaTokens"         ~: testFrecuenciaTokens,
	"normalizarExtractor"      ~: testNormalizarExtractor,
	"extraerFeatures"          ~: testExtraerFeatures,
	"distanciaEuclideana"      ~: testDistEuclideana,
	"distanciaCoseno"          ~: testDistCoseno
	]

testsSplit = test [
	split ',' ",PLP,"        ~?= ["PLP"],
	split ',' ",,P,,L,,P,,"  ~?= ["P", "L", "P"],
	split ',' " ,PLP, "      ~?= [" ","PLP"," "],
	split 'p' "PLP,PLP"      ~?= ["PLP,PLP"],
	split ' ' "Hola  Mundo!" ~?= ["Hola","Mundo!"]
	]

testsCuentas = test [
	cuentas ["",""]               ~?= [(2,"")],
	cuentas ["c","cc","ccc"]      ~?= [(1,"c"), (1,"cc"),(1,"ccc")],
	cuentas ["x","x","y","x","z"] ~?= [(3,"x"), (1,"y"), (1,"z")],
	cuentas ["x", "x", "x", "x"]  ~?= [(4,"x")]
	]

testLongitudPromedio = test [
	longitudPromedioPalabras "a-bc,def.g"                      ~?= 10.0,
	longitudPromedioPalabras "a-bc def.g"                      ~?= 4.5,
	longitudPromedioPalabras "Este test tiene palabras $$++$$" ~?= 5.4
	]

testRepeticionesPromedio = test [
	repeticionesPromedio "a-a b-b a-a-a"                      ~?= 1.0,
	repeticionesPromedio "lalala $$++$$ lalala lalala $$++$$" ~?= 2.5,
	map repeticionesPromedio texto_a_normalizar               ~?= [5/3, 2.0, 1.0]
	]

testFrecuenciaTokens = test [
	map (\x -> x * (genericLength tokens)) [f (concat (replicate 3 tokens)) | f <- frecuenciaTokens] ~?= replicate (length tokens) 1.0,
	(head frecuenciaTokens) "use_snake_case !" ~?= 0.125
	]

testNormalizarExtractor = test [
	map (normalizarExtractor texto_a_normalizar genericLength) ("aaaaaaaaaaaaaaaa":texto_a_normalizar) ~?= [16/15, 1.0, 7/15, 0.2],
	map (normalizarExtractor texto_a_normalizar repeticionesPromedio) ("a a a a":texto_a_normalizar) ~?= [2.0, 5/6, 1.0, 0.5]
	]

testExtraerFeatures = test [
	extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] textos_a ~?= [[0.33333334, 0.6666667], [0.12962963, 1.0], [1.0, 0.6666667]],
	extraerFeatures [] textos_a ~?= [[],[],[]],
	extraerFeatures [genericLength] [] ~?= [],
	extraerFeatures [genericLength] ["a","ab","abc"] ~?= [[1/3], [2/3], [3/3]]
	]

testDistEuclideana = test [
	distEuclideana [1.0, 0.75, 0.8125] [0.75, 1.0, 0.5]     ||~= 0.47186464,
	distEuclideana [-5.0, -2.5, -0.1] [-2.3, 0, 3.4]        ||~= 5.078386,
	distEuclideana [-3.0, -2.5] [-2.3, 0.3]                 ||~= 2.886174,
	distEuclideana [2.0, 1.0] [1.0, 0.0]                    ||~= sqrt 2,
	distEuclideana [0.5 * sqrt 2, -0.5 * sqrt 2] [0.0, 0.0] ||~= 1.0,
	distEuclideana [3.0, 0.0] [0.0, 4.0]                    ||~= 5.0
	]

testDistCoseno = test [
	distCoseno [0, 3, 4] [0, -3, -4]           ||~= -1.0,
	distCoseno [-1.3, -10] [-0.1, -0.6]        ||~= 0.99935657,
	distCoseno [3.0, -2.1] [-5.2, 0.1]         ||~= -0.8301066,
	distCoseno [-0.5, 0.5 * sqrt 3] [1.0, 0.0] ||~= -0.5
	]

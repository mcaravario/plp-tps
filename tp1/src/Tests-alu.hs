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
	"distanciaCoseno"          ~: testDistCoseno,
	"knn"                      ~: testKNN,
	"separarDatos"             ~: testSepararDatos,
	"accuracy"                 ~: testAccuracy,
	"nFoldCrossValidation"     ~: testNFoldCrossValidation
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

knn_a = knn 2 [[0, 1], [0, 2], [2, 1], [1, 1], [2, 3]] ["i", "i", "f", "f", "i"] distEuclideana
knn_b = knn 5 [[0, 1], [0, 2], [2, 1], [1, 1], [2, 3]] ["i", "i", "f", "f", "i"] distEuclideana
knn_c = knn 1 [[0, 1], [0, 2], [2, 1], [1, 1], [2, 3]] ["i", "i", "f", "f", "i"] distEuclideana
testKNN = test [
	knn_a [1, 1] ~?= "f",
	knn_b [1, 1] ~?= "i",
	knn_c [0, 1] ~?= "i"
	]

separarDatos_a = separarDatos [[1, 1], [2, 2], [3, 3], [4, 4], [5, 5], [6, 6], [7, 7]] ["1", "2", "3", "4", "5", "6", "7"]
separarDatos_b = separarDatos [[1, 1], [2, 2], [3, 3], [4, 4]] ["1", "2", "3", "4"]
separarDatos_c = separarDatos [[1, 1]] ["1"]
testSepararDatos = test [
	separarDatos_a 3 2 ~?= ([[1, 1], [2, 2], [5, 5], [6, 6]], [[3, 3], [4, 4]], ["1", "2", "5", "6"], ["3", "4"]),
	separarDatos_b 4 3 ~?= ([[1, 1], [2, 2], [4, 4]], [[3, 3]], ["1", "2", "4"], ["3"]),
	separarDatos_c 1 1 ~?= ([], [[1, 1]], [], ["1"])
	]

testAccuracy = test [
	accuracy ["f", "f", "i", "i", "f"] ["i", "f", "i", "f", "f"] ||~= 0.6,
	accuracy ["i", "f", "i", "f", "f"] ["i", "f", "i", "f", "f"] ||~= 1.0,
	accuracy ["?", "?", "?", "?", "?"] ["i", "f", "i", "f", "f"] ||~= 0.0
	]

nfcv_datos_a = [
	[1, 4], [2, 4], [3, 4], [4, 4], [5, 4],
	[1, 3], [2, 3], [3, 3], [4, 3], [5, 3],
	[1, 2], [2, 2], [3, 2], [4, 2], [5, 2],
	[1, 1], [2, 1], [3, 1], [4, 1], [5, 1]
	]
nfcv_datos_d = [
	[1, 5], [2, 5], [3, 5], [4, 5], [5, 5],
	[1, 4], [2, 4], [3, 4], [4, 4], [5, 4],
	[1, 3], [2, 3], [3, 3], [4, 3], [5, 3],
	[1, 2], [2, 2], [3, 2], [4, 2], [5, 2],
	[1, 1], [2, 1], [3, 1], [4, 1], [5, 1]
	]
nfcv_etiquetas_a = replicate 20 "i"
nfcv_etiquetas_b = concat $ replicate 2 ((replicate 5 "i") ++ (replicate 5 "f"))
nfcv_etiquetas_c = concat $ [["f", "i"] | _ <- [1..10]]
nfcv_etiquetas_d = [
	"i", "i", "f", "i", "i", -- 0.2
	"i", "f", "f", "f", "i", -- 0.6
	"f", "f", "f", "f", "f", -- 1.0
	"i", "f", "f", "f", "i", -- 0.6
	"i", "i", "f", "i", "i"  -- 0.2
	]
testNFoldCrossValidation = test [
	nFoldCrossValidation 4 nfcv_datos_a nfcv_etiquetas_a ||~= 1,
	nFoldCrossValidation 4 nfcv_datos_a nfcv_etiquetas_b ||~= 0,
	nFoldCrossValidation 4 nfcv_datos_a nfcv_etiquetas_c ||~= 0.4,
	nFoldCrossValidation 5 nfcv_datos_d nfcv_etiquetas_d ||~= (0.2 + 0.6 + 1.0 + 0.6 + 0.2) / 5
	]

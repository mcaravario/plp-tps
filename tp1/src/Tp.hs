module Tp where

import Data.List

type Texto = String
type Feature = Float
type Instancia = [Feature]
type Extractor = (Texto -> Feature)

type Datos = [Instancia]
type Etiqueta = String
type Modelo = (Instancia -> Etiqueta)
type Medida = (Instancia -> Instancia -> Float)

tryClassifier :: [Texto] -> [Etiqueta] -> Float
tryClassifier x y = let xs = extraerFeatures ([longitudPromedioPalabras, repeticionesPromedio] ++ frecuenciaTokens) x in
    nFoldCrossValidation 5 xs y

mean :: [Float] -> Float
mean xs = realToFrac (sum xs) / genericLength xs

split :: Eq a => a -> [a] -> [[a]]
split c = foldl (\acum x -> if x == c then acum++[[]] else (init acum)++[(last acum)++[x]]) [[]]

longitudPromedioPalabras :: Extractor
longitudPromedioPalabras t = mean $ map (genericLength) (split ' ' t)

apariciones :: Eq a => a -> [a] -> Int
apariciones e xs = length $ filter (==e) xs

cuentas :: Eq a => [a] -> [(Int, a)]
cuentas = map (\y -> (apariciones y xs, y))

repeticionesPromedio :: Extractor
repeticionesPromedio xs = mean $ map (\x -> fromIntegral (fst x)) (cuentas (split ' ' xs))

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
frecuenciaTokens = map (\t -> \xs -> frecuencia t xs) tokens
	where frecuencia t xs = fromIntegral (apariciones t xs) / genericLength xs

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor ts e = (\xs -> e xs / maximo)
	where maximo = maximum $ map abs (map e ts)

extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures es ts = let normalizados = map (normalizarExtractor ts) es in
	map (evaluar normalizados) ts
	where evaluar fs = (\x -> map (\f -> f x) fs)

prodInt :: [Float] -> [Float] -> Float
prodInt xs ys = sum $ zipWith (*) xs ys

norm :: [Float] -> Float
norm xs = sqrt $ prodInt xs xs

distEuclideana :: Medida
distEuclideana xs ys = norm $ zipWith (-) xs ys

distCoseno :: Medida
distCoseno xs ys = (prodInt xs ys) / (norm xs * norm ys)

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn = undefined

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy xs ys = fromIntegral (apariciones True (zipWith (==) xs ys)) / genericLength xs

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos = undefined

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation = undefined

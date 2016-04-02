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

cantAp :: Eq a => a -> [a] -> Int
cantAp a l = length $ filter (==a) l

cuentas :: Eq a => [a] -> [(Int, a)]
cuentas l = [(cantAp x l, x) | x <- nub l]

repeticionesPromedio :: Extractor
repeticionesPromedio xs = mean $ map (\x -> fromIntegral (fst x)) (cuentas (split ' ' xs))

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
frecuenciaTokens = [ (\xs -> fromIntegral (cantAp t xs) / genericLength xs) | t <- tokens ]

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor ts e = (\xs -> e xs / maximo)
	where maximo = maximum $ map abs (map e ts)

extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures es ts = [ [ e t | e <- map (normalizarExtractor ts) es ] | t <- ts ]

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
accuracy = undefined

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos = undefined

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation = undefined

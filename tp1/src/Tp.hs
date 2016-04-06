module Tp where

import Data.List
import Data.Function

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
cuentas xs = map (\y -> (apariciones y xs, y)) (nub xs)

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

calcular_max_moda :: [(Float,Etiqueta)] -> Etiqueta
calcular_max_moda ls = snd (maximumBy (compare `on` fst) (cuentas [snd x | x <- ls]))

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn k datos es dist pto = calcular_max_moda (take k (sort procesada))
  where procesada = zip (map (\x  -> dist x pto) datos) es

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy xs ys = fromIntegral (apariciones True (zipWith (==) xs ys)) / genericLength xs

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos ds es nparts part = (sinParticion part (ajustar ds),
                                  soloParticion part (ajustar ds),
                                  sinParticion part (ajustar es),
                                  soloParticion part (ajustar es))
	where ajustar = take (nparts * tampart)
	      sinParticion p xs = (take (tampart * (p - 1)) xs) ++ (drop (tampart * p) xs)
	      soloParticion p xs = take tampart (drop (tampart * (p - 1)) xs)
	      tampart = length ds `div` nparts

primero :: (a,b,c,d) -> a
primero (x,y,z,w) = x

segundo :: (a,b,c,d) -> b
segundo (x,y,z,w) = y

tercero :: (a,b,c,d) -> c
tercero (x,y,z,w) = z

cuarto :: (a,b,c,d) -> d
cuarto (x,y,z,w) = w
  
calcularEtiquetas :: Int -> (Datos, Datos, [Etiqueta], [Etiqueta]) -> Medida -> [Etiqueta]
calcularEtiquetas k x medida = (map (knn k (primero x) (tercero x) medida) (segundo x))

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos es = mean aplicarKnn
	where datosSeparados = [separarDatos datos es n p | p <- [1..n]]
	      aplicarKnn = [accuracy (calcularEtiquetas 15 part distEuclideana) (cuarto part) | part <- datosSeparados]

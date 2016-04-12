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
split c yss = filter (not . null) (split' c yss)
	where split' c xss = foldr (\x rec -> if x==c then agregarVacia rec else agregarAlPrimero x rec) [[]] xss
	      agregarVacia xss = [] : xss
	      agregarAlPrimero x xss = (x : head xss) : tail xss

longitudPromedioPalabras :: Extractor
longitudPromedioPalabras texto = mean $ map genericLength palabrasSeparadas
	where palabrasSeparadas = split ' ' texto

apariciones :: Eq a => a -> [a] -> Int
apariciones elem xs = length $ filter (==elem) xs

cuentas :: Eq a => [a] -> [(Int, a)]
cuentas xs = map cantidadApariciones (nub xs)
	where cantidadApariciones = \y -> (apariciones y xs, y)

repeticionesPromedio :: Extractor
repeticionesPromedio xs = mean $ map cantidad palabrasContadas
	where cantidad = fromIntegral . fst
	      palabrasContadas = cuentas (split ' ' xs)

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
frecuenciaTokens = map frecuencia tokens
	where frecuencia token texto = fromIntegral (apariciones token texto) / genericLength texto

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor textos extractor = \xs -> extractor xs / maximo
	where maximo = maximum $ map (abs . extractor) textos

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

calcularEtiquetaModa :: [(Float,Etiqueta)] -> Etiqueta
calcularEtiquetaModa ls = snd (maximumBy compararPorPrimero cantidadEtiquetas)
	where cantidadEtiquetas = cuentas $ map snd ls
	      compararPorPrimero = compare `on` fst

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn k datos etiquetas distancia punto = calcularEtiquetaModa kMasCercanas
	where kMasCercanas = take k (sort distanciaPorEtiqueta)
	      distanciaPorEtiqueta = zip listaDistancias etiquetas
	      listaDistancias = map distanciasAlPunto datos
	      distanciasAlPunto = distancia punto

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy xs ys = fromIntegral cantidadAciertos / genericLength xs
	where cantidadAciertos = apariciones True aciertos
	      aciertos = zipWith (==) xs ys

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos ds es nparts part = (sinParticion part (ajustarTamanio ds),
                                  soloParticion part (ajustarTamanio ds),
                                  sinParticion part (ajustarTamanio es),
                                  soloParticion part (ajustarTamanio es))
	where ajustarTamanio = take (nparts * tamanioParticion)
	      sinParticion p xs = (take (tamanioParticion * (p - 1)) xs) ++ (drop (tamanioParticion * p) xs)
	      soloParticion p xs = take tamanioParticion (drop (tamanioParticion * (p - 1)) xs)
	      tamanioParticion = length ds `div` nparts

calcularEtiquetas :: (Datos, Datos, [Etiqueta], [Etiqueta]) -> [Etiqueta]
calcularEtiquetas (datosEntrenamiento, datosValidacion, etiquetasEntrenamiento, _) = map aplicarKnn datosValidacion
	where aplicarKnn = knn 15 datosEntrenamiento etiquetasEntrenamiento distEuclideana


nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos es = mean resultadosIntermedios
	where resultadosIntermedios = [accuracy (etiquetasEntrenamiento particion) (etiquetasValidacion particion) | particion <- generarParticiones]
	      etiquetasEntrenamiento particion = calcularEtiquetas particion
	      generarParticiones = [separarDatos datos es n p | p <- [1..n]]
	      etiquetasValidacion (_, _, _, es) = es

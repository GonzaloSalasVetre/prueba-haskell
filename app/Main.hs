{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Main where
import Criterion
import Criterion.Main (defaultMain)

main :: IO ()
main = defaultMain [
             bgroup "busquedaLineal" [ bench "1"  $ whnf busquedaLinealPrueba 1
               , bench "6"  $ whnf busquedaLinealPrueba 6
               , bench "5"  $ whnf busquedaLinealPrueba 5
               , bench "8" $ whnf busquedaLinealPrueba 8
               ],
               bench "Burbuja" $ whnf ordenamientoBurbuja listaPrueba2
        ]

listaPrueba :: [Integer]
listaPrueba = [2,3,1,2,3,6,5,8]

listaPrueba2 :: [Integer]
listaPrueba2 = [9,8,7,6,5,4,3,2,1]

busquedaLineal :: Eq a => [a] -> a -> Bool
busquedaLineal [] _ = False
busquedaLineal (x:xs) elemento | x == elemento = True
                               | otherwise = busquedaLineal xs elemento

busquedaLinealPrueba :: Integer -> Bool
busquedaLinealPrueba = busquedaLineal listaPrueba


ordenamientoBurbuja :: Ord a => [a] -> [a]
ordenamientoBurbuja lista = foldl cambiarHasta [] lista

cambiarHasta :: Ord b => [b] -> b -> [b]
cambiarHasta [] elemento = [elemento]
cambiarHasta (siguienteElemento:cola) elemento =
    min elemento siguienteElemento : cambiarHasta cola (max elemento siguienteElemento)

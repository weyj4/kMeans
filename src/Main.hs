{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.ByteString.Lazy as L
import Data.Csv
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Math.KMeans
import System.IO (withFile, getLine, IOMode( ReadMode ))
import GHC.Generics

main :: IO ()
main = do
  putStrLn "input file:"
  inp <- getLine
  withFile inp ReadMode $ \dataHandle -> do
    content <- L.hGetContents dataHandle
    case getData content of
      (Left err) -> putStrLn err
      (Right dat) -> do
        print (V.head dat)
        print (V.length dat)
        print (getTypesOfClusters $ runKMeans dat)

data Flower = Flower { id :: !Double, sepalLengthCm :: !Double
                     , sepalWidthCm :: !Double, petalLengthCm :: !Double
                     , petalWidthCm :: !Double, species :: !String
                     } deriving (Generic, Show)

instance FromRecord Flower

getData :: L.ByteString -> Either String (V.Vector Flower)
getData content = decode HasHeader content :: Either String (V.Vector Flower)

kMeansData :: Flower -> U.Vector Double
kMeansData = U.fromList . doubles
  where doubles a = [sepalLengthCm a, sepalWidthCm a, petalLengthCm a, petalWidthCm a]

runKMeans :: V.Vector Flower -> Clusters Flower
runKMeans content = kmeans kMeansData euclidSq 3 (V.toList content)

getTypesOfClusters :: Clusters Flower -> V.Vector [Int]
getTypesOfClusters clusters = V.map flowerTypes clusters

flowerTypes :: Cluster Flower -> [Int]
flowerTypes (Cluster a) = [ length $ filter (\x -> species x == "Iris-virginica") a
                          , length $ filter (\x -> species x == "Iris-versicolor") a
                          , length $ filter (\x -> species x == "Iris-setosa") a
                          ]

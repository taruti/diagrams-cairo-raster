{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
module Main(main) where

import Control.DeepSeq
import Criterion.Main
import Data.Array.Repa as R hiding((++))
import Diagrams.Backend.Cairo.Raster
import Diagrams.Backend.Cairo.Raster.Repa
import Numeric.Noise
import Numeric.Noise.Perlin

main :: IO ()
main = defaultMain benchmarks

benchmarks :: [Benchmark]
benchmarks =
  [ bench "crgb"   $ nf3 crgb   (cn,cn,cn)
  , bench "crgbap" $ nf4 crgbap (cn,cn,cn,cn)
  , bench "crgba"  $ nf4 crgba  (cn,cn,cn,cn)
  , bench "PlainRaster" $ computePlainF cplus
  , repaF "RepaRaster" rplus
  , bench "PlainNoise" $ computePlainF $ noisePlain 1
  , repaF "RepaNoise" $ noiseRepa 1
  ]

w, h, cn :: Int
w = 947
h = 547
cn= 99

computePlainF :: PlainF -> IO ()
computePlainF f = whnfIO $ cairoRaster f w h

repaF :: String -> RepaF -> Benchmark
repaF name fun = bench name $ whnfIO $ cairoRepa (\d -> fromFunction d fun) w h


type PlainF= Int -> Int -> CairoColor
type RepaF = DIM2 -> CairoColor

cplus :: PlainF
cplus x y = crgb x y (x*y)
rplus :: RepaF
rplus !(Z :. y :. x) = crgb x y (x*y)

nf3 :: NFData b => (t1 -> t2 -> t3 -> b) -> (t1, t2, t3) -> Pure
nf3 = nf . uncurry3
nf4 :: NFData b => (t1 -> t2 -> t3 -> t4 -> b) -> (t1, t2, t3, t4) -> Pure
nf4 = nf . uncurry4

uncurry3 :: (t1 -> t2 -> t3 -> t) -> (t1, t2, t3) -> t
uncurry3 f (a,b,c) = f a b c
uncurry4 :: (t1 -> t2 -> t3 -> t4 -> t) -> (t1, t2, t3, t4) -> t
uncurry4 f (a,b,c,d) = f a b c d


noisePlain :: Int -> PlainF
noisePlain !seed =
  let noise   = noiseValue (perlin seed 6 4 0.5)
      fn c    = fromIntegral (c+1) / fromIntegral (div (w + h) 2)
      f !x !y = let !c = round ((1 + noise (fn x, fn y, 0)) * 127) in crgb 255 c (c :: Int)
  in f
noiseRepa :: Int -> RepaF
noiseRepa !seed =
  let noise            = noiseValue (perlin seed 6 4 0.5)
      fn c             = fromIntegral (c+1) / fromIntegral (div (w + h) 2)
      f !(Z :. y :. x) = let !c = round ((1 + noise (fn x, fn y, 0)) * 127) in crgb 255 c (c :: Int)
  in f
     

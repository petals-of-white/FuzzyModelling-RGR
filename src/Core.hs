module Core where

f, w :: Double -> Double
f = sin
w = sin

z :: Double -> Double -> Double
z x y = f x * w y

module Core where

g, k :: Double
g = 1
k = 9

f, w :: Double -> Double
f = sin
w = sin

z :: Double -> Double -> Double
z x y = f x * w y

xDomain, yDomain, zDomain :: (Double, Double)
xDomain = (k, 2*(k+1))
yDomain = (g, 2*(g+1))
zDomain = (-1, 1)

module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)

import Math(pi, sqrt)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = pi * (r * r)

main :: Effect Unit
main = do
  logShow (diagonal 3.0 4.0)
  logShow (circleArea 5.0)

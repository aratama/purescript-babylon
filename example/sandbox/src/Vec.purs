module Graphics.Babylon.Example.Vec where

type Vec = { x :: Number, y :: Number, z :: Number }

vec :: Number -> Number -> Number -> { x :: Number, y :: Number, z :: Number }
vec x y z = { x, y, z }


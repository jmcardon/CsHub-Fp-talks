{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}

module Main where

import           Text.PrettyPrint              as PP
import           Untyped                        ( replU )

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

fac' :: Integer -> Integer
fac' n = product [1 .. n]

-- Also called const
k :: forall a b . a -> b -> a
k = \a b -> a

main :: IO ()
main = replU

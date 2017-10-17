module Main where

import Parse (parse', xp)

main :: IO ()
main = print $ parse' xp "prover (h # t) ≡ prover (solves (h # t))"

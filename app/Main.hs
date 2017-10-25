module Main where

import Parse (parse', parser)

main :: IO ()
main = print "a"-- parse' parser "prover (h # t) ≡ prover (solves (h # t))"

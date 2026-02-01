{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import Buffer (fromString)
import Editor
import ScrabbleBuffer

main :: IO ()
main =
  runEditor editor $
    scrabbleBuffer $
      unlines
        [ "This buffer is for notes you don't want to save, and for",
          "evaluation of steam valve coefficients.",
          "To load a different file, type the character L followed",
          "by the name of the file."
        ]

scrabbleBuffer :: String -> ScrabbleBuffer
scrabbleBuffer = fromString

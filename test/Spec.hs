import Test.DocTest

main =
  doctest
    [ "-isrc",
      "src/CreditCardNumber.hs",
      "src/Hanoi.hs",
      "src/LogAnalysis.hs",
      "src/Golf.hs",
      "src/Homework4.hs",
      "src/Calc.hs",
      "src/JoinList.hs",
      "src/Scrabble.hs",
      "src/ScrabbleBuffer.hs",
      "src/Party.hs",
      "src/Typeclassopedia.hs"
    ]

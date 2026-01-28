import Test.DocTest

main =
  doctest
    [ "-isrc",
      "src/CreditCardNumber.hs",
      "src/Hanoi.hs",
      "src/LogAnalysis.hs",
      "src/Golf.hs",
      "src/Homework4.hs"
    ]

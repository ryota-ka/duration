import Test.DocTest (doctest)

main :: IO ()
main = doctest
   [ "-isrc"
   , "src/Data/Time/Clock/Ms.hs"
   , "src/Data/Time/Clock/Ms/QQ.hs"
   , "src/Data/Time/Clock/Ms/Types.hs"
   ]

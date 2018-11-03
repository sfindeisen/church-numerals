import Test.QuickCheck

type Church a = (a -> a) -> a -> a

church :: Integer -> Church Integer
church 0 = \f -> \x -> x
church n = \f -> \x -> f (church (n-1) f x)

unchurch :: Church Integer -> Integer
unchurch cn = cn (+ 1) 0

plus :: Church Integer -> Church Integer -> Church Integer
plus m n f x = m f (n f x)

---------------------------------------
---- QuickCheck
---------------------------------------

gen100 :: Gen Integer
gen100 = choose (0, 100)

prop_roundtrip x =
    x == unchurch (church x)

prop_plus x y =
    (x+y) == unchurch (plus (church x) (church y))

---------------------------------------
---- main
---------------------------------------

-- check = verboseCheck
check = quickCheck

main :: IO ()
main = do
    check $ forAll gen100 prop_roundtrip
    check $ forAll gen100 (\x -> forAll gen100 (prop_plus x))

import Test.QuickCheck

type Church a = (a -> a) -> a -> a

church :: Integer -> Church a
church 0 f x = x
church n f x = f (church (n-1) f x)

unchurch :: Church Integer -> Integer
unchurch cn = cn (+ 1) 0

zero :: Church a
zero = church 0

one :: Church a
one = church 1

two :: Church a
two = church 2

is_zero :: Church Integer -> Bool
is_zero n =
    1 == (n (\z -> 0) 1)

plus :: Church a -> Church a -> Church a
plus m n f x = m f (n f x)

inc :: Church a -> Church a
inc n f x = f (n f x)

mul :: Church a -> Church a -> Church a
mul m n f x = m (n f) x

---------------------------------------
-- QuickCheck
---------------------------------------

gen100 :: Gen Integer
gen100 = choose (0, 100)

prop_roundtrip x =
    x == unchurch (church x)

prop_church_string :: Integer -> Bool
prop_church_string x =
    x == (toInteger $ length $ (church x) ('c':) [])

prop_zero x =
    (0 == x) == is_zero (church x)

prop_plus x y =
    (x+y) == unchurch (plus (church x) (church y))

prop_inc x =
    (x+1) == unchurch (inc (church x))

prop_mul x y =
    (x*y) == unchurch (mul (church x) (church y))

---------------------------------------
-- main
---------------------------------------

-- check = verboseCheck
check = quickCheck

main :: IO ()
main = do
    check $ forAll gen100 prop_roundtrip
    check $ forAll gen100 prop_church_string
    check $ once (prop_zero 0)
    check $ forAll gen100 prop_zero
    check $ forAll gen100 (\x -> forAll gen100 (prop_plus x))
    check $ forAll gen100 prop_inc
    check $ forAll gen100 (\x -> forAll gen100 (prop_mul x))

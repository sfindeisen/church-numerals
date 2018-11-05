import Test.QuickCheck

type Church a = (a -> a) -> a -> a

church :: Integer -> Church a
church 0 f = id
church n f = f . (church (n-1) f)

zero :: Church a
zero = church 0

one :: Church a
one = church 1

inc :: Church a -> Church a
inc n f = f . n f

plus :: Church a -> Church a -> Church a
plus n m f = n f . m f

mul :: Church a -> Church a -> Church a
mul n m f = n (m f)

-- The exponential function: expo n m = m n == n^m . This works by the
-- virtue of currying:
--
-- (church 0) n = id
-- (church 1) n = n . id
-- (church 2) n = n . n . id
-- ...
--
-- However this function cannot be explicitly typed as:
--
-- expo :: Church a -> Church a -> Church a
--
-- although it would accept these parameters and yield the correct
-- (Church a) result (see example below).
expo n m = m n

unchurch :: (Num a) => Church a -> a
unchurch cn = cn (+ 1) 0

is_zero :: (Eq a, Num a) => Church a -> Bool
is_zero n =
    1 == (n (\z -> 0) 1)

---------------------------------------
-- QuickCheck
---------------------------------------

gen100 :: Gen Integer
gen100 = choose (0, 100)

gen10 :: Gen Integer
gen10 = choose (0, 10)

gen5 :: Gen Integer
gen5 = choose (0, 5)

prop_roundtrip :: Integer -> Bool
prop_roundtrip x =
    x == unchurch (church x)

prop_church_string :: Integer -> Bool
prop_church_string x =
    x == (toInteger $ length $ (church x) ('c':) [])

prop_zero :: Integer -> Bool
prop_zero x =
    (0 == x) == is_zero (church x)

prop_plus :: Integer -> Integer -> Bool
prop_plus x y =
    (x+y) == unchurch (plus (church x) (church y))

prop_inc :: Integer -> Bool
prop_inc x =
    (x+1) == unchurch (inc (church x))

prop_mul :: Integer -> Integer -> Bool
prop_mul x y =
    (x*y) == unchurch (mul (church x) (church y))

prop_expo :: Integer -> Integer -> Bool
prop_expo x y =
    (x^y) == unchurch (expo (church x) (church y))

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
    check $ forAll gen10  (\x -> forAll gen5 (prop_expo x))

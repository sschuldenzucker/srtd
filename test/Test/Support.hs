module Test.Support where

import Test.Tasty.HUnit

-- Don't wanna depend on extra right now.
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft f (Right y) = Right y

dropLeft :: Either a b -> Either () b
dropLeft = mapLeft (const ())

-- | Better than raw '@?=' because multi-line errors are formatted better.
shouldBeRight :: (Eq b, Show b) => Either String b -> b -> Assertion
shouldBeRight (Left errstr) _ = assertFailure errstr
shouldBeRight (Right x) expd = x @?= expd

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Main where



import ShuntingYard

import System.Exit ( exitFailure )
import Test.QuickCheck.Test ( quickCheckResult, isSuccess )
import Control.Monad ( unless )
import Test.QuickCheck
  (  Gen, Arbitrary
  , withMaxSuccess, verbose, arbitrary, sized, elements, choose
  )



instance Show Assoc where
  showsPrec _ L = showChar 'L'
  showsPrec _ N = showChar 'N'
  showsPrec _ R = showChar 'R'

instance Eq Assoc where
  L == L = True
  R == R = True
  N == N = True
  _ == _ = False



type Op = (Assoc, Int)

data Expr = Val | Tie Op Expr Expr
  deriving Eq


instance Operator Op Expr where
  prec = snd
  assoc = fst
  apply = Tie


instance Show Expr where
  showsPrec _ Val = showString "_"
  showsPrec q (Tie (a, p) l r) =
    showParen (q > 0) $ showsPrec 1 l . showChar ' ' . shows p . shows a
    . showChar ' ' . showsPrec 1 r



instance Arbitrary Assoc where
  arbitrary = elements [L, N, R]



instance Arbitrary Expr where
  arbitrary = sized $ \size -> arbitraryExpr (size + 1)


arbitraryExpr :: Int -> Gen Expr

arbitraryExpr n = do
  a <- arbitrary
  mkExpr (a, 0) n


mkExpr root 1 = pure Val

mkExpr (a, p) s = do
  sl <- choose (1, s - 1)
  let sr = s - sl

  pl <- a == L ? choose (p, p + 1) $ pure (p + 1)
  -- pl <- choose (p, p + 1) -- entails failure

  al <- p == pl ? pure L $ arbitrary
  -- al <- arbitrary -- entails failure

  left <- mkExpr (al, pl) sl


  pr <- a == R ? choose (p, p + 1) $ pure (p + 1)
  -- pr <- choose (p, p + 1) -- entails failure

  ar <- p == pr ? pure R $ arbitrary
  -- ar <- arbitrary -- entails failure

  right <- mkExpr (ar, pr) sr


  pure $ Tie (a, p) left right



serialize :: Expr -> (Expr, [(Op, Expr)])

serialize Val = (Val, [])

serialize (Tie o l r) = (le, los ++ ((o, re) : ros))
  where
    (le, los) = serialize l
    (re, ros) = serialize r



prop :: Expr -> Bool

prop expr = case shuntingYard e os of
              Right r -> r == expr
              Left (o1, o2) -> error $ show o1 ++ "/" ++ show o2
  where
    (e, os) = serialize expr


main :: IO ()

main = do
  result <- quickCheckResult . verbose . withMaxSuccess 10 $ prop
  --result <- quickCheckResult . withMaxSuccess 100000 $ prop
  unless (isSuccess result) exitFailure




infix 1 ?
(?) :: Bool -> p -> p -> p
c ? t = \f -> if c then t else f

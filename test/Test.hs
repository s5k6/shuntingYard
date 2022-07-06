{-# LANGUAGE TemplateHaskell #-}

module Main where



import ShuntingYard

import System.Exit ( exitFailure )
import System.Environment ( getArgs )
import Test.QuickCheck
  (  Gen, Arbitrary
  , arbitrary, sized, elements, choose, oneof
  , forAllProperties, quickCheckWithResult, stdArgs, maxSuccess
  )



{- Generating arbitrary associativity. -}

instance Show Assoc where
  showsPrec _ L = showChar 'L'
  showsPrec _ N = showChar 'N'
  showsPrec _ R = showChar 'R'

instance Eq Assoc where
  L == L = True
  R == R = True
  N == N = True
  _ == _ = False

instance Arbitrary Assoc where
  arbitrary = elements [L, N, R]



{- The operators have no functionality, only precedence and
associativity.  The expressions sport a leaf value, and an inner
binary node annotated with an operator.  We will generate random
expressions, and verify whether reconstruction succeeds. -}

type Op = (Assoc, Int)

data Expr = Val | Tie Op Expr Expr
  deriving Eq

instance Show Expr where
  showsPrec _ Val = id
  showsPrec q (Tie (a, p) l r) =
    showParen (q > 0) $ showsPrec 1 l . showChar ' ' . shows p . shows a
    . showChar ' ' . showsPrec 1 r



{- Generator for expressions.  The first argument specifies whether the
generated expression should be reconstructable by the Shunting Yard
algorithm.  `Just True` generates a valid erpression, `Just False`
generates an invalid expression, and `Nothing` implies random choice
between the two.  Note, that an invalid expression implies size of at
least three. -}

mkExpr :: Maybe Bool -> (Assoc, Int) -> Int -> Gen Expr

mkExpr _ _ 1 = pure Val

mkExpr good (a, p) size = do
  split <- choose (1, size - 1)
  left <- mkSub L split
  right <- mkSub R (size - split)
  pure $ Tie (a, p) left right

  where
    mkSub dir s = case good of
      Just True -> goGood
      Just False | s <= 3 -> goBad
                 | s < 3 -> error "Must not happen: not enough leeway"
      _ -> oneof [goGood, goBad]

      where
        goGood = do
          ps <- choose (a == dir ? p $ p + 1, p + 3)
          as <- p == ps ? pure dir $ arbitrary
          mkExpr good (as, ps) s

        goBad =  do
          a' <- case a of
                  N -> arbitrary
                  L -> oneof [pure N, pure R]
                  R -> oneof [pure N, pure L]
          mkExpr Nothing (a', p) s



{- Different types for good and bad expressions. -}

newtype GoodExpr = GoodExpr { unGoodExpr :: Expr }

instance Show GoodExpr where show = show . unGoodExpr

instance Arbitrary GoodExpr where
  arbitrary = sized $ \size -> do
    a <- arbitrary
    GoodExpr <$> mkExpr (Just True) (a, 0) (size + 1)



newtype BadExpr = BadExpr { unBadExpr :: Expr }

instance Show BadExpr where show = show . unBadExpr

instance Arbitrary BadExpr where
  arbitrary = sized $ \size -> do
    a <- arbitrary
    BadExpr <$> mkExpr (Just False) (a, 0) (size + 3)



{- The inverse of `shuntingYard` -}

serialize :: Expr -> (Expr, [(Op, Expr)])

serialize Val = (Val, [])

serialize (Tie o l r) = (le, los ++ ((o, re) : ros))
  where
    (le, los) = serialize l
    (re, ros) = serialize r



{- Property: Reconstructable expressions are recunstructed correctly. -}

prop_good :: GoodExpr -> Bool

prop_good (GoodExpr expr) =
  either (const False) (== expr) $ shuntingYard snd fst Tie e os
  where
    (e, os) = serialize expr



{- Property: Invalid expressions yield an error. -}

prop_bad :: BadExpr -> Bool

prop_bad (BadExpr expr) =
  either (const True) (const False) $ shuntingYard snd fst Tie e os
  where
    (e, os) = serialize expr



infix 1 ?
(?) :: Bool -> p -> p -> p
c ? t = \f -> if c then t else f



return [] -- need this for GHC 7.8



main :: IO ()

main = do
  args <- parseArgs <$> getArgs
  s <- $(forAllProperties) $ quickCheckWithResult args
  s ? return () $ exitFailure
  where
    parseArgs as
      = null as ? stdArgs $ stdArgs{ maxSuccess = read $ head as }

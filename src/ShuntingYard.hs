{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}


{- An implementation of Edsger Dijkstra's ‘Shunting yard algorithm’ [1].

An improvement is the fusing of the operator stack with the operand
stack, which makes the compiler prove that there is always the same
amount of items on both of them (at a carefully chosen point in time
of execution). -}

module ShuntingYard where


{- All operators are binary, have an integer precedence and *may*
associate to the left (L) or right (R), or not at all (N). -}

data Assoc = L | N | R

{- Conflict resolution is first by precedence, highest first

    1 / 2 + 3  →  (1 / 2) + 3
    1 + 2 * 3  →  1 + (2 * 3)

then by associativity

    2 / 2 / 3  →  (2 / 2) / 3
    2 ^ 2 ^ 3  →  2 ^ (2 ^ 3)

and non-associative operatores with same precedence will yield an
error

    1 < 3 > 2    -- is this comparing a boolean with an integer?

These are all good choices, also implemented by Haskell. -}


{- The implementation abstracts from the type `op` of operators and the
type `ex` of of expressions.  But it needs to get information from the
operators, and also relates the two types to each other for
application. -}

class Operator op ex | op -> ex where
  prec :: op -> Int
  assoc :: op -> Assoc
  apply :: op -> ex -> ex -> ex

-- FIXME: learn how to do this with type families?



{-The input type guarantees a sequence of operands correctly interleaved
with operators.  Note that we do not handle parenthesis, they cannot
occur in the use case I'm interested in. -}

shuntingYard :: Operator op ex => ex -> [(op, ex)] -> Either (op, op) ex

shuntingYard = go []
  where

    go stack@((e0, o1) : stackRest) e1 input@((o2, e2) : inputRest) =
      case compare (prec o1) (prec o2) of
        GT -> bind
        LT -> push
        EQ -> case (assoc o1, assoc o2) of
                (L, L) -> bind
                (R, R) -> push
                otherwise -> Left (o1, o2)
      where
        bind = go stackRest (apply o1 e0 e1) input
        push = go ((e1, o2) : stack) e2 inputRest

    -- stack empty, must consume input
    go [] e0 ((o1, e1) : inputRest) = go [(e0, o1)] e1 inputRest

    -- input empty, and stack is strictly ordered!
    go stack e [] = Right $ foldl (\e1 (e0, o1) -> apply o1 e0 e1) e stack


{- Variable naming convention: Assume the input to be a sequence of
expressions `e…`, interspersed with operators `o…`, enumerated such:

    e0 o1 e1 o2 e2 o3 e3 o4 e4 …    -- note: no operator `o0`!

The parser feeds this on the Shunting Yard as the first expression,
and a list of pairs:

    e0, [(o1, e1), (o2, e2), (o3, e3), (o4, e4), …

All decision making occurs with neither stack nor input empty.  If,
say, the first two operators went on the stack, it would be

    [(e1, o2), (e0, o1)]

see what I've done there?  I've shifted the pairing!  This becomes
more clear when one reads the stack backwards (indicated below by
Strachey/Scott brackets ⟦·⟧ in an abuse of notation).  The call to the
`go` function with this stack would represent the situation

    go  ⟦ (e0, o1), (e1, o2) ⟧  e2  [ (o3, e3), (o4, e4), … ]

which encodes “having moved into” the input sequence, up to e2, and
now weighing operator o2 against o3.  Who will win over e2's heart?
Will e2 bind to o2 and beget a new expression?  Or will it elope with
o3 onto the stack, making e3 the next scrutinee?  Will there be
unresolvable conflict?  See in the next season of getting carried away
with unreasonable pun…

Expressions and operators are stored on a stack only if it is empty
(`go`'s second case), or if its top operator looses against the next
operator on the input.  It is thus strictly ordered with respect to
conflict resolution, which is exploited in `go`'s last case when the
input is all consumed. -}



{-
[1]: https://en.wikipedia.org/wiki/Shunting_yard_algorithm
-}

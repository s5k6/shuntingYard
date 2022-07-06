
{- An implementation of Edsger Dijkstra's ‘Shunting yard algorithm’ [1].

An improvement is the fusing of the operator stack with the operand
stack, which makes the compiler prove that there is always the same
amount of items on both of them (at a carefully chosen point in time
of execution). -}

module ShuntingYard ( Assoc(L, R, N), shuntingYard ) where



{- All operators are binary, have an integer precedence and *may*
associate to the left (L), the right (R), or not at all (N). -}

data Assoc = L | N | R



{- Conflict resolution is first by precedence, highest first

    1 / 2 + 3  →  (1 / 2) + 3
    1 + 2 * 3  →  1 + (2 * 3)

then by associativity

    2 / 2 / 3  →  (2 / 2) / 3
    2 ^ 2 ^ 3  →  2 ^ (2 ^ 3)  .

Conflicts between operators with the same precedence and different
associativity yield an error, as do non-associative operators:

    1 < 3 > 2  →  ☠  -- is this comparing a boolean with an integer?

These are all good choices, also implemented by languages like
Haskell. -}



{- The implementation abstracts from the type `op` of operators and the
type `ex` of of expressions.  The first three arguments to
`shuntingYard` provide precedence and associativity of an operator,
and a means to construct a new expression by applying an operator to
two subexpressions.

The input sequence is provided as the head expression and a possibly
empty list of pairs of followup operators and expressions, thus
guaranteeing correct interleaving. -}

shuntingYard
  :: (op -> Int)             -- precedence of an operator
  -> (op -> Assoc)           -- associativity of an operator
  -> (op -> ex -> ex -> ex)  -- construct expression
  -> ex                      -- head expression of input stream
  -> [(op, ex)]              -- followup operators and expressions
  -> Either (op, op) ex      -- conflicting operators or expression root

shuntingYard prec assoc apply = go []
  where

    -- decide where `e1` belongs to, detailed description below
    go stack@((e0, o1) : stackRest) e1 input@((o2, e2) : inputRest) =
      case compare (prec o1) (prec o2) of
        GT -> bind
        LT -> push
        EQ -> case (assoc o1, assoc o2) of
                (L, L) -> bind
                (R, R) -> push
                _ -> Left (o1, o2)
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

The parser feeds this on the Shunting yard as the first expression,
and a list of pairs:

    e0, [(o1, e1), (o2, e2), (o3, e3), (o4, e4), …

All decision making occurs with neither stack nor input empty.  If,
say, the first two operators went on the stack, it would be

    [(e1, o2), (e0, o1)]

see what I've done there?  I've shifted the pairing!  This becomes
more clear when one reads the stack backwards (indicated below by
Strachey/Scott brackets ⟦·⟧ in an abuse of notation).  The call to the
`go` function

    go  ⟦ (e0, o1), (e1, o2) ⟧  e2  [ (o3, e3), (o4, e4), … ]

encodes “having moved into” the input sequence, up to e2, and now
weighing operator o2 against o3.  Who will win over e2's heart?  Will
e2 bind to o2 and beget a new expression?  Or will it elope with o3
onto the stack, making e3 the next scrutinee?  Will there be
unresolvable conflict?  See in the next season of getting carried away
with unreasonable pun…

Expressions and operators are stored on the stack only if it is empty
(`go`'s second case), or if its top operator looses against the next
operator on the input.  It is thus strictly ordered with respect to
conflict resolution, which is exploited in `go`'s last case when the
input is all consumed. -}



{-
[1]: https://en.wikipedia.org/wiki/Shunting_yard_algorithm
-}

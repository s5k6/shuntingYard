
{- An implementation of Edsger Dijkstra's ‘Shunting yard algorithm’ [1].

An improvement is the fusing of the operator stack with the operand
stack, which makes the compiler prove that there is always the same
amount of items on both of them (at a carefully chosen point in time
of execution).

The input type guarantees a sequence of operands correctly interleaved
with operators.  Note that we do not handle parenthesis, they cannot
occur in the use case I'm interested in. -}

module ShuntingYard where


{- All operators are binary, have an integer precedence and may
associate to the left ot right.  Conflict resolution is first by
precedence

    1 / 2 + 3  →  (1 / 2) + 3
    1 + 2 * 3  →  1 + (2 * 3)

then by associativity

    2 / 2 / 3  →  (2 / 2) / 3
    2 ^ 2 ^ 3  →  2 ^ (2 ^ 3)

and non-associative operatores with same precedence will yield an
error

    1 < 3 > 2

whese are all good choices, also implemented by Haskell. -}



-- Associativity of an operator is one of these three

data Assoc = L | N | R
  deriving Eq



{- Variable naming convention: Assume the input to be a sequence of
expressions `e…`, interspersed with operators `o…`, enumerated such:

    e0 o1 e1 o2 e2 o3 e3 o4 e4    -- note: no operator `o0`!

The parser feeds this on the Shunting Yard as the first expression,
and a list of pairs:

    e0, [(o1, e1), (o2, e2), (o3, e3), …

Unconsumed expressions and operators are stored on a stack (actually
two stacks, but this way the compiler proves that they have the same
amount of entries.  On the stack, if they go there at all, then they
are stored as

    [(e0, o1), (e1, o2) (e2, o3), …    -- but backwards

see what I've done there?  I've shifted the pairing. -}


shuntingYard
  :: (op -> Assoc)                -- associativity of operator
  -> (op -> Int)                  -- precedence of operator
  -> (op -> expr -> expr -> expr) -- tie two expressions with operator
  -> expr -> [(op, expr)]    -- input expressions interleaved with operators
  -> Either (op, op) expr    -- Left conflicting operators, Right result

shuntingYard assoc prec tie = go []
  where
    go stack@((e0, o1) : popped) e1 input@((o2, e2) : rest)
      | p1 == p2 && (a1 /= a2 || a1 == N || a2 == N) = Left (o1, o2)
      | p2 < p1 || (a1 == L && p1 == p2) = go popped (tie o1 e0 e1) input
      | otherwise = go ((e1, o2) : stack) e2 rest
      where
        (a1, p1) = (assoc o1, prec o1)
        (a2, p2) = (assoc o2, prec o2)

    go [] e0 ((o1, e1) : rest) = go [(e0, o1)] e1 rest
    go stack e [] = Right $ foldl (\e1 (e0, o1) -> tie o1 e0 e1) e stack


{-
[1]: https://en.wikipedia.org/wiki/Shunting_yard_algorithm
-}

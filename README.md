
Haskell *Shunting yard algorithm*
=================================

An implementation of [Edsger Dijkstra's *Shunting yard algorithm*][1],
which reconstructs an abstract syntax tree from a suitable expression
given in infix notation.

An advantage of using this algorithm, over fixity resolution in the
parser's grammar, is the increased flexibility.  Operators may be
added or removed without much changing the grammar, and fixity may
even be changed at runtime.  Also, the resulting grammar will be much
simpler.

This implementation improves on the [traditional presentation][1] by
using a single stack instead of two.  This yields more compile time
guarantees and thus cleaner code, since code for “impossible cases”
(invalid population of the two stacks) can be simply omitted.  This is
a consequence of choosing a rather narrow input type, which guarantees
correctly interleaved operands and operators.

Each operator is associated with a precedence (typically a natural
number), and an associativity (left, right, or none), together
occasionally referred to as “fixity”.  The next examples use
conventional fixities.  Conflict resolution is first by operator
precedence, highest first

    1 / 2 + 3  →  (1 / 2) + 3
    1 + 2 * 3  →  1 + (2 * 3)

and only then by operator associativity:

    2 / 2 / 3  →  (2 / 2) / 3
    2 ^ 2 ^ 3  →  2 ^ (2 ^ 3)

Conflicts between operators with the same precedence but different
associativity yield an error, as do non-associative operators:

    1 < 3 > 2  →  ☠       -- comparing a boolean with an integer?

These are all good choices, also implemented by languages like
Haskell.


Parenthesis
-----------

This implementation **does intentionally not handle parenthesis**,
they cannot occur in the use cases I'm interested in.  Let me put this
more affirmative: Having fixity resolution mingled with the handling
of parenthesis looks like an unnatural extension to the basic problem.
It may look straight forward if the language provides nothing more
than numbers and infix operators, but less so when a wider context
comes into view.

I find it much better design (cleaner, leaner) to have a language's
grammar be defined along the lines of:

    expression ::= singleton ( operator singleton )*

    singleton ::= valueLiteral          -- 42, 3.1, True, "lala", ...
                | functionCall
                | "(" expression ")"

    functionCall ::= functionName "(" ( expression ("," expression)* )? ")"

The production for `expression` can either use the traditional,
inflexible and cumbersome grammar-based way to specify fixity, or use
the presented Shunting Yard Algorithm instead.  Parenthesis are taken
care of in the production of `singleton`.


Higher arity operators
----------------------

A language designer may choose to allow forms like

    a < b < c < d

and interpret them as one of

    (a < b) & (b < c) & (c < d)
    all [a < b, b < c, c < d]
    ordered [a, b, c, d]

I would consider this a differnt syntactic form, and call it a an
n-ary operator, or an operator on lists (which only collapses into our
case if the number of operands is two).  This is not handled here.


[1]: https://en.wikipedia.org/wiki/Shunting_yard_algorithm

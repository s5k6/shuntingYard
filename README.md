
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

A demonstration of this is in `https://github.com/s5k6/sycalc`.

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

and only then by operator associativity

    2 / 2 / 3  →  (2 / 2) / 3
    2 ^ 2 ^ 3  →  2 ^ (2 ^ 3)  .

Conflicts between operators with the same precedence but different
associativity yield an error, as do non-associative operators:

    1 < 3 > 2  →  ☠

Is this comparing a boolean with an integer?

These are all good choices, also implemented by languages like
Haskell.

Note that this implementation does not handle parenthesis, they cannot
occur in the use cases I'm interested in: My grammars tend to have a
form where the parenthesis have already been taken care of when
looking at infix operators.


A bad idea
----------

The presence of a single non-associative operator yields an error,
unless resolved by precedence.  One might be tempted to allow mixing
of non-associative with associative operators, by letting the
associative operator also set the associativity of the other one.

Examples (using `<`, `~`, `>` for operators with respectively left,
none, and right associativity, all at the same precedence level):

    x < y ~ z  →  (x < y) ~ z
    x > y ~ z  →  x > (y ~ z)
    x ~ y < z  →  (x ~ y) < z
    x ~ y > z  →  x ~ (y > z)

This, however, breaks down when an associative operator appears
between two non-associative operators:

    w ~ x < y ~ z    or    w ~ x > y ~ z

Depending on the algorithm's processing order (we go left to right),
one of them (here: the former) will parse just fine

    w ~ x < y ~ z  →  (w ~ x) < y ~ z  →  ((w ~ x) < y) ~ z

while the other (here: the latter) will yield a conflict

    w ~ x > y ~ z  →  w ~ (x > y) ~ z  →  ☠

although a valid interpretation

    w ~ x > y ~ z  →  w ~ x > (y ~ z)  →  w ~ (x > (y ~ z))

exists.  I think the output of the algorithm should be agnostic on the
order in which it digests input (this is actually verified in the unit
tests).

Also, I doubt that squeezing semantics out of every possible syntactic
form necessarily increases readability of a language.  Lifting the
concept of Hamming Distance to PL syntax says otherwise.


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

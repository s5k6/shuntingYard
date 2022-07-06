
An implementation of [Edsger Dijkstra's ‘Shunting yard algorithm’][1].

An improvement is the fusing of the operator stack with the operand
stack, which makes the compiler prove that there is always the same
amount of items on both of them (at a carefully chosen point in time
of execution).

The input type guarantees a sequence of operands correctly interleaved
with operators.  Note that this implementation does not handle
parenthesis, they cannot occur in the use case I'm interested in: My
grammars tend to have the form

    Expression ::= Blah (Operator Blah)*

    Operator ::= `+` | `-` | `*` | …

    Blah ::= `(` Expression  (`,` Expression)* `)`
          | …

where the parenthesis have already been taken care of.  I'm using the
Shunting yard algorithm to resolve the parser's output for the first
rule in above grammar.

[1]: https://en.wikipedia.org/wiki/Shunting_yard_algorithm

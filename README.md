
An implementation of [Edsger Dijkstra's ‘Shunting yard algorithm’][1].

An improvement is the fusing of the operator stack with the operand
stack, which makes the compiler prove that there is always the same
amount of items on both of them (at a carefully chosen point in time
of execution).

The input type guarantees a sequence of operands correctly interleaved
with operators.  Note that we do not handle parenthesis, they cannot
occur in the use case I'm interested in.

[1]: https://en.wikipedia.org/wiki/Shunting_yard_algorithm

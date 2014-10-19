## HLF

HLF is an implementation of LF ala [Twelf][twelf]. It is implemented
as a DSL within Haskell because I hate parsing.

At the moment HLF doesn't have poor error handling, it has no error
handling. HLF will simply accept or reject your program. It's your job
to figure out why. ♥

For some examples of how to actually use this look at
`examples/`. Notably this includes the code for the typing judgments
of simply typed lambda calculus and natural number examples.

HLF doesn't have the logic programming half of Twelf yet. This
probably won't happen until after my midterms our done and I can hack
on this guilt free.

[twelf]: http://twelf.org

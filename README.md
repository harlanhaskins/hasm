# VM.hs
A polymorphic virtual machine module in Haskell

It supports a minimal subset of assembly instructions using indexed register access.

For example:

```haskell
> let cpu = fromLists [1..3] [1..3]
> cpu
CPU {counter = 0, registers = [Register {value = 1},Register {value = 2},Register {value = 3}], memory = [Register {value = 1},Register {value = 2},Register {value = 3}]}
-- add register 1 and 0, and store the result in 1.
> add 1 0 1 cpu
CPU {counter = 1, registers = [Register {value = 1},Register {value = 3},Register {value = 3}], memory = [Register {value = 1},Register {value = 2},Register {value = 3}]}
> run [Add 0 1 1, Mov 0 2, Load 2 1, Mul 2 1 0] cpu
CPU {counter = 4, registers = [Register {value = 3},Register {value = 3},Register {value = 1}], memory = [Register {value = 1},Register {value = 2},Register {value = 3}]}
```

It includes a small DSL for instructions, and will eventually have a parser for a custom configuration and assembly language.

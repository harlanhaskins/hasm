# hasm
An Assembly-like programming language and interpreter in Haskell

It supports a minimal subset of assembly instructions using indexed register access.
The recognized instructions are:

```haskell
nop

-- register storage
mov dsr r1

-- arithmetic
add dst r1 r2
sub dst r1 r2
mul dst r1 r2
div dst r1 r2
mod dst r1 r2

-- bitwise operations
xor dst r1 r2
or dst r1 r2
and dst r1 r2
sll dst r1 r2
srl dst r1 r2

-- branching
bne r1 r2 inst
beq r1 r2 inst
bge r1 r2 inst
ble r1 r2 inst
blt r1 r2 inst
bgt r1 r2 inst
bgtz r1 inst
bgez r1 inst
bltz r1 inst
blez r1 inst
beqz r1 inst
bnez r1 inst

-- control flow
jmp inst
```

For example, the fibonnacci sequence:

```haskell
hasm fib.hasm 0 0 0 0 5
Current CPU:
CPU 0  [r0: 0]  [r1: 0]  [r2: 0]  [r3: 0]  [r4: 10]
Mov (Reg 0) (Val 1)
Mov (Reg 1) (Val 1)
Mov (Reg 3) (Val 2)
Mov (Reg 2) (Reg 1)
Add (Reg 1) (Reg 1) (Reg 0)
Mov (Reg 0) (Reg 2)
Add (Reg 3) (Reg 3) (Val 1)
Blt (Reg 3) (Reg 4) (Addr 3)

Mov (Reg 0) (Val 1)
CPU 1  [r0: 1]  [r1: 0]  [r2: 0]  [r3: 0]  [r4: 10]
Mov (Reg 1) (Val 1)
CPU 2  [r0: 1]  [r1: 1]  [r2: 0]  [r3: 0]  [r4: 10]
Mov (Reg 3) (Val 2)
CPU 3  [r0: 1]  [r1: 1]  [r2: 0]  [r3: 2]  [r4: 10]
Mov (Reg 2) (Reg 1)
CPU 4  [r0: 1]  [r1: 1]  [r2: 1]  [r3: 2]  [r4: 10]
Add (Reg 1) (Reg 1) (Reg 0)
CPU 5  [r0: 1]  [r1: 2]  [r2: 1]  [r3: 2]  [r4: 10]
Mov (Reg 0) (Reg 2)
CPU 6  [r0: 1]  [r1: 2]  [r2: 1]  [r3: 2]  [r4: 10]
Add (Reg 3) (Reg 3) (Val 1)
CPU 7  [r0: 1]  [r1: 2]  [r2: 1]  [r3: 3]  [r4: 10]
Blt (Reg 3) (Reg 4) (Addr 3)
CPU 3  [r0: 1]  [r1: 2]  [r2: 1]  [r3: 3]  [r4: 10]
Mov (Reg 2) (Reg 1)
CPU 4  [r0: 1]  [r1: 2]  [r2: 2]  [r3: 3]  [r4: 10]
Add (Reg 1) (Reg 1) (Reg 0)
CPU 5  [r0: 1]  [r1: 3]  [r2: 2]  [r3: 3]  [r4: 10]
Mov (Reg 0) (Reg 2)
CPU 6  [r0: 2]  [r1: 3]  [r2: 2]  [r3: 3]  [r4: 10]
Add (Reg 3) (Reg 3) (Val 1)
CPU 7  [r0: 2]  [r1: 3]  [r2: 2]  [r3: 4]  [r4: 10]
Blt (Reg 3) (Reg 4) (Addr 3)
CPU 3  [r0: 2]  [r1: 3]  [r2: 2]  [r3: 4]  [r4: 10]
Mov (Reg 2) (Reg 1)
CPU 4  [r0: 2]  [r1: 3]  [r2: 3]  [r3: 4]  [r4: 10]
Add (Reg 1) (Reg 1) (Reg 0)
CPU 5  [r0: 2]  [r1: 5]  [r2: 3]  [r3: 4]  [r4: 10]
Mov (Reg 0) (Reg 2)
CPU 6  [r0: 3]  [r1: 5]  [r2: 3]  [r3: 4]  [r4: 10]
Add (Reg 3) (Reg 3) (Val 1)
CPU 7  [r0: 3]  [r1: 5]  [r2: 3]  [r3: 5]  [r4: 10]
Blt (Reg 3) (Reg 4) (Addr 3)
CPU 3  [r0: 3]  [r1: 5]  [r2: 3]  [r3: 5]  [r4: 10]
Mov (Reg 2) (Reg 1)
CPU 4  [r0: 3]  [r1: 5]  [r2: 5]  [r3: 5]  [r4: 10]
Add (Reg 1) (Reg 1) (Reg 0)
CPU 5  [r0: 3]  [r1: 8]  [r2: 5]  [r3: 5]  [r4: 10]
Mov (Reg 0) (Reg 2)
CPU 6  [r0: 5]  [r1: 8]  [r2: 5]  [r3: 5]  [r4: 10]
Add (Reg 3) (Reg 3) (Val 1)
CPU 7  [r0: 5]  [r1: 8]  [r2: 5]  [r3: 6]  [r4: 10]
Blt (Reg 3) (Reg 4) (Addr 3)
CPU 3  [r0: 5]  [r1: 8]  [r2: 5]  [r3: 6]  [r4: 10]
Mov (Reg 2) (Reg 1)
CPU 4  [r0: 5]  [r1: 8]  [r2: 8]  [r3: 6]  [r4: 10]
Add (Reg 1) (Reg 1) (Reg 0)
CPU 5  [r0: 5]  [r1: 13]  [r2: 8]  [r3: 6]  [r4: 10]
Mov (Reg 0) (Reg 2)
CPU 6  [r0: 8]  [r1: 13]  [r2: 8]  [r3: 6]  [r4: 10]
Add (Reg 3) (Reg 3) (Val 1)
CPU 7  [r0: 8]  [r1: 13]  [r2: 8]  [r3: 7]  [r4: 10]
Blt (Reg 3) (Reg 4) (Addr 3)
CPU 3  [r0: 8]  [r1: 13]  [r2: 8]  [r3: 7]  [r4: 10]
Mov (Reg 2) (Reg 1)
CPU 4  [r0: 8]  [r1: 13]  [r2: 13]  [r3: 7]  [r4: 10]
Add (Reg 1) (Reg 1) (Reg 0)
CPU 5  [r0: 8]  [r1: 21]  [r2: 13]  [r3: 7]  [r4: 10]
Mov (Reg 0) (Reg 2)
CPU 6  [r0: 13]  [r1: 21]  [r2: 13]  [r3: 7]  [r4: 10]
Add (Reg 3) (Reg 3) (Val 1)
CPU 7  [r0: 13]  [r1: 21]  [r2: 13]  [r3: 8]  [r4: 10]
Blt (Reg 3) (Reg 4) (Addr 3)
CPU 3  [r0: 13]  [r1: 21]  [r2: 13]  [r3: 8]  [r4: 10]
Mov (Reg 2) (Reg 1)
CPU 4  [r0: 13]  [r1: 21]  [r2: 21]  [r3: 8]  [r4: 10]
Add (Reg 1) (Reg 1) (Reg 0)
CPU 5  [r0: 13]  [r1: 34]  [r2: 21]  [r3: 8]  [r4: 10]
Mov (Reg 0) (Reg 2)
CPU 6  [r0: 21]  [r1: 34]  [r2: 21]  [r3: 8]  [r4: 10]
Add (Reg 3) (Reg 3) (Val 1)
CPU 7  [r0: 21]  [r1: 34]  [r2: 21]  [r3: 9]  [r4: 10]
Blt (Reg 3) (Reg 4) (Addr 3)
CPU 3  [r0: 21]  [r1: 34]  [r2: 21]  [r3: 9]  [r4: 10]
Mov (Reg 2) (Reg 1)
CPU 4  [r0: 21]  [r1: 34]  [r2: 34]  [r3: 9]  [r4: 10]
Add (Reg 1) (Reg 1) (Reg 0)
CPU 5  [r0: 21]  [r1: 55]  [r2: 34]  [r3: 9]  [r4: 10]
Mov (Reg 0) (Reg 2)
CPU 6  [r0: 34]  [r1: 55]  [r2: 34]  [r3: 9]  [r4: 10]
Add (Reg 3) (Reg 3) (Val 1)
CPU 7  [r0: 34]  [r1: 55]  [r2: 34]  [r3: 10]  [r4: 10]
Blt (Reg 3) (Reg 4) (Addr 3)
CPU 8  [r0: 34]  [r1: 55]  [r2: 34]  [r3: 10]  [r4: 10]
CPU 8  [r0: 34]  [r1: 55]  [r2: 34]  [r3: 10]  [r4: 10]
```

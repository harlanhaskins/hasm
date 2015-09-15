# hasm
An Assembly-like programming language and interpreter in Haskell

It supports a minimal subset of assembly instructions using indexed register access.
The recognized instructions are:

```asm
nop

; register storage
mov dst r1

; arithmetic
add dst r1 r2
sub dst r1 r2
mul dst r1 r2
div dst r1 r2
mod dst r1 r2
inc dst
dec dst

; bitwise operations
xor dst r1 r2
or dst r1 r2
and dst r1 r2
sll dst r1 r2
srl dst r1 r2

; branching
bne r1 r2 label
beq r1 r2 label
bge r1 r2 label
ble r1 r2 label
blt r1 r2 label
bgt r1 r2 label
bgtz r1 label
bgez r1 label
bltz r1 label
blez r1 label
beqz r1 label
bnez r1 label

; control flow
jmp label
```

For example, the fibonnacci sequence:

```haskell
hasm fib.hasm 0 0 0 0 5
Current CPU:
CPU 0  [r0: 0]  [r1: 0]  [r2: 0]  [r3: 0]  [r4: 5]
Mov (Reg 0) (Val 1)
Mov (Reg 1) (Val 1)
Mov (Reg 3) (Val 2)
Mov (Reg 2) (Reg 1)
Add (Reg 1) (Reg 1) (Reg 0)
Mov (Reg 0) (Reg 2)
Add (Reg 3) (Reg 3) (Val 1)
Blt (Reg 3) (Reg 4) (Addr 3)

Final CPU:
CPU 8  [r0: 3]  [r1: 5]  [r2: 3]  [r3: 5]  [r4: 5]
```

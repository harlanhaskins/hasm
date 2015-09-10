# VM.hs
A polymorphic virtual machine module in Haskell


It supports a minimal subset of assembly instructions using indexed register access.
The recognized instructions are:

```haskell
nop

-- register storage
mov r1 r2

-- arithmetic
add r1 r2 dst
sub r1 r2 dst
mul r1 r2 dst
div r1 r2 dst

-- bitwise operations
xor r1 r2 dst
or r1 r2 dst
and r1 r2 dst

-- conditional logic
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
jmp inst
```

For example, the fibonnacci sequence:

```haskell
./Hasm test.hasm 0 0 0 0 3
Current CPU:
CPU 0 [0,0,0,0,5]
Mov (Reg 0) (Val 1)
Mov (Reg 1) (Val 1)
Mov (Reg 3) (Val 2)
Mov (Reg 2) (Reg 1)
Add (Reg 1) (Reg 1) (Reg 0)
Mov (Reg 0) (Reg 2)
Add (Reg 3) (Reg 3) (Val 1)
Blt (Reg 3) (Reg 4) (Addr 3)

Mov (Reg 0) (Val 1)
CPU 1 [1,0,0,0,5]
Mov (Reg 1) (Val 1)
CPU 2 [1,1,0,0,5]
Mov (Reg 3) (Val 2)
CPU 3 [1,1,0,2,5]
Mov (Reg 2) (Reg 1)
CPU 4 [1,1,1,2,5]
Add (Reg 1) (Reg 1) (Reg 0)
CPU 5 [1,2,1,2,5]
Mov (Reg 0) (Reg 2)
CPU 6 [1,2,1,2,5]
Add (Reg 3) (Reg 3) (Val 1)
CPU 7 [1,2,1,3,5]
Blt (Reg 3) (Reg 4) (Addr 3)
CPU 3 [1,2,1,3,5]
Mov (Reg 2) (Reg 1)
CPU 4 [1,2,2,3,5]
Add (Reg 1) (Reg 1) (Reg 0)
CPU 5 [1,3,2,3,5]
Mov (Reg 0) (Reg 2)
CPU 6 [2,3,2,3,5]
Add (Reg 3) (Reg 3) (Val 1)
CPU 7 [2,3,2,4,5]
Blt (Reg 3) (Reg 4) (Addr 3)
CPU 3 [2,3,2,4,5]
Mov (Reg 2) (Reg 1)
CPU 4 [2,3,3,4,5]
Add (Reg 1) (Reg 1) (Reg 0)
CPU 5 [2,5,3,4,5]
Mov (Reg 0) (Reg 2)
CPU 6 [3,5,3,4,5]
Add (Reg 3) (Reg 3) (Val 1)
CPU 7 [3,5,3,5,5]
Blt (Reg 3) (Reg 4) (Addr 3)
CPU 8 [3,5,3,5,5]
CPU 8 [3,5,3,5,5]
```


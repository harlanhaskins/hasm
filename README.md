# hasm
An Assembly-like programming language and interpreter in Haskell

It supports a minimal subset of assembly instructions using indexed register access.
The recognized instructions are:

```asm
nop

; register storage
mov dst r1
ld dst reg
str dst reg
push reg
pop dst

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
jr reg
call label
ret
```

For example, the fibonnacci sequence:

```haskell
hasm fib.hasm 0 0 0 0 5
Starting CPU:
    r0:     10  r1:     0   r2:     0   r3:     0
    r4:     0   r5:     0   r6:     0   r7:     0
    r8:     0   r9:     0   r10:    0   r11:    0
    r12:    0   r13:    0   r14:    0   r15:    0
    r16:    0   r17:    0   r18:    0   r19:    0
    r20:    0   r21:    0   r22:    0   r23:    0
    r24:    0   r25:    0   r26:    0   r27:    0
    r28:    0   r29:    0   r30:    0   r31:    0

Mov (Reg 1) (Val 1)
Mov (Reg 2) (Val 1)
Beq (Reg 0) (Val 2) (Addr 8)
Mov (Reg 3) (Reg 2)
Add (Reg 2) (Reg 2) (Reg 1)
Mov (Reg 1) (Reg 3)
Sub (Reg 0) (Reg 0) (Val 1)
Jmp (Addr 2)

Final CPU:
    r0:     2   r1:     34  r2:     55  r3:     34
    r4:     0   r5:     0   r6:     0   r7:     0
    r8:     0   r9:     0   r10:    0   r11:    0
    r12:    0   r13:    0   r14:    0   r15:    0
    r16:    0   r17:    0   r18:    0   r19:    0
    r20:    0   r21:    0   r22:    0   r23:    0
    r24:    0   r25:    0   r26:    0   r27:    0
    r28:    0   r29:    0   r30:    0   r31:    0
```

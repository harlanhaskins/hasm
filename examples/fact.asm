            jmp start
fact:
            mov v0 1
mulloop:
            mul v0 v0 a0
            dec a0
            bgtz a0 mulloop
            ret
start:
            mov a0 r0
            call fact
            mov a0 v0
            syscall 3

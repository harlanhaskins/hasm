            jmp start
mod_loop:
            mov v0 a0               ; Put the `val` in the return address.
            blt a0 a1 mod_loop_done ; If the `val` is less than the `divisor`, skip the loop.
            sub a0 a0 a1            ; Subtract the divisor from the value and loop.
            jmp mod_loop
mod_loop_done:
            ret
start:
            mov a0 r0
            mov a1 r1
            call mod_loop
            mov a0 v0
            syscall 3

mod_loop:
            mov r2 r0           ; Put the `val` in the return address.
            blt r0 r1 done      ; If the `val` is less than the `divisor`, skip the loop.
            sub r0 r0 r1        ; Subtract the divisor from the value and loop.
            jmp mod_loop
done:

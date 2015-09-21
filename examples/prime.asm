0
                    jmp start
prime_check:
                    mod r12 r11 2               ; Always check for divisible by 2.
                    beqz r12 is_divisible       ; If it is, we're done.
                    mov r10 3                   ; Mov 3 into the current number to check
prime_loop:
                    mov r13 1                   ; Assume the number is prime until we find a divisor
                    bge r10 r11 prime_check_end ; If we're checking a number for divisibilty with itself, we're dealing with a prime.
                    mod r12 r11 r10             ; Get the mod of the two
                    beqz r12 is_divisible       ; If the mod is 0, jump to the true case.
is_not_divisible:
                    mov r20 r10
                    add r10 2
                    jmp prime_loop
is_divisible:
                    mov r13 0                   ; If we found a divisor, the number is not prime, so write a 0 in the return register.
prime_check_end:
                    ret
start:
                    mov r11 r0                  ; Mov r0 into the argument register
                    call prime_check            ; Run the subroutine
                    mov r0 r13                  ; Put the return value in r0

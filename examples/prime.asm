                    jmp start
prime_check:
                    mod t0 a0 2                ; Always check for divisible by 2.
                    beqz t0 is_divisible       ; If it is, we're done.
                    mov s0 3                   ; Mov 3 into the current number to check
prime_loop:
                    mov v0 1                   ; Assume the number is prime until we find a divisor
                    bge s0 a0 prime_check_end  ; If we're checking a number for divisibilty with itself, we're dealing with a prime.
                    mod t0 a0 s0               ; Get the mod of the two
                    beqz t0 is_divisible       ; If the mod is 0, jump to the true case.
is_not_divisible:
                    add s0 s0 2
                    jmp prime_loop
is_divisible:
                    mov v0 0                   ; If we found a divisor, the number is not prime, so write a 0 in the return register.
prime_check_end:
                    ret
start:
                    mov a0 r0                  ; Mov r0 into the argument register
                    call prime_check           ; Run the subroutine
                    mov a0 v0                  ; Put the return value in a0
                    syscall 3                  ; Perform the 'write int' syscall

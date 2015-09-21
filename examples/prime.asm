0

                    jmp start
prime_check:
                    mov r13 1                   ; Assume the number is prime until we find a divisor
                    beq r10 r11 prime_check_end ; If we're checking a number for divisibilty with itself, we're dealing with a prime.
                    mod r12 r11 r10             ; Get the mod of the two
                    beqz r12 is_divisible       ; If the mod is 0, jump to the true case.
is_not_divisible:
                    inc r10                     ; Otherwise, increment the test number
                    jmp prime_check             ; And jump back to the beginning.
is_divisible:
                    mov r13 0                   ; If we found a divisor, the number is not prime, so write a 0 in the return register.
prime_check_end:
                    ret

start:
                    mov r11 r0                  ; Mov r0 into the argument register
                    mov r10 2                   ; Mov 2 into the current number to check (Don't start with 1 because (n % 1 == 0)
                    call prime_check            ; Run the subroutine
                    mov r0 r13                  ; Put the return value in r0

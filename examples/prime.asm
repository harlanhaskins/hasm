0

                jmp start
prime_check:
                mov r13 1
                beq r10 r11 prime_check_end
                mod r12 r11 r10
                beqz r12 is_divisible
is_not_divisible:
                inc r10
                jmp prime_check
is_divisible:
                mov r13 0
prime_check_end:
                ret

start:
                mov r11 r0
                mov r10 2
                call prime_check
                mov r0 r13

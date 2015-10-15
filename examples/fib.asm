            mov r1 1
            mov r2 1
fib_start:
            beq r0 2 done                   ; Stop the counter once we hit our destination number
            mov r3 r2                       ; Store r1 in r2 for a temp swap space
            add r2 r2 r1
            mov r1 r3                       ; Move the previous number to r1
            dec r0                          ; Decrement the counter for which number we're on
            jmp fib_start
done:

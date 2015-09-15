0

        mov r0 10               ; move data into registers
        mov r1 20
        call add2nums            ; push address of next instruction
        jmp end                 ; exit with return code 0
add2nums:
        add r1 r0 r1        ; Add two numbers
        ret                     ; Return result in EAX
end:                            ; end of source code

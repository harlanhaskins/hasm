10

    mov r0 4
loop:
    str r0 r1
    inc r1
    ble r1 9 loop
end:
    dec r1
    ld r2 r1

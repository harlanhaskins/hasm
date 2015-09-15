0

mov r2 1
loop:
        mul r2 r2 r0
        dec r1
        bgtz r1 loop

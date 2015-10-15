            mov r1 1
mulloop:
            mul r1 r1 r0
            dec r0
            bgtz r0 mulloop

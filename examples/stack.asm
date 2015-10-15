                mov s0 5
stack_loop:
                bltz s0 end
                dec s0
                push a0
                inc a0
                jmp stack_loop
end:
                pop r0
                pop r0
                pop r0
                pop r0
                pop r0

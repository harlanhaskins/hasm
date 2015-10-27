                mov s0 5
stack_loop:
                blez s0 end
                dec s0
                push a0
                inc a0
                jmp stack_loop
end:
                pop a0
                syscall 3
                pop a0
                syscall 3
                pop a0
                syscall 3
                pop a0
                syscall 3
                pop a0
                syscall 3

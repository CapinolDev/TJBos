[BITS 32]
global memcpy
global memset
global load_idt_asm
global outb
global inb
global memcmp

section .text

memcpy:
    push ebp
    mov ebp, esp
    push edi
    push esi
    mov edi, [ebp + 8]   
    mov esi, [ebp + 12]  
    mov ecx, [ebp + 16]  
    rep movsb
    mov eax, [ebp + 8]
    pop esi
    pop edi
    pop ebp
    ret


memset:
    push ebp
    mov ebp, esp
    push edi
    
    mov edi, [ebp + 8]   
    mov eax, [ebp + 12]  
    mov ecx, [ebp + 16]  
    
    rep stosb            
    
    mov eax, [ebp + 8]
    pop edi
    pop ebp
    ret

memcmp:
    push ebp
    mov ebp, esp
    push esi
    push edi

    mov edi, [ebp + 8]   
    mov esi, [ebp + 12]  
    mov ecx, [ebp + 16]  

    xor eax, eax         
    repe cmpsb          
    je .doneMem

    
    mov eax, 1
    jg .doneMem
    mov eax, -1

.doneMem:
    pop edi
    pop esi
    pop ebp
    ret

global _gfortran_compare_string
_gfortran_compare_string:
    push ebp
    mov ebp, esp
    push edi
    push esi
    push ebx

    mov ecx, [ebp + 8]   
    mov edi, [ebp + 12]
    mov edx, [ebp + 16]
    mov esi, [ebp + 20]

    
    mov ebx, ecx
    cmp ebx, edx
    jle .do_compare
    mov ebx, edx   

.do_compare:
    test ebx, ebx
    jz .check_lengths
    repe cmpsb     
    jne .not_equal

.check_lengths:

    mov eax, ecx
    sub eax, edx
    jmp .done

.not_equal:

    movzx eax, byte [edi - 1]
    movzx ebx, byte [esi - 1]
    sub eax, ebx

.done:
    pop ebx
    pop esi
    pop edi
    pop ebp
    ret
    
load_idt_asm:
    mov eax, [esp + 4]
    lidt [eax]
    sti
    ret

outb:
    mov dx, [esp + 4]
    mov al, [esp + 8]
    out dx, al
    ret

inb:
    mov dx, [esp + 4]
    xor eax, eax
    in al, dx
    ret


global keyboard_handler_stub
extern keyboard_handler_fortran
keyboard_handler_stub:
    pushad                
    cld                   

    mov ebp, esp          
    and esp, 0xFFFFFFF0   
    sub esp, 4            
    
    call keyboard_handler_fortran
    
    mov esp, ebp          
    
    mov al, 0x20          
    out 0x20, al
    
    popad                 
    iretd                 

[BITS 32]
global memcpy
global memset

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

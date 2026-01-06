[BITS 32]
; Export symbols so gfortran/ld can find them
global memcpy, memset, memcmp, memmove
global load_idt_asm, outb, inb
global keyboard_handler_stub
global read_sector, cpu_halt

section .text

; --- MEMORY UTILITIES ---

; void* memcpy(void* dest, const void* src, size_t n)
memcpy:
    push ebp
    mov ebp, esp
    push edi
    push esi
    mov edi, [ebp + 8]    ; Argument 1: Destination address
    mov esi, [ebp + 12]   ; Argument 2: Source address
    mov ecx, [ebp + 16]   ; Argument 3: Number of bytes
    rep movsb             ; Copy ECX bytes from ESI to EDI
    mov eax, [ebp + 8]    ; Return dest
    pop esi
    pop edi
    pop ebp
    ret

; void* memset(void* s, int c, size_t n)
memset:
    push ebp
    mov ebp, esp
    push edi
    mov edi, [ebp + 8]    ; Destination address
    mov eax, [ebp + 12]   ; Value to set (lower 8 bits used)
    mov ecx, [ebp + 16]   ; Number of bytes
    rep stosb             ; Fill ECX bytes at EDI with AL
    mov eax, [ebp + 8]
    pop edi
    pop ebp
    ret

; int memcmp(const void* s1, const void* s2, size_t n)
memcmp:
    push ebp
    mov ebp, esp
    push esi
    push edi
    mov edi, [ebp + 8]    ; String 1
    mov esi, [ebp + 12]   ; String 2
    mov ecx, [ebp + 16]   ; Length
    xor eax, eax          ; Default return 0 (equal)
    repe cmpsb            ; Compare bytes while equal
    je .doneMem
    mov eax, 1            ; Not equal
    jg .doneMem
    mov eax, -1
.doneMem:
    pop edi
    pop esi
    pop ebp
    ret

; --- FORTRAN INTERNAL HELPER ---
; gfortran calls this specifically for string comparisons
global _gfortran_compare_string
_gfortran_compare_string:
    push ebp
    mov ebp, esp
    push edi
    push esi
    push ebx
    mov ecx, [ebp + 8]    ; len1
    mov edi, [ebp + 12]   ; str1
    mov edx, [ebp + 16]   ; len2
    mov esi, [ebp + 20]   ; str2

    mov ebx, ecx
    cmp ebx, edx          ; Find the minimum length
    jle .do_compare
    mov ebx, edx   

.do_compare:
    test ebx, ebx         ; If length 0, check lengths directly
    jz .check_lengths
    repe cmpsb     
    jne .not_equal

.check_lengths:
    mov eax, ecx          ; If prefix is same, difference is in length
    sub eax, edx
    jmp .done

.not_equal:
    movzx eax, byte [edi - 1] ; Return difference of last compared chars
    movzx ebx, byte [esi - 1]
    sub eax, ebx

.done:
    pop ebx
    pop esi
    pop edi
    pop ebp
    ret

; --- HARDWARE I/O ---

; void load_idt_asm(idt_ptr_t* ptr)
load_idt_asm:
    mov eax, [esp + 4]    ; Get pointer to IDT descriptor
    lidt [eax]            ; Load Interrupt Descriptor Table
    sti                   ; Enable hardware interrupts
    ret

; void outb(uint16_t port, uint8_t val)
outb:
    mov dx, [esp + 4]     ; Port address
    mov al, [esp + 8]     ; Value to send
    out dx, al            ; Write byte to I/O port
    ret

; uint8_t inb(uint16_t port)
inb:
    mov dx, [esp + 4]     ; Port address
    xor eax, eax
    in al, dx             ; Read byte from I/O port
    ret

; --- INTERRUPT HANDLING ---

extern keyboard_handler_fortran
keyboard_handler_stub:
    pushad                ; Save all general-purpose registers
    cld                   ; Clear direction flag (standard for C/Fortran)

    mov ebp, esp          ; Save stack pointer
    and esp, 0xFFFFFFF0   ; Align stack to 16-bytes for Fortran/GCC ABI
    sub esp, 4            ; Padding
    
    call keyboard_handler_fortran
    
    mov esp, ebp          ; Restore stack pointer
    
    mov al, 0x20          ; Send End-of-Interrupt (EOI) to PIC
    out 0x20, al
    
    popad                 ; Restore registers
    iretd                 ; Return from interrupt (32-bit)

; --- DISK I/O (ATA PIO Mode) ---

; void read_sector(uint32_t lba, void* buffer)
read_sector:
    push ebp
    mov ebp, esp
    pushad

    mov eax, [ebp + 8]    ; LBA address
    mov edi, [ebp + 12]   ; Destination buffer

    ; Send drive and bits 24-27 of LBA
    mov ebx, eax
    shr ebx, 24
    or bl, 0xE0           ; Select master drive + LBA mode
    mov dx, 0x1F6
    mov al, bl
    out dx, al

    ; Number of sectors to read (1)
    mov dx, 0x1F2
    mov al, 1
    out dx, al

    ; Send LBA bits 0-7, 8-15, 16-23
    mov dx, 0x1F3
    mov al, byte [ebp + 8]
    out dx, al
    mov dx, 0x1F4
    mov al, byte [ebp + 9]
    out dx, al
    mov dx, 0x1F5
    mov al, byte [ebp + 10]
    out dx, al

    ; Command 0x20: Read with retry
    mov dx, 0x1F7
    mov al, 0x20
    out dx, al

.wait_ready:
    in al, dx             ; Poll Status Register
    test al, 8            ; Wait for DRQ (Data Request) bit to be set
    jz .wait_ready

    ; Read 256 words (512 bytes) from Data Port
    mov ecx, 256
    mov dx, 0x1F0
    rep insw              ; Read from I/O port DX into EDI

    popad
    pop ebp
    ret 

; --- SYSTEM CONTROL ---

; memmove handles overlapping memory regions safely
memmove:
    push ebp
    mov ebp, esp
    push edi
    push esi
    mov edi, [ebp + 8]
    mov esi, [ebp + 12]
    mov ecx, [ebp + 16]
    test ecx, ecx
    jz .done
    cmp edi, esi
    jbe .copy_forward     ; If dest < src, standard forward copy is safe
    
    ; Overlap detected (dest > src), copy backwards
    add edi, ecx
    dec edi
    add esi, ecx
    dec esi
    std                   ; Set Direction Flag (copy backwards)
    rep movsb
    cld                   ; Clear Direction Flag (RESET to forward!)
    jmp .done

.copy_forward:
    cld
    rep movsb
.done:
    mov eax, [ebp + 8]
    pop esi
    pop edi
    pop ebp
    ret

cpu_halt:
    nop                   ; pls no racecunt
    hlt                   ; Halt duh
    ret

[BITS 16]
global start
section .boot

codeOffset equ 0x8
dataOffset equ 0x10

start:
   
    jmp real_start

BOOT_DRIVE db 0

real_start:
    mov [BOOT_DRIVE], dl 
    
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7c00
    sti

    
    mov di, 3
.retry_load:
    
    mov ah, 0x42
    mov dl, [BOOT_DRIVE]
    mov si, disk_packet     
    int 0x13
    jnc load_success  

.retry_dec:

    xor ax, ax
    mov dl, [BOOT_DRIVE]
    int 0x13
    
    dec di
    jnz .retry_load
    jmp disk_error


align 4
disk_packet:
    db 0x10 
    db 0x00 
    dw 30   
    dw 0x0000
    dw 0x1000
    dq 1
load_success:
   
	in al, 0x92          
    or al, 2             
    out 0x92, al
    cli
    lgdt [gdt_descriptor]
    mov eax, cr0
    or eax, 1
    mov cr0, eax
    jmp codeOffset:protmodeMain

disk_error:
    mov ah, 0x0e
    mov al, 'E'
    int 0x10
    jmp $

align 4
gdt_start:
    dq 0
gdt_code:
    dw 0xFFFF, 0x0000
    db 0x00, 10011010b, 11001111b, 0x00
gdt_data:
    dw 0xFFFF, 0x0000
    db 0x00, 10010010b, 11001111b, 0x00
gdt_end:

gdt_descriptor:
    dw gdt_end - gdt_start - 1
    dd gdt_start

[BITS 32]
extern kmain    
protmodeMain:
    mov ax, dataOffset
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov ss, ax
    mov gs, ax
    
    mov esp, 0x90000
    mov ebp, esp
    and esp, 0xFFFFFFF0 
    
    call kmain
    jmp $

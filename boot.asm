[BITS 16]
global start
section .boot
codeOffset equ gdt_code - gdt_start
dataOffset equ gdt_data - gdt_start
start:
    jmp short real_start
    nop
    
    db "MSWIN4.1" 
	dw 512        
	db 1          
	dw 33         
	db 2         
	dw 224       
	dw 2880      
	db 0xF0      
	dw 9         
	dw 18        
	dw 2         
	dd 0         
	dd 0         
	db 0         
	db 0         
	db 0x29      
	dd 0x12345678
	db "TJBOS      " 
	db "FAT12   "    
BOOT_DRIVE db 0

real_start:
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7c00
    sti

    mov [BOOT_DRIVE], dl 


    mov ah, 0x41
    mov bx, 0x55aa
    mov dl, [BOOT_DRIVE]
    int 0x13
    jc legacy_fallback

    mov di, 3
.retry_lba:
    mov ah, 0x42
    mov dl, [BOOT_DRIVE] 
    mov si, disk_packet
    int 0x13
    jnc load_success
    
    dec di
    jnz .retry_lba
    jmp disk_error

legacy_fallback:

    mov ax, 0x1000
    mov es, ax
    xor bx, bx
    mov ah, 0x02
    mov al, 20
    mov ch, 0
    mov dh, 0
    mov cl, 2
    mov dl, [BOOT_DRIVE]
    int 0x13
    jnc load_success
    jmp disk_error

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
    push ax  
    mov ah, 0x0e
    mov al, 'E'
    int 0x10
    
    pop ax     
    mov al, ah 
    shr al, 4      
    call print_hex
    pop ax
    mov al, ah     
    and al, 0x0F
    call print_hex
    jmp $

print_hex:         
    add al, '0'
    cmp al, '9'
    jbe .ok
    add al, 7
.ok:
    mov ah, 0x0e
    int 0x10
    ret

align 4
disk_packet:
    db 0x10          
    db 0x00          
    dw 64       
    dw 0x0000        
    dw 0x1000        
    dq 1             

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

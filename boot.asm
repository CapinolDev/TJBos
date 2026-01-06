[BITS 16]                       ; Tell NASM we are starting in 16-bit Real Mode
global start
section .boot

; --- GDT Offsets ---
codeOffset equ gdt_code - gdt_start    ; Offset 0x08: Points to the code segment
dataOffset equ gdt_data - gdt_start    ; Offset 0x10: Points to the data segment

start:
    jmp short real_start        ; Jump over the BIOS Parameter Block (BPB)
    nop

    ; --- BIOS Parameter Block (FAT12 Header) ---
    ; This is required for the floppy image to be recognized as a valid FAT12 disk
    db "MSWIN4.1"               ; OEM Name
    dw 512                      ; Bytes per sector
    db 1                        ; Sectors per cluster
    dw 64                       ; Reserved sectors (where our kernel lives)
    db 2                        ; Number of FATs
    dw 224                      ; Root directory entries
    dw 2880                     ; Total sectors (1.44MB Floppy)
    db 0xF0                     ; Media descriptor
    dw 9                        ; Sectors per FAT
    dw 18                       ; Sectors per track
    dw 2                        ; Number of heads
    dd 0                        ; Hidden sectors
    dd 0                        ; Large sector count
    db 0                        ; Drive number
    db 0                        ; Reserved
    db 0x29                     ; Signature
    dd 0x12345678               ; Volume ID
    db "TJBOS      "            ; Volume Label (11 bytes)
    db "FAT12   "               ; System ID (8 bytes)

BOOT_DRIVE db 0                 ; Variable to store the boot drive ID provided by BIOS

real_start:
    cli                         ; Disable interrupts during CPU setup
    xor ax, ax                  ; Clear AX
    mov ds, ax                  ; Set Data Segment to 0
    mov es, ax                  ; Set Extra Segment to 0
    mov ss, ax                  ; Set Stack Segment to 0
    mov sp, 0x7c00              ; Set Stack Pointer (grows down from 0x7c00)
    sti                         ; Re-enable interrupts

    mov [BOOT_DRIVE], dl        ; BIOS passes the boot drive ID in DL

    ; --- Check for LBA Support ---
    ; Modern BIOSes support Logical Block Addressing (LBA) instead of CHS
    mov ah, 0x41
    mov bx, 0x55aa
    mov dl, [BOOT_DRIVE]
    int 0x13
    jc legacy_fallback          ; If carry flag set, LBA is not supported

    ; --- Load Kernel via LBA (INT 13h, 42h) ---
    mov di, 3                   ; Retry counter
.retry_lba:
    mov ah, 0x42                ; Extended Read function
    mov dl, [BOOT_DRIVE] 
    mov si, disk_packet         ; Point to the Disk Address Packet
    int 0x13
    jnc load_success            ; Success!
    
    dec di
    jnz .retry_lba              ; Try again if failed
    jmp disk_error

legacy_fallback:
    ; --- Load Kernel via CHS (Legacy) ---
    mov ax, 0x1000              ; Load destination: 0x1000:0x0000
    mov es, ax
    xor bx, bx
    mov ah, 0x02                ; Read sectors function
    mov al, 20                  ; Read 20 sectors
    mov ch, 0                   ; Cylinder 0
    mov dh, 0                   ; Head 0
    mov cl, 2                   ; Start from sector 2 (Sector 1 is the bootloader)
    mov dl, [BOOT_DRIVE]
    int 0x13
    jnc load_success
    jmp disk_error

load_success:
    ; --- Enable A20 Line ---
    ; Unlocks access to memory above 1MB
    in al, 0x92                 
    or al, 2                    
    out 0x92, al

    ; --- Enter 32-bit Protected Mode ---
    cli                         ; Disable interrupts (important!)
    lgdt [gdt_descriptor]       ; Load the Global Descriptor Table
    mov eax, cr0
    or eax, 1                   ; Set bit 0 of CR0 (Protected Mode bit)
    mov cr0, eax
    jmp codeOffset:protmodeMain ; Perform a "Far Jump" to flush the pipeline

disk_error:
    ; Simple error reporter: Prints 'E' followed by the hex error code
    push ax  
    mov ah, 0x0e
    mov al, 'E'
    int 0x10
    
    pop ax     
    mov al, ah 
    shr al, 4                   ; Extract high nibble
    call print_hex
    pop ax
    mov al, ah      
    and al, 0x0F                ; Extract low nibble
    call print_hex
    jmp $                       ; Hang

print_hex:          
    add al, '0'
    cmp al, '9'
    jbe .ok
    add al, 7                   ; Convert to A-F if > 9
.ok:
    mov ah, 0x0e
    int 0x10
    ret

; --- Data Structures ---

align 4
disk_packet:                    ; Disk Address Packet for LBA reading
    db 0x10                     ; Packet size (16 bytes)
    db 0x00                     ; Reserved
    dw 64                       ; Number of sectors to read
    dw 0x0000                   ; Offset of destination buffer
    dw 0x1000                   ; Segment of destination buffer (0x1000:0x0000)
    dq 1                        ; LBA address to start reading (Sector 1)



align 4
gdt_start:                      ; The Global Descriptor Table defines memory access
    dq 0                        ; Null Descriptor (Required)
gdt_code:                       ; Code Segment Descriptor
    dw 0xFFFF, 0x0000           ; Limit 0-15, Base 0-15
    db 0x00, 10011010b, 11001111b, 0x00 ; Access/Flags (Ring 0, Exec/Read)
gdt_data:                       ; Data Segment Descriptor
    dw 0xFFFF, 0x0000           ; Limit 0-15, Base 0-15
    db 0x00, 10010010b, 11001111b, 0x00 ; Access/Flags (Ring 0, Read/Write)
gdt_end:

gdt_descriptor:
    dw gdt_end - gdt_start - 1  ; GDT Size
    dd gdt_start                ; GDT Address

[BITS 32]                       ; Switch to 32-bit code generation
extern kmain                    ; Our Fortran/C entry point
protmodeMain:
    ; Update all segment registers to use the GDT data selector
    mov ax, dataOffset
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov ss, ax
    mov gs, ax
    
    ; --- Setup Stack for Kernel ---
    mov esp, 0x90000            ; Set stack top at 0x90000
    mov ebp, esp
    and esp, 0xFFFFFFF0         ; Align stack to 16 bytes (important for ABI)
    
    call kmain                  ; CALL THE FORTRAN KERNEL!
    jmp $                       ; Hang if kernel ever returns

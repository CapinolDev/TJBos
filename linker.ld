
OUTPUT_FORMAT("elf32-i386")
ENTRY(start)

SECTIONS
{
   
    . = 0x7c00;
    .boot : {
        boot.o(.boot)
        . = 510;
        SHORT(0xAA55)
    }

    
    . = 0x10000;
    .kernel : AT(0x7e00) {
        *(.text.kmain) 
        *(.text .text.*)
        *(.rodata .rodata.*)
        *(.data .data.*)
        *(.bss .bss.*)
        *(COMMON)
    }
}

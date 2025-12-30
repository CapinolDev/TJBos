#!/bin/bash
rm -f *.o *.bin *.elf

nasm -f elf32 boot.asm -o boot.o
nasm -f elf32 lib.asm -o lib.o


FFLAGS="-m32 -c -fno-stack-protector -fno-underscoring -fno-pic -fno-pie -mpreferred-stack-boundary=2"
i686-elf-gfortran $FFLAGS kernel.f90 -o kernel.o


i686-elf-ld -m elf_i386 -T linker.ld -o kernel.elf boot.o kernel.o lib.o


echo "---------------------------------------"
echo "Checking linked addresses..."
i686-elf-nm -n kernel.elf | grep -E "kmain|keyboard_handler_stub|idt$"
echo "---------------------------------------"


i686-elf-objcopy -O binary kernel.elf kernel.bin

if [ -f kernel.bin ]; then
    truncate -s 65536 kernel.bin
    echo "SUCCESS: kernel.bin created."
fi

#!/bin/bash
rm -f *.o *.bin *.elf *.img

nasm -f elf32 boot.asm -o boot.o
nasm -f elf32 lib.asm -o lib.o
FFLAGS="-m32 -c -fno-stack-protector -fno-underscoring -fno-pic -fno-pie -mpreferred-stack-boundary=2"
i686-elf-gfortran $FFLAGS kernel.f90 -o kernel.o

i686-elf-ld -m elf_i386 -T linker.ld -o kernel.elf boot.o kernel.o lib.o
i686-elf-objcopy -O binary kernel.elf kernel.bin

dd if=/dev/zero of=tjbos.img bs=1024 count=1440


dd if=kernel.bin of=tjbos.img conv=notrunc


echo "HELLO FROM FORTRAN OS" > HELLO.TXT
mcopy -i tjbos.img hello.txt ::HELLO.TXT



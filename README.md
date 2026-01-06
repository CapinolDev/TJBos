#TJBos - An Operating System in Fortran

TJBos (TJB Operating System) is an experimental hobby operating system with a kernel written primarily in Fortran 90, complemented by x86 Assembly for low-level operations. It's a minimal bootable OS that runs in 32-bit protected mode, featuring a simple text-based shell, keyboard input, VGA output, basic commands, and FAT12 filesystem support.
## Features
- Custom bootloader (16-bit Assembly) that loads the kernel from a FAT12 floppy image
- Switches to 32-bit protected mode with GDT and IDT setup
- Keyboard interrupt handling (IRQ1) with basic US QWERTY mapping
- VGA text mode output with color support
- **Simple shell with commands:**
	HELP - List available commands
	CLEAR - Clear the screen
	DIR - List files in root directory
	ECHO <text> - Print text
	USERNAME <name> - Set a custom username (up to 10 chars)
	CAT <file> - View file contents (e.g., CAT HELLO.TXT)

- Basic FAT12 filesystem reading (root directory and file loading)
- Includes a sample HELLO.TXT file with a greeting from the OS


## Build Requirements

NASM (for assembling .asm files)
i686-elf-gfortran (cross-compiling GNU Fortran toolchain)
i686-elf-ld and i686-elf-gcc (for linking)
mtools (for mcopy to create floppy image)
QEMU (recommended for testing)

You can install a cross-compiler toolchain following guides like the osdev.org wiki Bare Bones tutorial.

## Project Structure

[boot.asm](boot.asm) - Bootloader
[kernel.f90](kernel.f90) - Main kernel in Fortran
[lib.asm](lib.asm) - Utility functions (memory ops, I/O, interrupts)
[linker.ld](linker.ld) - Linker script
[build.sh](build.sh) - Build script
[tjbos.img](tjbos.img) - Bootable floppy image (generated)

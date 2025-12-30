module kernel_data
    use iso_c_binding
    implicit none

    
    type, bind(c) :: idt_entry
        integer(c_int16_t) :: base_low
        integer(c_int16_t) :: selector
        integer(c_int8_t)  :: zero = 0
        integer(c_int8_t)  :: flags
        integer(c_int16_t) :: base_high
    end type idt_entry

    type, bind(c) :: idtr_t
        integer(c_int8_t) :: data(6)
    end type idtr_t

    type(idt_entry), target, save :: idt(0:255)
    type(idtr_t), save :: idtr
    integer, save :: cursor_pos = 481

    interface
        function inb(port) bind(c, name="inb")
            use iso_c_binding
            integer(c_int), value :: port
            integer(c_int8_t) :: inb
        end function inb
        subroutine outb(port, val) bind(c, name="outb")
            import c_int, c_int8_t
            integer(c_int), value :: port
            integer(c_int8_t), value :: val
        end subroutine
        subroutine load_idt_asm(p) bind(c, name="load_idt_asm")
            import idtr_t
            type(idtr_t) :: p
        end subroutine
        subroutine keyboard_handler_stub() bind(c, name="keyboard_handler_stub")
        end subroutine
    end interface

    integer(kind=c_int8_t), pointer :: vga(:)
    character(len=1), dimension(0:127) :: kbd_map
    character(len=13), parameter :: OS_NAME = "TJBOS FORTRAN"
    character(len=7), parameter :: OS_GREETING = "WELCOME!"

contains

    subroutine init_vga()
        type(c_ptr) :: p
        p = transfer(int(z'B8000', c_intptr_t), c_null_ptr)
        call c_f_pointer(p, vga, [4000])
    end subroutine init_vga

    subroutine clear_vga(attr)
        integer(kind=c_int8_t), value :: attr
        integer :: i
        do i = 1, 4000, 2
            vga(i)   = int(32, c_int8_t)
            vga(i+1) = attr
        end do
    end subroutine clear_vga

    subroutine print_str(row, col, msg, length, attr)
        integer(c_int), value :: row, col, length
        integer(kind=c_int8_t), value :: attr
        character(kind=c_char) :: msg(*)
        integer :: i, pos
        pos = ((row - 1) * 80 + (col - 1)) * 2 + 1
        do i = 1, length
            vga(pos)   = int(ichar(msg(i)), c_int8_t)
            vga(pos+1) = attr
            pos = pos + 2
        end do
    end subroutine print_str

    function keyboard_ready() result(res)
        logical :: res
        res = iand(int(inb(int(z'64'))), 1) == 1
    end function keyboard_ready

    function get_scancode() result(res)
        integer(kind=c_int8_t) :: res
        res = inb(int(z'60'))
    end function get_scancode

    subroutine init_kbd_map()
        integer :: i
        do i = 0, 127 ; kbd_map(i) = char(0) ; end do

        
        kbd_map(int(z'02')) = '1'; kbd_map(int(z'03')) = '2'
        kbd_map(int(z'04')) = '3'; kbd_map(int(z'05')) = '4'
        kbd_map(int(z'06')) = '5'; kbd_map(int(z'07')) = '6'
        kbd_map(int(z'08')) = '7'; kbd_map(int(z'09')) = '8'
        kbd_map(int(z'0A')) = '9'; kbd_map(int(z'0B')) = '0'
        kbd_map(int(z'0C')) = '-'; kbd_map(int(z'0D')) = '='

        ! --- Row 2: QWERTY... ---
        kbd_map(int(z'10')) = 'Q'; kbd_map(int(z'11')) = 'W'
        kbd_map(int(z'12')) = 'E'; kbd_map(int(z'13')) = 'R'
        kbd_map(int(z'14')) = 'T'; kbd_map(int(z'15')) = 'Y'
        kbd_map(int(z'16')) = 'U'; kbd_map(int(z'17')) = 'I'
        kbd_map(int(z'18')) = 'O'; kbd_map(int(z'19')) = 'P'
        kbd_map(int(z'1A')) = '['; kbd_map(int(z'1B')) = ']'

        
        kbd_map(int(z'1E')) = 'A'; kbd_map(int(z'1F')) = 'S'
        kbd_map(int(z'20')) = 'D'; kbd_map(int(z'21')) = 'F'
        kbd_map(int(z'22')) = 'G'; kbd_map(int(z'23')) = 'H'
        kbd_map(int(z'24')) = 'J'; kbd_map(int(z'25')) = 'K'
        kbd_map(int(z'26')) = 'L'; kbd_map(int(z'27')) = ';'
        kbd_map(int(z'28')) = "'"; kbd_map(int(z'29')) = '`'

        
        kbd_map(int(z'2B')) = '\'
        kbd_map(int(z'2C')) = 'Z'; kbd_map(int(z'2D')) = 'X'
        kbd_map(int(z'2E')) = 'C'; kbd_map(int(z'2F')) = 'V'
        kbd_map(int(z'30')) = 'B'; kbd_map(int(z'31')) = 'N'
        kbd_map(int(z'32')) = 'M'; kbd_map(int(z'33')) = ','
        kbd_map(int(z'34')) = '.'; kbd_map(int(z'35')) = '/'

        
        kbd_map(int(z'39')) = ' '  
    end subroutine init_kbd_map

    subroutine init_idt()
        integer(c_intptr_t) :: addr
        integer(c_int16_t) :: limit_val
        integer :: i

        
        do i = 0, 255
            idt(i)%base_low = 0
            idt(i)%selector = 0
            idt(i)%zero = 0
            idt(i)%flags = 0
            idt(i)%base_high = 0
        end do

        
        addr = transfer(c_funloc(keyboard_handler_stub), addr)
        idt(33)%base_low = int(iand(addr, int(z'FFFF', c_intptr_t)), c_int16_t)
        idt(33)%base_high = int(iand(ishft(addr, -16), int(z'FFFF', c_intptr_t)), c_int16_t)
        idt(33)%selector = 8
        idt(33)%flags = int(z'8E', c_int8_t)

        
        call outb(int(z'20', c_int), int(z'11', c_int8_t)) 
        call outb(int(z'A0', c_int), int(z'11', c_int8_t))
        call outb(int(z'21', c_int), int(z'20', c_int8_t)) 
        call outb(int(z'A1', c_int), int(z'28', c_int8_t)) 
        call outb(int(z'21', c_int), int(z'04', c_int8_t))
        call outb(int(z'A1', c_int), int(z'02', c_int8_t))
        call outb(int(z'21', c_int), int(z'01', c_int8_t))
        call outb(int(z'A1', c_int), int(z'01', c_int8_t))
        call outb(int(z'21', c_int), int(z'FD', c_int8_t))

        
        limit_val = int(256 * 8 - 1, c_int16_t)
        addr = transfer(c_loc(idt(0)), addr)
        
        idtr%data(1) = int(iand(limit_val, z'FF'), c_int8_t)
        idtr%data(2) = int(iand(ishft(limit_val, -8), z'FF'), c_int8_t)
        idtr%data(3) = int(iand(addr, z'FF'), c_int8_t)
        idtr%data(4) = int(iand(ishft(addr, -8), z'FF'), c_int8_t)
        idtr%data(5) = int(iand(ishft(addr, -16), z'FF'), c_int8_t)
        idtr%data(6) = int(iand(ishft(addr, -24), z'FF'), c_int8_t)

        call load_idt_asm(idtr)
    end subroutine init_idt
end module kernel_data
subroutine keyboard_handler_fortran() bind(c, name="keyboard_handler_fortran")
    use kernel_data
    implicit none
    integer(kind=c_int8_t) :: scancode
    character(len=1) :: char_out

    
    scancode = inb(int(z'60'))

    
    if (scancode == int(z'0E', c_int8_t)) then
    
        if (cursor_pos > 1) then
            cursor_pos = cursor_pos - 2
            vga(cursor_pos) = int(32, c_int8_t) 
            vga(cursor_pos + 1) = int(z'1F', c_int8_t)
        end if

    else if (scancode == int(z'1C', c_int8_t)) then
    
        cursor_pos = ((cursor_pos / 160) + 1) * 160 + 1

    else if (scancode > 0 .and. scancode < 128) then
    
        char_out = kbd_map(int(scancode))
        if (char_out /= char(0)) then
            vga(cursor_pos) = int(ichar(char_out), c_int8_t)
            vga(cursor_pos + 1) = int(z'1E', c_int8_t)
            cursor_pos = cursor_pos + 2
        end if
    end if

    
end subroutine
subroutine kmain() bind(c, name="kmain")
    use kernel_data
    implicit none
    call init_vga()
    call init_kbd_map()
    call clear_vga(int(z'1F', c_int8_t))
    call print_str(1, 1, OS_NAME, 13, int(z'1A', c_int8_t))
    
    call init_idt() 
    do
        
    end do
end subroutine kmain

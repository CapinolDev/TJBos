module kernel_data
    use iso_c_binding
    implicit none
    interface
        function inb(port) bind(c, name="inb")
            use iso_c_binding
            integer(c_int), value :: port
            integer(c_int8_t) :: inb
        end function inb
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
    
    subroutine print_digit(row, col, digit, attr)
        integer, value :: row, col, digit
        integer(kind=c_int8_t), value :: attr
        integer :: pos
        pos = ((row - 1) * 80 + (col - 1)) * 2 + 1
        vga(pos) = int(48 + digit, c_int8_t)
        vga(pos+1) = attr
    end subroutine print_digit

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
        do i = 0, 127
            kbd_map(i) = char(0) 
        end do
    
        
        kbd_map(int(z'01')) = char(27)
        kbd_map(int(z'02')) = '1'; kbd_map(int(z'03')) = '2'
        kbd_map(int(z'04')) = '3'; kbd_map(int(z'05')) = '4'
        kbd_map(int(z'06')) = '5'; kbd_map(int(z'07')) = '6'
        kbd_map(int(z'08')) = '7'; kbd_map(int(z'09')) = '8'
        kbd_map(int(z'0A')) = '9'; kbd_map(int(z'0B')) = '0'
        kbd_map(int(z'0C')) = '-'; kbd_map(int(z'0D')) = '='
        kbd_map(int(z'0E')) = char(8) 

        
        kbd_map(int(z'0F')) = char(9)
        kbd_map(int(z'10')) = 'Q'; kbd_map(int(z'11')) = 'W'
        kbd_map(int(z'12')) = 'E'; kbd_map(int(z'13')) = 'R'
        kbd_map(int(z'14')) = 'T'; kbd_map(int(z'15')) = 'Y'
        kbd_map(int(z'16')) = 'U'; kbd_map(int(z'17')) = 'I'
        kbd_map(int(z'18')) = 'O'; kbd_map(int(z'19')) = 'P'
        kbd_map(int(z'1A')) = '['; kbd_map(int(z'1B')) = ']'
        kbd_map(int(z'1C')) = char(10) 

    
        kbd_map(int(z'1E')) = 'A'; kbd_map(int(z'1F')) = 'S'
        kbd_map(int(z'20')) = 'D'; kbd_map(int(z'21')) = 'F'
        kbd_map(int(z'22')) = 'G'; kbd_map(int(z'23')) = 'H'
        kbd_map(int(z'24')) = 'J'; kbd_map(int(z'25')) = 'K'
        kbd_map(int(z'26')) = 'L'; kbd_map(int(z'27')) = ';'
        kbd_map(int(z'28')) = "'"; kbd_map(int(z'29')) = '`'

    
        kbd_map(int(z'2B')) = '\'; kbd_map(int(z'2C')) = 'Z'
        kbd_map(int(z'2D')) = 'X'; kbd_map(int(z'2E')) = 'C'
        kbd_map(int(z'2F')) = 'V'; kbd_map(int(z'30')) = 'B'
        kbd_map(int(z'31')) = 'N'; kbd_map(int(z'32')) = 'M'
        kbd_map(int(z'33')) = ','; kbd_map(int(z'34')) = '.'
        kbd_map(int(z'35')) = '/'

    
        kbd_map(int(z'39')) = ' '
    end subroutine init_kbd_map
end module kernel_data

subroutine kmain() bind(c, name="kmain")
	
    use iso_c_binding
    use kernel_data
    implicit none
	integer(kind=c_int8_t) :: scancode
	integer :: i
	integer :: cursor_pos
   
    call init_vga()
	call init_kbd_map()

    call clear_vga(int(z'1F', c_int8_t))


    call print_str(1, 1, OS_NAME, 13, int(z'1A', c_int8_t))
    call print_str(2, 1, OS_GREETING, 7, int(z'1F', c_int8_t))
	
    cursor_pos = 481
	
    do
        if (keyboard_ready()) then
            scancode = get_scancode()
            
            
            if (int(scancode) > 0 .and. int(scancode) < 128) then
                
                select case (int(ichar(kbd_map(int(scancode)))))
                case (8)
                    if (cursor_pos > 481) then
                        cursor_pos = cursor_pos - 2
                        vga(cursor_pos) = int(32, 1)
                        vga(cursor_pos+1) = int(z'1F', 1)
                    end if
                    
                case (10)
                   
                    cursor_pos = cursor_pos + (160 - mod(cursor_pos - 1, 160))

                case (0)
                    

                case default 
                    vga(cursor_pos) = int(ichar(kbd_map(int(scancode))), 1)
                    vga(cursor_pos+1) = int(z'1A', 1)
                    cursor_pos = cursor_pos + 2
                end select

                
                do while(keyboard_ready())
                    i = int(get_scancode())
                end do
            end if
        end if
        
    end do
end subroutine kmain

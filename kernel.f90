module kernel_data
    use iso_c_binding
    implicit none
    integer(kind=c_int8_t), pointer :: vga(:)
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

end module kernel_data

subroutine kmain() bind(c, name="kmain")
	
    use iso_c_binding
    use kernel_data
    implicit none
	integer :: counter, i, j
   
    call init_vga()


    call clear_vga(int(z'1F', c_int8_t))


    call print_str(1, 1, OS_NAME, 13, int(z'1A', c_int8_t))
    call print_str(2, 1, OS_GREETING, 7, int(z'1F', c_int8_t))

	counter = 0
    do
		call print_digit(1, 79, mod(counter, 10), int(z'1F', c_int8_t))
        
        counter = counter + 1
        
        
        do i = 1, 10000
            do j = 1, 10000
                
            end do
        end do
    end do
end subroutine kmain

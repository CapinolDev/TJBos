module kernel_data
    use iso_c_binding
    implicit none
    integer(kind=c_int8_t), pointer :: vga(:)
    character(len=13), parameter :: OS_NAME = "TJBOS FORTRAN"

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

end module kernel_data

subroutine kmain() bind(c, name="kmain")
    use iso_c_binding
    use kernel_data
    implicit none
    
    call init_vga()

    ! Blue background = 0x1F
    call clear_vga(int(z'1F', c_int8_t))

    ! Yellow text = 0x1E
    call print_str(1, 1, OS_NAME, 13, int(z'1E', c_int8_t))

    do
    end do
end subroutine kmain

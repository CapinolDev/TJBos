module kernel_data
    use iso_c_binding
    implicit none

	integer(kind=c_int8_t), target, save :: sector_buffer(512)

	interface
		subroutine read_sector(lba, buffer) bind(c, name="read_sector")
			import c_int, c_int8_t
			integer(c_int), value :: lba
			integer(c_int8_t) :: buffer(512)
		end subroutine
		subroutine cpu_halt() bind(c, name="cpu_halt")
		end subroutine cpu_halt
	end interface
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
	character, save, dimension(10) :: command, param
	integer, save ::  commPos = 1, paramPos = 1
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
    character(len=10), parameter :: OS_GREETING = "WELCOME :3"
    character(len=10) :: OS_OUT_BUFF = "         "
    character(len=4), parameter :: OS_USERNAME_DEFAULT = "NULL"
    character(len=10) :: OS_USERNAME = OS_USERNAME_DEFAULT
    integer, save :: OS_USERNAME_LEN = 4
    integer :: currInput = 0
    character(len=11) :: fat_name
    character(len=11) :: current_disk_name
    ! i have dementia
    integer, parameter :: FAT_START_SECTOR = 64
    integer, parameter :: ROOT_DIR_SECTOR = 82
    integer, parameter :: DATA_START_SECTOR = 96
    

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
    
    subroutine process_command()
        implicit none
        character(len=10) :: cmd_str, prm_str
        integer :: i

    
        do i = 1, 10
            cmd_str(i:i) = command(i)
            prm_str(i:i) = param(i)
        end do

    
        cursor_pos = ((cursor_pos / 160) + 1) * 160 + 1

        if (cmd_str(1:4) == "HELP") then
            call print_str(cursor_pos/160 + 1, 1, "SYSTEM CMDS: HELP, CLEAR, DIR, ECHO, USERNAME", 45, int(z'1B', c_int8_t))
        else if (cmd_str(1:5) == "CLEAR") then
            call clear_vga(int(z'1F', c_int8_t))
            cursor_pos = 1 
            call print_str(1, 1, OS_NAME, 13, int(z'1A', c_int8_t))
            call print_str(2, 1, OS_GREETING, 10, int(z'1E', c_int8_t))
            call print_str(2, 60, "USER", 4, int(z'1E', c_int8_t))
			call print_str(2, 65, OS_USERNAME, OS_USERNAME_LEN, int(z'1F', c_int8_t))
            cursor_pos = 481
        else if (cmd_str(1:3) == "DIR") then
            call list_files()
        else if (cmd_str(1:4)=="ECHO") then
			call print_str(cursor_pos/160 + 1, 1, param, paramPos, int(z'1D', c_int8_t))
		else if (cmd_str(1:4)=="ECHO") then
			call print_str(cursor_pos/160 + 1, 1, param, paramPos, int(z'1D', c_int8_t))
		else if (cmd_str(1:8)=="USERNAME") then
				OS_USERNAME = "          "
				OS_USERNAME_LEN = paramPos
				do i=1,OS_USERNAME_LEN
					OS_USERNAME(i:i) = param(i)
				end do
				call print_str(2, 60, "USER", 4, int(z'1E', c_int8_t))
				call print_str(2, 65, OS_USERNAME, OS_USERNAME_LEN, int(z'1F', c_int8_t))
				call print_str(cursor_pos/160 + 1, 1, "DONE", 4, int(z'1A', c_int8_t))
		else if (cmd_str(1:3) == "CAT") then
			OS_OUT_BUFF = "          "
            if (paramPos > 1) then
				do i = 1, min(paramPos - 1, 10)
                    OS_OUT_BUFF(i:i) = param(i)
                end do
                
                call format_fat_filename(OS_OUT_BUFF, fat_name)
                call view_file(fat_name)
            else
                call print_str(cursor_pos/160 + 1, 1, "USAGE: CAT FILE.TXT", 19, int(z'1C', c_int8_t))
            end if
		
        else
            call print_str(cursor_pos/160 + 1, 1, "HUH?", 4, int(z'1C', c_int8_t))
        end if
    end subroutine
	
	subroutine find_file(filename)
        character(len=11) :: filename
        integer :: i, j, offset
        logical :: match
        
		call read_sector(ROOT_DIR_SECTOR, sector_buffer)
		
        do i = 0, 15
            offset = (i * 32) + 1
            match = .true.
            
            do j = 0, 10
                if (char(int(sector_buffer(offset + j))) /= filename(j+1:j+1)) then
                    match = .false.
                    exit
                end if
            end do

            if (match) then
                call print_str(cursor_pos/160 + 1, 1, "FILE FOUND!", 11, int(z'1A', c_int8_t))
                return
            end if
        end do
       call print_str(cursor_pos/160 + 1, 1, "FILE NOT FOUND", 14, int(z'1C', c_int8_t))
    end subroutine
    
    subroutine load_file_to_buffer(first_cluster, buffer, max_size)
        integer(c_int16_t), value :: first_cluster
        integer(kind=c_int8_t), intent(out) :: buffer(:)
        integer, value :: max_size
        integer(c_int16_t) :: current_cluster
        integer :: lba, i, buf_offset
        
        current_cluster = first_cluster
        buf_offset = 1
        
        do while (current_cluster < int(z'FF8') .and. buf_offset < max_size)
            
            lba = DATA_START_SECTOR + (int(current_cluster) - 2)
            
            call read_sector(lba, sector_buffer)
            
            do i = 1, 512
                if (buf_offset <= max_size) then
                    buffer(buf_offset) = sector_buffer(i)
                    buf_offset = buf_offset + 1
                end if
            end do
            
            current_cluster = get_fat_entry(current_cluster)
        end do
    end subroutine
    subroutine view_file(filename)
        character(len=11) :: filename
        integer :: i, j, offset, start_cluster
        logical :: match
        integer(kind=c_int8_t) :: file_data(512) 
        
		call read_sector(ROOT_DIR_SECTOR, sector_buffer)
        
        do i = 0, 15
            offset = (i * 32) + 1
            if (sector_buffer(offset) == 0) exit
            
            do j = 1, 11
                current_disk_name(j:j) = char(int(sector_buffer(offset + j - 1)))
            end do
			!call debug_compare(current_disk_name, filename)
            if (current_disk_name(1:1) == filename(1:1)) then
                
            match = .true.
            do j = 0, 10
                if (char(int(sector_buffer(offset + j))) /= filename(j+1:j+1)) then
					match = .false.
					exit
				end if
            end do

            if (match) then
               
                start_cluster = iand(int(sector_buffer(offset + 26)), z'FF') + &
                                ishft(iand(int(sector_buffer(offset + 27)), z'FF'), 8)
                
				call load_file_to_buffer(int(start_cluster, c_int16_t), file_data, 512)                
                cursor_pos = ((cursor_pos / 160) + 1) * 160 + 1
                
                do j = 1, 512
                    if (file_data(j) == 0) exit
                    call print_str(cursor_pos/160 + 1, mod(cursor_pos, 160)/2 + 1, &
                                   char(int(file_data(j))), 1, int(z'0F', c_int8_t))
                    cursor_pos = cursor_pos + 2
                end do
                return
            end if
        end if
        end do
        
        call print_str(cursor_pos/160 + 1, 1, "FILE NOT FOUND", 14, int(z'1C', c_int8_t))
    end subroutine
    
    function get_fat_entry(n) result(next_cluster)
        integer(c_int16_t), value :: n
        integer(c_int16_t) :: next_cluster
        integer :: fat_sector, fat_offset
        
        fat_sector = FAT_START_SECTOR + (int(n) * 3 / 2) / 512
        fat_offset = mod((int(n) * 3 / 2), 512)
        
        call read_sector(fat_sector, sector_buffer)
        
        if (mod(n, 2) == 0) then
            next_cluster = iand(int(sector_buffer(fat_offset + 1)), z'FF') + &
                           ishft(iand(int(sector_buffer(fat_offset + 2)), z'0F'), 8)
        else
            next_cluster = ishft(iand(int(sector_buffer(fat_offset + 1)), z'F0'), -4) + &
                           ishft(iand(int(sector_buffer(fat_offset + 2)), z'FF'), 4)
        end if
    end function
    
   subroutine list_files()
        integer :: i, offset, line_count, j
        character(len=11) :: fname
        
        line_count = 0
        
        call read_sector(ROOT_DIR_SECTOR, sector_buffer)
        
        if (sector_buffer(1) == 0) then
             call print_str(cursor_pos/160 + 2, 1, "DEBUG: ROOT DIR EMPTY", 21, int(z'1C', c_int8_t))
        end if
        
        do i = 0, 15
            offset = (i * 32) + 1
            
            if (sector_buffer(offset) == 0) exit 
            if (sector_buffer(offset) == int(z'E5', c_int8_t)) cycle
            
            if (iand(int(sector_buffer(offset + 11)), z'18') /= 0) cycle

            do j = 0, 10
                fname(j+1:j+1) = char(int(sector_buffer(offset + j)))
            end do
            
            call print_str(cursor_pos/160 + 1 + line_count, 1, fname, 11, int(z'1B', c_int8_t))
            line_count = line_count + 1
        end do
    end subroutine
    subroutine format_fat_filename(input, output)
        character(len=10), intent(in) :: input
        character(len=11), intent(out) :: output
        integer :: i, dot_pos, ext_len, char_code
        
        
        output = "           " 
        dot_pos = 0
        
        
        do i = 1, 10
            if (input(i:i) == '.') then
                dot_pos = i
                exit
            end if
        end do

        if (dot_pos > 1) then
            do i = 1, min(8, dot_pos - 1)
                output(i:i) = input(i:i)
            end do
            ext_len = 0
            do i = dot_pos + 1, 10
                char_code = ichar(input(i:i))
                if (char_code <= 32) exit 
                ext_len = ext_len + 1
                if (ext_len <= 3) then
                    output(8 + ext_len : 8 + ext_len) = input(i:i)
                end if
            end do
        else
            do i = 1, min(8, 10)
                char_code = ichar(input(i:i))
                if (char_code <= 32) exit
                output(i:i) = input(i:i)
            end do
        end if
        do i = 1, 11
            char_code = ichar(output(i:i))
            if (char_code >= 97 .and. char_code <= 122) then
                output(i:i) = char(char_code - 32)
            end if
        end do
    end subroutine
    
    
    subroutine print_hex_byte(row, col, val, attr)
        integer, value :: row, col
        integer(c_int8_t), value :: val
        integer(c_int8_t), value :: attr
        character(len=16) :: hex_chars = "0123456789ABCDEF"
        integer :: high, low
        
        high = iand(ishft(int(val), -4), 15) + 1
        low  = iand(int(val), 15) + 1
        
        call print_str(row, col, hex_chars(high:high), 1, attr)
        call print_str(row, col + 1, hex_chars(low:low), 1, attr)
    end subroutine

    subroutine debug_compare(disk_name, search_name)
        character(len=11) :: disk_name, search_name
        integer :: j
        
        call print_str(cursor_pos/160 + 1, 1, "DISK:", 5, int(z'0E', c_int8_t))
        do j = 1, 11
    
            call print_hex_byte(cursor_pos/160 + 1, 7 + (j-1)*3, &
                                int(ichar(disk_name(j:j)), c_int8_t), int(z'0F', c_int8_t))
        end do
        
        cursor_pos = cursor_pos + 160 
        
        call print_str(cursor_pos/160 + 1, 1, "SRCH:", 5, int(z'0D', c_int8_t))
        do j = 1, 11
            
            call print_hex_byte(cursor_pos/160 + 1, 7 + (j-1)*3, &
                                int(ichar(search_name(j:j)), c_int8_t), int(z'0F', c_int8_t))
        end do
        cursor_pos = cursor_pos + 160
    end subroutine
    
end module kernel_data
subroutine keyboard_handler_fortran() bind(c, name="keyboard_handler_fortran")
    use kernel_data
    implicit none
    integer(kind=c_int8_t) :: scancode
    character(len=1) :: char_out

    
    scancode = inb(int(z'60'))

    
    if (scancode == int(z'0E', c_int8_t)) then 
        
       if (currInput == 0) then
			if (commPos > 1) then
				
				commPos = commPos - 1
						   
				command(commPos:commPos) = ' '
				
				cursor_pos = cursor_pos - 2
				vga(cursor_pos) = int(32, c_int8_t) 
				vga(cursor_pos + 1) = int(z'1F', c_int8_t)
				
				
			end if
		else
			if (paramPos > 1) then
				
				paramPos = paramPos - 1
						   
				param(paramPos:paramPos) = ' '
				
				cursor_pos = cursor_pos - 2
				vga(cursor_pos) = int(32, c_int8_t) 
				vga(cursor_pos + 1) = int(z'1F', c_int8_t)
				
				
			end if
		end if
    else if (scancode == int(z'1C', c_int8_t)) then
		call process_command()
		currInput = 0
        commPos = 1
        paramPos = 1
        command = "          "
        cursor_pos = ((cursor_pos / 160) + 1) * 160 + 1

    else if (scancode > 0 .and. scancode < 128) then
		if (scancode == 57) then 
			if (currInput == 0) then
				currInput = 1
				char_out = kbd_map(int(scancode))
				vga(cursor_pos) = int(ichar(char_out), c_int8_t)
				vga(cursor_pos + 1) = int(z'1E', c_int8_t)
				cursor_pos = cursor_pos + 2
				return
			end if
		end if
		char_out = kbd_map(int(scancode))
		if (currInput == 0) then
			if (char_out /= char(0) .and. commPos <= 10) then
				vga(cursor_pos) = int(ichar(char_out), c_int8_t)
				vga(cursor_pos + 1) = int(z'1E', c_int8_t)
				cursor_pos = cursor_pos + 2
					
				command(commPos:commPos) = char_out
				commPos = commPos + 1
			end if
		else 
			if (char_out /= char(0) .and. paramPos <= 10) then
				vga(cursor_pos) = int(ichar(char_out), c_int8_t)
				vga(cursor_pos + 1) = int(z'1E', c_int8_t)
				cursor_pos = cursor_pos + 2
					
				param(paramPos:paramPos) = char_out
				paramPos = paramPos + 1
			end if
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
    call print_str(2, 1, OS_GREETING, 10, int(z'1E', c_int8_t))
    call print_str(2, 60, "USER", 4, int(z'1E', c_int8_t))
    call print_str(2, 65, OS_USERNAME, OS_USERNAME_LEN, int(z'1F', c_int8_t))
    
    call init_idt() 
    do
       call cpu_halt()
    end do
end subroutine kmain

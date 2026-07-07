module hp_version
    implicit none
    ! Build-provenance stamp (see android_build/stamp_exe.sh). A fresh build
    ! carries a 126-char field of '=' between two bars; once a build is approved
    ! it is overwritten IN PLACE (never rebuilt) with the source revision and
    ! build date. A bar-bounded run of 126 '=' cannot occur in real code or
    ! data, so the stamper locates the field unambiguously. VOLATILE stops the
    ! optimiser folding the initialiser away, so -V reads the patched bytes from
    ! memory rather than an inlined copy of the original.
    character(len=128), volatile, save :: hp_stamp = '|' // repeat('=',126) // '|'
end module hp_version

program hp15c
    use calc_state
    use linked_list
    use clib, only: install_exit_handlers
    use hp_version, only: hp_stamp

    implicit none

    integer            :: ios, i
    character(100)     :: buff
    integer            :: blen
    integer            :: argl, argc
    type(llist)        :: tokens
    logical            :: ok
    logical            :: have_expression
    logical            :: show_prompt
    character(len=100) :: msg

    ! Exit cleanly on Ctrl-C / Ctrl-\ instead of dumping core
    call install_exit_handlers()

    ! All calculator state (stack, registers, constants, modes, locale) lives in
    ! the calc_state module; the program owns only the input/token layer.
    call state_init()

    ! Interrogate argument list
    argc = command_argument_count()
    have_expression = .false.
    show_prompt = .true.
    do i=1,argc
        call get_command_argument(i, buff, argl)
        if (buff(1:argl) == '-d') then
            call set_verbosity(1)
            cycle

        else if (buff(1:argl) == '-c') then
            call set_complex_mode(.true.)
            cycle

        else if (buff(1:argl) == '-v') then
            call set_verbose_mode(.true.)
            cycle

        else if (buff(1:argl) == '-np') then
            show_prompt = .false.
            cycle

        else if (buff(1:argl) == '-h') then
            call help
            stop

        else if (buff(1:argl) == '-V') then
            write(*,'(a)') trim(hp_stamp(2:127))
            stop

        end if

        have_expression = .true.

        ! Break the string up into a linked-list of tokens
        call tokenize(buff(1:argl))
        if (is_verbose()) call tokens%print

        ! Interpret each token as a command and apply it.  apply_command is a
        ! module procedure, so passing it here needs no static chain / trampoline
        ! (the reason the flang/Android build now works without an exec stack).
        ok = tokens%iterate(apply_command)

        call report_after_arg()

        ! Tidy
        call tokens%clear

        if (.not. ok) stop

    end do

    if (.not. have_expression) then
        call show_stack()
    end if

    ! Loop until quit
    all :do
        buff = ''
        if (show_prompt) then
            write(6,'(a)',advance='no') ':: '
        end if
        read(5,fmt='(a)',iostat=ios,iomsg=msg) buff
        if (is_iostat_end(ios)) then
            write(6,'(a)') ''        ! Ctrl-D / end of input: quit cleanly, like 'q'
            exit all
        else if (ios /= 0) then
            write(6,'(/a)') 'Command:['//trim(buff)//']'//'; '//msg
            cycle all
        end if

        buff = trim(adjustl(buff))
        blen = len_trim(buff)
        if (blen == 0) cycle all

        ! Tokenize input string
        call tokenize(buff(1:blen))
        ok = tokens%iterate(apply_command)
        if (.not. ok) exit all

        call report_after_line()
    end do all

    call tokens%clear
    stop

contains

    subroutine tokenize(com)
        character(*), intent(in)      :: com
        integer                       :: start, end
        character(len=:), allocatable :: command

        call tokens%clear
        if (len_trim(com) == 0) then
            return
        end if
        start = 1
        ! Ensure there are no leading and trailing spaces
        command = trim(adjustl(com))
        end = index(command,' ')
        end = merge(len(command),end-1,end==0)
        do
            call tokens%append(command(start:end))
            if (end == len(command)) exit
            start = end + nsp(command(end+1:))
            end = index(command(start:),' ') - 1
            end = merge(len(command),end+start-1,end == -1)
        end do
    end subroutine tokenize

    integer function nsp(command)
        implicit none
        character(*), intent(in) :: command
        integer :: i
        do i=1,len(command)
            if (command(i:i) /= ' ') then
                nsp = i
                return
            end if
        end do
        nsp = 0
    end function nsp

end program hp15c

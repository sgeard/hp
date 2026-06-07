module clib
    use iso_c_binding
    
    integer, parameter :: S_ISUID = int(o'4000')   ! set-user-ID bit
    integer, parameter :: S_ISGID = int(o'2000')   ! set-group-ID bit (see below)
    integer, parameter :: S_ISVTX = int(o'1000')   ! sticky bit (see below)

    integer, parameter :: S_IRWXU = int(o'0700')   ! owner has read, write, and execute permission
    integer, parameter :: S_IRUSR = int(o'0400')   ! owner has read permission
    integer, parameter :: S_IWUSR = int(o'0200')   ! owner has write permission
    integer, parameter :: S_IXUSR = int(o'0100')   ! owner has execute permission

    integer, parameter :: S_IRWXG = int(o'0070')   ! group has read, write, and execute permission
    integer, parameter :: S_IRGRP = int(o'0040')   ! group has read permission
    integer, parameter :: S_IWGRP = int(o'0020')   ! group has write permission
    integer, parameter :: S_IXGRP = int(o'0010')   ! group has execute permission

    integer, parameter :: S_IRWXO = int(o'0007')   ! others (not in group) have read, write, and execute permission
    integer, parameter :: S_IROTH = int(o'0004')   ! others have read permission
    integer, parameter :: S_IWOTH = int(o'0002')   ! others have write permission
    integer, parameter :: S_IXOTH = int(o'0001')   ! others have execute permission

    ! POSIX signal numbers (Linux/glibc): Ctrl-C and Ctrl-backslash
    integer(c_int), parameter :: SIGINT  = 2
    integer(c_int), parameter :: SIGQUIT = 3

    interface

        function mkdir(pathname, mode) bind(c)
            import
            integer(c_int) :: mkdir
            character(c_char), intent(in)     :: pathname(*)
            integer(c_int), intent(in), value :: mode
        end function mkdir

        function mkdirat(pathname, mode) bind(c)
            import
            integer(c_int) :: mkdirat
            character(c_char), intent(in)     :: pathname(*)
            integer(c_int), intent(in), value :: mode
        end function mkdirat

        ! void (*signal(int signum, void (*handler)(int)))(int);
        function signal(signum, handler) result(previous) bind(c)
            import
            integer(c_int), value :: signum
            type(c_funptr), value :: handler
            type(c_funptr)        :: previous
        end function signal

        ! ssize_t write(int fd, const void *buf, size_t count);
        function posix_write(fd, buf, count) result(nwritten) bind(c, name='write')
            import
            integer(c_int),    value      :: fd
            character(c_char), intent(in) :: buf(*)
            integer(c_size_t), value      :: count
            integer(c_size_t)             :: nwritten
        end function posix_write

        ! void _exit(int status);  -- async-signal-safe immediate termination
        subroutine c_exit(status) bind(c, name='_exit')
            import
            integer(c_int), value :: status
        end subroutine c_exit

    end interface

contains

    ! Install a graceful-exit handler for the interactive interrupt signals so
    ! Ctrl-C / Ctrl-\ leave cleanly instead of letting the Fortran runtime print
    ! "forrtl: error (79)" and dump core. Useful for any interactive CLI tool.
    subroutine install_exit_handlers()
        implicit none
        type(c_funptr) :: ignored
        ignored = signal(SIGINT,  c_funloc(on_interrupt))
        ignored = signal(SIGQUIT, c_funloc(on_interrupt))
    end subroutine install_exit_handlers

    ! Signal handler. Only write(2) and _exit(2) are used -- both are on the
    ! async-signal-safe list -- so this is safe to run from inside a blocking
    ! read. Emit a newline to stdout (fd 1) so the shell prompt starts on its
    ! own line, then terminate with status 0.
    subroutine on_interrupt(signum) bind(c)
        implicit none
        integer(c_int), value :: signum
        integer(c_size_t)     :: ignored
        ignored = posix_write(1_c_int, c_new_line, 1_c_size_t)
        call c_exit(0_c_int)
    end subroutine on_interrupt

end module clib
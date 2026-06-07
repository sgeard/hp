!! Calculator state and command dispatch.
!!
!! Holds the calculator's mutable state (stack, memory registers, constants,
!! statistics buffers, sequence-entry state and display modes) at module scope,
!! and exposes the command dispatcher `apply_command` as a *module* procedure.
!!
!! This separation exists for a portability reason as much as a structural one.
!! When `apply_command` was an internal procedure of `program hp15c`, passing it
!! as a procedure argument to `llist%iterate` forced gfortran/flang to build a
!! stack trampoline (executable code on the stack carrying the host static
!! chain). Android (W^X / SELinux) forbids an executable stack, so that build
!! could never run there. As a module procedure with its state reached by
!! use-association, no static chain is needed and no trampoline is generated.
!!
!! Rule for anything added here later: dispatch targets stay module procedures;
!! never register an internal procedure as a command action.

module calc_state
    use rpn_stack, rpn_s_init => init
    use amap, only: amap_t
    implicit none
    private

    ! ---- hoisted calculator state (module-private) --------------------------
    ! A stack of size 5 (x,y,z,s,t). The flang/Android build uses the
    ! non-parameterised stack_t; ifx/gfortran use the PDT form.
#ifdef NO_PDT
    type(stack_t)        :: stack
#else
    type(stack_t(5))     :: stack
#endif
    type(rpn_t)          :: mem(0:9) = rpn_t()
    type(amap_t)         :: constants
    type(amap_t)         :: stats

    ! Sequence entry: in_sequence is 0 (none), 1 (collecting), 2 (just closed).
    integer              :: in_sequence = 0
    integer              :: n_seq = 0
    logical              :: seq_is_x
    logical              :: tmp_cmode
    real(8), allocatable :: x_seq(:), y_seq(:)

    ! Display / diagnostic modes
    logical              :: veMode = .false.
    integer              :: verbosity = 0

    ! ---- public API: the only surface the driver sees -----------------------
    public :: state_init        !! Populate constants, stack legend, default modes, locale
    public :: apply_command     !! Dispatch one command token (matches llist command_fun)
    public :: help              !! Print the help text
    public :: show_stack        !! Print the stack honouring verbose mode
    public :: report_after_arg  !! End-of-expression reporting (command-line path)
    public :: report_after_line !! End-of-line reporting (interactive path)
    public :: set_verbosity     !! CLI -d
    public :: set_verbose_mode  !! CLI -v
    public :: set_complex_mode  !! CLI -c
    public :: is_verbose        !! True when token tracing is on

    interface
        module subroutine state_init()
        end subroutine state_init

        module subroutine apply_command(command, ok)
            character(*), intent(in) :: command
            logical, intent(out)     :: ok    !! .false. asks the driver to stop
        end subroutine apply_command

        module subroutine help()
        end subroutine help

        module subroutine show_stack()
        end subroutine show_stack

        module subroutine report_after_arg()
        end subroutine report_after_arg

        module subroutine report_after_line()
        end subroutine report_after_line

        module subroutine set_verbosity(level)
            integer, intent(in) :: level
        end subroutine set_verbosity

        module subroutine set_verbose_mode(on)
            logical, intent(in) :: on
        end subroutine set_verbose_mode

        module subroutine set_complex_mode(on)
            logical, intent(in) :: on
        end subroutine set_complex_mode

        module function is_verbose() result(r)
            logical :: r
        end function is_verbose
    end interface

end module calc_state

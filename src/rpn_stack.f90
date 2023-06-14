module rpn_stack
    use iso_fortran_env, only: real64
    implicit none
    
    ! Type for the data that's going on to the stack
    type rpn_t
        private
        complex(8), private   :: zdata   = 0
        logical, private      :: is_cart = .true.
    contains
        procedure, private :: write_rpns
        generic, public    :: write(formatted) => write_rpns
        procedure :: get_value                 => get_value_rpns
        procedure :: set_value                 => set_value_rpns
        procedure :: is_integer                => is_integer_rpns
        procedure :: is_real                   => is_real_rpns
        procedure :: is_positive_real          => is_positive_real_rpns
        procedure :: is_cartesian              => is_cartesian_rpns
        procedure :: set_angle_unit            => set_angle_unit_rpns
        procedure, private :: add_rpns
        generic, public    :: operator(+) => add_rpns
        procedure, private :: subtract_rpns
        generic, public    :: operator(-) => subtract_rpns
        procedure, private :: multiply_rpns
        generic, public    :: operator(*) => multiply_rpns
        procedure, private :: divide_rpns
        generic, public    :: operator(/) => divide_rpns
        procedure, private :: power_rpns
        generic, public    :: operator(**) => power_rpns
        procedure, private :: set_to_rpns
        generic, public    :: assignment(=) => set_to_rpns
    end type rpn_t
    
    ! Make the stack a parameterized derived type in case we want a different size
    type stack_t(ssize)
        integer, len :: ssize
        private
        type(rpn_t)  :: sdata(ssize)
        character(2) :: legend(ssize)
        integer      :: high_water = 0
    contains
        procedure, private :: push_stackt
        procedure, private :: push_all_stackt
        procedure, private :: push_r_stackt
        generic, public    :: push => push_stackt, push_all_stackt, push_r_stackt
        procedure          :: peek => peek_stackt
        procedure          :: pop  => pop_stackt
        procedure          :: set  => set_stackt
        procedure          :: clear => clear_stackt
        procedure          :: swap  => swap_stackt
        procedure          :: rotate_up  => rotate_up_stackt
        procedure          :: rotate_down  => rotate_down_stackt
        procedure          :: print => print_stackt
        procedure          :: get_size => get_size_stackt
        procedure          :: set_legend => set_legend_stackt
    end type stack_t
    
    
    interface
        
        module subroutine set_legend_stackt(stk, legend)
            class(stack_t(*)), intent(inout) :: stk
            character(len=2), intent(in)     :: legend(:)
        end subroutine set_legend_stackt
        module function get_size_stackt(stk) result(r)
            class(stack_t(*)), intent(in) :: stk
            integer :: r
        end function get_size_stackt
        module subroutine print_stackt(stk, ve_mode)
            class(stack_t(*)), intent(in) :: stk
            logical, intent(in)           :: ve_mode
        end subroutine print_stackt
        module subroutine push_stackt(stk, z)
            class(stack_t(*)), intent(inout) :: stk
            type(rpn_t) :: z
        end subroutine push_stackt
        module subroutine push_r_stackt(stk, x)
            class(stack_t(*)), intent(inout) :: stk
            real(real64) :: x
        end subroutine push_r_stackt
        module subroutine push_all_stackt(stk, z, is_cart)
            class(stack_t(*)), intent(inout) :: stk
            complex(8), intent(in) :: z
            logical, intent(in), optional :: is_cart
        end subroutine push_all_stackt
        module subroutine set_stackt(stk, z, idx)
            class(stack_t(*)), intent(inout) :: stk
            type(rpn_t), intent(in) :: z
            integer, optional, intent(in) :: idx
        end subroutine set_stackt
        module function peek_stackt(stk, idx) result(r)
            class(stack_t(*)), intent(inout) :: stk
            integer, intent(in) :: idx 
            type(rpn_t) :: r
        end function peek_stackt
        module function pop_stackt(stk) result(r)
            class(stack_t(*)), intent(inout) :: stk
            type(rpn_t) :: r
        end function pop_stackt
        module subroutine clear_stackt(stk)
            class(stack_t(*)), intent(inout) :: stk
        end subroutine clear_stackt
        module subroutine swap_stackt(stk)
            class(stack_t(*)), intent(inout) :: stk
        end subroutine swap_stackt
        module subroutine rotate_up_stackt(stk)
            class(stack_t(*)), intent(inout) :: stk
        end subroutine rotate_up_stackt
        module subroutine rotate_down_stackt(stk)
            class(stack_t(*)), intent(inout) :: stk
        end subroutine rotate_down_stackt
    end interface
    
    interface
        module subroutine write_rpns(se, unit, iotype, v_list, iostat, iomsg)
            class(rpn_t), intent(in)    :: se
            integer, intent(in)         :: unit
            character(*), intent(in)    :: iotype
            integer, intent(in)         :: v_list(:)
            integer, intent(out)        :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine write_rpns
        module function is_integer_rpns(this) result(r)
            class(rpn_t), intent(in) :: this
            logical :: r
        end function is_integer_rpns
        module function is_cartesian_rpns(this) result(r)
            class(rpn_t), intent(in) :: this
            logical :: r
        end function is_cartesian_rpns
        module function is_real_rpns(this) result(r)
            class(rpn_t), intent(in) :: this
            logical :: r
        end function is_real_rpns
        module function is_positive_real_rpns(this) result(r)
            class(rpn_t), intent(in) :: this
            logical :: r
        end function is_positive_real_rpns
        module subroutine set_angle_unit_rpns(this, degrees)
            class(rpn_t), intent(inout) :: this
            logical, intent(in) :: degrees
        end subroutine set_angle_unit_rpns
        module function get_value_rpns(this, is_cartesian) result(r)
            class(rpn_t), intent(in)       :: this
            logical, optional, intent(out) :: is_cartesian
            complex(8) :: r
        end function get_value_rpns
        module subroutine set_value_rpns(this, z, is_cartesian)
            class(rpn_t), intent(inout)      :: this
            complex(8), optional, intent(in) :: z
            logical, optional, intent(in)    :: is_cartesian
        end subroutine set_value_rpns
        module subroutine set_to_rpns(this, z)
            class(rpn_t), intent(inout) :: this
            type(rpn_t), intent(in) :: z
        end subroutine set_to_rpns
        module function add_rpns(a, b) result(r)
            class(rpn_t), intent(in) :: a
            type(rpn_t), intent(in)  :: b
            type(rpn_t) :: r
        end function add_rpns
        module function subtract_rpns(a, b) result(r)
            class(rpn_t), intent(in) :: a
            type(rpn_t), intent(in)  :: b
            type(rpn_t) :: r
        end function subtract_rpns
        module function multiply_rpns(a, b) result(r)
            class(rpn_t), intent(in) :: a
            type(rpn_t), intent(in)  :: b
            type(rpn_t) :: r
        end function multiply_rpns
        module function divide_rpns(a, b) result(r)
            class(rpn_t), intent(in) :: a
            type(rpn_t), intent(in)  :: b
            type(rpn_t) :: r
        end function divide_rpns
        module function power_rpns(this, x) result(r)
            class(rpn_t), intent(in) :: this
            real(real64), intent(in) :: x
            type(rpn_t) :: r
        end function power_rpns        
    end interface
    
    real(real64), parameter :: pi = 4*atan(1.0d0)
    real(real64), parameter :: to_rad = pi/180
    real(real64), parameter :: to_deg = 180/pi

    character(5), private :: decimal = 'POINT'
    
    integer                  :: nroots = 0
    type(rpn_t), allocatable :: roots(:)
    integer, private         :: current_root = 0
    
    character(9), private :: f_large
    character(9), private :: f_small
    integer               :: dec_places = 6
    logical               :: degrees_mode = .true.
    logical               :: complex_mode = .false.
    real(real64)               :: eps = 1.0d-14
    
    ! Functions interface
    interface
        module function add_fr(a,b) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t), intent(in) :: b
            type(rpn_t) :: r
        end function add_fr
        
        module function subtract_fr(a,b) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t), intent(in) :: b
            type(rpn_t) :: r
        end function subtract_fr
        
        module function multiply_fr(a,b) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t), intent(in) :: b
            type(rpn_t) :: r
        end function multiply_fr
        
        module function divide_fr(a,b) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t), intent(in) :: b
            type(rpn_t) :: r
        end function divide_fr

        module function power_fr(a, b) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t), intent(in) :: b
            type(rpn_t) :: r
        end function power_fr
        
        module function percent_fr(a, b) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
            type(rpn_t), intent(in) :: b
        end function percent_fr
        
        module function power_2_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function power_2_fr

        module function power_3_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function power_3_fr

        module function sqrt_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function sqrt_fr

        module function cbrt_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function cbrt_fr

        module function reciprocal_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function reciprocal_fr

        module function conj_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function conj_fr
    
        module function len_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function len_fr
        
        module function swap_real_imaginary_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function swap_real_imaginary_fr
        
        module function chs_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function chs_fr
        
        module function sine_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function sine_fr

        module function cosine_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function cosine_fr

        module function tangent_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function tangent_fr

        module function hsine_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function hsine_fr

        module function hcosine_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function hcosine_fr

        module function htangent_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function htangent_fr

        module function asine_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function asine_fr

        module function acosine_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function acosine_fr

        module function atangent_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function atangent_fr

        module function ahsine_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function ahsine_fr

        module function ahcosine_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function ahcosine_fr

        module function ahtangent_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function ahtangent_fr
    
        module function exp_2_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function exp_2_fr

        module function exp_e_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function exp_e_fr

        module function exp_10_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function exp_10_fr

        module function ln_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function ln_fr

        module function log2_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function log2_fr

        module function lg_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function lg_fr

        module function gamma_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function gamma_fr

        module function fact_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function fact_fr

        module function ncr_fr(a, b) result(r)
            type(rpn_t), intent(in) :: a, b
            type(rpn_t) :: r
        end function ncr_fr

        module function npr_fr(a, b) result(r)
            type(rpn_t), intent(in) :: a, b
            type(rpn_t) :: r
        end function npr_fr
        
        module function root_fr(a, b) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t), intent(in) :: b
            type(rpn_t) :: r
        end function root_fr
        
        module function next_root_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function next_root_fr
        
        module function previous_root_fr(a) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function previous_root_fr

        module function atangent2_fr(a, b) result(r)
            type(rpn_t), intent(in) :: a
            type(rpn_t), intent(in) :: b
            type(rpn_t) :: r
        end function atangent2_fr
        
        module function round(x) result(r)
            real(real64), intent(in) :: x
            real(real64) ::r
        end function round

        module subroutine set_places(n)
            integer, intent(in) :: n
        end subroutine set_places
        
        module function get_places() result(r)
            integer :: r
        end function get_places
        
        module subroutine to_string(x, str)
            real(real64), intent(in) :: x
            character(len=:), allocatable, intent(out) :: str
        end subroutine to_string
    end interface
         
    public
    
    interface to_polar
        module function to_polar_rpns(stk_z) result(r)
            type(rpn_t), intent(in) :: stk_z
            type(rpn_t) :: r
        end function to_polar_rpns
    end interface
    
    interface to_cartesian
        module function to_cartesian_rpns(stk_z) result(r)
            type(rpn_t), intent(in) :: stk_z
            type(rpn_t) :: r
        end function to_cartesian_rpns
    end interface
    
    abstract interface
        function binary_f(a,b) result(r)
            import
            type(rpn_t), intent(in) :: a, b
            type(rpn_t) :: r
        end function binary_f
        function unary_f(a) result(r)
            import
            type(rpn_t), intent(in) :: a
            type(rpn_t) :: r
        end function unary_f
    end interface
        
end module rpn_stack

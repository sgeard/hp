
module numerical

    use iso_fortran_env

    ! Only need first derivatives from the autodiff library
    use AVD, only: T0=>avd_b, T1=>avd_d1, T2=>avd_d2, avd_init=>init

    implicit none

    logical, save :: deb_out = .false.
    public :: deb_out

    abstract interface
    
        function value_fun_g(v, err)
            import; implicit none
            class(T1), allocatable         :: value_fun_g
            class(T1), intent(in)          :: v
            integer, optional, intent(out) :: err ! 0 if no error
        end function value_fun_g
    
        function value_fun(v, err)
            import; implicit none
            type(T1)                       :: value_fun
            type(T1), intent(in)           :: v
            integer, optional, intent(out) :: err ! 0 if no error
        end function value_fun
    
        function value_fun2(v, err)
            import; implicit none
            type(T2)                       :: value_fun2
            type(T2), intent(in)           :: v
            integer, optional, intent(out) :: err ! 0 if no error
        end function value_fun2
    
        function rvalue_fun(x)
            import; implicit none
            class(T0), intent(in) :: x
            real(8)               :: rvalue_fun
        end function rvalue_fun
            
    end interface
    

    ! Information about the result
    type res_info_t
        integer :: n_iterations = 0
        logical :: solved = .false.
        real(8) :: solution = 0.0d0
    end type res_info_t

    type sframe_t
        real(8) :: a = 0.0d0
        real(8) :: b = 0.0d0
    contains
        procedure :: get_mid_point => get_mid_point_sframe_t
        procedure :: print => print_sframe_t
        procedure :: to_str => to_str_sframe_t
    end type sframe_t
    
    interface
        module pure function get_mid_point_sframe_t(this) result(r)
            class(sframe_t), intent(in) :: this
            real(8)                     :: r    
        end function get_mid_point_sframe_t
        module subroutine print_sframe_t(this)
            class(sframe_t), intent(in) :: this  
        end subroutine print_sframe_t
        module pure function to_str_sframe_t(this) result(r)
            class(sframe_t), intent(in)   :: this
            character(len=:), allocatable :: r    
        end function to_str_sframe_t
    end interface
    
    interface newton_raphson
        module subroutine newton_raphson_ncl(f, x0, eps, res, ilimit, solns)
            procedure(value_fun_g)          :: f
            real(8), intent(in)             :: x0
            real(8), intent(in)             :: eps
            type(res_info_t), intent(inout) :: res
            integer, optional, intent(in)   :: ilimit
            real(8), intent(in), optional   :: solns(:)  ! Solutions already found
        end subroutine newton_raphson_ncl
    end interface

    interface modified_newton_raphson
        module subroutine modified_newton_raphson_ncl(f, x0, eps, res, ilimit, solns)
            procedure(value_fun_g)          :: f
            real(8), intent(in)             :: x0
            real(8), intent(in)             :: eps
            type(res_info_t), intent(out)   :: res
            integer, intent(in), optional   :: ilimit
            real(8), intent(in), optional   :: solns(:)  ! Solutions already found
        end subroutine modified_newton_raphson_ncl
    end interface
    
    interface locate_solution_frame
        module subroutine locate_solution_frames(f, x0, direction, frames)
            procedure(value_fun_g)   :: f
            real(8), intent(in)      :: x0
            character(*), intent(in) :: direction
            type(sframe_t), allocatable, intent(out)  :: frames(:)
        end subroutine locate_solution_frames
    end interface
    
!    interface deflate
!        module function deflate(f, solns) result(res)
            
            
       
    interface D1
        module function D1_ncl(g, x) result(res)
            procedure(rvalue_fun), pointer    :: g
            real(8), intent(in) :: x
            real(8)             :: res
        end function D1_ncl
    end interface
    
    interface D2
        module function D2_ncl(g, x) result(res)
            procedure(rvalue_fun), pointer    :: g
            real(8), intent(in) :: x
            real(8)             :: res
        end function D2_ncl
    end interface
    
contains

end module numerical

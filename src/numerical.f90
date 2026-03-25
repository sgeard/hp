
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

    interface newton_raphson
        module subroutine newton_raphson_ncl(f, x0, eps, ilimit, res)
            procedure(value_fun_g)          :: f
            real(8), intent(in)             :: x0
            real(8), intent(in)             :: eps
            integer, optional, intent(in)   :: ilimit
            type(res_info_t), intent(inout) :: res
        end subroutine newton_raphson_ncl
    end interface

    interface modified_newton_raphson
        module subroutine modified_newton_raphson_ncl(f, x0, eps, ilimit, res)
            procedure(value_fun_g)          :: f
            real(8), intent(in)             :: x0
            real(8), intent(in)             :: eps
            integer, intent(in), optional   :: ilimit
            type(res_info_t), intent(out)   :: res
        end subroutine modified_newton_raphson_ncl
    end interface
    
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

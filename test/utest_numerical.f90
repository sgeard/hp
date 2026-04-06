! Unit test for numerical

program utest_numerical

    use numerical
    use AVD, T1=>avd_d1, T2=>avd_d2, avd_init=>init

    implicit none

    procedure(value_fun_g), pointer  :: f
    type(res_info_t)   :: res
    real(8)            :: x0, ref
    real(8), parameter :: eps = 1.0d-12

    ! Newton-Raphson: 1 solution
    nsolns_1: block
        write(*, '(a)') 'N-R with one solution: '
        f => s1_g
        x0 = 0.0d0
        call newton_raphson(f, x0, eps, res, 10)
        ref = log(2.0d0)/log(3.0d0)
        call check_solution('Standard NR',res,ref)

        x0 = 0.0d0
        call modified_newton_raphson(f, x0, eps, res, 10)
        call check_solution('Modified NR',res,ref)

    end block nsolns_1

    ! Newton-Raphson: 2 solutions
    nsolns_2: block
        type(sframe_t), allocatable :: sframes(:)
 
        write(*,'(/a)') 'N-R with two solution: '
        deb_out = .false.
        f => s2_g
     
        call locate_solution_frames(f, 0.0d0, 'both', sframes)
        
        x0 = sframes(1)%get_mid_point()
        call newton_raphson(f, x0, eps, res, 10)

        ref = -1.68616308d0
        call check_solution('Standard NR',res,ref)

        x0 = sframes(2)%get_mid_point()
        call modified_newton_raphson(f, x0, eps, res, 10)
        ref = 0.788566183d0
        call check_solution('Modified NR',res,ref)

    end block nsolns_2
    
    nsolns_2a: block
        real(8) :: solns(1)
        write(*,'(/a)') 'N-R with two solution and built-in deflation: '
        deb_out = .false.
        f => s2_g
        
        x0 = 0.0d0
        call newton_raphson(f, x0, eps, res, 10)

        ref = 0.788566183d0
        call check_solution('Standard NR',res,ref)

        solns(1) = res%solution

        x0 = 0.0d0
        call newton_raphson(f, x0, eps, res, 10, solns)
        
        ref = -1.68616308d0
        call check_solution('Standard NR',res,ref)

    end block nsolns_2a

    ! Newton-Raphson: 3 solutions
    nsolns_3: block
        real(8) :: solns(2)
        type(sframe_t), allocatable :: sframes(:)
        
        write(*,'(/a)') 'N-R with three solution: '
        deb_out = .false.
        
        f => s3_g ! 9**x - x**6
        
        call locate_solution_frames(f, x0, 'both', sframes)
        
        x0 = sframes(1)%get_mid_point()
        call modified_newton_raphson(f, x0, eps, res, 10)

        ref = -0.75769698d0
        call check_solution('Modified NR',res,ref)
        solns(1) = res%solution

        !deb_out = .true.
        x0 = sframes(2)%get_mid_point()
        call modified_newton_raphson(f, x0, eps, res, solns=solns(1:1))
        ref = 2.478052680d0
        call check_solution('Modified NR',res,ref)
        
        solns(2) = res%solution
        !deb_out = .true.

        x0 = sframes(3)%get_mid_point()
        ref = 3.0d0
        call modified_newton_raphson(f, x0, eps, res, 200, solns)
        call check_solution('Modified NR',res,ref)

    end block nsolns_3

    ! Newton-Raphson: 3 solutions
    nr_hard: block
        real(8) :: solns(2)
        type(sframe_t), allocatable :: sframes(:)
        
        write(*,'(/a)') 'N-R with three solution (hard): '
        deb_out = .false.
        f => s4_g
        
        call locate_solution_frames(f, x0, 'both', sframes)
        x0 = sframes(1)%get_mid_point()
        call newton_raphson(f, x0, eps, res, 32)

        ref = -0.913114982d0
        call check_solution('Standard NR',res,ref)

        ! Divide out first solution
        solns(1) = res%solution

        !deb_out = .true.
        x0 = sframes(2)%get_mid_point()
        call modified_newton_raphson(f, x0, eps, res, 100)

        ref = 1.117681521d0
        call check_solution('Modified NR',res,ref)

        ! Divide out second solution
        solns(2) = res%solution

        x0 = sframes(3)%get_mid_point()
        call modified_newton_raphson(f, x0, eps, res, 200)

        ref = 36.0d0
        call check_solution('Modified NR',res,ref)

    end block nr_hard

    stop

contains

    subroutine check_solution(name, sln, ref)
        character(*), intent(in)     :: name
        type(res_info_t), intent(in) :: sln
        real(8), intent(in)          :: ref

        if (sln%solved) then
            if (abs(sln%solution-ref) < 1.0d-8) then
                write (*,'(4x,a,i0,a)') name//' - PASSED: ',res%n_iterations,' iterations'
            else
                write (*,'(4x,2(a,f0.8))') name//' - FAILED: ',res%solution,' != ',ref
            end if
        else
            write (*,'(4x,a)') name//' - FAILED: iteration limit exceeded'
        end if


    end subroutine check_solution

    ! ---------------------------------------------------------------

    function s1_g(x, err) result(r)
        class(T1), allocatable         :: r
        class(T1), intent(in)          :: x
        integer, optional, intent(out) :: err
        if (present(err)) err = 0
        allocate(r, source = 3**x - 2.0d0)
    end function s1_g
    
    ! ---------------------------------------------------------------
 
    function s2_g(x, err) result(r)
        class(T1), allocatable         :: r
        class(T1), intent(in)          :: x
        integer, optional, intent(out) :: err
        if (present(err)) err = 0
        allocate(r, source = (3**x - 3.0d0 + x**2))
    end function s2_g
        
    ! ---------------------------------------------------------------

    function s3_g(x, err) result(r)
        class(T1), allocatable         :: r
        class(T1), intent(in)          :: x
        integer, optional, intent(out) :: err
        if (present(err)) err = 0
        allocate(r, source = 9**x - x**6)
    end function s3_g

    ! -------------------------------------------------------------

    function s4_g(x, err) result(r)
        class(T1), allocatable         :: r
        class(T1), intent(in)          :: x
        integer, optional, intent(out) :: err
        if (present(err)) err = 0
        allocate(r, source = 6**x - x**18)
    end function s4_g

    ! -------------------------------------------------------------


end program utest_numerical

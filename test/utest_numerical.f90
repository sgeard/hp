! Unit test for numerical

program utest_numerical

    use numerical
    use AVD, T1=>avd_d1, T2=>avd_d2, avd_init=>init

    implicit none

    procedure(value_fun_g), pointer  :: f
    procedure(value_fun_g), pointer :: f2
    type(res_info_t)   :: res
    real(8)            :: ps1, ps2
    real(8)            :: x0, ref
    real(8), parameter :: eps = 1.0d-8

    ! Newton-Raphson: 1 solution
    nsolns_1: block
        type(res_info_t), allocatable :: solns(:)
        write(*, '(a)') 'N-R with one solution: '
        f => s1_g
        x0 = 0.0d0
        call newton_raphson(f, x0, eps, 10, res)
        ref = log(2.0d0)/log(3.0d0)
        if (abs(res%solution-ref) < eps) then
            write (*,'(4x,a,i0,a)') 'Standard NR - PASSED: ',res%n_iterations,' iterations'
        else
            write (*,'(4x,2(a,f0.8))') 'Standard NR - FAILED: ',res%solution,' != ',ref
        end if

        f2 => s1_g
        x0 = 0.0d0
        call modified_newton_raphson(f2, x0, eps, 10, res)
        if (abs(res%solution-ref) < eps) then
            write (*,'(4x,a,i0,a)') 'Modified NR - PASSED: ',res%n_iterations,' iterations'
        else
            write (*,'(4x,2(a,f0.8))') 'Modified NR - FAILED: ',res%solution,' != ',ref
        end if

    end block nsolns_1

    ! Newton-Raphson: 2 solutions
    nsolns_2: block

        write(*,'(/a)') 'N-R with two solution: '
        deb_out = .false.
        f => s2_g
        x0 = 0.0d0
        call newton_raphson(f, x0, eps, 10, res)

        ref = 0.788566183d0
        if (abs(res%solution-ref) < eps .and. res%solved) then
            write (*,'(4x,a,i0,a)') 'Standard NR - PASSED: ',res%n_iterations,' iterations'
        else
            write (*,'(4x,2(a,f0.8))') 'Standard NR - FAILED: ',res%solution,' != ',ref
        end if
        ps1 = res%solution

        f2 => s2_p_g
        x0 = 0.0d0

        ref = -1.68616308d0
        call modified_newton_raphson(f2, x0, eps, 10, res)
        if (abs(res%solution-ref) < eps .and. res%solved) then
            write (*,'(4x,a,i0,a)') 'Modified NR - PASSED: ',res%n_iterations,' iterations'
        else
            write (*,'(4x,2(a,f0.8))') 'Modified NR - FAILED: ',res%solution,' != ',ref
        end if

    end block nsolns_2

    ! Newton-Raphson: 3 solutions
    nsolns_3: block

        write(*,'(/a)') 'N-R with three solution: '
        deb_out = .false.
        f => s3_g
        x0 = 0.0d0
        call newton_raphson(f, x0, eps, 10, res)

        ref = -0.75769698d0
        if (abs(res%solution-ref) < eps .and. res%solved) then
            write (*,'(4x,a,i0,a)') 'Standard NR - PASSED: ',res%n_iterations,' iterations'
        else
            write (*,'(4x,2(a,f0.8))') 'Standard NR - FAILED: ',res%solution,' != ',ref
        end if
        ps1 = res%solution

        !deb_out = .true.
        f2 => s3_2p_g
        x0 = 0.0d0

        call newton_raphson(f, x0, eps, 10, res)
        if (abs(res%solution-ref) < eps .and. res%solved) then
            write (*,'(4x,a,i0,a)') 'Modified NR - PASSED: ',res%n_iterations,' iterations'
        else
            write (*,'(4x,2(a,f0.8))') 'Modified NR - FAILED: ',res%solution,' != ',ref
        end if
        ps2 = res%solution
        !deb_out = .true.
        f2 => s3_3p_g
        ref = 2.478052680288d0
        call modified_newton_raphson(f2, x0, eps, 20, res)
        if (abs(res%solution-ref) < eps .and. res%solved) then
            write (*,'(4x,a,i0,a)') 'Modified NR - PASSED: ',res%n_iterations,' iterations'
        else
            write (*,'(4x,2(a,f0.8))') 'Modified NR - FAILED: ',res%solution,' != ',ref
        end if

    end block nsolns_3
    
    stop

contains

    ! ---------------------------------------------------------------

    function s1_g(x, err) result(r)
        class(T1), allocatable         :: r
        class(T1), intent(in)          :: x
        integer, optional, intent(out) :: err
        allocate(r, source = 3**x - 2.0d0)
    end function s1_g
    
    ! ---------------------------------------------------------------

    function s2_g(x, err) result(r)
        class(T1), allocatable         :: r
        class(T1), intent(in)          :: x
        integer, optional, intent(out) :: err
        allocate(r, source = (3**x - 3.0d0 + x**2))
    end function s2_g

    function s2_p_g(x, err) result(r)
        class(T1), allocatable         :: r
        class(T1), intent(in)          :: x
        integer, optional, intent(out) :: err
        allocate(r, source = (3**x - 3.0d0 + x**2)/(x-ps1))
    end function s2_p_g
    
    ! ---------------------------------------------------------------

    function s3_g(x, err) result(r)
        class(T1), allocatable         :: r
        class(T1), intent(in)          :: x
        integer, optional, intent(out) :: err
        allocate(r, source = 9**x - x**6)
    end function s3_g

    function s3_2p_g(x, err) result(r)
        class(T1), allocatable         :: r
        class(T1), intent(in) :: x
        integer, optional, intent(out) :: err
        allocate(r, source = (9**x - x**6)/(x-ps1))
    end function s3_2p_g

    function s3_3p_g(x, err) result(r)
        class(T1), allocatable         :: r
        class(T1), intent(in) :: x
        integer, optional, intent(out) :: err
        allocate(r, source = (9**x - x**6)/((x-ps1)*(x-ps2)))
    end function s3_3p_g

    ! -------------------------------------------------------------
    

end program utest_numerical

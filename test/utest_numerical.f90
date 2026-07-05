! Unit test for numerical

! Newton-Raphson test generators. These are passed to newton_raphson as
! procedure pointers, so they must be module-level procedures: an internal
! (host-contained) procedure pointer makes flang emit an executable-stack
! trampoline, which segfaults under the project's -Wl,-z,noexecstack link.
module nr_test_funcs

    use AVD, T1=>avd_d1
    implicit none
    private
    public :: s1_g, s2_g, s3_g, s4_g, nr1_g

contains

    function s1_g(x, ctx, err) result(r)
        class(T1), allocatable         :: r
        class(T1), intent(in)          :: x
        class(*), intent(in), optional :: ctx ! unused: these test functions need no context
        integer, optional, intent(out) :: err
        if (present(err)) err = 0
        allocate(r, source = 3**x - 2.0d0)
    end function s1_g

    function s2_g(x, ctx, err) result(r)
        class(T1), allocatable         :: r
        class(T1), intent(in)          :: x
        class(*), intent(in), optional :: ctx ! unused: these test functions need no context
        integer, optional, intent(out) :: err
        if (present(err)) err = 0
        allocate(r, source = (3**x - 3.0d0 + x**2))
    end function s2_g

    function s3_g(x, ctx, err) result(r)
        class(T1), allocatable         :: r
        class(T1), intent(in)          :: x
        class(*), intent(in), optional :: ctx ! unused: these test functions need no context
        integer, optional, intent(out) :: err
        if (present(err)) err = 0
        allocate(r, source = 9**x - x**6)
    end function s3_g

    function s4_g(x, ctx, err) result(r)
        class(T1), allocatable         :: r
        class(T1), intent(in)          :: x
        class(*), intent(in), optional :: ctx ! unused: these test functions need no context
        integer, optional, intent(out) :: err
        if (present(err)) err = 0
        allocate(r, source = 6**x - x**18)
    end function s4_g

    function nr1_g(x, ctx, err) result(r)
        class(T1), allocatable         :: r
        class(T1), intent(in)          :: x
        class(*), intent(in), optional :: ctx ! unused: these test functions need no context
        integer, optional, intent(out) :: err
        if (present(err)) err = 0
        allocate(r, source = 9**x + x**2 - 7)
    end function nr1_g

end module nr_test_funcs

program utest_numerical

    use numerical
    use hp_maths
    use nr_test_funcs
    use AVD, T1=>avd_d1, T2=>avd_d2, avd_init=>init

    implicit none

    procedure(value_fun_g), pointer  :: f
    type(res_info_t)   :: res
    real(8)            :: x0, ref
    real(8), parameter :: eps = 1.0d-12

    ! Pure-kernel and Newton-Raphson test bookkeeping
    integer            :: kpass = 0, kfail = 0, nrfail = 0
    real(8), parameter :: ktol = 1.0d-9
    real(8), parameter :: pi   = 4*atan(1.0d0)

    ! Pure elementary-maths kernels (hp_maths), tested independently of the
    ! calculator. Run first so they are exercised on every compiler regardless
    ! of the Newton-Raphson tests below.
    maths_kernels: block
        write(*,'(a)') 'hp_maths pure kernels: '
        ! powers / roots
        call check_c('c_sqrt',      c_sqrt((-1.0d0,0.0d0)),     (0.0d0, 1.0d0))
        call check_c('c_cbrt',      c_cbrt((8.0d0,0.0d0)),      (2.0d0, 0.0d0))
        ! sign / parts
        call check_c('c_conj',      c_conj((1.0d0,2.0d0)),      (1.0d0,-2.0d0))
        call check_c('c_negate',    c_negate((1.0d0,2.0d0)),    (-1.0d0,-2.0d0))
        call check_c('c_swap_reim', c_swap_reim((1.0d0,2.0d0)), (2.0d0, 1.0d0))
        ! trigonometric (radians)
        call check_c('c_sin',  c_sin(cmplx(pi/2,0.0d0,8)),  (1.0d0,0.0d0))
        call check_c('c_cos',  c_cos((0.0d0,0.0d0)),        (1.0d0,0.0d0))
        call check_c('c_tan',  c_tan((0.0d0,0.0d0)),        (0.0d0,0.0d0))
        call check_c('c_asin', c_asin((1.0d0,0.0d0)),       cmplx(pi/2,0.0d0,8))
        call check_c('c_acos', c_acos((1.0d0,0.0d0)),       (0.0d0,0.0d0))
        call check_c('c_atan', c_atan((1.0d0,0.0d0)),       cmplx(pi/4,0.0d0,8))
        ! hyperbolic
        call check_c('c_sinh', c_sinh((0.0d0,0.0d0)),       (0.0d0,0.0d0))
        call check_c('c_cosh', c_cosh((0.0d0,0.0d0)),       (1.0d0,0.0d0))
        call check_c('c_tanh', c_tanh((0.0d0,0.0d0)),       (0.0d0,0.0d0))
        call check_c('c_asinh',c_asinh((0.0d0,0.0d0)),      (0.0d0,0.0d0))
        call check_c('c_acosh',c_acosh((1.0d0,0.0d0)),      (0.0d0,0.0d0))
        call check_c('c_atanh',c_atanh((0.0d0,0.0d0)),      (0.0d0,0.0d0))
        ! exponential / logarithmic
        call check_c('c_exp',  c_exp((0.0d0,0.0d0)),        (1.0d0,0.0d0))
        call check_c('c_exp2', c_exp2((3.0d0,0.0d0)),       (8.0d0,0.0d0))
        call check_c('c_exp10',c_exp10((2.0d0,0.0d0)),      (100.0d0,0.0d0))
        call check_c('c_ln',   c_ln((1.0d0,0.0d0)),         (0.0d0,0.0d0))
        call check_c('c_log2', c_log2((8.0d0,0.0d0)),       (3.0d0,0.0d0))
        call check_c('c_log10',c_log10((1000.0d0,0.0d0)),   (3.0d0,0.0d0))
        ! real-only kernels
        call check_r('r_cbrt',       r_cbrt(27.0d0),        3.0d0)
        call check_r('r_cbrt(-8)',   r_cbrt(-8.0d0),       -2.0d0)
        call check_r('r_hypot',      r_hypot(3.0d0,4.0d0),  5.0d0)
        call check_r('r_gamma',      r_gamma(5.0d0),        24.0d0)
        call check_r('r_factorial',  r_factorial(5.0d0),    120.0d0)
        call check_r('r_factorial(0)',r_factorial(0.0d0),   1.0d0)
        call check_r('r_ncr',        r_ncr(5.0d0,2.0d0),    10.0d0)
        call check_r('r_ncr(sym)',   r_ncr(52.0d0,47.0d0),  2598960.0d0)
        ! beyond gamma's range (n! overflows for n > 170): the ratio loop must not
        call check_r('r_ncr(200,3)', r_ncr(200.0d0,3.0d0),  1313400.0d0)
        call check_r('r_ncr(r>n)',   r_ncr(2.0d0,5.0d0),    0.0d0)
        call check_r('r_npr',        r_npr(5.0d0,2.0d0),    20.0d0)
        call check_r('r_npr(200,3)', r_npr(200.0d0,3.0d0),  7880400.0d0)
        call check_r('r_atan2',      r_atan2(1.0d0,1.0d0),  pi/4)
        write(*,'(4x,a,i0,a,i0)') 'kernels - passed: ',kpass,'  failed: ',kfail
        if (kfail > 0) error stop 'hp_maths kernel test failed'
    end block maths_kernels

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

    nr_1: block
        real(8) :: solns(1)
        type(sframe_t), allocatable :: sframes(:)
        
        write(*,'(/a)') 'N-R 1: '
        deb_out = .false.
        f => nr1_g
        
        call locate_solution_frames(f, x0, 'both', sframes)
        x0 = sframes(1)%get_mid_point()
        call newton_raphson(f, x0, eps, res)

        ref = -2.64518596d0
        call check_solution('Standard NR',res,ref)
 
        ! Divide out first solution
        solns(1) = res%solution
        x0 = sframes(2)%get_mid_point()
        call modified_newton_raphson(f, x0, eps, res)

        ref = 0.83756123d0
        call check_solution('Modified NR',res,ref)
  
    end block nr_1

    if (nrfail > 0) error stop 'Newton-Raphson tests failed'
    stop

contains

    subroutine check_c(name, got, expected)
        character(*), intent(in) :: name
        complex(8),   intent(in) :: got, expected
        if (abs(got - expected) < ktol) then
            kpass = kpass + 1
        else
            kfail = kfail + 1
            write(*,'(4x,a)') name//' - FAILED'
            write(*,'(8x,a,2f0.10)') 'got      = ',got
            write(*,'(8x,a,2f0.10)') 'expected = ',expected
        end if
    end subroutine check_c

    subroutine check_r(name, got, expected)
        character(*), intent(in) :: name
        real(8),      intent(in) :: got, expected
        if (abs(got - expected) < ktol) then
            kpass = kpass + 1
        else
            kfail = kfail + 1
            write(*,'(4x,2(a,f0.10))') name//' - FAILED: got ',got,' != ',expected
        end if
    end subroutine check_r

    subroutine check_solution(name, sln, ref)
        character(*), intent(in)     :: name
        type(res_info_t), intent(in) :: sln
        real(8), intent(in)          :: ref

        if (sln%solved) then
            if (abs(sln%solution-ref) < 1.0d-8) then
                write (*,'(4x,a,i0,a)') name//' - PASSED: ',sln%n_iterations,' iterations'
            else
                nrfail = nrfail + 1
                write (*,'(4x,2(a,f0.8))') name//' - FAILED: ',sln%solution,' != ',ref
            end if
        else
            nrfail = nrfail + 1
            write (*,'(4x,a)') name//' - FAILED: iteration limit exceeded'
        end if


    end subroutine check_solution

end program utest_numerical

submodule (numerical) numerical_sm

    use AVD, T0=>avd_b, T1=>avd_d1, T2=>avd_d2

    use iso_fortran_env, only: output_unit
    implicit none

contains

    module pure function get_mid_point_sframe_t(this) result(r)
        class(sframe_t), intent(in) :: this
        real(8)                     :: r   
        r = (this%a+this%b)/2
    end function get_mid_point_sframe_t

    module subroutine print_sframe_t(this)
        class(sframe_t), intent(in) :: this
        write(*,'(2(a,f0.4),a)') '(',this%a,', ',this%b,')'
    end subroutine print_sframe_t
    
    module pure function to_str_sframe_t(this) result(r)
        class(sframe_t), intent(in)   :: this
        character(len=:), allocatable :: r
        character(len=64) :: buff
        write(buff,'(2(a,f0.4),a)') '(',this%a,', ',this%b,')'
        r = trim(buff)
    end function to_str_sframe_t
    
    module subroutine locate_solution_frames(f, x0, direction, frames)
        ! Return an array of sframe_t in ascending order
        procedure(value_fun_g)   :: f
        real(8), intent(in)      :: x0
        character(*), intent(in) :: direction
        type(sframe_t), allocatable, intent(out)  :: frames(:)

        real(8), parameter :: step = 0.01d0
        integer, parameter :: max_steps = 10000, max_frames = 64
        type(sframe_t)     :: tmp(max_frames)
        integer            :: nframes, i
        real(8)            :: x_curr, x_next, f_curr, f_next
        type(T1)           :: ftmp

        if (direction /= 'up' .and. direction /= 'down' .and. direction /= 'both') then
            stop 'Invalid search direction specified'
        end if

        nframes = 0

        if (direction == 'down' .or. direction == 'both') then
            x_curr = x0 - max_steps*step
            ftmp = f(T1(x_curr, 0))
            f_curr = ftmp%v
            do i = max_steps-1,0,-1
                x_next = x0 - i*step
                call detect_frame
            end do
        end if

        if (direction == 'up' .or. direction == 'both') then
            x_curr = x0
            ftmp = f(T1(x0, 0))
            f_curr = ftmp%v
            do i = 1, max_steps
                x_next = x0 + i*step
                call detect_frame
            end do
        end if

        frames = tmp(1:nframes)

    contains
        subroutine detect_frame
            ftmp = f(T1(x_next, 0))
            f_next = ftmp%v
            if (f_curr * f_next <= 0.0d0) then
                ! If this frame connects to the previous one merge them
                if (nframes == 0) then
                    call add_frame
                    !print *,'New frame = ',tmp(nframes)%to_str()
                else
                    if (abs(tmp(nframes)%b-x_curr) < step/2) then
                        tmp(nframes)%b = x_next
                        !print *,'Merged frame = ',tmp(nframes)%to_str()
                    else
                        call add_frame
                    end if
                end if
            end if
            x_curr = x_next
            f_curr = f_next
        end subroutine detect_frame
        
        subroutine add_frame
            nframes = nframes + 1
            tmp(nframes) = sframe_t(x_curr, x_next)
        end subroutine add_frame
    end subroutine locate_solution_frames

    ! Approximate first and second derivative
    module function D1_ncl(g, x) result(res)
        procedure(rvalue_fun), pointer    :: g
        real(8), intent(in) :: x
        real(8)             :: res
        real(8), parameter  :: h = 1.0d-5
        res = (g(T0(x+h))-g(T0(x)))/h
    end function D1_ncl

    module function D2_ncl(g, x) result(res)
        procedure(rvalue_fun), pointer    :: g
        real(8), intent(in) :: x
        real(8)             :: res
        real(8), parameter  :: h = 1.0d-5
        res = (g(T0(x+2*h))-2*g(T0(x+h))+g(T0(x)))/h**2
    end function D2_ncl

    module subroutine newton_raphson_ncl(f, x0, eps, res, ilimit, solns)
        procedure(value_fun_g)          :: f
        real(8), intent(in)             :: x0
        real(8), intent(in)             :: eps
        type(res_info_t), intent(inout) :: res
        integer, optional, intent(in)   :: ilimit
        real(8), intent(in), optional   :: solns(:)  ! Solutions already found

        integer  :: max_iterations
        integer  :: counter, i
        real(8)  :: x
        type(T1) :: r
        
        if (present(ilimit)) then
            max_iterations = ilimit
        else
            max_iterations = 256
        end if
        
        call avd_init
        x = x0
        associate (fx => r%v, dfx => r%d1)
            nr: do counter=1,max_iterations
                r = f(T1(x,1))
                if (present(solns)) then
                    do i=1,size(solns)
                        r = r / (T1(x,1) - solns(i))
                    end do
                end if
                if (deb_out) then
                    write(output_unit,'(3x,i3,3x,4(a,f0.8))') counter,"f(",x,") = ",fx," ; f'(x) = ",dfx, &
                        " ; step = ",fx/dfx
                end if
                ! Convergence test: normalise by |f'(x)| to give a scale-independent
                ! step-size criterion rather than an absolute function value criterion
                if (abs(dfx) > 0.0d0 .and. abs(fx/dfx) < eps) then
                    exit nr
                end if
                x = x - fx/dfx
            end do nr
        end associate
        res = res_info_t(counter,counter < max_iterations,x)
        if (deb_out) then
            write(output_unit,'(3x,a,g0.8,a,g0.6)') 'Solution: x = ',x,'; f(x) = ',r%v
        end if
        
    end subroutine newton_raphson_ncl

    module subroutine modified_newton_raphson_ncl(f, x0, eps, res, ilimit, solns)
        procedure(value_fun_g)           :: f
        real(8), intent(in)             :: x0
        real(8), intent(in)             :: eps
        type(res_info_t), intent(out)   :: res
        integer, intent(in), optional   :: ilimit
        real(8), intent(in), optional   :: solns(:)  ! Solutions already found

        
        integer  :: max_iterations
        integer  :: counter, i

        real(8)  :: x, q, qf, f0, df0, df1, df2, dfq
        type(T2) :: r
        
        ! Taken from
        !
        ! Root-finding: from Newton to Halley and beyond  by  Richard J. Martin
        !
        ! https://arxiv.org/html/2312.12305v1
        
        if (present(ilimit)) then
            max_iterations = ilimit
        else
            max_iterations = 256
        end if
        call avd_init
        x = x0
        nr: do counter=1,max_iterations
            r = f(T2(x,1,0))
            if (present(solns)) then
                do i=1,size(solns)
                    r = r / (T2(x,1,0) - solns(i))
                end do
            end if
            associate (fx => r%v, dfx => r%d1, d2fx => r%d2)
                f0 = r%v
                q = fx*d2fx/dfx**2
                if (q >= 0) then
                    qf = 1 + (q/2)*(1+q/3)
                else
                    qf = 1/(1-q/2)
                end if
                if (deb_out) then
                    write(output_unit,'(3x,i3,3x,8(a,g12.5))') counter,"f(",x,") = ",fx," ; f'(x) = ",dfx," ; f''(x) = ",d2fx, &
                    ' ; q = ',q,' ; qf = ',qf,' ; dfq = ',dfq,' ; step = ',qf*fx/dfx
                    write(output_unit,'(18x,3(a,g12.5)/)') '   df0 = ',df0,'     df1 = ',df1,'      df2 = ',df2
                end if
                ! Convergence test: normalise by |f'(x)| to give a scale-independent
                ! step-size criterion rather than an absolute function value criterion
                if (abs(dfx) > 0.0d0 .and. abs(fx/dfx) < eps) then
                    exit nr
                end if
                x = x - qf*fx/dfx
            end associate
        end do nr

        if (deb_out) then
            write(output_unit,'(3x,a,g0.8,a,g0.6)') 'Solution: x = ',x,'; f(x) = ',r%v
        end if
        res = res_info_t(counter,counter < max_iterations,x)
        
    end subroutine modified_newton_raphson_ncl

end submodule numerical_sm

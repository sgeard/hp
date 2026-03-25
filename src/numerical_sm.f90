submodule (numerical) numerical_sm
    use numerical
    use AVD, T1=>avd_d1, T2=>avd_d2

    use iso_fortran_env, only: output_unit
    implicit none

contains
    
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

    module subroutine newton_raphson_ncl(f, x0, eps, ilimit, res)
        procedure(value_fun_g)          :: f
        real(8), intent(in)             :: x0
        real(8), intent(in)             :: eps
        integer, optional, intent(in)   :: ilimit
        type(res_info_t), intent(inout) :: res
        
        integer  :: max_iterations
        integer  :: counter
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

    module subroutine modified_newton_raphson_ncl(f, x0, eps, ilimit, res)
        procedure(value_fun_g)           :: f
        real(8), intent(in)             :: x0
        real(8), intent(in)             :: eps
        integer, intent(in), optional   :: ilimit
        type(res_info_t), intent(out)   :: res
        
        integer  :: max_iterations
        integer  :: counter

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

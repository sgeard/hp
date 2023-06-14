! Implementation code for stack
submodule (rpn_stack) stack_sm
    use iso_fortran_env, only: output_unit
    implicit none

contains

    module subroutine set_legend_stackt(stk, legend)
        class(stack_t(*)), intent(inout) :: stk
        character(len=2), intent(in)     :: legend(:)
        stk%legend = legend
    end subroutine set_legend_stackt

    module function get_size_stackt(stk) result(r)
        class(stack_t(*)), intent(in) :: stk
        integer :: r
        r = stk%high_water
    end function get_size_stackt

    module subroutine print_stackt(stk, ve_mode)
        class(stack_t(*)), intent(in) :: stk
        logical, intent(in)           :: ve_mode
        integer :: i
        if (ve_mode) then
            do i=stk%high_water,1,-1
                write(output_unit,fmt='(a)',advance='no') stk%legend(i)//' '
                write(output_unit,'(dt)') stk%sdata(i)
            end do
        else
            write(output_unit,fmt='(dt)') stk%sdata(1)
        end if
    end subroutine print_stackt
    
    module subroutine push_stackt(stk, z)
        class(stack_t(*)), intent(inout) :: stk
        type(rpn_t) :: z
        integer :: i
        do i=stk%ssize,2,-1
            stk%sdata(i) = stk%sdata(i-1)
        end do
        stk%sdata(1) = z
        if (stk%high_water < stk%ssize) &
            stk%high_water = stk%high_water + 1
    end subroutine push_stackt
    
    module subroutine push_r_stackt(stk, x)
        class(stack_t(*)), intent(inout) :: stk
        real(real64) :: x
        type(rpn_t) :: z
        z = rpn_t(cmplx(x,0.0d0))
        call stk%push_stackt(z)
    end subroutine push_r_stackt
    
    module subroutine push_all_stackt(stk, z, is_cart)
        class(stack_t(*)), intent(inout) :: stk
        complex(8), intent(in) :: z
        logical, intent(in), optional :: is_cart
        integer :: i
        do i=stk%ssize,2,-1
            stk%sdata(i) = stk%sdata(i-1)
        end do
        if (present(is_cart)) then
            call stk%set(rpn_t(z,is_cart))
        else
            call stk%set(rpn_t(z))
        end if
        if (stk%high_water < stk%ssize) &
            stk%high_water = stk%high_water + 1
    end subroutine push_all_stackt

    module subroutine set_stackt(stk, z, idx)
        class(stack_t(*)), intent(inout) :: stk
        type(rpn_t), intent(in) :: z
        integer, optional, intent(in) :: idx
        if (present(idx)) then
            stk%sdata(idx) = z
        else
            stk%sdata(1) = z
        end if
    end subroutine set_stackt
    
    module function peek_stackt(stk, idx) result(r)
        class(stack_t(*)), intent(inout) :: stk
        integer, intent(in) :: idx 
        type(rpn_t) :: r
        if (idx >= 1 .and. idx <= stk%ssize) then
            r = stk%sdata(idx)
        else
            write(*,'(a,i0,a)') '***Invalid index (',idx,')'
            r = rpn_t()
        end if
    end function peek_stackt
    
    module function pop_stackt(stk) result(r)
        class(stack_t(*)), intent(inout) :: stk
        type(rpn_t) :: r
        integer :: i
        r = stk%sdata(1)
        do i=1,stk%ssize-1
            stk%sdata(i) = stk%sdata(i+1)
        end do
        stk%sdata(stk%ssize) = rpn_t()
        if (stk%high_water > 0) &
            stk%high_water = stk%high_water - 1
    end function pop_stackt
    
    module subroutine clear_stackt(stk)
        class(stack_t(*)), intent(inout) :: stk
        integer :: i
        do i=1,stk%ssize
            stk%sdata(i) = rpn_t()
        end do
        stk%high_water = 0
    end subroutine clear_stackt
    
    module subroutine swap_stackt(stk)
        class(stack_t(*)), intent(inout) :: stk
        type(rpn_t) :: z
        z = stk%sdata(1)
        stk%sdata(1) = stk%sdata(2)
        stk%sdata(2) = z
    end subroutine swap_stackt
    
    module subroutine rotate_up_stackt(stk)
        class(stack_t(*)), intent(inout) :: stk
        type(rpn_t) :: z
        z = stk%pop()
        stk%high_water = stk%high_water + 1
        call stk%set(z,stk%high_water)
    end subroutine rotate_up_stackt
    
    module subroutine rotate_down_stackt(stk)
        class(stack_t(*)), intent(inout) :: stk
        type(rpn_t) :: z
        z = stk%peek(stk%high_water)
        stk%high_water = stk%high_water - 1
        call stk%push(z)
    end subroutine rotate_down_stackt

end submodule stack_sm

! Implementation code for rpn_t
submodule (rpn_stack) rpn_sm
    use iso_fortran_env, only: output_unit
    implicit none

contains

    module subroutine write_rpns(se, unit, iotype, v_list, iostat, iomsg)
        class(rpn_t), intent(in)    :: se
        integer, intent(in)         :: unit
        character(*), intent(in)    :: iotype
        integer, intent(in)         :: v_list(:)
        integer, intent(out)        :: iostat
        character(*), intent(inout) :: iomsg
        complex(8) :: z
        character(len=:), allocatable :: str_re, str_im
        z = se%zdata
        if (complex_mode) then
            call to_string(z%re,str_re)
            call to_string(z%im,str_im)
            if (se%is_cartesian()) then
                write(unit,'(a)',iostat=iostat) '('//str_re//','//str_im//')'
            else
                write(unit,'(a)',iostat=iostat) '('//str_re//','//str_im//') p'
            end if
        else
            call to_string(z%re,str_re)
            write(unit,'(a)',iostat=iostat) str_re
        end if
        if (iostat /= 0) then
            iomsg = 'output error'
        end if
    end subroutine write_rpns
    
    ! Convert real to string inserting a leading 0 if necessary
    module subroutine to_string(x, str)
        real(real64), intent(in) :: x
        character(len=:), allocatable, intent(out) :: str
        character(len=32) :: s
        s = ' '
        if (f_small == 'i0') then
            write(s,fmt='('//f_small//')') nint(x)
        else
            if (x == 0 .or. (abs(x) < 1.0d7 .and. abs(x) > 1.0d-7)) then
                write(s(2:),fmt='('//f_small//')') x
            else
                write(s(2:),fmt='('//f_large//')') x
            end if
            if (s(2:3) == '-.') then
                s(1:3) = '-0.'
            else if (s(2:2) == '.') then
                s(1:2) = '0.'
            end if
        end if
        str = trim(adjustl(s))
    end subroutine to_string

    module function is_integer_rpns(this) result(r)
        class(rpn_t), intent(in) :: this
        logical :: r
        real(real64) :: x
        x = this%zdata%re
        r = (abs(nint(x)-x) < eps .and. abs(this%zdata%im) < eps)
    end function is_integer_rpns

    module function is_cartesian_rpns(this) result(r)
        class(rpn_t), intent(in) :: this
        logical :: r
        r = this%is_cart
    end function is_cartesian_rpns

    module function is_real_rpns(this) result(r)
        class(rpn_t), intent(in) :: this
        logical :: r
        r = this%zdata%im == 0
    end function is_real_rpns

    module function is_positive_real_rpns(this) result(r)
        class(rpn_t), intent(in) :: this
        logical :: r
        r = this%zdata%im == 0 .and. this%zdata%re > 0
    end function is_positive_real_rpns

    module subroutine set_angle_unit_rpns(this, degrees)
        class(rpn_t), intent(inout) :: this
        logical, intent(in) :: degrees
        if (.not. this%is_cart) then
            this%zdata%im = this%zdata%im*merge(to_deg, to_rad, degrees)
        end if
    end subroutine set_angle_unit_rpns

    module function get_value_rpns(this, is_cartesian) result(r)
        class(rpn_t), intent(in)       :: this
        logical, optional, intent(out) :: is_cartesian
        complex(8) :: r
        r = this%zdata
        if (present(is_cartesian)) then
            is_cartesian = this%is_cart
        end if
    end function get_value_rpns

    module subroutine set_value_rpns(this, z, is_cartesian)
        class(rpn_t), intent(inout)      :: this
        complex(8), optional, intent(in) :: z
        logical, optional, intent(in)    :: is_cartesian
        if (present(z)) then
            this%zdata = z
        end if
        if (present(is_cartesian)) then
            this%is_cart = is_cartesian
        end if
    end subroutine set_value_rpns

    module subroutine set_to_rpns(this, z)
        class(rpn_t), intent(inout) :: this
        type(rpn_t), intent(in) :: z
        this%zdata = z%zdata
        this%is_cart = z%is_cart
    end subroutine set_to_rpns

    module function add_rpns(a, b) result(r)
        class(rpn_t), intent(in) :: a
        type(rpn_t), intent(in)  :: b
        type(rpn_t) :: r
        type(rpn_t) :: s
        logical :: is_cart
        is_cart = a%is_cartesian() ! The output will be set to this
        if (a%is_cartesian()) then
            r = a
        else
            r = to_cartesian(a)
        end if
        if (b%is_cartesian()) then
            r%zdata = r%zdata + b%zdata
        else
            s = to_cartesian(a)
            r%zdata = r%zdata + s%zdata
        end if
        if (.not. is_cart) then
            r = to_polar(r)
        end if
    end function add_rpns
    
    module function subtract_rpns(a, b) result(r)
        class(rpn_t), intent(in) :: a
        type(rpn_t), intent(in)  :: b
        type(rpn_t) :: r
        type(rpn_t) :: s
        logical :: is_cart
        is_cart = a%is_cartesian() ! The output will be set to this
        if (a%is_cartesian()) then
            r = a
        else
            r = to_cartesian(a)
        end if
        if (b%is_cartesian()) then
            r%zdata = r%zdata - b%zdata
        else
            s = to_cartesian(a)
            r%zdata = r%zdata - s%zdata
        end if
        if (.not. is_cart) then
            r = to_polar(r)
        end if
    end function subtract_rpns
    
    module function multiply_rpns(a, b) result(r)
        class(rpn_t), intent(in) :: a
        type(rpn_t), intent(in)  :: b
        type(rpn_t) :: r
        type(rpn_t) :: s
        logical :: is_cart
        is_cart = a%is_cartesian() ! The output will be set to this
        if (a%is_cartesian()) then
            r = a
        else
            r = to_cartesian(a)
        end if
        if (b%is_cartesian()) then
            r%zdata = r%zdata * b%zdata
        else
            s = to_cartesian(a)
            r%zdata = r%zdata * s%zdata
        end if
        if (.not. is_cart) then
            r = to_polar(r)
        end if
    end function multiply_rpns
    
    module function divide_rpns(a, b) result(r)
        class(rpn_t), intent(in) :: a
        type(rpn_t), intent(in)  :: b
        type(rpn_t) :: r
        type(rpn_t) :: s
        logical :: is_cart
        is_cart = a%is_cartesian() ! The output will be set to this
        if (a%is_cartesian()) then
            r = a
        else
            r = to_cartesian(a)
        end if
        if (b%is_cartesian()) then
            r%zdata = r%zdata / b%zdata
        else
            s = to_cartesian(a)
            r%zdata = r%zdata / s%zdata
        end if
        if (.not. is_cart) then
            r = to_polar(r)
        end if
    end function divide_rpns
    
    module function power_rpns(this, x) result(r)
        class(rpn_t), intent(in) :: this
        real(real64), intent(in) :: x
        type(rpn_t) :: r
        type(rpn_t) :: z
        logical     :: is_cart
        is_cart = this%is_cartesian()
        if (.not. is_cart) then
            z = to_cartesian(this)
        else
            z = this
        end if
        r%zdata = z%zdata**x
        if (.not. is_cart) then
            r = to_polar(r)
        end if
    end function power_rpns
    
    module function to_cartesian_rpns(stk_z) result(r)
        type(rpn_t), intent(in) :: stk_z
        type(rpn_t) :: r
        real(real64) :: s
        real(real64) :: theta
        if (.not. stk_z%is_cartesian()) then
            s = stk_z%zdata%re
            theta = stk_z%zdata%im * merge(to_rad,1.0d0,degrees_mode)
            r%zdata%re = round(s * cos(theta))
            r%zdata%im = round(s * sin(theta))
            r%is_cart = .true.
        else
            r = stk_z
        end if
    end function to_cartesian_rpns

        
    module function to_polar_rpns(stk_z) result(r)
        type(rpn_t), intent(in) :: stk_z
        type(rpn_t) :: r
        if (stk_z%is_cartesian()) then
            call r%set_value(to_polar_internal(stk_z%get_value()),is_cartesian = .false.)
        else
            r = stk_z
        end if
    contains
        complex(8) function to_polar_internal(z)
            complex(8), intent(in) :: z
            real(real64) :: r
            real(real64) :: theta
            r = sqrt(real(z * conjg(z),8))
            theta = atan2(aimag(z), real(z))
            to_polar_internal%re = r
            to_polar_internal%im = theta * merge(1/to_rad,1.0d0,degrees_mode)
        end function to_polar_internal
    end function to_polar_rpns
    
    module function add_fr(a,b) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t), intent(in) :: b
        type(rpn_t) :: r
        r = a + b
    end function add_fr

    module function subtract_fr(a,b) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t), intent(in) :: b
        type(rpn_t) :: r
        r = a - b
    end function subtract_fr
    
    module function multiply_fr(a,b) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t), intent(in) :: b
        type(rpn_t) :: r
        r = a * b
    end function multiply_fr
    
    module function divide_fr(a,b) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t), intent(in) :: b
        type(rpn_t) :: r
        r = a / b
    end function divide_fr
    
    module function percent_fr(a,b) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t), intent(in) :: b
        type(rpn_t) :: r
        r = a * b / rpn_t(cmplx(100.0d0,0.0d0))
    end function percent_fr

    module function power_fr(a, b) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t), intent(in) :: b
        type(rpn_t) :: r
        r = a ** real(b%zdata)
    end function power_fr
    
    module function power_2_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = a ** 2.0d0
    end function power_2_fr

    module function power_3_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = a ** 3.0d0
    end function power_3_fr

    module function sqrt_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        call r%set_value(sqrt(a%zdata))
    end function sqrt_fr

    module function cbrt_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        call r%set_value(a%zdata ** (1.0d0/3))
    end function cbrt_fr

    module function reciprocal_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(1.0d0)/a
    end function reciprocal_fr

    module function conj_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = a
        r%zdata%im = -r%zdata%im
    end function conj_fr
    
    module function len_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        type(rpn_t) :: s
        s = a * conj_fr(a)
        r = rpn_t(cmplx(sqrt(real(s%zdata%re)),0.0d0,8))
    end function len_fr
    
    module function swap_real_imaginary_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        real(real64) :: x
        r = a
        x = r%zdata%re
        r%zdata%re = r%zdata%im
        r%zdata%im = x
    end function swap_real_imaginary_fr
    
    module function chs_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(cmplx(-a%zdata%re,-a%zdata%im))
    end function chs_fr
    
    module function sine_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(sin(a%zdata * merge(to_rad,1.0d0,degrees_mode)))
    end function sine_fr

    module function cosine_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(cos(a%zdata%re * merge(to_rad,1.0d0,degrees_mode)))
    end function cosine_fr

    module function tangent_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(tan(a%zdata * merge(to_rad,1.0d0,degrees_mode)))
    end function tangent_fr

    module function hsine_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(sinh(a%zdata))
    end function hsine_fr

    module function hcosine_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(cosh(a%zdata))
    end function hcosine_fr

    module function htangent_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(tanh(a%zdata))
    end function htangent_fr

    module function asine_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(asin(a%zdata) * merge(1/to_rad,1.0d0,degrees_mode))
    end function asine_fr

    module function acosine_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(acos(a%zdata) * merge(1/to_rad,1.0d0,degrees_mode))
    end function acosine_fr

    module function atangent_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(atan(a%zdata) * merge(1/to_rad,1.0d0,degrees_mode))
    end function atangent_fr

    module function ahsine_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(asinh(a%zdata))
    end function ahsine_fr

    module function ahcosine_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(acosh(a%zdata))
    end function ahcosine_fr

    module function ahtangent_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(atanh(a%zdata))
    end function ahtangent_fr
 
    module function exp_2_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(2**a%zdata)
    end function exp_2_fr

    module function exp_e_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(exp(a%zdata))
    end function exp_e_fr

    module function exp_10_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(10**a%zdata)
    end function exp_10_fr

    module function ln_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(log(a%zdata))
    end function ln_fr

    module function log2_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(log(a%zdata)/log(2.0d0))
    end function log2_fr

    module function lg_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(log(a%zdata)/log(10.0d0))
    end function lg_fr

    module function gamma_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        r = rpn_t(gamma(a%zdata%re))
    end function gamma_fr

    module function fact_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        if (a%zdata%re == 0) then
            r = rpn_t(1)
        else
            r = rpn_t(a%zdata%re*gamma(a%zdata%re))
        end if
    end function fact_fr

    module function ncr_fr(a, b) result(r)
        type(rpn_t), intent(in) :: a, b
        type(rpn_t) :: r
        r = fact_fr(a)/(fact_fr(b)*fact_fr(a-b))
    end function ncr_fr

    module function npr_fr(a, b) result(r)
        type(rpn_t), intent(in) :: a, b
        type(rpn_t) :: r
        r = fact_fr(a)/fact_fr(b)
    end function npr_fr

    module function root_fr(a, b) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t), intent(in) :: b
        type(rpn_t) :: r
        real(real64)     :: bc
        integer     :: i
        type(rpn_t) :: base
        complex(8)  :: z
        logical     :: a_is_cart
        real(real64)     :: s, delta_theta, theta0, phi
        real(real64), parameter :: two_pi = 8*atan(1.0d0)
       
        bc = real(b%get_value())
        r = power_fr(a, rpn_t(1.0d0/bc))
        ! If b is an integer >= 2 calculate all roots
        if (b%is_integer() .and. bc >= 2) then
            nroots = nint(bc)
            if (allocated(roots)) then
                deallocate(roots)
            end if
            a_is_cart = a%is_cartesian()
            base = to_polar_rpns(a)
            z = base%get_value()
            s = z%re ** (1.0d0/bc)
            theta0 = merge(z%im*to_rad,z%im,degrees_mode)/nroots
            delta_theta = two_pi/nroots
            allocate(roots(nroots))
            do i=1, nroots
                phi = theta0 + (i-1)*delta_theta
                if (a_is_cart) then
                    roots(i) = rpn_t(cmplx(round(s*cos(phi)),round(s*sin(phi))),a_is_cart)
                else
                    if (degrees_mode) phi = phi*to_deg
                    roots(i) = rpn_t(cmplx(s,round(phi)),a_is_cart)
                end if
            end do
            r = roots(1)
            current_root = 1
        else
            r = a ** (1.0d0/bc)
        end if
        
    end function root_fr

    module function next_root_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        if (nroots > 0) then
            if (current_root == nroots) then
                current_root = 1
            else
                current_root = current_root + 1
            end if
            r = roots(current_root)
        else
            r = a
        end if
    end function next_root_fr

    module function previous_root_fr(a) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t) :: r
        if (nroots > 0) then
            if (current_root == 1) then
                current_root = nroots
            else
                current_root = current_root - 1
            end if
            r = roots(current_root)
        else
            r = a
        end if
    end function previous_root_fr

    module function atangent2_fr(a, b) result(r)
        type(rpn_t), intent(in) :: a
        type(rpn_t), intent(in) :: b
        type(rpn_t) :: r
        r%zdata = atan2(real(a%zdata),real(b%zdata)) * merge(to_deg,1.0d0,degrees_mode)
    end function atangent2_fr
    
    module function round(x) result(r)
        real(real64), intent(in) :: x
        real(real64) :: r
        if (abs(x) < eps) then
            r = 0
        else
            r = x
        end if
    end function round

    module subroutine set_places(n)
        integer, intent(in) :: n
        if (n == 0) then
            f_small = 'i0'
        else
            write(f_small,'(2(a,i0),a)')  'f',0,'.',n
            write(f_large,'(2(a,i0),a)') 'en',10+n,'.',n
        end if
        dec_places = n
    end subroutine set_places
    
    module function get_places() result(r)
        integer :: r
        r = dec_places
    end function get_places
    
end submodule rpn_sm

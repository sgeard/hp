submodule (calc_state) calc_state_sm
    implicit none

contains

    ! ---- public API ---------------------------------------------------------

    module subroutine state_init()
        real(8), parameter :: ag = 9.80665d0    ! Acceleration due to gravity (g)
        real(8), parameter :: g  = 6.67430d-11  ! Gravitational constant (G)
        real(8), parameter :: e  = exp(1.0d0)   ! 'e'
        real(8), parameter :: c  = 2.99792458d8 ! Speed of light (m/s)
        integer            :: stat
        logical            :: lang_en
        character(5)       :: lang

        call stack%set_legend(['x:','y:','z:','s:','t:'])
        degrees_mode = .true.
        complex_mode = .false.
        eps = 1.0d-14

        ! Constants
        call constants%set('g',ag)
        call constants%set('G',g)
        call constants%set('e',e)
        call constants%set('c',c)
        call constants%set('pi',pi)
        call constants%set('two_pi',2*pi)
        call constants%set('pi_over_2',pi/2)

        ! Try to read the LANG environment variable
        call get_environment_variable('LANG',lang,status=stat)
        lang_en = stat /= 0
        if (.not. lang_en) then
            lang_en = merge(.true.,.false.,lang(1:3) == 'en_')
        end if
        lang = merge('POINT','COMMA',lang_en)

        call rpn_s_init(lang)
    end subroutine state_init

    module subroutine show_stack()
        call stack%print(veMode)
    end subroutine show_stack

    module subroutine report_after_arg()
        ! Do not print the stack at the end of a sequence - it's confusing
        if (in_sequence /= 2) then
            call stack%print(veMode)
        end if
    end subroutine report_after_arg

    module subroutine report_after_line()
        if (in_sequence == 1) then
            write(6,'(i0)') n_seq
        else if (in_sequence == 2) then
            in_sequence = 0
        else
            call stack%print(veMode)
        end if
    end subroutine report_after_line

    module subroutine set_verbosity(level)
        integer, intent(in) :: level
        verbosity = level
    end subroutine set_verbosity

    module subroutine set_verbose_mode(on)
        logical, intent(in) :: on
        veMode = on
    end subroutine set_verbose_mode

    module subroutine set_complex_mode(on)
        logical, intent(in) :: on
        complex_mode = on
    end subroutine set_complex_mode

    module function is_verbose() result(r)
        logical :: r
        r = verbosity > 0
    end function is_verbose

    module subroutine help()
        write(6,'(/a)') 'Command Calculator'
        write(6,'(a/)') '=================='
        write(6,'(a)')  'Introduction'
        write(6,'(a/)') '------------'
        write(6,'(a)')  'This is a command-line calculator. It supports both real and complex modes, as well'
        write(6,'(a)')  'as degrees/radians selection and precision control. It can be run interactively or as an'
        write(6,'(a/)') 'expression parser.'
        write(6,'(a/)') '-------------------------------------------------------------------------------'
        write(6,'(a)')  'Command line options'
        write(6,'(a)')  '       -c : start in complex mode'
        write(6,'(a)')  '       -v : start in verbose mode (the stack is shown after every action)'
        write(6,'(a)')  '      -np : do not display a prompt'
        write(6,'(a/)') '       -d : display actions, useful for debugging'
        write(6,'(a/)') '-------------------------------------------------------------------------------'
        write(6,'(a)')  'Operators : + - * / ^ ^/x ^x ^2 ^/2 ^3 ^/3 ^*2 ^*10 || ! %'
        write(6,'(a)')  'Constants : pi e g G c two_pi pi_over_2'
        write(6,'(a)')  'Functions : sin cos tan asin acos atan sinh cosh tanh log2 log lg len sq sqrt cb cbrt'
        write(6,'(a)')  '            alog2 alog alog10 gamma ncr npr rem int nint'
        write(6,'(a)')  '            W (LambertW function) if -1/e < x < 0 W0 is placed in the x and W-1 in y'
        write(6,'(a)')  ' Controls : fix[0-9] clx cl cla ss'
        write(6,'(a)')  '    Modes : real complex verbose terse degrees radians'
        write(6,'(a)')  ' Memories : n=0...9  st<n> sw<n> rc<n> cl<n> m<n>+ m<n>- m<n>* m<n>/ msh'
        write(6,'(a)')  '  Complex : ri _ || to_pol to_cart'
        write(6,'(a)')  '  Actions : 1/ -- R r ? > < split drop'
        write(6,'(a)')  '    Stats : { x1 x2 ... } { x1,y1 x2,y2 ... }'
        write(6,'(a)')  '            n ux sx mx lqx uqx uy sy my lqy uqy a b cov corr'
        write(6,'(a/)') '    Quits : = q'
        write(6,'(a/)') '-------------------------------------------------------------------------------'
        write(6,'(a)')  'Examples'
        write(6,'(a)')  '--------'
        write(6,'(4x,a)')  'hp "fix2 18 2 - 8 2 / * ="                    -> 64.00'
        write(6,'(4x,a)')  'hp "2 -- complex sqrt ="                      -> (0.000000,-1.414214)'
        write(6,'(4x,a/)') 'hp -c "radians (1,pi_over_2)p ^ * degrees ="  -> (1.000000,180.000000) p'

    end subroutine help

    module subroutine apply_command(command, ok)
        use, intrinsic :: ieee_arithmetic
        character(*), intent(in) :: command
        logical, intent(out)     :: ok

        real(8)                 :: r, im
        complex(8)              :: z
        real(8), allocatable    :: tmp_seq(:)
        type(rpn_t)             :: us, zs
        integer                 :: m, idx
        integer, parameter      :: mem_block_size = 10

        real(8)                 :: x

        ok = .true.
        if (len_trim(command) == 0) then
            return
        end if

        if (verbosity > 0) then
            write(*,'(a)') 'Applying: '//command
        end if

        if (in_sequence == 1) then
            if (command == '}') then
                in_sequence = 2
                complex_mode = tmp_cmode
                call calculate_stats

            else
                ! All elements must be the same so either all x or all x,y
                idx = index(command,',')
                if (n_seq == 0) then
                    seq_is_x = (idx == 0)
                end if
                if (seq_is_x .neqv. (idx == 0)) then
                    goto 901
                end if
                if (seq_is_x) then
                    read(command,*,err=901,end=901) r
                    im = 0
                else
                    ! a pair is exactly  x,y  -- reject a second comma or a missing half
                    if (index(command(idx+1:),',') /= 0) goto 901
                    read(command(1:idx-1),*,err=901,end=901) r
                    read(command(idx+1:),*,err=901,end=901) im
                end if
                ! Initial allocation
                if (n_seq == 0 .and. .not. allocated(x_seq)) then
                    allocate(x_seq(mem_block_size))
                    if (.not. seq_is_x) then
                        allocate(y_seq(mem_block_size))
                    end if
                end if

                if (n_seq == size(x_seq)) then
                    ! Expand array
                    allocate(tmp_seq(n_seq + mem_block_size))
                    tmp_seq(1:n_seq) = x_seq
                    call move_alloc(tmp_seq, x_seq)
                    if (.not. seq_is_x) then
                        allocate(tmp_seq(n_seq + mem_block_size))
                        tmp_seq(1:n_seq) = y_seq
                        call move_alloc(tmp_seq, y_seq)
                   end if
                end if
                n_seq = n_seq + 1
                x_seq(n_seq) = r
                if (.not. seq_is_x) then
                    y_seq(n_seq) = im
                end if
                if (verbosity > 0) then
                    print *,x_seq(1:n_seq)
                end if
            end if
            return
        end if

        select case(command)

        case('q','quit')
           ok = .false.
           return

        case('=')
           ok = .false.
           return

        case ('ss')
            call stack%print(.true.)

        case('{')
            ! Start sequence
            in_sequence = 1
            n_seq = 0
            ! Fresh arrays per sequence: an x-only sequence must not inherit a
            ! missing/short y_seq from an earlier pair sequence (or vice versa)
            if (allocated(x_seq)) deallocate(x_seq)
            if (allocated(y_seq)) deallocate(y_seq)
            tmp_cmode = complex_mode
            complex_mode = .false.
            call stats%clear()

        case('--')
            call invoke_unary(chs_fr, command)

        case('^')
            if (.not. require_operands(1, command)) return
            call stack%push(stack%peek(1))

        case('+')
            call invoke_binary(add_fr, command)

        case('-','−')
            call invoke_binary(subtract_fr, command)

        case('*','×')
            call invoke_binary(multiply_fr, command)

        case('/','÷','∕')
            call invoke_binary(divide_fr, command)

        case('^x')
            call invoke_binary(power_fr, command)

        case('^/x')
            ! Only raising to a real power is supported
            zs = stack%peek(1)
            if (zs%is_real()) then
                call invoke_binary(root_fr, command)
            else
                goto 901
            end if

        case('>')
            call invoke_unary(next_root_fr, command)

        case('<')
            call invoke_unary(previous_root_fr, command)

        case('%')
            call invoke_binary(percent_fr, command)

        case('xy','XY')
           if (.not. require_operands(2, command)) return
           call stack%swap

        case('R')
           call stack%rotate_down

        case('r')
           call stack%rotate_up

        case('CLA','cla')
            call stack%clear
            mem = rpn_t()
            call stats%clear

        case('CL','cl')
            call stack%clear


        case('CLX','clx')
           call stack%set(rpn_t())

        case('_')
            if (complex_mode) then
                call invoke_unary(conj_fr, command)
            endif

        case('len','||')
            if (complex_mode) then
                call invoke_unary(len_fr, command)
                ! Length is always reported as (x,0) and marked is_cartesian
                zs = stack%peek(1)
                call zs%set_value(is_cartesian=.true.)
                call stack%set(zs,1)
            else
                call invoke_binary(hypot_fr, command)
            end if

        case('split')
            if (.not. require_operands(1, command)) return
            if (.not. complex_mode) then
                zs = stack%pop()
                z = zs%get_value()
                x = z%re
                if (x > 0) then
                    r = floor(x)
                else
                    r = ceiling(x)
                end if
                im = x - r
                call stack%push(im)
                call stack%push(r)
            end if

        case('int')
            if (.not. require_operands(1, command)) return
            if (.not. complex_mode) then
                zs = stack%peek(1)
                z = zs%get_value()
                x = z%re
                if (x > 0) then
                    r = floor(x)
                else
                    r = ceiling(x)
                end if
                call zs%set_value(cmplx(r,0,8))
                call stack%set(zs,1)
            end if

        case('nint')
            if (.not. require_operands(1, command)) return
            if (.not. complex_mode) then
                zs = stack%peek(1)
                z = zs%get_value()
                x = z%re
                ! Nearest integer, an exact .5 tie rounding to the even neighbour
                r = nint(x)
                if (abs(x - aint(x)) == 0.5d0 .and. mod(abs(r),2.0d0) == 1) then
                    r = r - sign(1.0d0,r)
                end if
                call zs%set_value(cmplx(r,0,8))
                call stack%set(zs,1)
            end if

        case('rem')
            if (.not. require_operands(1, command)) return
            if (.not. complex_mode) then
                zs = stack%peek(1)
                z = zs%get_value()
                x = z%re
                if (x > 0) then
                    r = floor(x)
                else
                    r = ceiling(x)
                end if
                im = x - r
                call zs%set_value(cmplx(im,0,8))
                call stack%set(zs,1)
            end if

        case('drop')
           if (.not. require_operands(1, command)) return
           zs = stack%pop()

        case('ri')
           ! Swap real and imaginary parts
            if (complex_mode) then
                call invoke_unary(swap_real_imaginary_fr, command)
            end if

        case('to_pol')
            ! Convert x + iy to r + i theta
            if (complex_mode) then
                if (.not. require_operands(1, command)) return
                zs = stack%peek(1)
                call stack%set(to_polar(zs))
            end if

        case('to_cart')
            ! Convert (r,theta) to (x,y)
            if (complex_mode) then
                if (.not. require_operands(1, command)) return
                zs = stack%peek(1)
                call stack%set(to_cartesian(zs))
            end if

        case('1/')
            call invoke_unary(reciprocal_fr, command)

        case('^2','sq')
            call invoke_unary(power_2_fr, command)

        case('^/2','sqrt','√')
            call invoke_unary(sqrt_fr, command)

        case('^3','cb')
            call invoke_unary(power_3_fr, command)

        case('^/3','cbrt')
            call invoke_unary(cbrt_fr, command)

        case('^*2','alog2')
            call invoke_unary(exp_2_fr, command)

        case('^*10','alog10')
            call invoke_unary(exp_10_fr, command)

        case('exp','alog')
            call invoke_unary(exp_e_fr, command)

        case('ln')
            call invoke_unary(ln_fr, command)

        case('log2')
            call invoke_unary(log2_fr, command)

        case('lg')
            call invoke_unary(lg_fr, command)

        case('sinh')
            call invoke_unary(hsine_fr, command)

        case('cosh')
            call invoke_unary(hcosine_fr, command)

        case('tanh')
            call invoke_unary(htangent_fr, command)

        case('sin')
            call invoke_unary(sine_fr, command)

        case('cos')
            call invoke_unary(cosine_fr, command)

        case('tan')
            call invoke_unary(tangent_fr, command)

        case('asin')
            call invoke_unary(asine_fr, command)

        case('asinh')
            call invoke_unary(ahsine_fr, command)

        case('acos')
            call invoke_unary(acosine_fr, command)

        case('acosh')
            call invoke_unary(ahcosine_fr, command)

        case('atan')
            call invoke_unary(atangent_fr, command)

        case('atanh')
            call invoke_unary(ahtangent_fr, command)

        case('atan2')
            call invoke_binary(atangent2_fr, command)

        case('gamma')
            call invoke_unary(gamma_fr, command)

        case('W')
            ! Can be two results so need to do something special have_expression
            block
                real(8), allocatable :: r(:)
                complex(8) :: cv
                if (.not. require_operands(1, command)) return
                if (.not. complex_mode) then
                    zs = stack%peek(1)
                    cv = zs%get_value()
                    if (abs(cv%re + exp(-1.0d0)) < 1.0d-8) then
                        call zs%set_value(cmplx(-1.0d0,0,8))
                        call stack%set(zs,1)
                    else if (abs(cv%re) < 1.0d-9) then
                        call zs%set_value(cmplx(0.0d0,0,8))
                        call stack%set(zs,1)
                    else if (cv%re > -exp(-1.0d0)) then
                        r = w_fr(zs)
                        if (size(r) == 2) then
                            ! Second root to y via a real push so the stack
                            ! count includes it, first root replaces x
                            call zs%set_value(cmplx(r(2),0,8))
                            call stack%set(zs,1)
                            call zs%set_value(cmplx(r(1),0,8))
                            call stack%push(zs)
                        else
                            call zs%set_value(cmplx(r(1),0,8))
                            call stack%set(zs,1)
                        end if
                    else
                        write(*,'(a)') '***Error: argument out of range, must be > -1/e'
                    end if
                end if
            end block

        case('!')
            if (.not. require_operands(1, command)) return
            zs = stack%peek(1)
            if (zs%is_positive_real()) then
                call invoke_unary(fact_fr, command)
            else
                goto 901
            end if

        case('ncr')
            if (.not. require_operands(2, command)) return
            zs = stack%peek(1)
            us = stack%peek(2)
            if (zs%is_positive_real() .and. us%is_positive_real()) then
                call invoke_binary(ncr_fr, command)
            else
                goto 901
            end if

        case('npr')
            if (.not. require_operands(2, command)) return
            zs = stack%peek(1)
            us = stack%peek(2)
            if (zs%is_positive_real() .and. us%is_positive_real()) then
                call invoke_binary(npr_fr, command)
            else
                goto 901
            end if

        case('m0+','m1+','m2+','m3+','m4+','m5+','m6+','m7+','m8+','m9+')
           if (.not. require_operands(1, command)) return
           read(command(2:2),'(i1)',err=901) m
           mem(m) = mem(m) + stack%peek(1)

        case('m0-','m1-','m2-','m3-','m4-','m5-','m6-','m7-','m8-','m9-')
           if (.not. require_operands(1, command)) return
           read(command(2:2),'(i1)',err=901) m
           mem(m) = mem(m) - stack%peek(1)

        case('m0*','m1*','m2*','m3*','m4*','m5*','m6*','m7*','m8*','m9*')
           if (.not. require_operands(1, command)) return
           read(command(2:2),'(i1)',err=901) m
           mem(m) = mem(m) * stack%peek(1)

        case('m0/','m1/','m2/','m3/','m4/','m5/','m6/','m7/','m8/','m9/')
           if (.not. require_operands(1, command)) return
           read(command(2:2),'(i1)',err=901) m
           mem(m) = mem(m) / stack%peek(1)

        case('st0','st1','st2','st3','st4','st5','st6','st7','st8','st9')
           if (.not. require_operands(1, command)) return
           read(command(3:3),'(i1)',err=901) m
           mem(m) = stack%peek(1)

        case('sw0','sw1','sw2','sw3','sw4','sw5','sw6','sw7','sw8','sw9')
           if (.not. require_operands(1, command)) return
           read(command(3:3),'(i1)',err=901) m
           zs = stack%peek(1)
           call stack%set(mem(m))
           mem(m) = zs

        case('rc0','rc1','rc2','rc3','rc4','rc5','rc6','rc7','rc8','rc9')
           read(command(3:3),'(i1)',err=901) m
           call stack%push(mem(m))

        case('cl0','cl1','cl2','cl3','cl4','cl5','cl6','cl7','cl8','cl9')
           read(command(3:3),'(i1)',err=901) m
           mem(m) = rpn_t()

        case('msh')
            block
                integer :: i
                write(6,'(i3,a,dt)') (i,': ',mem(i),i=0,size(mem)-1)
            end block

        case('fix0','fix1','fix2','fix3','fix4','fix5','fix6','fix7','fix8','fix9')
           read(command(4:4),'(i1)') m
           call set_places(m)

        case('DEG','deg','DEGREES','degrees')
            call toggle_degrees_mode(.true.)

        case('RAD','rad','RADIANS','radians')
            call toggle_degrees_mode(.false.)

        case('mC','COMPLEX','complex')
           complex_mode = .true.

        case('mR','REAL','real')
           complex_mode = .false.

        case('mV','VERBOSE','verbose')
           veMode = .true.

        case('mT','TERSE','terse')
           veMode = .false.

        case('?')
           write(6,advance='no',fmt='(a)') 'Status: '
           write(6,advance='no',fmt='(2a)') merge('degrees','radians',degrees_mode),' ; '
           write(6,advance='no',fmt='(a,i0)') 'dp = ',get_places()
           if (complex_mode) then
              write(6,advance='no',fmt='(a)') ' ; mode = complex'
           else
              write(6,advance='no',fmt='(a)') ' ; mode = real'
           end if
           write(6,advance='no',fmt='(a,i0)') ' ; stack size = ',stack%get_size()
           write(6,'(a/)') ''

        case('help','h')
           call help

        case default
            ! Process constants first
            block
                integer :: lc,split_idx,end_idx
                logical :: is_integer
                character(len=:), allocatable :: re_comp, im_comp
                lc = len_trim(command)
                is_integer = (index(command,'.') == 0)
                if (complex_mode) then
                    if (command(1:1) == '(') then
                        split_idx = index(command,',')
                        end_idx = index(command,')')
                        re_comp = command(2:split_idx-1)
                        im_comp = command(split_idx+1:end_idx-1)
                        if (constants%contains(re_comp)) then
                            z%re = constants%get_value(re_comp)
                        else
                            read(re_comp,*,err=901,end=901) z%re
                        end if
                        if (constants%contains(im_comp)) then
                            z%im = constants%get_value(im_comp)
                        else
                            read(im_comp,*,err=901,end=901) z%im
                        end if
                        if (command(lc:lc) == 'p') then
                            call stack%push(z,.false.)
                        else
                            call stack%push(z)
                        end if
                    else
                        if (constants%contains(command)) then
                            x = constants%get_value(command)
                        else
                            read(command,*,err=901,end=901) x
                        end if
                        call stack%push(cmplx(x,0.0d0,8))
                    end if

                else
                    if (constants%contains(command)) then
                        x = constants%get_value(command)
                    else if (stats%contains(command)) then
                        x = stats%get_value(command)
                    else
                        read(command,*,err=901,end=901) x
                    end if
                    call stack%push(cmplx(x,0.0d0,8))
                end if
            end block
        end select
        return

901 continue
        write(6,'(a)') command//' ???'
        return

    end subroutine apply_command

    subroutine calculate_stats
        real(8) :: a, b, c, sxy
        real(8) :: s(5,2)

        if (n_seq == 0) then
            write(6,'(a)') 'empty set { } -- no statistics'
            return
        end if

        call summary_stats(x_seq(1:n_seq),s(1,1),s(2,1),s(3,1),s(4,1),s(5,1))
        call stats%set('n',real(n_seq,8))
        call stats%set('ux',s(1,1))
        call stats%set('mx',s(2,1))
        call stats%set('sx',s(3,1))
        call stats%set('lqx',s(4,1))
        call stats%set('uqx',s(5,1))
        if (seq_is_x) then
                 write(6,10) '    count n -> ',n_seq
            call print_value('    mean ux -> ',s(1,1))
            call print_value('  stddev sx -> ',s(3,1))
            call print_value('  median mx -> ',s(2,1))
            call print_value('lower_q lqx -> ',s(4,1))
            call print_value('upper_q uqx -> ',s(5,1))
       else
            call summary_stats(y_seq(1:n_seq),s(1,2),s(2,2),s(3,2),s(4,2),s(5,2))
                 write(6,10) '    count n        -> ',n_seq
            call print_value('    means ux , uy  -> ',s(1,1),s(1,2))
            call print_value('  stddevs sx , xy  -> ',s(3,1),s(3,2))
            call print_value('  medians mx , my  -> ',s(2,1),s(2,2))
            call print_value('lower_qs lqx , lqy -> ',s(4,1),s(4,2))
            call print_value('upper_qs uqx , uqy -> ',s(5,1),s(5,2))
            call stats%set('uy',s(1,2))
            call stats%set('my',s(2,2))
            call stats%set('sy',s(3,2))
            call stats%set('lqy',s(4,2))
            call stats%set('uqy',s(5,2))

            if (n_seq >= 2) then
                call calculate_regression(s(1,1),s(1,2),a,b,c,sxy)
                call stats%set('a',a)
                call stats%set('b',b)
                call stats%set('corr',c)
                call stats%set('cov',sxy)
                write(6,'(/a)') 'Regression:  y = ax + b'
                call print_value('   gradient a      ->',a)
                call print_value('  intercept b      -> ',b)
                call print_value(' covariance cov    -> ',sxy)
                call print_value('correlation corr   -> ',c)
            else
                write(6,'(/a)') 'Regression: needs at least 2 points'
            end if
        end if
        10 format(a,i0)

    end subroutine calculate_stats

    subroutine calculate_regression(mean_x, mean_y, a, b, c, sxy)
        real(8), intent(in) :: mean_x, mean_y
        real(8), intent(out) :: a, b, c, sxy
        real(8) :: sxx, syy
        sxy = sum(x_seq(1:n_seq)*y_seq(1:n_seq))/n_seq - mean_x*mean_y
        sxx = sum(x_seq(1:n_seq)*x_seq(1:n_seq))/n_seq - mean_x**2
        syy = sum(y_seq(1:n_seq)*y_seq(1:n_seq))/n_seq - mean_y**2
        a = sxy/sxx
        b = mean_y - a*mean_x
        c = sxy/sqrt(sxx*syy)
    end subroutine calculate_regression

    subroutine print_value(name, x, y)
        character(len=*), intent(in)  :: name
        real(8), intent(in)           :: x
        real(8), intent(in), optional :: y
        character(len=:), allocatable :: fmt_x, fmt_y
        call to_string(x, fmt_x)
        if (present(y)) then
            call to_string(y, fmt_y)
            fmt_x = fmt_x//' , '//fmt_y
        end if
        write(6,'(a)') name//fmt_x
    end subroutine print_value

    subroutine summary_stats(a, mean, median, stddev, lower_q, upper_q)
        real(8), intent(in)  :: a(:)
        real(8), intent(out) :: mean, median, stddev, lower_q, upper_q
        real(8) :: b(size(a))
        real(8) :: s, s2
        integer :: m, n
        n = size(a)
        b = a
        s = sum(b)
        s2 = sum(b**2)
        mean = s/n
        stddev = sqrt(s2/n - (s/n)**2)
        call sort(b)
        median = calc_median(b, m)
        if (n < 3) then
            lower_q = median
            upper_q = median
        else
            lower_q = calc_median(b(1:m))
            if (mod(n,2) == 0) then
                upper_q = calc_median(b(m+1:n))
            else
                upper_q = calc_median(b(m:n))
            end if
        end if
    end subroutine summary_stats

    function calc_median(a, mid) result(r)
        real(8), intent(in)  :: a(:)
        integer, intent(out), optional :: mid
        real(8) :: r
        integer :: m, n
        n = size(a)
        m = n/2
        if (mod(n,2) == 0) then
            r = (a(m) + a(m+1))/2.0d0
        else
            m = m + 1
            r = a(m)
        end if
        if (present(mid)) then
            mid = m
        end if
    end function calc_median

    ! 'a' won't be very big so a simple n**2 algorithm will do
    subroutine sort(a)
        real(8), intent(inout) :: a(:)
        real(8) :: b(size(a))
        integer :: i, j
        logical :: mask(size(a))
        mask = .true.
        b = a
        do i=1,size(a)
            ! dim=1 gives the scalar location of the minimum unmasked element
            j = minloc(b, dim=1, mask=mask)
            a(i) = b(j)
            mask(j) = .false.
        end do
    end subroutine

    subroutine toggle_degrees_mode(new_mode)
        logical, intent(in) :: new_mode
        integer    :: i
        type(rpn_t) :: rz

        ! Only do something if the modes are different
        if (new_mode .eqv. degrees_mode) return

        degrees_mode = .not. degrees_mode

        ! Convert all polar complex numbers
        ! 1) In the stack
        do i=1,stack%get_size()
            rz = stack%peek(i)
            call update_angle_unit(rz)
            call stack%set(rz,i)
        end do

        ! 2) in memory
        do i=lbound(mem,1),ubound(mem,1)
            call update_angle_unit(mem(i))
        end do

        ! 3) in multiple roots
        do i=1,nroots
            call update_angle_unit(roots(i))
        end do

    end subroutine toggle_degrees_mode

    subroutine update_angle_unit(rz)
        type(rpn_t), intent(inout) :: rz
        complex(8) :: zs
        logical    :: is_cart
        zs = rz%get_value(is_cart)
        if (is_cart) return
        zs%im = zs%im*merge(to_deg, to_rad, degrees_mode)
        call rz%set_value(zs,is_cart)
    end subroutine update_angle_unit

    ! Stack-underflow guard: report like an unknown command and leave the
    ! stack untouched so the following stack dump shows the unchanged state
    function require_operands(n, command) result(r)
        integer, intent(in)      :: n
        character(*), intent(in) :: command
        logical :: r
        r = stack%get_size() >= n
        if (.not. r) then
            if (n == 1) then
                write(6,'(a)') command//' ??? needs 1 operand'
            else
                write(6,'(a,i0,a)') command//' ??? needs ',n,' operands'
            end if
        end if
    end function require_operands

    subroutine invoke_binary(action, command)
        procedure(binary_f), pointer, intent(in) :: action
        character(*), intent(in) :: command
        type(rpn_t) :: us, zs
        logical :: is_cart

        if (.not. require_operands(2, command)) return
        zs = stack%pop()
        if (complex_mode) then
            is_cart = zs%is_cartesian()
            if (.not. is_cart) then
                zs = to_cartesian(zs)
            end if
            us = stack%peek(1)
            if (.not. us%is_cartesian()) then
                us = to_cartesian(us)
                ! Either operand being polar keeps the result polar
                is_cart = .false.
            end if
            us = action(us,zs)
            if (.not. is_cart) then
                us = to_polar(us)
            end if
            call stack%set(us)
        else
            us = stack%peek(1)
            call stack%set(action(us,zs))
        end if
    end subroutine invoke_binary

    subroutine invoke_unary(action, command)
        procedure(unary_f), pointer, intent(in) :: action
        character(*), intent(in) :: command
        logical :: is_cart
        type(rpn_t) :: z
        if (.not. require_operands(1, command)) return
        if (complex_mode) then
            z = stack%peek(1)
            is_cart = z%is_cartesian()
            if (.not. is_cart) then
                z = to_cartesian(z)
            end if
            z = action(z)
            if (.not. is_cart) then
                z = to_polar(z)
            end if
            call stack%set(z)
        else
            call stack%set(action(stack%peek(1)))
        end if
    end subroutine invoke_unary

end submodule calc_state_sm

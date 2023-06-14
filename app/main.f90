program hp15c
    use rpn_stack
    use linked_list, print_ll => print, clear_ll => clear, size_ll => size
    use amap
    
    implicit none
    
    real(real64)                   :: x
    integer                   :: ios, i
    integer                   :: verbosity = 0
    character(100)            :: buff
    integer                   :: blen
    integer                   :: argl, argc
    type(llist)               :: tokens

    real(real64), parameter :: ag = 9.80665d0
    real(real64), parameter :: g = 6.67430d-11
    real(real64), parameter :: e = exp(1.0d0)
    real(real64), parameter :: c = 2.99792458d8
    type(amap_t)       :: constants
    
    type(amap_t)       :: stats
    integer            :: in_sequence = 0
    logical            :: seq_is_x
    real(real64), allocatable  :: x_seq(:), y_seq(:)
    integer            :: n_seq = 0
    
    logical      :: veMode = .false.
    logical      :: tmp_cmode
    logical      :: ok
    logical      :: have_expression
    integer      :: stat
    character(len=100) :: msg

    type(rpn_t)  :: mem(0:9) = rpn_t()
    
    ! Create a stack of size 5
    type(stack_t(5)) :: stack

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

    ! Decimal places
    call set_places(dec_places)
        
    ! Interrogate argument list
    argc = command_argument_count()
    have_expression = .false.
    do i=1,argc
        call get_command_argument(i, buff, argl)
        if (buff(1:argl) == '-d') then
            verbosity = 1
            cycle
            
        else if (buff(1:argl) == '-c') then
            complex_mode = .true.
            cycle
            
        else if (buff(1:argl) == '-v') then
            veMode = .true.
            cycle
        
        else if (buff(1:argl) == '-h') then
            call help
            stop
       
        end if

        have_expression = .true.
        
        ! Break the string up into a linked-list of tokens
        call tokenize(buff(1:argl))
        if (verbosity > 0) call print_ll(tokens)
        
        ! Interpret each token as a command and appky it
        ok = tokens%iterate(apply_command)
        
        ! Do not print the stack at the end of a sequence -it's confusing
        if (in_sequence /= 2) then
            call stack%print(veMode)
        end if
        
        ! Tidy
        call clear_ll(tokens)

        if (.not. ok) stop
        
    end do
    if (.not. have_expression) then
        call stack%print(veMode)
    end if
    
    ! Loop until quit
    all :do
        x = 0.0d0
        buff = ''
        write(6,'(a)',advance='no') ':: '
        read(5,fmt='(a)',iostat=ios,iomsg=msg) buff
        if (ios /= 0) then
            write(6,'(/a)') 'Command:['//buff(1:blen)//']'//'; '//msg
            cycle all
        end if
        buff = trim(adjustl(buff))
        blen = len_trim(buff)
        if (blen == 0) cycle all

        ! Tokenize input string
        call tokenize(buff(1:blen))
        ok = tokens%iterate(apply_command)
        if (.not. ok) exit all
        
        if (in_sequence == 1) then
            write(6,'(i0)') n_seq
        else if (in_sequence == 2) then
            in_sequence = 0
        else
            call stack%print(veMode)
        end if
    end do all

    call clear_ll(tokens)
    stop

contains
       
  subroutine tokenize(com)
    character(*), intent(in)      :: com
    integer                       :: start, end
    character(len=:), allocatable :: command
    
    call clear_ll(tokens)
    if (len_trim(com) == 0) then
        return
    end if
    start = 1
    ! Ensure there are no leading and trailing spaces
    command = trim(adjustl(com))
    end = index(command,' ')
    end = merge(len(command),end-1,end==0)
    do
        call append(tokens,command(start:end))
        if (end == len(command)) exit
        start = end + nsp(command(end+1:))
        end = index(command(start:),' ') - 1
        end = merge(len(command),end+start-1,end == -1)
     end do
  end subroutine tokenize

  subroutine help
    write(6,'(/a)') 'Command Calculator'
    write(6,'(a/)') '=================='
    write(6,'(a)')  'Introduction'
    write(6,'(a/)') '------------'
    write(6,'(a)')  'This is a command-line calculator. It supports both real and complex modes, as well'
    write(6,'(a)')  'as degrees/radians selection and precision control. It can be run interactively or as an'
    write(6,'(a/)') 'expression parser. This help is deliberately terse to encourage exploration.'
    write(6,'(a/)') '-------------------------------------------------------------------------------'
    write(6,'(a)')  'Operators: + - * / ^ ^/x ^x ^2 ^/2 ^3 ^/3 ^*2 ^*10 || ! %'
    write(6,'(a)')  'Constants: pi e g G c two_pi pi_over_2'
    write(6,'(a)')  'Functions: sin cos tan asin acos atan sinh cosh tanh log2 log lg len sq sqrt cb cbrt'
    write(6,'(a)')  '           alog2 alog alog10 gamma ncr npr rem int nint'
    write(6,'(a)')  ' Controls: fix[0-9] clx cl cla '
    write(6,'(a)')  '    Modes: real complex verbose terse degrees radians'
    write(6,'(a)')  ' Memories: n=0...9  st<n> sw<n> rc<n> cl<n> m<n>+ m<n>- m<n>* m<n>/ msh'
    write(6,'(a)')  '  Complex: ri _ || to_pol to_cart'
    write(6,'(a)')  '  Actions: 1/ -- R r ? > < split drop'
    write(6,'(a)')  '    Stats: { x1 x2 ... } { x1,y1 x2,y2 ... }'
    write(6,'(a)')  '           n ux sx mx lqx uqx uy sy my lqy uqy a b cov corr'
    write(6,'(a/)') '    Quits: q ='
    write(6,'(a/)') '-------------------------------------------------------------------------------'
    write(6,'(a)')  'Examples'
    write(6,'(a)')  '--------'
    write(6,'(4x,a)')  'hp "fix2 18 2 - 8 2 / * ="                    -> 64.00'
    write(6,'(4x,a)')  'hp "2 -- complex sqrt ="                      -> (0.00000,-1.414214)'
    write(6,'(4x,a/)') 'hp -c "radians (1,pi_over_2)p ^ * degrees ="  -> (1.000000,180.000000) p'

  end subroutine help

  subroutine apply_command(command, ok)
    use, intrinsic :: ieee_arithmetic
    implicit none
    character(*), intent(in) :: command
    logical, intent(out)     :: ok

    real(real64)                 :: r, im
    complex(8)              :: u, z
    real(real64), allocatable    :: tmp_seq(:)
    type(rpn_t)             :: us, zs
    integer                 :: m, idx
    
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
                read(command,*,err=901) r
                im = 0
            else
                read(command(1:idx-1),*,err=901) r
                read(command(idx+1:len(command)),*,err=901) im
            end if
            ! Initial allocation
            if (n_seq == 0 .and. .not. allocated(x_seq)) then
                allocate(x_seq(10))
                if (.not. seq_is_x) then
                    allocate(y_seq(10))
                end if
            end if
            
            if (n_seq < size(x_seq)) then
                n_seq = n_seq + 1
            else
                ! Expand array
                allocate(tmp_seq(n_seq + 10))
                tmp_seq(1:n_seq) = x_seq
                call move_alloc(tmp_seq, x_seq)
                if (.not. seq_is_x) then
                    allocate(tmp_seq(n_seq + 10))
                    tmp_seq(1:n_seq) = y_seq
                    call move_alloc(tmp_seq, y_seq)
               end if
            end if
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
       
    case('{')
        ! Start sequence
        in_sequence = 1
        n_seq = 0
        tmp_cmode = complex_mode
        complex_mode = .false.
        call stats%clear()
        
    case('--')
        call invoke_unary(chs_fr)
       
    case('^')
        call stack%push(stack%peek(1))
       
    case('+')
        call invoke_binary(add_fr)
       
    case('-')
        call invoke_binary(subtract_fr)
       
    case('*')
        call invoke_binary(multiply_fr)
       
    case('/')
        call invoke_binary(divide_fr)
       
    case('^x')
        call invoke_binary(power_fr)
       
    case('^/x')
        ! Only raising to a real power is supported
        zs = stack%peek(1)
        if (zs%is_real()) then
            call invoke_binary(root_fr)
        else
            goto 901
        end if
        
    case('>')
        call invoke_unary(next_root_fr)
       
    case('<')
        call invoke_unary(previous_root_fr)
        
    case('%')
        call invoke_binary(percent_fr)
        
    case('xy','XY')
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
            call invoke_unary(conj_fr)
        endif

    case('len','||')
        if (complex_mode) then
            call invoke_unary(len_fr)
            ! Length is always reported as (x,0) and marked is_cartesian
            zs = stack%peek(1)
            call zs%set_value(is_cartesian=.true.)
            call stack%set(zs,1)
        else
            zs = stack%peek(1)
            z = zs%get_value()
            us = stack%peek(2)
            u = us%get_value()
            call zs%set_value(cmplx(hypot(z%re,u%re),0,8))
            call stack%set(zs)
        end if
        
    case('split')
        if (.not. complex_mode) then
            zs = stack%pop()
            x = zs%get_value()
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
        if (.not. complex_mode) then
            zs = stack%peek(1)
            x = zs%get_value()
            if (x > 0) then
                r = floor(x)
            else
                r = ceiling(x)
            end if
            call zs%set_value(cmplx(r,0,8))
            call stack%set(zs,1)
        end if
         
    case('nint')
        if (.not. complex_mode) then
            zs = stack%peek(1)
            x = zs%get_value()
            r = nint(x)
            if (mod(r,2.0d0) == 1) then
                r = r - 1
            end if
            call zs%set_value(cmplx(r,0,8))
            call stack%set(zs,1)
        end if
         
    case('rem')
        if (.not. complex_mode) then
            zs = stack%peek(1)
            x = zs%get_value()
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
       zs = stack%pop()
       
    case('ri')
       ! Swap real and imaginary parts
        if (complex_mode) then
            call invoke_unary(swap_real_imaginary_fr)
        end if

    case('to_pol')
        ! Convert x + iy to r + i theta
        if (complex_mode) then
            zs = stack%peek(1)
            call stack%set(to_polar(zs))
        end if

    case('to_cart')
        ! Convert (r,theta) to (x,y)
        if (complex_mode) then
            zs = stack%peek(1)
            call stack%set(to_cartesian(zs))
        end if
       
    case('1/')
        call invoke_unary(reciprocal_fr)
       
    case('^2','sq')
        call invoke_unary(power_2_fr)
       
    case('^/2','sqrt')
        call invoke_unary(sqrt_fr)
        
    case('^3','cb')
        call invoke_unary(power_3_fr)
      
    case('^/3','cbrt')
        call invoke_unary(cbrt_fr)
       
    case('^*2','alog2')
        call invoke_unary(exp_2_fr)
    
    case('^*10','alog10')
        call invoke_unary(exp_10_fr)

    case('exp','alog')
        call invoke_unary(exp_e_fr)

    case('ln')
        call invoke_unary(ln_fr)

    case('log2')
        call invoke_unary(log2_fr)

    case('lg')
        call invoke_unary(lg_fr)

    case('sinh')
        call invoke_unary(hsine_fr)

    case('cosh')
        call invoke_unary(hcosine_fr)

    case('tanh')
        call invoke_unary(htangent_fr)

    case('sin')
        call invoke_unary(sine_fr)

    case('cos')
        call invoke_unary(cosine_fr)

    case('tan')
        call invoke_unary(tangent_fr)

    case('asin')
        call invoke_unary(asine_fr)

    case('asinh')
        call invoke_unary(ahsine_fr)

    case('acos')
        call invoke_unary(acosine_fr)

    case('acosh')
        call invoke_unary(ahcosine_fr)

    case('atan')
        call invoke_unary(atangent_fr)

    case('atanh')
        call invoke_unary(ahtangent_fr)

    case('atan2')
        call invoke_binary(atangent2_fr)

    case('gamma')
        call invoke_unary(gamma_fr)

    case('!')
        zs = stack%peek(1)
        if (zs%is_positive_real()) then
            call invoke_unary(fact_fr)
        else
            goto 901
        end if

    case('ncr')
        zs = stack%peek(1)
        us = stack%peek(2)
        if (zs%is_positive_real() .and. us%is_positive_real()) then
            call invoke_binary(ncr_fr)
        else
            goto 901
        end if

    case('npr')
        zs = stack%peek(1)
        us = stack%peek(2)
        if (zs%is_positive_real() .and. us%is_positive_real()) then
            call invoke_binary(npr_fr)
        else
            goto 901
        end if
       
    case('m0+','m1+','m2+','m3+','m4+','m5+','m6+','m7+','m8+','m9+')
       read(command(2:2),'(i1)',err=901) m
       mem(m) = mem(m) + stack%peek(1)

    case('m0-','m1-','m2-','m3-','m4-','m5-','m6-','m7-','m8-','m9-')
       read(command(2:2),'(i1)',err=901) m
       mem(m) = mem(m) - stack%peek(1)

    case('m0*','m1*','m2*','m3*','m4*','m5*','m6*','m7*','m8*','m9*')
       read(command(2:2),'(i1)',err=901) m
       mem(m) = mem(m) * stack%peek(1)

    case('m0/','m1/','m2/','m3/','m4/','m5/','m6/','m7/','m8/','m9/')
       read(command(2:2),'(i1)',err=901) m
       mem(m) = mem(m) / stack%peek(1)

    case('st0','st1','st2','st3','st4','st5','st6','st7','st8','st9')
       read(command(3:3),'(i1)',err=901) m
       mem(m) = stack%peek(1)

    case('sw0','sw1','sw2','sw3','sw4','sw5','sw6','sw7','sw8','sw9')
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
        write(6,'(i3,a,dt)') (i,': ',mem(i),i=0,size(mem)-1)
            
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
        print *,'apply_command: default'
        block
            integer :: lc,split_idx,end_idx
            character(len=:), allocatable :: re_comp, im_comp
            lc = len_trim(command)
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
                    print *,command//' is constant = ',x
                else if (stats%contains(command)) then
                    x = stats%get_value(command)
                    print *,command//' is stats'
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
        real(real64) :: a, b, c, sxy
        real(real64) :: s(5,2)

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
        end if
        10 format(a,i0)
        
    end subroutine calculate_stats
    
    subroutine calculate_regression(mean_x, mean_y, a, b, c, sxy)
        real(real64), intent(in) :: mean_x, mean_y
        real(real64), intent(out) :: a, b, c, sxy
        real(real64) :: sxx, syy
        sxy = sum(x_seq(1:n_seq)*y_seq(1:n_seq))/n_seq - mean_x*mean_y
        sxx = sum(x_seq(1:n_seq)*x_seq(1:n_seq))/n_seq - mean_x**2
        syy = sum(y_seq(1:n_seq)*y_seq(1:n_seq))/n_seq - mean_y**2
        a = sxy/sxx
        b = mean_y - a*mean_x
        c = sxy/sqrt(sxx*syy)
    end subroutine calculate_regression
    
    subroutine print_value(name, x, y)
        character(len=*), intent(in)  :: name
        real(real64), intent(in)           :: x
        real(real64), intent(in), optional :: y
        character(len=:), allocatable :: fmt_x, fmt_y
        call to_string(x, fmt_x)
        if (present(y)) then
            call to_string(y, fmt_y)
            fmt_x = fmt_x//' , '//fmt_y
        end if
        write(6,'(a)') name//fmt_x
    end subroutine print_value

    subroutine summary_stats(a, mean, median, stddev, lower_q, upper_q)
        real(real64), intent(in)  :: a(:)
        real(real64), intent(out) :: mean, median, stddev, lower_q, upper_q
        real(real64) :: b(size(a))
        real(real64) :: s, s2
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
        real(real64), intent(in)  :: a(:)
        integer, intent(out), optional :: mid
        real(real64) :: r
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
        real(real64), intent(inout) :: a(:)
        real(real64) :: b(size(a))
        integer :: i, j(size(a))
        logical :: mask(size(a))
        mask = .true.
        b = a
        do i=1,size(a)
            j = minloc(b, mask)
            associate (j1 => j(1))
                a(i) = b(j1)
                mask(j1) = .false.
            end associate
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
        do i=1,stack%ssize
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
    
    subroutine invoke_binary(action)
        procedure(binary_f), pointer, intent(in) :: action
        type(rpn_t) :: us, zs
        logical :: is_cart

        zs = stack%pop()
        if (complex_mode) then
            is_cart = zs%is_cartesian()
            if (.not. is_cart) then
                zs = to_cartesian(zs)
            end if
            us = stack%peek(1)
            if (.not. us%is_cartesian()) then
                us = to_polar(us)
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
        
    subroutine invoke_unary(action)
        procedure(unary_f), pointer, intent(in) :: action
        logical :: is_cart
        type(rpn_t) :: z
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

   integer function nsp(command)
     implicit none
     character(*), intent(in) :: command
     integer :: i
     do i=1,len(command)
        if (command(i:i) /= ' ') then
           nsp = i
           return
        end if
     end do
     nsp = 0
   end function nsp

end program hp15c

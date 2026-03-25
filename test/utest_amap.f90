program test_amap
    use amap
    use iso_fortran_env, only: output_unit
    implicit none
    
    type(amap_t) :: my_amap
    type(value_t) :: x
    
    write(output_unit,fmt='(a)',advance='no') 'checking set ... '
    call my_amap%set('one',1.0d0)
    call my_amap%set('two',2.d0)
    call my_amap%set('three',3.0d0)
    call my_amap%set('four',7.0d0)
    call my_amap%set('four',4.0d0)
    call my_amap%set('five',5.0d0)
    if (my_amap%size() == 5) then
        write(output_unit,'(a)') 'passed'
    else
        write(output_unit,'(a)') 'FAILED'
    end if
    
    write(output_unit,fmt='(a)',advance='no') 'checking existence ... '
    if (my_amap%contains('one') .and. .not. my_amap%contains('ten')) then
        write(output_unit,'(a)') 'passed'
    else
        write(output_unit,'(a)') 'FAILED'
    end if
    
    write(output_unit,fmt='(a)',advance='no') 'checking data ... '
    if (my_amap%get('four') == 4.0d0) then
        write(output_unit,'(a)') 'passed'
    else
        write(output_unit,'(a)') 'FAILED'
    end if

end program test_amap
